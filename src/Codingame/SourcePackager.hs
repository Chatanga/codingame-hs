{-# LANGUAGE OverloadedStrings, LambdaCase #-}

{-|
A small module to create monolithic sources from multiples files.
-}
module Codingame.SourcePackager
    ( createMonolithicSource
    , createMonolithicSourceWithMode
    ) where

import Control.Arrow ( Arrow((&&&)) )
import Control.Monad ( foldM )
import Control.Monad.Trans.Except ( runExcept )
import Control.Exception ( tryJust )
import Data.ByteString.Char8 ( pack, unpack )
import Data.Function ( (&) )
import Data.List ( nub, nubBy )
import Hpp
    ( addDefinition,
      emptyHppState,
      expand,
      preprocess,
      HppOutput(HppOutput),
      HppState )
import qualified Hpp.Config as C
import qualified Hpp.Types as T
import qualified Data.Map.Lazy as Map
import Data.Ord ( comparing )
import qualified Data.Text as Text
import Language.Haskell.Exts
    ( defaultParseMode,
      getTopPragmas,
      parseModuleWithMode,
      prettyPrint,
      Extension(EnableExtension),
      KnownExtension(..),
      ParseMode(extensions),
      ParseResult(ParseFailed, ParseOk),
      SrcLoc(srcFilename, srcLine),
      SrcSpanInfo,
      Decl(PatBind, TypeSig, FunBind),
      ImportDecl(importModule),
      Match(InfixMatch, Match),
      Module(Module),
      ModuleHead(ModuleHead),
      ModuleName(..),
      ModulePragma(LanguagePragma),
      Name(Ident),
      Pat(PVar) )
import System.FilePath ( (<.>), (</>), takeDirectory )
import System.IO.Error ( isDoesNotExistError )

----------------------------------------------------------------------------------------------------

type ModuleSourceMap = Map.Map FilePath ([ModulePragma SrcSpanInfo], Module SrcSpanInfo)

{- | Create a monolithic source by concatenating a Haskell source file and all its local
dependencies.

The result is not just a string concatenation and source files are actually parsed and processed
such as the returned source forms a valid Haskell content. Only the local content is incorporated in
the resulting source, whereas the external modules are simply kept as imported modules (eg. the
source code of \"System.IO\" won’t be included). An internal module is a module whose source file
can be found in the same hierachical directory as the initial file provided. Per instance, when
creating a monolithic source from \"src\/Machin.hs\", the source of an imported module named
\"Truc.Bidule\" will be searched in \"src\/Truc\". These transformations remains rather simple and
won't be able to solve any name clash between functions, nor handle incompatible qualified and/or
hidden import directives.

Since this source concatenation remains pretty basic, processing directives are now supported to
help keeping the generated source compatible with Codingame, mainly by prunning any unwanted
dependencies (the CG_ARENA identifier is automatically defined). This way, it becomes possible to
better integrate a bot with a development environment.

Lastly, any function named \"runMain\" will be renamed \"main\". It allows you to generate a new
valid program by only selecting a subset of your code base which uses another main function.
-}
createMonolithicSource :: FilePath -> IO String
createMonolithicSource = createMonolithicSourceWithMode defaultParseMode

{- | Create a monolithic source by concatenating a Haskell source file and all its local
dependencies.

This function works the same way as createMonolithicSource but offer a way to customize the parse
mode in order to deal with some parsing limitations. Indeed files are only parsed separately, not
compiled as a whole and some things are beyond the parser capability, such as operator fixities.
If you are defining you own operators in a file and use then into another, you need to declare them
by overriding the parse mode:

@
let importedFixities = infixl_ 8 [\"|>\"]
    fixities' = importedFixities ++ fromMaybe [] (fixities defaultParseMode)
    parseMode = defaultParseMode{ fixities = Just fixities' }

source <- createMonolithicSourceWithMode parseMode \"src/Golgoth.hs\"
@
 -}
createMonolithicSourceWithMode :: ParseMode -> FilePath -> IO String
createMonolithicSourceWithMode parseMode sourceFile = do
    moduleSourceMap <- processModule parseMode Map.empty sourceFile

    let contributions = fmap (getContribution moduleSourceMap) (Map.assocs moduleSourceMap)
        srcLoc = error "no srcLoc"
        pragmas = mergePragmas (fmap fst contributions)
        importDecls = mergeImportDecls (fmap (fst . snd) contributions)
        decls = fmap patchRunMain (concatMap (snd . snd) contributions)
        moduleHead = Nothing
        mergedCode = Module srcLoc moduleHead pragmas importDecls decls

    return (prettyPrint mergedCode)

getContribution :: ModuleSourceMap
    -> (FilePath, ([ModulePragma SrcSpanInfo], Module SrcSpanInfo))
    -> ([ModulePragma SrcSpanInfo], ([ImportDecl SrcSpanInfo], [Decl SrcSpanInfo]))
getContribution moduleSourceMap (sourceFile, moduleSource) =
    (fst moduleSource, (getExternalImportDecls importDecls, decls))
    where
        (Module _ (Just (ModuleHead _ moduleName _ _)) _ importDecls decls) =
            snd moduleSource
        srcDir = getSrcDir sourceFile (getModuleName moduleName)
        getExternalImportDecls importDecls = importDecls
            & fmap (id &&& (locateImport srcDir . getModuleName . importModule))
            & filter (not . flip Map.member moduleSourceMap . snd)
            & fmap fst

processModule :: ParseMode -> ModuleSourceMap -> FilePath -> IO ModuleSourceMap
processModule parseMode moduleSourceMap sourceFile =
    if Map.member sourceFile moduleSourceMap
        then return moduleSourceMap
        else readAndPreProcessFile sourceFile >>= parseModuleSource parseMode moduleSourceMap sourceFile

processInternalModule :: ParseMode -> ModuleSourceMap -> FilePath -> IO ModuleSourceMap
processInternalModule parseMode moduleSourceMap sourceFile = do
    let selectDoesNotExistError e = if isDoesNotExistError e then Just e else Nothing
    result <- tryJust selectDoesNotExistError (processModule parseMode moduleSourceMap sourceFile)
    case result of
        Left _ -> return moduleSourceMap
        Right moduleSourceMap' -> return moduleSourceMap'

parseModuleSource :: ParseMode -> ModuleSourceMap -> FilePath -> String -> IO ModuleSourceMap
parseModuleSource parseMode moduleSourceMap sourceFile source = do
    let pragmas = case getTopPragmas source of
            ParseOk pragmas
                -> pragmas
            ParseFailed srcLoc message
                -> error (message ++ "\nAt: " ++ show srcLoc{ srcFilename = sourceFile } ++ "\n" ++ dumpSource (srcLine srcLoc))
        enabledLanguageExtensions = flip concatMap pragmas $ \case
            LanguagePragma _ names -> map toExtension names
            _ -> []
        moduleSource = case parseModuleWithMode parseMode{ extensions = enabledLanguageExtensions } source of
            ParseOk moduleSource
                -> moduleSource
            ParseFailed srcLoc message
                -> error (message ++ "\nAt: " ++ show srcLoc{ srcFilename = sourceFile } ++ "\n" ++ dumpSource (srcLine srcLoc))

        -- TOREDO To work around the fact that SrcLoc becomes useless once HPP has been run on the source.
        dumpSource lineNumber =
            let radius = 5
                start = max 1 (lineNumber - radius)
            in  concat $ zipWith (\n line -> show n ++ "\t" ++ line ++ "\n") [start..] (take (radius * 2 + 1) . drop (start - 1) $ lines source)

        dependencies = getDependencies sourceFile moduleSource

        -- Reserve a slot for the module with an undefined value to avoid recursion.
        moduleSourceMap' = Map.insert sourceFile undefined moduleSourceMap

    moduleSourceMap'' <-
        foldM (processInternalModule parseMode) moduleSourceMap' (fmap snd dependencies)

    return $ Map.insert sourceFile (pragmas, moduleSource) moduleSourceMap''

-- TOREDO I really don’t like this.
toExtension :: Name SrcSpanInfo -> Extension
toExtension (Ident _ "OverlappingInstances") = EnableExtension OverlappingInstances
toExtension (Ident _ "UndecidableInstances") = EnableExtension UndecidableInstances
toExtension (Ident _ "IncoherentInstances") = EnableExtension IncoherentInstances
toExtension (Ident _ "InstanceSigs") = EnableExtension InstanceSigs
toExtension (Ident _ "DoRec") = EnableExtension DoRec
toExtension (Ident _ "RecursiveDo") = EnableExtension RecursiveDo
toExtension (Ident _ "ParallelListComp") = EnableExtension ParallelListComp
toExtension (Ident _ "MultiParamTypeClasses") = EnableExtension MultiParamTypeClasses
toExtension (Ident _ "MonomorphismRestriction") = EnableExtension MonomorphismRestriction
toExtension (Ident _ "FunctionalDependencies") = EnableExtension FunctionalDependencies
toExtension (Ident _ "Rank2Types") = EnableExtension Rank2Types
toExtension (Ident _ "RankNTypes") = EnableExtension RankNTypes
toExtension (Ident _ "PolymorphicComponents") = EnableExtension PolymorphicComponents
toExtension (Ident _ "ExistentialQuantification") = EnableExtension ExistentialQuantification
toExtension (Ident _ "ScopedTypeVariables") = EnableExtension ScopedTypeVariables
toExtension (Ident _ "PatternSignatures") = EnableExtension PatternSignatures
toExtension (Ident _ "ImplicitParams") = EnableExtension ImplicitParams
toExtension (Ident _ "FlexibleContexts") = EnableExtension FlexibleContexts
toExtension (Ident _ "FlexibleInstances") = EnableExtension FlexibleInstances
toExtension (Ident _ "EmptyDataDecls") = EnableExtension EmptyDataDecls
toExtension (Ident _ "CPP") = EnableExtension CPP
toExtension (Ident _ "KindSignatures") = EnableExtension KindSignatures
toExtension (Ident _ "BangPatterns") = EnableExtension BangPatterns
toExtension (Ident _ "TypeSynonymInstances") = EnableExtension TypeSynonymInstances
toExtension (Ident _ "TemplateHaskell") = EnableExtension TemplateHaskell
toExtension (Ident _ "ForeignFunctionInterface") = EnableExtension ForeignFunctionInterface
toExtension (Ident _ "Arrows") = EnableExtension Arrows
toExtension (Ident _ "Generics") = EnableExtension Generics
toExtension (Ident _ "ImplicitPrelude") = EnableExtension ImplicitPrelude
toExtension (Ident _ "NamedFieldPuns") = EnableExtension NamedFieldPuns
toExtension (Ident _ "PatternGuards") = EnableExtension PatternGuards
toExtension (Ident _ "GeneralizedNewtypeDeriving") = EnableExtension GeneralizedNewtypeDeriving
toExtension (Ident _ "DeriveAnyClass") = EnableExtension DeriveAnyClass
toExtension (Ident _ "ExtensibleRecords") = EnableExtension ExtensibleRecords
toExtension (Ident _ "RestrictedTypeSynonyms") = EnableExtension RestrictedTypeSynonyms
toExtension (Ident _ "HereDocuments") = EnableExtension HereDocuments
toExtension (Ident _ "MagicHash") = EnableExtension MagicHash
toExtension (Ident _ "BinaryLiterals") = EnableExtension BinaryLiterals
toExtension (Ident _ "TypeFamilies") = EnableExtension TypeFamilies
toExtension (Ident _ "StandaloneDeriving") = EnableExtension StandaloneDeriving
toExtension (Ident _ "UnicodeSyntax") = EnableExtension UnicodeSyntax
toExtension (Ident _ "UnliftedFFITypes") = EnableExtension UnliftedFFITypes
toExtension (Ident _ "LiberalTypeSynonyms") = EnableExtension LiberalTypeSynonyms
toExtension (Ident _ "TypeOperators") = EnableExtension TypeOperators
toExtension (Ident _ "ParallelArrays") = EnableExtension ParallelArrays
toExtension (Ident _ "RecordWildCards") = EnableExtension RecordWildCards
toExtension (Ident _ "RecordPuns") = EnableExtension RecordPuns
toExtension (Ident _ "DisambiguateRecordFields") = EnableExtension DisambiguateRecordFields
toExtension (Ident _ "OverloadedStrings") = EnableExtension OverloadedStrings
toExtension (Ident _ "GADTs") = EnableExtension GADTs
toExtension (Ident _ "MonoPatBinds") = EnableExtension MonoPatBinds
toExtension (Ident _ "RelaxedPolyRec") = EnableExtension RelaxedPolyRec
toExtension (Ident _ "ExtendedDefaultRules") = EnableExtension ExtendedDefaultRules
toExtension (Ident _ "UnboxedTuples") = EnableExtension UnboxedTuples
toExtension (Ident _ "DeriveDataTypeable") = EnableExtension DeriveDataTypeable
toExtension (Ident _ "ConstrainedClassMethods") = EnableExtension ConstrainedClassMethods
toExtension (Ident _ "PackageImports") = EnableExtension PackageImports
toExtension (Ident _ "LambdaCase") = EnableExtension LambdaCase
toExtension (Ident _ "EmptyCase") = EnableExtension EmptyCase
toExtension (Ident _ "ImpredicativeTypes") = EnableExtension ImpredicativeTypes
toExtension (Ident _ "NewQualifiedOperators") = EnableExtension NewQualifiedOperators
toExtension (Ident _ "PostfixOperators") = EnableExtension PostfixOperators
toExtension (Ident _ "QuasiQuotes") = EnableExtension QuasiQuotes
toExtension (Ident _ "TransformListComp") = EnableExtension TransformListComp
toExtension (Ident _ "ViewPatterns") = EnableExtension ViewPatterns
toExtension (Ident _ "XmlSyntax") = EnableExtension XmlSyntax
toExtension (Ident _ "RegularPatterns") = EnableExtension RegularPatterns
toExtension (Ident _ "TupleSections") = EnableExtension TupleSections
toExtension (Ident _ "GHCForeignImportPrim") = EnableExtension GHCForeignImportPrim
toExtension (Ident _ "NPlusKPatterns") = EnableExtension NPlusKPatterns
toExtension (Ident _ "DoAndIfThenElse") = EnableExtension DoAndIfThenElse
toExtension (Ident _ "RebindableSyntax") = EnableExtension RebindableSyntax
toExtension (Ident _ "ExplicitForAll") = EnableExtension ExplicitForAll
toExtension (Ident _ "DatatypeContexts") = EnableExtension DatatypeContexts
toExtension (Ident _ "MonoLocalBinds") = EnableExtension MonoLocalBinds
toExtension (Ident _ "DeriveFunctor") = EnableExtension DeriveFunctor
toExtension (Ident _ "DeriveGeneric") = EnableExtension DeriveGeneric
toExtension (Ident _ "DeriveTraversable") = EnableExtension DeriveTraversable
toExtension (Ident _ "DeriveFoldable") = EnableExtension DeriveFoldable
toExtension (Ident _ "NondecreasingIndentation") = EnableExtension NondecreasingIndentation
toExtension (Ident _ "InterruptibleFFI") = EnableExtension InterruptibleFFI
toExtension (Ident _ "CApiFFI") = EnableExtension CApiFFI
toExtension (Ident _ "JavaScriptFFI") = EnableExtension JavaScriptFFI
toExtension (Ident _ "ExplicitNamespaces") = EnableExtension ExplicitNamespaces
toExtension (Ident _ "DataKinds") = EnableExtension DataKinds
toExtension (Ident _ "PolyKinds") = EnableExtension PolyKinds
toExtension (Ident _ "MultiWayIf") = EnableExtension MultiWayIf
toExtension (Ident _ "SafeImports") = EnableExtension SafeImports
toExtension (Ident _ "Safe") = EnableExtension Safe
toExtension (Ident _ "Trustworthy") = EnableExtension Trustworthy
toExtension (Ident _ "DefaultSignatures") = EnableExtension DefaultSignatures
toExtension (Ident _ "ConstraintKinds") = EnableExtension ConstraintKinds
toExtension (Ident _ "RoleAnnotations") = EnableExtension RoleAnnotations
toExtension (Ident _ "PatternSynonyms") = EnableExtension PatternSynonyms
toExtension (Ident _ "PartialTypeSignatures") = EnableExtension PartialTypeSignatures
toExtension (Ident _ "NamedWildCards") = EnableExtension NamedWildCards
toExtension (Ident _ "TypeApplications") = EnableExtension TypeApplications
toExtension (Ident _ "TypeFamilyDependencies") = EnableExtension TypeFamilyDependencies
toExtension (Ident _ "OverloadedLabels") = EnableExtension OverloadedLabels
toExtension (Ident _ "DerivingStrategies") = EnableExtension DerivingStrategies
toExtension (Ident _ "UnboxedSums") = EnableExtension UnboxedSums
toExtension (Ident _ "TypeInType") = EnableExtension TypeInType
toExtension (Ident _ "Strict") = EnableExtension Strict
toExtension (Ident _ "StrictData") = EnableExtension StrictData
toExtension (Ident _ "DerivingVia") = EnableExtension DerivingVia
toExtension (Ident _ "QuantifiedConstraints") = EnableExtension QuantifiedConstraints
toExtension (Ident _ "BlockArguments") = EnableExtension BlockArguments
toExtension name = error $ "Unknown extension: " ++ show name

getDependencies :: FilePath -> Module SrcSpanInfo -> [(ImportDecl SrcSpanInfo, FilePath)]
getDependencies sourceFile moduleSource = fmap (id &&& getLocation) importDecls
    where
        (Module _ (Just (ModuleHead _ (ModuleName _ moduleName) _ _)) exportSpec importDecls decls) =
            moduleSource
        srcDir = getSrcDir sourceFile moduleName
        getLocation = locateImport srcDir . getModuleName . importModule

getSrcDir :: FilePath -> String -> FilePath
getSrcDir sourceFile moduleName = srcDir where
    parents = moduleName
        & fmap Text.unpack . Text.splitOn (Text.pack ".") . Text.pack
    srcDir = iterate takeDirectory sourceFile !! length parents

locateImport :: FilePath -> String -> FilePath
locateImport srcDir importedModuleName = importedSourceFile where
    (parents, child) = importedModuleName
        & fmap Text.unpack . Text.splitOn (Text.pack ".") . Text.pack
        & (init &&& last)
    importedSourceFile = foldl (</>) srcDir parents </> child <.> ".hs"

mergePragmas :: [[ModulePragma SrcSpanInfo]] -> [ModulePragma SrcSpanInfo]
mergePragmas allPragmas = nub $ concat allPragmas -- TODO Works, but not elaborated enough.

mergeImportDecls :: [[ImportDecl SrcSpanInfo]] -> [ImportDecl SrcSpanInfo]
mergeImportDecls decls =
    -- TODO module modifiers (hiding, qualified) are not merged!
    nubBy (\d1 d2 -> (EQ == ) $ comparing (getModuleName . importModule) d1 d2) (concat decls)

getModuleName :: ModuleName SrcSpanInfo -> String
getModuleName (ModuleName _ moduleName) = moduleName

patchRunMain :: Decl SrcSpanInfo -> Decl SrcSpanInfo
patchRunMain decl = case decl of
    (TypeSig srcLoc names t) -> TypeSig srcLoc (fmap patchName names) t
    (FunBind srcLoc matchs) -> FunBind srcLoc (fmap patchMatch matchs)
    (PatBind srcLoc pat rhs binds) -> PatBind srcLoc (patchPat pat) rhs binds
    _ -> decl
    where
        patchName (Ident srcLoc "runMain") = Ident srcLoc "main"
        patchName name = name

        patchMatch (Match srcLoc name pats rhs binds) =
            Match srcLoc (patchName name) pats rhs binds
        patchMatch (InfixMatch srcLoc pat name pats rhs binds) =
            InfixMatch srcLoc pat (patchName name) pats rhs binds

        patchPat (PVar srcLoc name) = PVar srcLoc (patchName name)
        patchPat pat = pat

hppConfig :: HppState -> HppState
hppConfig = T.over T.config opts
  where opts = T.setL C.inhibitLinemarkersL True

readAndPreProcessFile :: FilePath -> IO String
readAndPreProcessFile filePath =
    case addDefinition "CG_ARENA" "1" (hppConfig emptyHppState) of
        Just state -> readFile filePath >>= hpp (hppConfig state)
        Nothing -> error "Preprocessor definition did not parse"

hpp :: HppState -> String -> IO String
hpp st src =
    case runExcept (expand st (preprocess (map pack (lines src)))) of
        Left e -> error ("Error running hpp: " ++ show e)
        Right (HppOutput _ ls, _) -> return (unlines (map unpack ls))
