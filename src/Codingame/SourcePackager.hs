{-# LANGUAGE OverloadedStrings #-}

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
import Data.ByteString.Char8 (pack, unpack)
import Data.Function ((&))
import Data.List (isPrefixOf, nubBy)
import Hpp
    ( addDefinition,
      emptyHppState,
      expand,
      preprocess,
      HppOutput(HppOutput),
      HppState )
import qualified Data.Map.Lazy as Map
import Data.Ord ( comparing )
import qualified Data.Set as Set
import qualified Data.Text as Text
import Language.Haskell.Exts
    ( defaultParseMode,
      parseModuleWithMode,
      prettyPrint,
      ParseMode(..),
      ParseResult(ParseFailed, ParseOk),
      SrcLoc(srcFilename, srcLine),
      SrcSpanInfo,
      Decl(PatBind, TypeSig, FunBind),
      ImportDecl(importModule),
      Match(InfixMatch, Match),
      Module(Module),
      ModuleHead(ModuleHead),
      ModuleName(..),
      Name(Ident),
      Pat(PVar) )
import System.FilePath ( (<.>), (</>), takeDirectory )
import System.IO.Error ( isDoesNotExistError )

----------------------------------------------------------------------------------------------------

type ModuleSourceMap = Map.Map FilePath (Module SrcSpanInfo)

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

    let rewrittenModuleSourceMap = Map.map rewriteModule moduleSourceMap

    let contributions = fmap (getContribution moduleSourceMap) (Map.assocs moduleSourceMap)
        srcLoc = error "no srcLoc"
        pragmas = []
        warningText = Nothing
        exportSpec = Nothing
        importDecls = mergeImportDecls (fmap fst contributions)
        decls = fmap patchRunMain (concatMap snd contributions)
        -- moduleHead = Just (ModuleHead noSrcSpan (ModuleName noSrcSpan "MainTest") warningText exportSpec)
        moduleHead = Nothing
        mergedCode = Module srcLoc moduleHead pragmas importDecls decls

    return (prettyPrint mergedCode)

getContribution :: ModuleSourceMap -> (FilePath, Module SrcSpanInfo) -> ([ImportDecl SrcSpanInfo], [Decl SrcSpanInfo])
getContribution moduleSourceMap (sourceFile, moduleSource) = (getExternalImportDecls importDecls, decls)
    where
        (Module _ (Just (ModuleHead _ moduleName _ _)) exportSpec importDecls decls) =
            moduleSource
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
parseModuleSource parseMode moduleSourceMap sourceFile rawSource = do
    -- HPP (which is not CPP) add its own directive in the output…
    let source = unlines (filter (not . isPrefixOf "#") (lines rawSource))
    -- Extensions need to be provided by the user this way.
    let moduleSource = case parseModuleWithMode parseMode source of
            ParseOk moduleSource
                -> moduleSource
            ParseFailed srcLoc message
                -- Line number is useless once cpp has been run on the source.
                -> error (message ++ "\nAt: " ++ show srcLoc{ srcFilename = sourceFile } ++ "\n" ++ dumpSource (srcLine srcLoc))

        dumpSource lineNumber =
            let radius = 5
                start = max 1 (lineNumber - radius)
            in  concat $ zipWith (\n line -> show n ++ "\t" ++ line ++ "\n") [start..] (take (radius * 2 + 1) . drop (start - 1) $ lines source)

        dependencies = getDependencies sourceFile moduleSource

        -- Reserve a slot for the module with an undefined value to avoid recursion.
        moduleSourceMap' = Map.insert sourceFile undefined moduleSourceMap

    moduleSourceMap'' <-
        foldM (processInternalModule parseMode) moduleSourceMap' (fmap snd dependencies)

    return $ Map.insert sourceFile moduleSource moduleSourceMap''

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

mergeImportDecls :: [[ImportDecl SrcSpanInfo]] -> [ImportDecl SrcSpanInfo]
mergeImportDecls decls =
    nubBy (\d1 d2 -> (EQ == ) $ comparing (getModuleName . importModule) d1 d2) (concat decls)

getModuleName :: ModuleName SrcSpanInfo -> String
getModuleName (ModuleName _ moduleName) = moduleName

rewriteModule :: Module SrcSpanInfo -> Module SrcSpanInfo
rewriteModule moduleSource = moduleSource

findUsageFromMain :: ModuleSourceMap -> Set.Set (Decl SrcSpanInfo)
findUsageFromMain = undefined

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

readAndPreProcessFile :: FilePath -> IO String
readAndPreProcessFile filePath = do
    case addDefinition "CG_ARENA" "1" emptyHppState of
        Just state -> readFile filePath >>= hpp state
        Nothing -> error "Preprocessor definition did not parse"

hpp :: HppState -> String -> IO String
hpp st src =
  case runExcept (expand st (preprocess (map pack (lines src)))) of
    Left e -> error ("Error running hpp: " ++ show e)
    Right (HppOutput _ ls, _) -> return (unlines (map unpack ls))
