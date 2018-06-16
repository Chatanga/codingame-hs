{-|
A small module to create monolithic sources from multiples files.
-}
module Codingame.SourcePackager
    ( createMonolithicSource
    , createMonolithicSourceWithMode
    ) where

import Control.Exception
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function ((&))
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import qualified Data.Text as Text
import Language.Haskell.Exts
-- import Language.Haskell.Exts.Parser
import Prelude hiding (catch)
import System.FilePath
import System.IO
import System.IO.Error hiding (catch)

import Codingame.Debug

----------------------------------------------------------------------------------------------------

type ModuleSourceMap = Map.Map FilePath ([ImportDecl SrcSpanInfo], [Decl SrcSpanInfo])

{- | Create a monolithic source by concatenating a Haskell source file and all its local
dependencies.

The result is not just a string concatenation and source files are actually parsed and processed
such as the returned source forms a valid Haskell content. Only the local content is incorporated in
the resulting source, whereas the external modules are simply kept as imported modules (eg. the
source code of \"System.IO\" won’t be included). An internal module is a module whose source file
can be found in the same hierachical directory as the initial file provided. Per instance, when
creating a monolithic source from \"src\/Machin.hs\", the source of an imported module named
\"Truc.Bidule\" will be searched in \"src\/Truc\".

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

    let contributions = Map.elems moduleSourceMap :: [([ImportDecl SrcSpanInfo], [Decl SrcSpanInfo])]
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

processModule :: ParseMode -> ModuleSourceMap -> FilePath -> IO ModuleSourceMap
processModule parseMode moduleSourceMap sourceFile =
    if Map.member sourceFile moduleSourceMap
    then return moduleSourceMap
    else readFile sourceFile >>= parseModuleSource parseMode moduleSourceMap sourceFile

processInternalModule :: ParseMode -> ModuleSourceMap -> FilePath -> IO ModuleSourceMap
processInternalModule parseMode moduleSourceMap sourceFile = do
    let selectDoesNotExistError e = if isDoesNotExistError e then Just e else Nothing
    result <- tryJust selectDoesNotExistError (processModule parseMode moduleSourceMap sourceFile)
    case result of
        Left _ -> return moduleSourceMap
        Right moduleSourceMap' -> return moduleSourceMap'

parseModuleSource :: ParseMode -> ModuleSourceMap -> FilePath -> String -> IO ModuleSourceMap
parseModuleSource parseMode moduleSourceMap sourceFile source = do
    let moduleCode = case parseModuleWithMode parseMode source of
            ParseOk moduleCode
                -> moduleCode
            ParseFailed srcLoc message
                -> error (message ++ "\nAt: " ++ show srcLoc{ srcFilename = sourceFile })

        (Module _ (Just (ModuleHead _ (ModuleName _ moduleName) pragmas warningText)) exportSpec importDecls decls) =
            moduleCode

        srcDir = getSrcDir sourceFile moduleName

        dependencies :: [(ImportDecl SrcSpanInfo, FilePath)]
        dependencies =
            fmap (id &&& (locateImport srcDir . getModuleName . importModule)) importDecls

        -- Reserve a slot for the module with an undefined value to avoid recursion.
        moduleSourceMap' = Map.insert sourceFile (undefined, decls) moduleSourceMap

    moduleSourceMap'' <-
        foldM (processInternalModule parseMode) moduleSourceMap' (fmap snd dependencies)

    let externalImportDecls = dependencies
            & filter (not . flip Map.member moduleSourceMap'' . snd)
            & fmap fst
        -- Fill the reserved slot with a proper value.
        moduleSourceMap''' = Map.insert sourceFile (externalImportDecls, decls) moduleSourceMap''

    return moduleSourceMap'''

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
