module Utils
  ( parseFile
  , parseString
  , Qualifier
  , qualifierNodes
  , fromHierarchy
  , Import(..)
  , ImpType(..)
  , splitQualifier
  , ModName
  , splitModName
  , joinModName
  , relPaths
  , modToFile
  , suffixes
  ) where

import Language.Haskell.Lexer(lexerPass0,Token(..),PosToken,line)

import Control.Monad(mplus, filterM)
import Control.Exception(evaluate)
import Data.List(intercalate,isPrefixOf,nub)
import System.Directory(doesFileExist)
import qualified System.IO as IO
import System.FilePath

data Import = Import { impMod :: ModName, impType :: ImpType }
                deriving Show

data ImpType = NormalImp | SourceImp
                deriving (Show,Eq,Ord)

-- | Get the imports of a file.
parseFile          :: FilePath -> IO (ModName,[Import])
parseFile f =
  do h <- IO.openFile f IO.ReadMode
     IO.hSetEncoding h IO.utf8
     (modName, imps) <- (parseString . get_text) `fmap` IO.hGetContents h
     _ <- evaluate (length imps) -- this is here so that the file gets closed
     IO.hClose h
     if ext == ".imports"
       then return (splitModName (takeBaseName f), imps)
       else case modName of
            -- disambiguate Main modules with no qualifiers
            (Hierarchy [],"Main") -> return (splitFilePath f,imps)
            _ -> return (modName, imps)


  where get_text txt = if ext == ".lhs" then delit txt else txt
        ext          = takeExtension f

-- | Get the imports from a string that represents a program.
parseString        :: String -> (ModName,[Import])
parseString         = parse . dropApproxCPP . dropComments . lexerPass0


-- | Drop comments, but keep {-# SOURCE #-} pragmas.
dropComments :: [PosToken] -> [PosToken]
dropComments = filter (not . skip)
  where
  skip (t, (_,txt))
    |  t == Whitespace
    || t == Commentstart
    || t == Comment
    || t == LiterateComment = True
    |  t == NestedComment   = not (isSourcePragma txt)
    | otherwise             = False


isSourcePragma :: String -> Bool
isSourcePragma txt = case words txt of
                       ["{-#", "SOURCE", "#-}"] -> True
                       _                        -> False


dropApproxCPP :: [PosToken] -> [PosToken]

 -- this is some artifact of the lexer
dropApproxCPP ((_, (_,"")) : more) = dropApproxCPP more

dropApproxCPP ((Varsym, (_,"#")) : (_, (pos,tok)) : more)
  | tok `elem` [ "if", "ifdef", "ifndef" ] = dropToEndif more
  | tok `elem` [ "include", "define", "undef" ] = dropToEOL more
  where
  dropToEndif ((Varsym, (_,"#")) : (_, (_,"endif")) : rest)
                         = dropApproxCPP rest
  dropToEndif (_ : rest) = dropToEndif rest
  dropToEndif []         = []

  dropToEOL ((_, (pos1,_)) : rest)
    | line pos == line pos1 = dropToEOL rest
  dropToEOL xs  = dropApproxCPP xs

dropApproxCPP (x : xs) = x : dropApproxCPP xs
dropApproxCPP []       = []


-- 'import' maybe_src maybe_safe optqualified maybe_pkg modid
--                                                        maybeas maybeimpspec
isImp :: [PosToken] -> Maybe (Import, [PosToken])
isImp ts = attempt (1::Int) (drop 1 ts)
  where
  attempt n toks
    -- import safe qualified "package" ModId
    | n > 4     = Nothing
    | otherwise = mplus (isMod toks) (attempt (n+1) (drop 1 toks))

  isMod ((ty, (_,x)) : xs) = case ty of
                               Conid  -> Just (toImp x,xs)
                               Qconid -> Just (toImp x,xs)
                               _      -> Nothing
  isMod _                   = Nothing

  toImp x = Import { impMod = splitModName x, impType = isSrc }
  isSrc   = case ts of
              _ : (_,(_,x)) : _ | isSourcePragma x -> SourceImp
              _                                    -> NormalImp



parse              :: [PosToken] -> (ModName,[Import])
parse ((Reservedid,(_,"module")) : (_,(_,m)) : is) =
                                                  (splitModName m,imports is)
parse is            = ((Hierarchy [],"Main"),imports is)
-- TODO: special handling for Main modules, 
-- to disambiguate multiple main Modules in a single project

imports            :: [PosToken] -> [Import]
imports ts          = case isImp $ dropWhile (not . (("import" ==) . snd . snd)) ts of
                        Just (x,xs) -> x : imports xs
                        _           -> []

-- | A hierarchical module name.
-- We make this an opaque type with accessors 'qualifierNodes' and 'fromHierarchy' 
-- so that we can transparently add new structure to this type.
data Qualifier = Hierarchy [String]
    | FromFile [String] deriving (Show)
qualifierNodes :: Qualifier -> [String]
qualifierNodes (Hierarchy qs) = qs
qualifierNodes (FromFile qs) = qs
fromHierarchy :: [String] -> Qualifier
fromHierarchy = Hierarchy

type ModName        = (Qualifier,String)

-- | Convert a string name into a hierarchical name qualifier.
splitQualifier     :: String -> Qualifier
splitQualifier cs = case break ('.'==) cs of
                        (xs,_:ys)  -> let Hierarchy qs = splitQualifier ys
                            in Hierarchy (xs:qs)
                        _          -> Hierarchy [cs]

-- | The 'Qualifier' for a Main module is the path leading to it, 
-- the module name is the file's basename, which is Main in typical cases.  
splitFilePath :: FilePath -> ModName
splitFilePath path = let (d,f) = splitFileName path
    in (FromFile . splitDirectories . takeDirectory $ d, dropExtensions f)

-- | Convert a string name into a hierarchical name.
-- It is important that 
-- 
-- @
-- f `elem` (('relPaths' . 'splitFilePath') f)
-- @
splitModName       :: String -> ModName
splitModName cs     = case break ('.'==) cs of
                        (xs,_:ys)  -> let (Hierarchy as,bs) = splitModName ys
                                   in (Hierarchy (xs:as),bs)
                        _ -> (Hierarchy [],cs)

joinModName        :: ModName -> String
joinModName (q,y)  = intercalate "." (qualifierNodes q ++ [y])

-- | The files in which a module might reside.
relPaths           :: ModName -> [FilePath]
relPaths (q,y)     = [ prefix ++ suffix | suffix <- suffixes ]
  where prefix      = foldr (</>) y (qualifierNodes q)

suffixes           :: [String]
suffixes            = [".hs",".lhs", ".imports"]

-- | The files in which a module might reside.
-- We report only files that exist.
modToFile          :: [FilePath] -> ModName -> IO [FilePath]
modToFile dirs m    = nub `fmap` filterM doesFileExist paths
  where
  paths             = [ d </> r | d <- dirs, r <- relPaths m ]


delit :: String -> String
delit txt = unlines $ bird $ lines txt
  where
  bird (('>' : cs) : ls)  = (' ' : cs) : bird ls
  bird (l : ls)
    | "\\begin{code}" `isPrefixOf` l  = in_code ls
    | otherwise                       = bird ls
  bird []                             = []

  in_code (l : ls)
    | "\\end{code}" `isPrefixOf` l    = bird ls
    | otherwise                       = l : in_code ls
  in_code []                          = []    -- unterminated code...



