module Main where

import System.Environment
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.String as L
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Token
import Control.Monad
import Data.Maybe
import Data.List
import Control.Applicative ((<$>))

-- fetches a package with all its direct dependencies
main = do
 [p, maxdepth'] <- getArgs
 r <- L.parseFromFile parsePackages "Packages"
 case r of
    Right packages ->
        do
          putStrLn $ "Read " ++ show (length packages) ++ " packages."
          let files = reverse $ filename `map` resolve_deps [p] (read maxdepth') packages
          putStrLn $ "Need to fetch " ++ show (length files) ++ " packages."
          mapM_ putStrLn files
    Left e ->
        putStrLn $ show e

resolve_deps :: (Integral t) =>  [String] -> t -> [Pkg] -> [Pkg]
resolve_deps names maxd packageDb = resolve_deps_impl names packageDb [] maxd
     
resolve_deps_impl :: (Integral t) =>
                     [String] -> [Pkg] -> [Pkg] -> t -> [Pkg]
resolve_deps_impl _  _  acc 0  = acc
resolve_deps_impl [] _  acc _  = acc
resolve_deps_impl _  [] acc _  = acc
resolve_deps_impl names packageDb acc (n+1) = 
    let 
        (foundPkgs, packageDb') = lookup_partition packageDb names
        depNames = join $ depends <$> foundPkgs
        acc' = foundPkgs ++ acc 
    in 
      resolve_deps_impl depNames packageDb' acc' n

lookup_pkg :: [Pkg] -> String -> Pkg
lookup_pkg pkgs name = let Just foundPkg = find ((==name) . package) pkgs in foundPkg

lookup_partition :: [Pkg] -> [String] -> ([Pkg], [Pkg])
lookup_partition pdb = foldr (\name (pkgs, pdb') -> 
                                  let (ps, pdbWOp) = partition ((==name) . package) pdb'
                                  in (ps ++ pkgs, pdbWOp)) ([], pdb)

type Version = String

data Pkg = Pkg { package :: String
              , filename :: String
              , version :: Version
              , depends :: [String]}
          deriving (Show, Eq)

toPkg :: [(String, String)] -> Pkg
toPkg kvs = Pkg
           (fromJust ("Package"  `lookup` kvs))
           (fromJust ("Filename" `lookup` kvs))
           (fromJust ("Version"  `lookup` kvs))
           (parsePkgDeps ("Depends" `lookup` kvs))
   where
     parsePkgDeps Nothing = []
     parsePkgDeps (Just str) = parseDeps str

parseDeps :: String -> [String]
parseDeps str = case parse parseD ("Depends " ++ str) str of
                 Right ds -> ds
                 Left e -> error $ show e
   where parseD = ((many (noneOf ",\n")) `sepBy` (char ',')) >>= (return . map (head . words))


parsePackages =  keyValueBlocks >>= return . map toPkg

keyValueBlocks = many newline >> many1 keyValueLine `sepEndBy` newline

keyValueLine = do
 key <- manyTill (noneOf "\n") (char ':')
 many space
 v1 <- manyTill anyChar eol
 vs <- (many1 (char ' ' >> tillEol)) <|> return []
 return (key, join $ intersperse "\n" (v1:vs) )

tillEol = manyTill anyChar eol

eol = newline <|> (eof >> return '\n')