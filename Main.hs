
module Main(main) where

import Parser
import Rules
import System.Directory.Extra
import System.IO.Extra


main = do
    test "C:/Neil/hoogle/src" checkHoogle
    test "C:/Neil/hlint/src" checkHLint
    test "C:/Neil/shake/src" checkShake
    putStrLn "done"

test src checkRules = do
    files <- listFilesRecursive src
    imports <- mapM (fmap parseModule . readFile') files
    writeFile "imports.txt" $ unlines $ map show imports
    mapM_ (uncurry checkRules) [(a,b) | (a,bs) <- imports, b <- bs, b `elem` map fst imports]
