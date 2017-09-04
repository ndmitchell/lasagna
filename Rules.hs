
module Rules(checkShake, checkHLint, checkHoogle) where

import Data.List

checkShake :: String -> String -> IO ()
checkShake from to
    | '.' `notElem` from = return ()
    | "Paths_shake" == to = return ()
    | "Development.Shake.Classes" == to = return ()
    | "General." `isPrefixOf` to = return ()
    | "Development.Ninja" `isPrefixOf` from, "Development.Ninja." `isPrefixOf` to = return ()
    | "Development.Ninja" `isPrefixOf` from, "Development.Shake" `isPrefixOf` to = return ()
    | "Development.Shake" `isPrefixOf` from, "Development.Shake" `isPrefixOf` to = return ()
    | "Development.Shake.Config" == from, "Development.Ninja." `isPrefixOf` to = return ()
    | "Test" `isPrefixOf` from = return ()
    | otherwise = fail $ "Import from " ++ from ++ " to " ++ to

checkHLint :: String -> String -> IO ()
checkHLint from to
    | '.' `notElem` from = return ()
    | '.' `notElem` to = return ()
    | dot1 from == dot1 to = return ()
    | to == "HSE.All" = return ()
    | to == "Hint.All" = return ()
    | to `elem` ["Config.Type","Config.Read","Config.Compute"] = return ()
    | otherwise = fail $ "Import from " ++ from ++ " to " ++ to

checkHoogle :: String -> String -> IO ()
checkHoogle from to
    | root from || root to = return ()
    | dot1 to == "General" = return ()
    | dot1 from == "Action", dot1 to `elem` ["Input","Output"] = return ()
    | dot1 from == dot1 to = return ()
    | dot1 from == "Output", to == "Input.Item" = return ()
    | from == "General.Web", to == "Action.CmdLine" = return () -- This is a bit dodgy
    | otherwise = fail $ "Import from " ++ from ++ " to " ++ to

root = notElem '.'
dot1 = takeWhile (/= '.')


-- a.b and a.c can import each other freely
-- a can import a.b and a.c freely
