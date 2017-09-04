
module Parser(parseModule) where

import Data.Tuple.Extra
import Data.List.Extra
import Data.Char


parseModule :: String -> (String, [String])
parseModule = f . words
    where
        f ("module":xs) | Just (x,xs) <- mname xs = (x, snd $ f xs)
        f ("import":xs) | Just (x,xs) <- mname $ gunk xs = second (x:) $ f xs
        f (x:xs) = f xs
        f [] = ("Main", [])

        mname (x:xs) | Just (c,_) <- uncons x, isUpper c = Just (takeWhile (/= '(') x, xs)
        mname _ = Nothing

        gunk ("qualified":xs) = gunk xs -- import qualified ...
        gunk (('"':_):xs) = gunk xs -- import "package" ...
        gunk xs = xs
