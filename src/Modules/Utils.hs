module Modules.Utils (
    Dict (..), containsValue, containsKey, extract, extractOrElse
) where

data Dict a b = Dict [(a, b)]

containsValue :: Eq b => Dict a b -> b -> Bool
containsValue (Dict []) _ = False
containsValue (Dict ((_, b1):rest)) b
    | b1 == b      = True
    | otherwise    = containsValue (Dict rest) b

containsKey :: Eq a => Dict a b -> a -> Bool
containsKey (Dict []) _ = False
containsKey (Dict ((a1, _):rest)) a
    | a1 == a    = True
    | otherwise  = containsKey (Dict rest) a

extract :: Eq a => Dict a b -> a -> Maybe b
extract (Dict []) _    = Nothing
extract (Dict ((a1, b1):rest)) a
    | a1 == a   = Just b1
    | otherwise = extract (Dict rest) a

extractOrElse :: Eq a => Dict a b -> a -> b -> b
extractOrElse (Dict []) _ b' = b'
extractOrElse (Dict ((a1, b1):rest)) a b'
    | a1 == a         = b1
    | otherwise       = extractOrElse (Dict rest) a b'

instance Functor (Dict a) where
    fmap f (Dict entries) = Dict $ fmap (\(a1, b1) -> (a1, f b1)) entries
