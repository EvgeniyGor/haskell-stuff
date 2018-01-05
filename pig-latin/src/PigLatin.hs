module PigLatin where

vowels = "aeiouAEIOU";

pigTransform :: String -> String
pigTransform "" = ""
pigTransform src = transform "" src
    where
        transform :: String -> String -> String
        transform acc "" = (reverse acc) ++ "ay"
        transform acc (x:phrase) =
            if x `elem` vowels
            then (x:phrase) ++ (reverse acc) ++ "ay"
            else transform (x:acc) phrase
