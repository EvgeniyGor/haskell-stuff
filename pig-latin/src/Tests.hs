module Tests where

import PigLatin

data TestResult = OK | ERROR deriving (Show)

testCases :: [(String, String)]
testCases = [
    ("latin", "atinlay"),
    ("trash", "ashtray"),
    ("eat", "eatay"),
    ("always", "alwaysay"),
    ("", ""),
    ("LaTiN", "aTiNLay"),
    ("EAT", "EATay")
    ]

runTest :: (String, String) -> TestResult
runTest (input, output) =
    let transformed = pigTransform input in
    if transformed == output
    then OK
    else ERROR

testResultToString :: TestResult -> String
testResultToString OK = "TEST PASSED"
testResultToString ERROR = "TEST FAILED"
testResultToString _ = "UNDEFINED"

runTests :: [String]
runTests = map (\testCase -> testResultToString (runTest testCase)) testCases
