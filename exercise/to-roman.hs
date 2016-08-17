#!/usr/bin/runhugs
import Test.HUnit

decimalToRoman n | 0 > n || n >= 4000 = error "need 0 <= n < 4000"

decimalToRoman n = loop numerals n where
  loop _ 0 = ""
  loop (magnitude:bigger) n = let (digits, digit) = n `divMod` 10 in
    loop bigger digits ++ getNumeral magnitude digit
  getNumeral _ 0 = ""
  getNumeral magnitude digit = magnitude !! (digit - 1)

numerals =
  [
    ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"],
    ["X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"],
    ["C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"],
    ["M", "MM", "MMM"]
  ]

main = runTests

runTests = runTestTT $ TestList $ map runTestCase tests where
  runTestCase (n, r) = TestCase $ assertEqual (show n) r (decimalToRoman n)
  tests =
    [
      (0,    ""),
      (1,    "I"),
      (5,    "V"),
      (10,   "X"),
      (50,   "L"),
      (100,  "C"),
      (500,  "D"),
      (1000, "M"),
      (207,  "CCVII"),
      (900,  "CM"),
      (1066, "MLXVI"),
      (1904, "MCMIV"),
      (1910, "MCMX"),
      (1954, "MCMLIV"),
      (1990, "MCMXC"),
      (2014, "MMXIV"),
      (3999, "MMMCMXCIX")
    ]

