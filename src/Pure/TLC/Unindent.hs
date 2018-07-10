{-# LANGUAGE ViewPatterns #-}
module Pure.TLC.Unindent where

import Data.Char

-- unindent is from the great interpolate package by Simon Hengel.
--
-- Originally licensed as MIT, sublicensing as BSD3.
--
-- Original License:
--
{-
Copyright (c) 2013-2015 Simon Hengel <sol@typeful.net>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
{-# INLINE unindent #-}
unindent :: String -> String
unindent = concat . removeIndentation . trimLastLine . removeLeadingEmptyLine . lines_
  where
    isEmptyLine = all isSpace

    lines_ s
      | null s = []
      | otherwise =
          case span (/= '\n') s of
            (first,'\n':rest) -> (first ++ "\n") : lines_ rest
            (first,rest)      -> first : lines_ rest

    removeLeadingEmptyLine xs =
      case xs of
        y:ys | isEmptyLine y -> ys
        _                    -> xs

    trimLastLine [] = []
    trimLastLine (a : b : r) = a : trimLastLine (b : r)
    trimLastLine [a]
      | all isSpace a = []
      | otherwise = [a]

    removeIndentation ys = map (dropSpaces indentation) ys
      where
        dropSpaces 0 s = s
        dropSpaces n s =
          case s of
            ((isSpace -> True):r) -> dropSpaces (n - 1) r
            _       -> s

        indentation = minimalIndentation ys

        minimalIndentation =
            safeMinimum 0
          . map (length . takeWhile isSpace)
          . removeEmptyLines

        removeEmptyLines = filter (not . isEmptyLine)

        safeMinimum x [] = x
        safeMinimum _ xs = minimum xs
