module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

lookUp :: String -> [(String, a)] -> [a]
lookUp a ls = [n | (str,n) <- ls, a == str]

splitText :: [Char] -> String -> (String, [String])
splitText chrs [] = ("", [""])
splitText chrs (x:xs)
  | elem x chrs = (x : seps, "" : word_tokens)
  | otherwise   = (seps, (x:a) : ab)
  where
      (seps, word_tokens) = splitText chrs xs
      (a:ab)              = word_tokens

combine :: String -> [String] -> [String]
combine [] b          = b
combine (y:ys) (x:xs) = x : [y] : combine ys xs 

getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs (line:xs_lines) = (key,value) : getKeywordDefs xs_lines  
  where 
    (seps, word_tokens)  = splitText " " line
    (key:xs_keys)        = word_tokens
    (_:xs_seps)          = seps
    value                = concat (combine xs_seps xs_keys)

expand :: FileContents -> FileContents -> FileContents
expand text definitions = expandEachSet word_tokens 0
  where
    (seps, word_tokens) = splitText separators text
    (_, keyword_tokens) = (splitText "\n" definitions)
    keys                = getKeywordDefs keyword_tokens
    sets_of_definitions = length (lookUp "#" keys) + 1

    expandEachSet :: [String] -> Int -> FileContents
    expandEachSet word_tokens def_no
      | def_no == sets_of_definitions = ""
      | sets_of_definitions == 1      = rebuilt_text
      | otherwise                     = rebuilt_text ++ "-----\n" ++ next_definition
      where 
        replaced_tokens  = map (replaceWord keys def_no) word_tokens
        rebuilt_text     = concat (combine seps replaced_tokens)
        next_definition  = expandEachSet word_tokens (def_no + 1)

    replaceWord :: KeywordDefs ->Int -> String -> String
    replaceWord keys def_no "" = ""
    replaceWord keys def_no word
      | head word == '$' = (lookUp word keys) !! def_no
      | otherwise        = word

    
-----------------------------------------------------

-- The provided main program which uses your functions to merge a
-- template and source file.
main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")
