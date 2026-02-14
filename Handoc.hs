{-
 - Handoc - tiny markdown to HTML converter
 - Author: Siddeshwar <siddeshwar.work@gmail.com>
 -}

import Data.List
import Data.Char
import System.Environment
import System.IO
import Control.Exception

parse :: String -> String
parse input = "<html><body>" ++ parseChunk (lines input) ++ "</body></html>"

parseInner :: String -> String
parseInner line = concat $ sm False False line []
  where
    sm, parseBold, parseItalic :: Bool -> Bool -> String -> [String] -> [String]

    parseBold bold italic rst acc =
      let tag = if bold then "</strong>" else "<strong>"
        in sm (not bold) italic rst (tag : acc)

    parseItalic bold italic rst acc =
      let tag = if italic then "</i>" else "<i>"
        in sm bold (not italic) rst (tag : acc)

    sm bold italic ""  acc = reverse acc
    sm bold italic rst acc
      | "**" `isPrefixOf` rst = parseBold   bold italic (drop 2 rst) acc
      | "__" `isPrefixOf` rst = parseBold   bold italic (drop 2 rst) acc
      | "_"  `isPrefixOf` rst = parseItalic bold italic (drop 1 rst) acc
      | c:rest <-         rst = sm bold italic rest ([c] : acc)

parseLine :: String -> String
parseLine line =
  case trySimpleTag line of
    Just (tag, content) -> "<" ++ tag ++ ">" ++ parseInner content ++ "</" ++ tag ++ ">"
    Nothing             -> parseInner line
  where
    trySimpleTag :: String -> Maybe (String, String)
    trySimpleTag line
      | Just content <- "###### " `stripPrefix` line = Just("h6", content)
      | Just content <- "##### "  `stripPrefix` line = Just("h5", content)
      | Just content <- "#### "   `stripPrefix` line = Just("h4", content)
      | Just content <- "### "    `stripPrefix` line = Just("h3", content)
      | Just content <- "## "     `stripPrefix` line = Just("h2", content)
      | Just content <- "# "      `stripPrefix` line = Just("h1", content)
      | Just content <- "> "      `stripPrefix` line = Just("span", content) -- <blockquote> be added parseChunk
      | otherwise                                    = Nothing

parseChunk :: [String] -> String
parseChunk ls = unlines $ map go $ splitChunks $ normalize ls
  where
    -- Make sure all heading have blank line below them
    normalize :: [String] -> [String]
    normalize ls = go [] ls
      where
        go :: [String] -> [String] -> [String]
        go acc []        = reverse acc
        go acc (cur:rst) = if "#" `isPrefixOf` cur
          then go ("" : cur : acc) rst
          else go (cur : acc)      rst

    splitChunks :: [String] -> [[String]]
    splitChunks [] = []
    splitChunks ls = let (grp, rst) = span (/= "") ls
      in grp : splitChunks (dropWhile (== "") rst)

    go :: [String] -> String
    go ls = case tryTag ls of
      Just tag -> "<" ++ tag ++ ">" ++ inner ls ++ "</" ++ tag ++ ">"
      Nothing  -> inner ls
      where
        tryTag :: [String] -> Maybe String
        tryTag (line:_)
          | Just _ <- "#" `stripPrefix` line = Nothing -- <h#> added by parseLine
          | Just _ <- ">" `stripPrefix` line = Just("blockquote")
          | otherwise              = Just("p")

        inner :: [String] -> String
        inner ls = unlines (map parseLine ls)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []       -> do input <- getContents; putStr $ parse input
    [file]   -> do
      err <- try $ readFile file :: IO (Either IOException String)
      case err of
        Left  e -> hPutStrLn stderr $ "Cannot read file: " ++ show e
        Right c -> putStr $ parse c
    _       -> hPutStrLn stderr "Multiple files not supported."
