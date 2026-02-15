{-
 - Handoc - tiny markdown to HTML converter
 - Author: Siddeshwar <siddeshwar.work@gmail.com>
 -}

import Data.List
import Data.Char
import Data.Maybe
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

    -- NOTE: Monadic was a bad idea here. But anyways!
    tryLink :: String -> Maybe (String, String, String) -- label, url, rest
    tryLink "" = Nothing
    tryLink l  = "[" `stripPrefix` l
      >>= (\rst -> let (label, rst') = span (/= ']') rst
        in if (null rst') then Nothing else Just(label, (drop 1 rst')))
      >>= (\(label, rst)  -> Just (label, ("(" `stripPrefix` rst)))
      >>= (\(label, mRst) -> case mRst of
            Just rst -> let (url,rst') = span (/= ')') rst
              in Just(label, url, (drop 1 rst'))
            Nothing -> Nothing)

    tryImg :: String -> Maybe (String, String, String) -- alt, url, rest
    tryImg "" = Nothing
    tryImg l  = "!" `stripPrefix` l >>= (\rst -> tryLink rst)

    wrapLink :: String -> String -> String
    wrapLink label url = "<a href=\"" ++ url ++ "\">" ++ label ++ "</a>"

    wrapImg :: String -> String -> String
    wrapImg alt url = "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\" />"

    sm bold italic ""  acc = reverse acc
    sm bold italic rst acc
      | Just (label, url, rst') <- tryLink rst =
        sm bold italic rst' ((wrapLink label url) : acc)
      | Just (alt, url, rst') <- tryImg rst =
        sm bold italic rst' ((wrapImg alt url) : acc)
      | "**" `isPrefixOf` rst = parseBold   bold italic (drop 2 rst) acc
      | "__" `isPrefixOf` rst = parseBold   bold italic (drop 2 rst) acc
      | "_"  `isPrefixOf` rst = parseItalic bold italic (drop 1 rst) acc
      | c:rest <-         rst = sm bold italic rest ([c] : acc)

wrapTag :: String -> String -> String
wrapTag tag content =  "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

parseLine :: String -> String
parseLine line =
  case trySimpleTag line of
    Just (tag, content) -> wrapTag tag (parseInner content)
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
    -- Make sure all heading & lines have blank line below them
    normalize :: [String] -> [String]
    normalize ls = go [] ls
      where
        go :: [String] -> [String] -> [String]
        go acc []        = reverse acc
        go acc (cur:rst) = if shouldBreak cur
          then go ("" : cur : acc) rst
          else go (cur : acc)      rst

        shouldBreak :: String -> Bool
        shouldBreak line = "#" `isPrefixOf` line
          || "---" `isPrefixOf` line

    -- Chunk by paragraph, but also ensure a code block is
    -- a single chunk
    splitChunks :: [String] -> [[String]]
    splitChunks = go []
      where
        flush :: [String] -> [[String]]
        flush [] = []
        flush xs = [reverse xs]

        go :: [String] -> [String] -> [[String]]
        go acc []                = flush acc
        go acc (l:ls)
          | "```" `isPrefixOf` l = flush acc ++ collectCodeBlock (l:ls)
          | null l               = flush acc ++ go [] ls
          | otherwise            = go (l:acc) ls

        parseCodeLine :: String -> String
        parseCodeLine = (++ "<br>") . concatMap escapeChar
          where
            escapeChar ' '   = "&nbsp;"
            escapeChar '<'   = "&lt;"
            escapeChar '>'   = "&gt;"
            escapeChar '&'   = "&amp;"
            escapeChar '"'   = "&quot;"
            escapeChar '\''  = "&#39;"
            escapeChar c     = [c]

        collectCodeBlock :: [String] -> [[String]]
        collectCodeBlock (start:ls) = go [start] ls
          where
            go acc []                = flush acc
            go acc (l:ls)
              | "```" `isPrefixOf` l = flush (l:acc) ++ splitChunks ls
              | otherwise            = go (l':acc) ls where l' = parseCodeLine l

    go :: [String] -> String
    go ls
      | isLine ls                       = "<hr />"
      | Just (lang, code) <- tryCode ls = wrapCode lang code
      | Just tag <- tryTag ls           = wrapTag tag (inner ls)
      | otherwise                       = inner ls
      where
        isLine :: [String] -> Bool
        isLine []       = False
        isLine (line:_) = "---" `isPrefixOf` line

        tryCode :: [String] -> Maybe (String, String)
        tryCode (first:rest) = stripPrefix "```" first
          >>= (\l -> Just(l, unlines $ init $ rest))

        wrapCode :: String -> String -> String
        wrapCode lang code = "<code class=\"lang-"
          ++ (if lang /= "" then lang else "none")
          ++ "\">"
          ++ code
          ++ "</code>"

        tryTag :: [String] -> Maybe String
        tryTag [] = Nothing
        tryTag (line:_)
          | "#" `isPrefixOf` line = Nothing -- <h#> added by parseLine
          | ">" `isPrefixOf` line = Just("blockquote")
          | otherwise             = Just("p")

        inner :: [String] -> String
        inner [] = ""
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
