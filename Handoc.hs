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

    tryLink :: String -> Maybe (String, String, String) -- label, url, rest
    tryLink ('[':xs) =
      case span (/= ']') xs of
        (label, ']':'(':rest) ->
          case span (/= ')') rest of
            (url, ')':rst') ->
              Just (label, url, rst')
            _ -> Nothing
        _ -> Nothing
    tryLink _ = Nothing

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

getListContent :: String -> Maybe (String, Bool) -- content, isOrderedList
getListContent l
  | Just rst <- "- "  `stripPrefix`  l = Just (rst, False)
  | otherwise = parseOrderedList l
  where
    isNumber :: Char -> Bool
    isNumber c = c >= '0' && c <= '9'

    parseOrderedList :: String -> Maybe (String, Bool)
    parseOrderedList xs
      | all isNumber xs                  = Nothing
      | (digits, '.':' ':content) <- span isNumber xs
      , not (null digits)                = Just (content, True)
      | otherwise                        = Nothing

isListPrefix :: String -> Maybe Bool -- isOrderedList
isListPrefix l = case getListContent l of
  Nothing -> Nothing
  Just(_, isOrdered) -> Just isOrdered

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

    -- Chunk by paragraph, but also ensure a code block and
    -- lists are in separate chunk
    splitChunks :: [String] -> [[String]]
    splitChunks = go []
      where
        flush :: [String] -> [[String]]
        flush [] = []
        flush xs = [reverse xs]

        go :: [String] -> [String] -> [[String]]
        go acc []                = flush acc
        go acc (l:ls)
          | "```" `isPrefixOf` l     = flush acc ++ collectCodeBlock (l:ls)
          | Just _ <- isListPrefix l = flush acc ++ collectList (l:ls)
          | null l                   = flush acc ++ go [] ls
          | otherwise                = go (l:acc) ls

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
            go :: [String] -> [String] -> [[String]]
            go acc []                = flush acc
            go acc (l:ls)
              | "```" `isPrefixOf` l = flush (l:acc) ++ splitChunks ls
              | otherwise            = go (l':acc) ls where l' = parseCodeLine l

        collectList :: [String] -> [[String]]
        collectList (start:ls) = case getListContent start of
          Just (content, isOrdered) -> go [wrapTag "li" content, ("<" ++ listTag ++ ">")] ls
            where
              listTag :: String
              listTag = if isOrdered then "ol" else "ul"

              go :: [String] -> [String] -> [[String]]
              go acc [] = flush acc
              go acc (l:ls)
                | Just (content, _) <- getListContent l =
                  let l' = wrapTag "li" content in go (l':acc) ls
                | otherwise = flush (("</" ++ listTag ++ ">"):l:acc) ++ splitChunks ls

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
          | Just isOrdered <- isListPrefix line = Just(if isOrdered then "ol" else "ul")
          | "#" `isPrefixOf` line               = Nothing -- <h#> added by parseLine
          | ">" `isPrefixOf` line               = Just("blockquote")
          | otherwise                           = Just("p")

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
