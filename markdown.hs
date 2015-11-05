-- ========================================================================== --
-- By: Aaron Carson
--
-- Takes a markdown file as input, and outputs a formatted html file.
-- Only accepts a subset of valid markdown syntax.  There may be some slight
-- variations from the official versions.
--
-- Originally developed at Western Oregon University for CS 262, Lab 8.
-- Date Written: 12/10/2014
-- ========================================================================== --

-- ************************************************************************** --
-- to compile:		ghc --make markdown -o markdown
-- to run:			./markdown input.md output.html
-- ************************************************************************** --
module Main
(
	main,
	convertToHTML
)where
import System.Environment (getArgs)
import System.IO
import Data.List
import Data.List.Split

-- ========================================================================== --
-- Exported Functions
-- ========================================================================== --

-- Entry point function.  Reads from the command line 
-- arguments to find the file to process.
main :: IO ()
main = do
    args <- getArgs                     
    let (infile,outfile) = (\(x:y:ys)->(x,y)) args
    putStrLn $ "Input file:  " ++ infile
    putStrLn $ "Output file: " ++ outfile
    contents <- readFile infile
    writeFile outfile $ convertToHTML contents

--Entry point for all markdown code
convertToHTML :: String -> String
convertToHTML = iDocument . symbolSafe



-- ========================================================================== --
-- Local Functions.
-- ========================================================================== --

--"constants"
meta = "<meta http-equiv=\"Content-type=\" content=\"text/html; charset=utf-8\">"
em = "em"
strong = "strong"
ol = "ol"
ul = "ul"
li = "li"
hr = "hr"

-- ========================================================================== --
-- PRIMARY FUNCTIONS:

-- These create the head, body, and html elements, as well as the final document.
-- ========================================================================== --

--no indenting of outer tags
headTag :: [Char] -> [Char]
headTag  s = newlineTag "head" $ concat 
                [meta, "\n", tag "title" $ head $ lines s]

bodyTag :: [Char] -> [Char]
bodyTag s  = newlineTag "body" $ unlines . tail $ lines s				 

document :: [Char] -> [Char]
document s = (++) "<!DOCTYPE HTML>\n" $ newlineTag "html" $ concat 
                 [headTag s, "\n", bodyTag s]

--attempts to format everything with indentation
		 
iHeadTag :: [Char] -> [Char]
iHeadTag s = indentedTag "head" $ relines [meta, tag "title" $ head $ lines s]

iBodyTag :: [Char] -> [Char]
iBodyTag s = indentedTag "body" $ formatBody $ relines . tail $ lines s

iDocument :: [Char] -> [Char]
iDocument s = (++) "<!DOCTYPE HTML>\n" $ newlineTag "html" $  
               relines [iHeadTag s, iBodyTag s]
			   
--force indentation of outer <html> tag
iDocument' :: [Char] -> [Char]
iDocument' s = (++) "<!DOCTYPE HTML>\n" $ indentedTag "html" $  
               relines [iHeadTag s, iBodyTag s]


-- ========================================================================== --
-- SymbolSafe converts the symbols '<' and '&' to "&lt;" and "&amp;"
-- ========================================================================== --
symbolSafe :: [Char] -> [Char]
symbolSafe [] = []
symbolSafe (x:xs)
	| x == '&'  = '&':'a':'m':'p':';':symbolSafe xs
	| x == '<'  = '&':'l':'t':';':symbolSafe xs
	| otherwise = x:symbolSafe xs



-- ========================================================================== --
-- TAG FUNCTIONS:

-- These all wrap a String in a tag.
-- Some do it in line, and some will indent contents to a new line.
-- ========================================================================== --
tag :: [Char] -> [Char] -> [Char]
tag t [] = concat["<",t," />"]
tag t s  = concat ["<",t,">",s,"</",t,">"]

newlineTag :: [Char] -> [Char] -> [Char]
newlineTag t [] = concat["<",t," />"]
newlineTag t s  = concat ["<",t,">\n",s,"\n</",t,">"]

indentedTag :: [Char] -> [Char] -> [Char]	
indentedTag t [] = concat ["<",t," />"]
indentedTag t s  = concat ["<",t,">\n"] 
                     ++ indentLines 1 (lines s) 
                     ++ concat ["\n","</",t,">"]

attributeTag :: [Char] -> [Char] -> [Char] -> [Char]	
attributeTag t a [] = concat["<",t," ",a," />"]
attributeTag t a s  = concat ["<",t," ",a,">",s,"</",t,">"]

indentedAttributeTag :: [Char] -> [Char] -> [Char] -> [Char]	
indentedAttributeTag t a [] = concat ["<",t," ",a," />"]
indentedAttributeTag t a s  = concat ["<",t," ",a,">\n"] 
                     ++ indentLines 1 (lines s) 
                     ++ concat ["\n","</",t,">"]

-- ========================================================================== --
-- INDENT FUNCTIONS - Indent single or multiple lines.
-- ========================================================================== --
indent :: Int -> [Char] -> [Char]
indent i s = tab i ++ s

indentLines :: Int -> [[Char]] -> [Char]
indentLines i ls = relines $ map (indent i) ls


-- ========================================================================== --
-- my version of unlines, it is the true inverse of lines.
-- ========================================================================== --
relines :: [[Char]] -> [Char]
relines = intercalate "\n" 

--silly function that intercalates with two newlines.
relinelines :: [[Char]] -> [Char]
relinelines = intercalate "\n\n"	

-- ========================================================================== --
-- TAB FUNCTIONS
-- ========================================================================== --
tab :: Int -> String
tab x
 | x < 1     = []
 | otherwise = ' ':' ':' ':' ': tab (x - 1)

-- ========================================================================== --
-- PARAGRAPH formatting
-- ========================================================================== --

--Breaks a String into "paragraphs", or a list of Strings.
paragraphs :: [Char] -> [[Char]]
paragraphs s = map trimNewlines $ splitOn "\n\n" s
	where trimNewlines = trimChar '\n' 

--adds an indented paragraph tag.
pTag :: [Char] -> [Char]	
pTag s = indentedTag "p" s	

--formats the entire Body portion.
formatBody :: [Char] -> [Char]	
formatBody s = relinelines $ map fParagraph $ paragraphs s	

-- ========================================================================== --
--paragraph-level formatting
fParagraph :: [Char] -> [Char]
fParagraph [] = []
fParagraph s
	| isHeader s       = relinelines [header $ firstLine $ format s, ifMoreLines fParagraph $ format s]
	| isHr s           = relinelines [horizontalRule,  ifMoreLines fParagraph $ s]
	| isUl s           = ulTag $ format s
	| isNumberedOL s   = numberedOlTag $ format s
	| isUpperRomanOl s = upperRomanTag $ format s
	| isLowerRomanOl s = lowerRomanTag $ format s
	| isUppercaseOL s  = uppercaseOlTag $ format s
	| isLowercaseOL s  = lowercaseOlTag $ format s
	| otherwise        = pTag $ format s

-- ========================================================================== --
--line-level formatting (inside each "paragraph" chunk)
format :: [Char] -> [Char]
format [] = []
format s
	| isHr s       = relines [horizontalRule,  ifMoreLines format s]
	| containsList "**" s = inlineTag format "**" "strong" s
	| containsList "__" s = inlineTag format "__" "strong" s
	| contains '*' s = inlineTag format "*" "em" s
	| contains '_' s = inlineTag format "_" "em" s
	| contains '`' s = inlineTag format "`" "code" s
	| otherwise    = relines [firstLine s, ifMoreLines format s]

--applies a function to 
ifMoreLines :: ([Char] -> [Char]) -> [Char] -> [Char]
ifMoreLines f s
	| elem '\n' s = f $ otherLines s
	| otherwise   = []	
					
--check if the string is a header.
isHeader :: [Char] -> Bool
isHeader s = head s == '#'

--create a header tag, h1 through h6, by counting the preceding hashes.  
header :: [Char] -> [Char]
header s = tag ('h':hashCountString s) $ trimChar '#' s
 	where hashCountString s = show $ max 1 $ min 6 $ leadingHashes s 	
 
-- ========================================================================== --
-- OL (ORDERED LIST) FUNCTIONS

-- these functions handle processing of ordered lists.
-- ========================================================================== --

--trimOL trims the list markdown notation from each line (ex. "1. " or "A. ")
trimOL :: [Char] -> [Char]
trimOL (_:_:'.':' ':rest) = rest
trimOL (_:'.':' ':rest) = rest
trimOL list = list

--creates an ordered-list tag around the string s, with the specified string attributes a, and uses a boolean function f to check if the beginning of a line is a new list element or not.
attributeOlTag :: ([Char] -> Bool) -> [Char] -> [Char] -> [Char]
attributeOlTag f a s = indentedAttributeTag ol a $ relines $ 
					   map liTag $ map trimOL $ olLines f s 

--function that groups List Elements each to their own line, using a function f to determine if the beginning of a string is a new list element.
olLines :: ([Char] -> Bool) -> [Char] -> [[Char]]
olLines f s = group $ map trimSpaces $ lines s
	where
		group (x:y:xs)
			| f x && f y = x:group (y:xs)
			| otherwise  = group (concat[x,"\n",y]:xs)
		group s = s	
		

--NumberedLists								 
isNum :: Char -> Bool
isNum c = elem c ['0'..'9']

isNumberedOL :: [Char] -> Bool
isNumberedOL (w:x:'.':' ':zs) = isNum w && isNum x
isNumberedOL (x:'.':' ':zs) = isNum x
isNumberedOL _ = False

numberedOlTag :: [Char] -> [Char]
numberedOlTag s = indentedTag ol $ relines $ map liTag $ numberedListLines s

numberedListLines :: [Char] -> [[Char]]
numberedListLines s = map trimOL $ olLines isNumberedOL s


--Uppercase Lists
isUppercaseChar :: Char -> Bool
isUppercaseChar c = elem c ['A'..'Z']

isUppercaseOL :: [Char] -> Bool
isUppercaseOL (x:'.':' ':zs) = isUppercaseChar x
isUppercaseOL _ = False

uppercaseOlTag :: [Char] -> [Char]
uppercaseOlTag s = attributeOlTag isUppercaseOL "type=\"A\"" s


--Lowercase lists
isLowercaseChar :: Char -> Bool
isLowercaseChar c = elem c ['a'..'z']

isLowercaseOL :: [Char] -> Bool
isLowercaseOL (x:'.':' ':zs) = isLowercaseChar x
isLowercaseOL _ = False

lowercaseOlTag :: [Char] -> [Char]
lowercaseOlTag s = attributeOlTag isLowercaseOL "type=\"a\"" s


--roman numeral lists
isUpperRoman :: Char -> Bool
isUpperRoman c = elem c "IVXLCDM"
isLowerRoman :: Char -> Bool
isLowerRoman c = elem c "ivxlcdm"

isUpperRomanOl :: [Char] -> Bool
isUpperRomanOl s = isRomanOL isUpperRoman s
isLowerRomanOl :: [Char] -> Bool
isLowerRomanOl s = isRomanOL isLowerRoman s

isRomanOL :: (Char -> Bool) -> [Char] -> Bool
isRomanOL f (a:b:c:d:e:g:'.':' ':rest) = f a && f b && f c && f d && f e && f g  
isRomanOL f (a:b:c:d:e:'.':' ':rest)   = f a && f b && f c && f d && f e   
isRomanOL f (a:b:c:d:'.':' ':rest)     = f a && f b && f c && f d   
isRomanOL f (a:b:c:'.':' ':rest)       = f a && f b && f c
isRomanOL f (a:b:'.':' ':rest)         = f a && f b
isRomanOL f (a:'.':' ':rest)           = f a
isRomanOL f rest                       = False

trimUpperRoman :: [Char] -> [Char]
trimUpperRoman s = trimRoman isUpperRoman s
trimLowerRoman :: [Char] -> [Char]
trimLowerRoman s = trimRoman isLowerRoman s

trimRoman :: (Char -> Bool) -> [Char] -> [Char]
trimRoman f s@(a:b:c:d:e:g:'.':' ':rest) = if isRomanOL f s then rest else s
trimRoman f s@(a:b:c:d:e:'.':' ':rest)   = if isRomanOL f s then rest else s  
trimRoman f s@(a:b:c:d:'.':' ':rest)     = if isRomanOL f s then rest else s
trimRoman f s@(a:b:c:'.':' ':rest)       = if isRomanOL f s then rest else s
trimRoman f s@(a:b:'.':' ':rest)         = if isRomanOL f s then rest else s
trimRoman f s@(a:'.':' ':rest)           = if isRomanOL f s then rest else s
trimRoman f rest				         = rest

upperRomanLines :: [Char] -> [[Char]]
upperRomanLines s = map trimUpperRoman $ olLines isUpperRomanOl s
lowerRomanLines :: [Char] -> [[Char]]
lowerRomanLines s = map trimLowerRoman $ olLines isLowerRomanOl s

upperRomanTag :: [Char] -> [Char]
upperRomanTag s = indentedAttributeTag ol "type=\"I\"" $ relines $ map liTag $ upperRomanLines s 
lowerRomanTag :: [Char] -> [Char]
lowerRomanTag s = indentedAttributeTag ol "type=\"i\"" $ relines $ map liTag $ lowerRomanLines s 

-- ========================================================================== --
-- UL (UN-ORDERED LIST) FUNCTIONS

-- These functions handle processing of unordered lists. <
-- ========================================================================== --
--wrap a string in a ul tag, wrapping each list element with a li tag
ulTag :: [Char] -> [Char]
ulTag s = indentedTag ul $ relines $ map liTag $ listLines s

--checks if a string is an unordered list
isUl :: [Char] -> Bool
isUl s = isPrefixOf "* " s

--wraps a string in a li tag, using an indented tag if it contains a newline
liTag :: [Char] -> [Char]
liTag s
	| elem '\n' s = indentedTag li s
	| otherwise   = tag li s
	
--splits a string into a list of strings, where each is a new list element	
listLines :: [Char] -> [[Char]]
listLines s = map trimNewlines $ filterBlanks $ splitOn "* " $ removeIndent s

--Removes any spaces from the beginning and end of each line in the string.
removeIndent :: [Char] -> [Char]
removeIndent s = relines $ map trimSpaces $ lines s

-- ========================================================================== --
-- Other FUNCTIONS
-- ========================================================================== --
 
--CountChar counts how many of the Char c is at the beginning of a String.	
leadingChars :: Eq a => a -> [a] -> Int
leadingChars c s = length $ takeWhile (==c) s 

--Count the leading hash characters.
leadingHashes :: [Char] -> Int
leadingHashes s = leadingChars '#' s

--Count the leading dash characters.
leadingDashes :: [Char] -> Int
leadingDashes s = leadingChars '-' s

--TrimChar trims every instance of the Char c from both ends of a string s.
trimChar :: Eq t => t -> [t] -> [t]
trimChar _ [] = []
trimChar c s = dropWhileEnd (==c) $ dropWhile (==c) s

trimSpaces :: [Char] -> [Char]
trimSpaces s = trimChar ' ' s

trimNewlines :: [Char] -> [Char]
trimNewlines s = trimChar '\n' s

-- remove empty strings from a list of strings.
filterBlanks :: [[Char]] -> [[Char]]
filterBlanks = filter (/="")

-- replaces every occurance of an old Char with a new Char in a string.
replaceChar :: Char -> Char -> [Char] -> [Char]
replaceChar _ _ [] = []
replaceChar old new s = foldr(\x acc -> switch x: acc) [] s
	where switch x
			| x == old = new
			|otherwise = x

-- Get the first line of a string. (quicker than lines/relines)
firstLine :: [Char] -> [Char]
firstLine [] = []
firstLine (x:xs)
	| x == '\n' = []
	| otherwise = x:firstLine xs

-- get all lines but the first of a string.
otherLines :: [Char] -> [Char]
otherLines [] = []
otherLines (x:xs)
	| x == '\n' = xs
	| otherwise = otherLines xs

--check if a string is a horizontal rule
isHr :: [Char] -> Bool
isHr s = leadingDashes s >= 3

--make a horizontal rule
horizontalRule :: [Char]
horizontalRule = tag hr []



--check if a Char c doesn't have a space immediately adjacent to the right.
rightOfNonSpace :: Char -> [Char] -> Bool
rightOfNonSpace c (x:y:xs)
	| c == x && ' ' < y = True 
	| otherwise         = rightOfNonSpace c (y:xs)
rightOfNonSpace _ (x:xs) = False
rightOfNonSpace _ []     = False

--check if a Char c doesn't have a space immediately adjacent to the left.
leftOfNonSpace :: Char -> [Char] -> Bool
leftOfNonSpace c (x:y:xs)
	| c == x && ' ' < y = True 
	| otherwise         = rightOfNonSpace c (y:xs)
leftOfNonSpace _ (x:xs) = False
leftOfNonSpace _ []     = False

--tell if l is adjacent to r in sequential order within a list.
adjacent :: Eq a => a -> a -> [a] -> Bool
adjacent l r (x:y:xs)
	| l == x && r == y = True 
	| otherwise        = adjacent l r (y:xs)
adjacent _ _ (x:xs) = False
adjacent _ _ []     = False


-- ========================================================================== --
-- INLINE TAGS - for Bold, Italic, Monospace, etc
-- ========================================================================== --

{-- inlineTag intersperses a a tag that can occurr multiple times within a string. 
It takes a function 'f' which is re-applied after this completes (a function such
as format would work.  It is designed to be run within a recursive, terminating 
function).  

It splits the String 's' by a given delimiter 'delim' and wraps every-other string 
with a tag 't'. --}
inlineTag :: ([Char] -> t) -> [Char] -> [Char] -> [Char] -> t
inlineTag f delim t s = f $ concat [check t | t <- zip [0..] $ splitOn delim s]
	where check (num,str) 
		| mod num 2 == 1 && 0 < length str = tag t str
		| otherwise = str 

--checks if a String contains a Char to the left and right of a non-space.
contains :: Char -> [Char] -> Bool
contains c s = leftOfNonSpace c s && rightOfNonSpace c s

--checks if the list 'as' exists in-order in the list 'bs'.
--containsList "you" "Do you dance?" == True
containsList :: [Char]-> [Char] -> Bool
containsList as bs = checkList as as bs bs
	where
		checkList _ [] _ _ = True
		checkList _ _ [] _ = False
		checkList pattern (l:ls) (j:js) (k:ks) 
			| l == j    = checkList pattern ls js (k:ks)
			| otherwise = checkList pattern pattern ks ks