# Ch. 14

setwd("C:/Users/Kim F/Desktop/R&SQL_for_Business_Analytics/3. Zusammenfassungen & Guidelines/Ch._14_Strings")

library(stringr)
library(tidyverse)


str_length("monde")

# from lecture: Sessions 5 & 6 
##### stringr
#Regular expressions = regexps (a concise language for describing patterns in strigs)
#Strings from the "stringr" package all starts with str_.

string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'
string1
string2
double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"
literal_backslash <- "\\"
double_quote
single_quote
literal_backslash

#The printed representation of a string is not the same as the string itself, 
#because the printed representation shows the escapes. To see the raw contents
#of the string, use writeLines():

x <- c("\"", "\\")
x
writeLines(x)
y <- c("\n\nxx", "\txx", "\u00b5")
y
writeLines(y)

?'"'
?"'"
?stringr



str_length(c("a", "R for data science", NA))
str_c("x", "y")
str_c("x", "y", "z")
str_c("x", "y", sep = ", ")
x <- c("abc", NA)
x
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")
str_c("prefix-", c("a", "b", "c"), "-suffix")

#Objects of length 0 are dropped
name <- "Hadley"
time_of_day <- "morning"
birthday <-FALSE
str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)

str_c(c("x", "y", "z"), collapse = ", ")
str_c(c("x", "y", "z"), collapse = "")

#  --------------------------------------
#Subsetting strings
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)
str_sub("a", 1, 5)
str_sub(x, 1, 55)
#modifying strings:
x
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x
str_sub(x, 1, 1) <- str_to_upper(str_sub(x, 1, 1))
x
y <- c("hello world")
y
str_sub(y) <- str_to_title(str_sub(y))
y
z <- c("Apple", "Pear", "Banana")
str_sort(z)
z


#Regexps are a very terse language that allow you to describe patterns in 
#strings. 
#str_view() and str_view_all() functions take a character vector and a 
#regular expression, and show you how they match. 

x <- c("apple", "banana", "pear")
str_view_all(x, "a")
str_view(x, "an")
str_view(x, ".a.")
str_view(c("abc", "def", "fgh"), "[aeiou]")


xxx <- c("a\ple")
xxx <- c("a\\ple")
xxx
writeLines(xxx)
str_view(x, ".a.")
str_view(x, "a.")
str_view(x, ".a")

#"." matches any character, and in order to match the character "." you
#need to use an "escape" to tell the regular expression that you want to 
#match it - not use its special behaviour. Like strings, regexps use the 
#backslash, \, to escape special behaviour. So to match an . , you need 
#the regexp \. . However, we use strings to represent regular expressions, 
#and \ is also used as an escape symbol in strings. So to create the regular 
#expression \. we need the string "\\." . Ref. Wickhan
dot <- "\\."
writeLines(dot)
str_view(c("abc", "a.c", "bef"), "a\\.c")
str_view(c("abc", "a.c", "bef"), "a\.c")
# To create the regular expression, we need \\
dot <- "\\."
# But the expression itself only contains one:
writeLines(dot)
#> \.
# And this tells R to look for an explicit "."
str_view(c("abc", "a.c", "bef"), "a\\.c")



#If \ is used as an escape character in regular expressions, 
#how do you match a literal \? Well you need to escape it, creating the 
#regular expression \\. To create that regular expression, you need to 
#use a string, which also needs to escape \. That means to match a 
#literal \ you need to write "\\\\" . Ref. Wickhan

#In the book, a regular expression is written as \. and strings that
#represent the regular expression as "\\." . Ref. Wickhan

x <- "a\\b"
writeLines(x)
str_view(x, "\\\\")



# ^ used to match the start of a string
# $ used to match the end of a string

x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")
str_view(x, "^apple$")

#\d matches any digit - so to create a regular expression containing \d (or \s)
#you need to escape the \ for the string \\d (\\s)
#\s matches any whitespace (space, new line, tab etc)
#[abc] matches a, b, or c
#[^abc] matches anything except a, b, or c
#abc|d..f matches either "abc" or "deaf". Good idea to use parentheses if the 
#expressions get to complicated.
str_view(c("grey", "gray"), "gr(e|a)y")


#Repetition - 
#	?: 0 or 1
#	+: 1 or more
#	*: 0 or more

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
x
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, 'C[LX]+') #remember that [LX] matches L or X

y <- "MDCCCLXXXLVIII"
str_view(y, 'C[LX]+') 
#str_view(y, 'C[LX]*')
#str_view(y, 'C[LX]?')

#Specify the number of matches prcisely:
#	{n}: exactly n
#	{n,}: n or more
#	{,m}: at most m
#	{n,m}: between n and m

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")
str_view(x, 'C{2,3}?') #the shortest string possible
str_view(x, 'C[LX]+?')

#Grouping and backreferences
#str_view(fruit, "(..)\\1", match = TRUE) #regular expression 
#finds all fruits that have a repeated pair of letters:
fruit <- c("banana", "coconut", "cucumber", "jujube", "papaya", "salal berry", "apple")
str_view(fruit, "(..)\\1", match = TRUE)
str_view(fruit, "(.)\\1", match = TRUE)
str_view(fruit, "(.)\\1", match = FALSE)

#Detect matches
x <- c("apple", "banana", "pear")
str_detect(x, "e")

# How many common words start with t?
?words
words
# How many common words start with t?
sum(str_detect(words, "^t"))
# What proportion of common words starts with t?
mean(str_detect(words, "^t"))
# What proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))


# Find all words containing at least one vowel, and negate
words
no_vowels_1 <- !str_detect(words, "[aeiou]")
no_vowels_1
# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
no_vowels_2
identical(no_vowels_1, no_vowels_2)

words[str_detect(words, "x$")] #logical subsetting
str_subset(words, "x$") #str_subset wrapper

?seq_along()

df <- tibble(
  word = words, 
  i = seq_along(word) #word number
)

View(df)

df %>% 
  filter(str_detect(words, "x$")) # 108, 747, 772, and 841



#Count
x <- c("apple", "banana", "pear", "cucumber")
str_count(x, "a") #Count number of matches
str_detect(x, "a") #Yes/NO - TRUE/FALSE


# On average, how many vowels per word?
str_count(words, "[aeiou]")
mean(str_count(words, "[aeiou]"))

df2 <- df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )
df2

x <- c("apple", "banana", "pear")

str_count(x, "a")

#Matches do not overlap:
str_count("abababa", "aba")
str_view_all("abababa", "aba")

#Extract matches
sentences
length(sentences)
head(sentences)


?str_c
colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colours
colour_match <- str_c(colours, collapse = "|")
colour_match


#Select the sentences that contain a colour:
has_colour <- str_subset(sentences, colour_match)
has_colour
#Extract the colour to figure out which one it is:
matches <- str_extract(has_colour, colour_match) #str_extract() only 
#extracts the first match.
matches

more <- sentences[str_count(sentences, colour_match) > 1]
more
str_view_all(more, colour_match)
str_extract(more, colour_match) #Extracts the first match.
str_extract_all(more, colour_match) #Extracts all the matches. Returns a list
str_extract_all(more, colour_match, simplify = TRUE) #Extracts all the matches. 
#Returns a matrix
x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE) #expanded to the same length 
#as the longest


#Grouped matches
noun <- "(a|the|A|The) ([^  ]+)"
noun
sentences
str_view_all(sentences, noun)
has_noun <- sentences %>%
  str_subset(noun) %>% #gives us a vector of sentences with nouns 
  head(10)
has_noun
class(sentences)

grouped02 <- has_noun %>% 
  str_extract(noun) #gives us a complete match in one vector: 
#article and noun.
class(grouped02)
grouped02

grouped03 <- has_noun %>% 
  str_match(noun) #gives us a complete match in one matrix (only  
#first match for each sentence): article and noun + article + noun.
class(grouped03)
grouped03

grouped04 <- has_noun %>% 
  str_match_all(noun) #gives us a complete match in a list:
#article and noun + article + noun.
class(grouped04)
grouped04

#tidyr::extract() - easier, but have to name the matches, 
#and data should be in a tibble 

a <- tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)", #sequence of 
    #at least one character that isn't a space.
    remove = FALSE
  ) #sentence is the input data, c("", "") are the new names
a

?tidyr::extract
?tibble()

#Replacing matches

x <- c("apple", "pear", "banana")

str_replace(x, "[aeiou]", "-")
str_replace_all(x, "[aeiou]", "-")

#Multiple replacements by supplying a vector.
x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))

x <- c("1 house", "2 cars", "3 people")
y <- c("1" = "one", "2" = "two", "3" = "three")
str_replace_all(x, y)

sentences %>%
  head(5)
sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% 
  #Backreferences - flip the order of the first three words.
  #Still maintaining the rest of the sentence.
  head(5)

#Splitting
sentences %>%
  head(5) %>% 
  str_split(" ") #Splitting sentences up into pieces: words. Type=list.


"a|b|c|d" %>% 
  str_split("\\|") #Returns a list.

"a|b|c|d" %>% 
  str_split("\\|") %>% 
  .[[1]] #Results in a vector instead of a vector.


sentences %>%
  head(5) %>% 
  str_split(" ", simplify = TRUE) #Returns a matrix

sentences %>%
  head(5) %>% 
  str_split(" ", simplify = FALSE) #Returns a list


?str_split #If FALSE, the default, returns a list of character 
#vectors. If TRUE returns a character matrix.



fruits <- c(
  "apples and oranges and pears and bananas",
  "pineapples and mangos and guavas"
)

str_split(fruits, " and ")
str_split(fruits, " and ", simplify = TRUE)

# Specify n to restrict the number of possible matches
str_split(fruits, " and ", n = 3)
str_split(fruits, " and ", n = 2)
# If n greater than number of pieces, no padding occurs
str_split(fruits, " and ", n = 5)

# Use fixed to return a character matrix
str_split_fixed(fruits, " and ", 3)
str_split_fixed(fruits, " and ", 4)

#A max number of pieces n=2
fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE)


x <- "This is a sentence.  This is another sentence."
str_view_all(x, boundary("word"))
str_view(x, boundary("sentence"))

str_split(x, " ")[[1]] #.,; etc included
str_split(x, boundary("word"))[[1]] #.,; etc not included

#str_locate() and str_locate_all() 

str_locate(sentences, boundary("word"))
str_locate_all(sentences, boundary("word"))


#Other types of pattern
str_view(fruit, "nana") # Is shorthand for 
str_view(fruit, regex("nana"))

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE))

x <- "Line 1\nLine 2\nLine 3" 
str_view_all(x, "^Line") # Begins with "^"
str_extract_all(x, "^Line")[[1]]    
str_extract_all(x, regex("^Line", multiline = TRUE))[[1]]

str_match("514-791-8141", regex("
                                \\(?      # optional opening parens
                                (\\d{3}) # area code
                                [)- ]?   # optional closing parens, dash, or space
                                (\\d{3}) # another three numbers
                                [ -]?    # optional space or dash
                                (\\d{3}) # three more numbers
                                ", comments = TRUE))

#Other uses of regexps
apropos("replace") #useful if you cannot quite remember the 
#name of the function.

