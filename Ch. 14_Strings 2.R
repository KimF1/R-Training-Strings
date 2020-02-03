# Strings with stringr 

# Lecture Session 6
setwd("C:/Users/Kim F/Desktop/Semester 1/R&SQL_for_Business_Analytics/3. Zusammenfassungen & Guidelines/Ch._14_Strings")
libary(tidyverse)
library(stringr)
library(dplyr)

#[Regular expressions - a very powerful tool to check and filter sting data and extract
#information from raw text.

#Regular expressions - describe the pattern that matches the text and extract the desired 
#part from the text.

#Regular expressions solve problems using two steps: the first is to find a pattern to
#match the text and the second is to group the patterns to extract the information in need.


#Regular expressions = regexps (a concise language for describing patterns in strigs)

##Strings from the "stringr" package all starts with str_.


string1 <- "This is a string"
string1
double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"
double_quote
single_quote
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'
string2
literal_backslash <- "\\" # use double backlash to have single backlash in string
literal_backslash

x <- "Ich bin so \"kluk\", \"k-l-u-k\""

#The printed representation of a string is not the same as the string itself, 
#because the printed representation shows the escapes. To see the raw contents
#of the string, use writeLines() or cat():

x <- c("\"", "\\")
x
writeLines(x)
cat(x)
writeLines(x)

# some more \* commands:
z <- "Ich bin \nKim"
writeLines(z)

w <- "Ich bin \tKim"
writeLines(w)

u <- "\u00b5"
u

# 2 new lines, tab and müh
y <- c("\n\nxx", "\txx", "\u00b5")
writeLines(y)

# count number of characters in a string: str_length [base R equivalent: nchar()]
str_length(c("a", "R for data science", NA))

# combine strings to one character vector 
t1 <- str_c("Ich bin so", "klug")
t1
writeLines(t1)
class(t1)
# vs. baseR's paste() (inserts ws by default)
t2 <- paste("Ich bin so", "klug")
t2
writeLines(t2)

str_c("x", "y")
str_c("x", "y", "z")
# include separator 
str_c("x", "y", sep = ", ")

# str_c function is vectorized:
str_c("prefix-", c("a", "b", "c"), "-suffix")

# however, different elements of a character vector itself cannot be combined with str_c or paste:
t1_v <- str_c(c("Ich bin so", "klug"))
t1_v <- paste(c("Ich bin so", "klug"))
t1_v # still 2 separate character vectors

# NAs are contagious
str_c("abc", NA)
x <- c("abc", NA)
x
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")
str_c("aba", "bbd", NA)
str_c("aba", "bbd")

x <- c("aba", "bbd")
y <- c("aba", NA)

str_c(str_replace_na(y),x)

#Objects of length (e.g. FALSE) are dropped
name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE
str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)

# collapse argument to combine vector elements
(t2 <- str_c(c("Ich bin so", "klug"), collapse = ","))
t1
"Ich bin so 'kluk', 'k-l-u-k'"
'Ich bin so "kluk", "k-l-u-k"'


t <- str_c(c("x", "y", "z"), collapse = ", ")
t
is.vector(t)
# collapse vs sep argument
(t1 <- str_c("x", "y", "z", sep = ", "))


#Subsetting strings
x <- c("apple", "banana", "pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)
str_sub("a", 1, 5)
str_sub(x, 1, 55)

y <- "apple"
str_sub(y, 1, 3)

#modifying strings:
x <- c("apple", "banana", "pear")
x <- str_to_lower(str_sub(x, 1, 1))
x
# apply change only to the subset of the vector, and do not overwrite the whole vectors
# as fasly done in line 135
str_sub(x, 1, 1) <- str_to_upper(str_sub(x, 1, 1))
x
y <- c("hello world")
y
str_sub(y) <- str_to_title(str_sub(y))
y
z <- c("Apple", "Pear", "Banana")
str_sort(z)
z


#Regexps are a very terse language that allow you to describe patterns in strings. 

#str_view() and str_view_all() functions take a character vector and a 
#regular expression, and show you how they match in RStudio's viewer. 

x <- c("apple", "banana", "pear")
?str_view
str_view_all(x, "a")
str_view(x, "a")
str_view(x, "an")
# dot matches any character
str_view(x, ".a.")
str_view(x, "a.")
str_view(x, ".a")

##### check
str_view(c("abc", "def", "fgh"), "[aeiou]")


# FO1 ---------------------------------------------------------------------


# "." matches any character, and in order to match the character "." you
# need to use an "escape" to tell the regular expression that you want to 
# match it - not use its special behaviour. 
# Like strings, regexps use the backslash, \, to escape special behaviour. 
# So to match a . , you need the regexp \. .
# However, we use strings to represent regular expressions, and \ is also used 
# as an escape symbol in strings. 
# So to create the regular expression \. we need the string "\\." . Ref. Wickham

dot <- "\\."
# But the expression itself only contains one:
writeLines(dot)
# And this tells R to look for an explicit "."
str_view(c("abc", "a.c", "bef"), "a\\.c")
str_view(c("abc", "a.c", "bef"), "a\.c")


# If \ is used as an escape character in regexps, how do you match a literal \? 
# Well you need to escape it, creating the regexp \\ 
# To create that regular expression, you need to use a string, which also needs to escape \
# That means to match a literal \ you need to write "\\\\" . Ref. Wickham

# from lecturer:
# In connection with literal R strings the backslash is an escape character. The literal string "\\" 
# is a single backslash.  
# In regexprs, the backslash is also an escape character. The regexp "\\" matches a single backslash. 
# The regexp as an R string becomes "\\\\"

x <- "a\\b"
writeLines(x)
str_view(x, "\\\\")


# ^ used to match the start of a string
# $ used to match the end of a string

x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")

x <- c("apple pie", "a black cat", "apple cake")
str_view(x, "apple")
str_view(x, "^apple$")

# more symbols:
#\d matches any digit - so to create a regular expression containing \d (or \s)
# you need to escape the \ for the string \\d (\\s)
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
str_view(x, "CC?") # will provide matches in the text w/ 0 or 1 times CC
str_view(x, "CC+") # will provide matches with min. 1 times the pattern CC in the text


y <- "MDCCCLXXVIII" 
str_view(y, "C[LX]+") # 'starts with C, followed by L or X once or multiple times'
str_view(y, "C[LX]*") # 'starts with C, followed by L or X 0 or multiple times'
str_view(y, 'CC[LX]?')

# Specify the number of matches precisely:
#	{n}: exactly n
#	{n,}: n or more
#	{,m}: at most m
#	{n,m}: between n and m

x <- "1888 is the longest year in Roman numerals: MDCCCCLXXXVIII"
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")
str_view(x, 'C{2,3}?') #the shortest string possible
str_view(x, 'C[LX]+?')

## check
#Grouping and backreferences
#str_view(fruit, "(..)\\1", match = TRUE) #regular expression 
#finds all fruits that have a repeated pair of letters:
fruit <- c("banana", "coconut", "cucumber", "jujube", "papaya", "salal berry", "apple")
str_view(fruit, "(..)\\1", match = TRUE)
str_view(fruit, "(.)\\1", match = TRUE)
str_view(fruit, "(.)\\1", match = FALSE)
str_view(fruit, "(.)\\1")
?str_view
#If TRUE, shows only strings that match the pattern. If FALSE, shows only the strings that 
#don't match the pattern. Otherwise (the default, NA) displays both matches and non-matches.


#Detect matches
x <- c("apple", "banana", "pear")
str_detect(x, "(na)+") # output is a logical vector

# str_detect in conjunction with aggregation functions
# How many common words start with t?
?words 
words 
# "words" is a sample character vector from the rcorpora package for practicing string manipulations
# How many common "words" (hence elements of the character vector) start with t?
sum(str_detect(words, "^t"))
# What proportion of common words starts with t?
mean(str_detect(words, "^t"))
# What proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))

# 2 ways of finding only words with only consonants in a text
# 1. Find all words containing at least one vowel and negate the whole expression
words
no_vowels_1 <- !str_detect(words, "[aeiou]")
no_vowels_1
# 2. Use a negated character class 

no_vowels_2 <- str_detect(words, "^[^aeiou]+$") # negated character class; 
# we have to anchor it here on both sides
# because otherwise R will find a match for a word like "asphalt", where we have for sph a string in which for min 
# 1 time there is no aeiou; so we have to set that this should hold from the beginning to the end of the string
no_vowels_2
identical(no_vowels_1, no_vowels_2)

words[str_detect(words, "x$")] #logical subsetting
str_subset(words, "x$") #str_subset wrapper

# when you have character vector as part of tibble: use filter in conjunction with str_detect

df <- tibble(
  word = words, 
  i = seq_along(word) #word number
)

View(df)

df %>% 
  filter(str_detect(words, "x$")) # 108, 747, 772, and 841


?seq_along()

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
?sentences # collection of sample sentences from stringr package, that can be used to train on regex
length(sentences)
head(sentences)

colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colours
colour_match <- str_c(colours, collapse = "|")
colour_match

?str_subset
?str_extract
#Select the sentences that contain a colour:
has_colour <- str_subset(sentences, colour_match)
has_colour
#Extract the colour to figure out which one it is:
matches <- str_extract(has_colour, colour_match) #str_extract() only 
#extracts each first match.
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
# to create a "group" in a regex, use parentheses
noun <- "(a|the|A|The) ([^  ]+)"
noun

# grouped matches: 
sentences
str_view_all(sentences, noun)

?str_subset
has_noun <- sentences %>%
  str_subset(noun) %>% #gives us a vector of sentences with nouns 
  head(10)
has_noun
class(sentences)

grouped02 <- has_noun %>% 
  str_extract(noun) #gives us a complete match in one vector: article and noun.
grouped02
class(grouped02)


grouped03 <- has_noun %>% 
  str_match(noun) #gives us a complete match in one matrix (only first match for each sentence): 
# article and noun + article + noun.
grouped03
class(grouped03)


grouped04 <- has_noun %>% 
  str_match_all(noun) #gives us a complete match in a list:
#article and noun + article + noun.
grouped04
class(grouped04)

View(grouped04) #Error

# tidyr::extract() - easier, but have to name the matches, 
# and data should be in a tibble 

a <- tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)", #sequence of 
    #at least one character that isn't a space.
    remove = FALSE #If TRUE then the "sentence" is no longer part of the output
  ) #sentence is the input data, c("", "") are the new names
a

?tidyr::extract
?tibble()

#Replacing matches

x <- c("apple", "pear", "banana")
?str_replace
str_replace(x, "[aeiou]", "-")
str_replace_all(x, "[aeiou]", "-")

#Multiple replacements by supplying a vector.
x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))
str_replace_all(x, c("\\d\\s." = "0 b", "e" = "two"))
str_replace_all(x, c("1" = "one", "2" = "two"))

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
  .[[1]] #Results in a vector instead of a list


sentences %>%
  head(5) %>% 
  str_split(" ", simplify = TRUE) #Returns a matrix

sentences %>%
  head(5) %>% 
  str_split(" ", simplify = FALSE) #Returns a list
#If FALSE, the default, returns a list of character vectors. 
#If TRUE returns a character matrix.


fruits <- c(
  "apples and oranges and pears and bananas",
  "pineapples and mangos and guavas"
)

str_split(fruits, " and ")
str_split(fruits, " and ", simplify = TRUE)

# Specify n to restrict the number of possible matches
str_split(fruits, " and ", n = 3, simplify = TRUE)
str_split(fruits, " and ", n = 2, simplify = TRUE)
# If n greater than number of pieces, no padding occurs
str_split(fruits, " and ", n = 5)

# Use fixed to return a character matrix
str_split_fixed(fruits, " and ", 3)
str_split_fixed(fruits, " and ", 4)


x <- "This is a sentence.  This is another sentence."
str_view_all(x, boundary("word"))
str_view(x, boundary("sentence"))
str_view_all(x, boundary("sentence"))

str_split(x, " ")[[1]] #.,; etc included
str_split(x, boundary("word"))[[1]] #.,; etc not included

#str_locate() and str_locate_all() 

matrix <- str_locate(sentences, boundary("word"))
list <- str_locate_all(sentences, boundary("word"))

?str_locate
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
str_view_all(x, regex("^Line", multiline = TRUE))
#multiline = TRUE allows ^ and $ to match the start and the end of each line rather 
#than the start and the end of the complete string 

### I did not do that
str_match("514-791-8141", regex("
                                \\(?      # optional opening parens
                                (\\d{3}) # area code
                                [)- ]?   # optional closing parens, dash, or space
                                (\\d{3}) # another three numbers
                                [ -]?    # optional space or dash
                                (\\d{4}) # four more numbers
                                ", comments = TRUE))

#Other uses of regexps
apropos("replace") #useful if you cannot quite remember the 
#name of the function.