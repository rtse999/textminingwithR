# ------------------------------------------------------------------------
# Exercises and examples from the book - Text Mining with R
# Link:
#
# Location: /Users/raymondtse/Dropbox/Analysis/Books/TextMiningWithR_Code.r
# First created: 13:58 - Saturday 3 February 2018
# Last modified: 13:58 - Saturday 3 February 2018
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time 
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Session Info
# ------------------------------------------------------------------------
devtools::session_info()

# ------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------
library(dplyr)
library(tidytext)
library(stringr)
library(janeaustenr)

# ------------------------------------------------------------------------
# Chapter 1: The Tidy Text Format
# ------------------------------------------------------------------------
text <- c(" Because I could not stop for Death -", 
          "He kindly stopped for me -", 
          "The Carriage held but just Ourselves -", 
          "and Immortality")
text

text_df <- data_frame(line = 1:4, text = text)
text_df

text_df %>% unnest_tokens(word, text)

# Jane Austen Example
original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text,
                     regex(" ^ chapter [\\ divxlc]", ignore_case = TRUE))
                     )) %>% 
  ungroup()
original_books

tidy_books <- original_books %>% 
  unnest_tokens(word, text)
tidy_books





