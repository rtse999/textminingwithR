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
library(ggplot2)
library(gutenbergr)
library(janeaustenr)
library(scales)
library(stringr)
library(tidyr)
library(tidytext)

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

data(stop_words)
tidy_books <- tidy_books %>%  
  anti_join(stop_words)

tidy_books %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 600) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

hgwells <- gutenberg_download( c( 35, 36, 5230, 159))

tidy_hgwells <- hgwells %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_hgwells %>% 
  count(word, sort = TRUE)

bronte <- gutenberg_download( c( 1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_bronte %>% 
  count(word, sort = TRUE)

frequency = bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                      mutate(tidy_hgwells, author = "H. G. Wells"),
                      mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>% 
  group_by(author) %>% 
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Bronte Sisters`:`H. G. Wells`)
  
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, 
                      colour = abs(`Jane Austen` - proportion))) +
  geom_abline(colour = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_colour_gradient(limits = c(0, 0.001),
                        low = "darkslategray4",
                        high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position = "none") +
  labs(y = "Jane Austen", x = NULL)

cor.test(data = frequency[frequency$author == "Bronte Sisters",],
  ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H. G. Wells",],
         ~ proportion + `Jane Austen`)

# ------------------------------------------------------------------------
# Chapter 2: Sentiment Analysis with Tidy Data
# ------------------------------------------------------------------------
sentiments

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

nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>% 
  filter(book == "Emma") %>% 
  inner_join(nrcjoy) %>% 
  count(word, sort = TRUE)

janeaustensentiment <- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(book, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

pride_prejudice

afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>% 
    mutate(method = "Bing et. al."),
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc") %>% 
    filter(sentiment %in% c("positive", "negative"))) %>% 
    mutate(method = "NRC")) %>% 
  count(method, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)  

bind_rows(afinn, bing_and_nrc) %>% 
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ method, ncol = 1, scales = "free_y")

bing_word_counts <- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()

bing_word_counts %>% 
  group_by(sentiment) %>%
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(y = "Contribute to sentiment",
       x = NULL) +
  coord_flip()
  
PandP_sentences <- data.frame(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

PandP_sentences$sentence[2]

austen_chapters <- austen_books() %>% 
  group_by(book) %>% 
  unnest_tokens(chapter, text, token = "regex",
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>% 
  ungroup()

austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())

bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_books %>% 
  group_by(book, chapter) %>% 
  summarise(words = n())

tidy_books %>% 
  semi_join(bingnegative) %>% 
  group_by(book,chapter) %>% 
  summarise(negativewords = n()) %>% 
  left_join(wordcounts, by = c("book", "chapter")) %>% 
  mutate(ratio = negativewords / words) %>% 
  filter(chapter != 0) %>% 
  top_n(1) %>% 
  ungroup()

# ------------------------------------------------------------------------
# Chapter 3: Analyzing Word and Document Frequency: tf-idf
# ------------------------------------------------------------------------  








