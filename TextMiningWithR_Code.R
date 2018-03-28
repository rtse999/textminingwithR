# ------------------------------------------------------------------------
# Exercises and examples from the book - Text Mining with R
# Link:
#
# Location: /Users/raymondtse/Dropbox/Analysis/Books/TextMiningWithR_Code.r
# First created: 13:58 - Saturday 3 February 2018
# Last modified: 20:40 - Sunday 25 March 2018
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
library(ggraph)
library(gutenbergr)
library(igraph)
library(janeaustenr)
library(readr)
library(scales)
library(stringr)
library(tidyr)
library(tidytext)
library(tm)
library(widyr)

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
book_words <- austen_books() %>% 
  unnest_tokens(word, text) %>% 
  count(book, word, sort = TRUE) %>% 
  ungroup()
  
total_words <- book_words %>% 
  group_by(book) %>% 
  summarise(total = sum(n))

book_words <- left_join(book_words, total_words)

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, .0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(),
         `term frequency` = n/total)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, colour = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

book_words <- book_words %>% 
  bind_tf_idf(word, book, n)

book_words %>% 
  select(-total) %>% 
  arrange(desc(tf_idf))

book_words %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = book)) + 
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales="free") +
  coord_flip()

#physics <-gutenberg_download(c(37729, 14725, 13476, 5001),
#                             meta_fields = "author")
physics <- read_csv("physics.csv")

physics_words <- physics %>% 
  unnest_tokens(word, text) %>% 
  count(author, word, sort = TRUE) %>% 
  ungroup()

plot_physics <- physics_words %>% 
  bind_tf_idf(word, author, n) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

plot_physics %>% 
  group_by(author) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

physics %>% 
  filter(str_detect(text, "eq\\.")) %>% 
  select(text)

physics %>% 
  filter(str_detect(text, "K1")) %>% 
  select(text)

physics %>% 
  filter(str_detect(text, "AK")) %>% 
  select(text)

mystopwords <- data_frame(word = c("eq", "co", "rc", "ac", "ak", "bn",
                                   "fig", "file", "cg", "cb", "cm"))
physics_words <- anti_join(physics_words, mystopwords, by = "word")

plot_physics <- physics_words %>% 
  bind_tf_idf(word, author, n) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(author) %>% 
  top_n(15, tf_idf) %>% 
  ungroup %>% 
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()
  
# ------------------------------------------------------------------------
# Chapter 4: Relationships between words: N-grams and correlations
# ------------------------------------------------------------------------  
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams %>% 
  count(bigram, sort = TRUE)

bigrams_separated <- austen_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <-  bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>% 
  unite(bigram, word1, word2, sep = " ")

austen_books() %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>% 
  count(word1, word2, word3, sort = TRUE)

bigrams_filtered %>% 
  filter(word2 == "street") %>% 
  count(book, word1, sort = TRUE)

bigram_tf_idf <- bigrams_united %>% 
  count(book, bigram) %>% 
  bind_tf_idf(bigram, book, n) %>% 
  arrange(desc(tf_idf))

bigrams_separated %>% 
  filter(word1 == "not") %>% 
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>% 
  filter(word1 == "not") %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word2, score, sort = TRUE) %>% 
  ungroup()

not_words %>% 
  mutate(contribution = n * score) %>% 
  arrange(desc(abs(contribution))) %>% 
  head(20) %>% 
  mutate(word2 = reorder(word2, contribution)) %>% 
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurences") +
  coord_flip()

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>% 
  filter(word1 %in% negation_words) %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word1, word2, score, sort = TRUE) %>% 
  ungroup()

bigram_counts

bigram_graph <- bigram_counts %>% 
  filter(n > 20) %>% 
  graph_from_data_frame()

bigram_graph

set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(0.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(0.07, 'inches')) +
  geom_node_point(colour = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

count_bigrams <- function(dataset) {
  dataset %>% 
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
    separate(bigram, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>% 
    count(word1, word2, sort = TRUE)
}  

visualise_bigrams <- function( bigrams) {
  set.seed( 2016) 
  a <- grid::arrow( type = "closed", length = unit(.15, "inches")) 
  bigrams %>% 
    graph_from_data_frame() %>% 
    ggraph(layout = "fr") + 
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) + 
    geom_node_point(color = "lightblue", size = 5) + 
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
    theme_void() 
}

# The King James Bible is book 10 in gutenbergr
kjv <- gutenberg_download(10)

kjv_bigrams <- kjv %>% 
  count_bigrams()

#filter out rare combinations and digits
kjv_bigrams %>% 
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>% 
  visualise_bigrams()

# The Importance of Being Ernest
tiobe <- gutenberg_download(844)

tiobe_bigrams <- tiobe %>% 
  count_bigrams()  

tiobe_bigrams %>% 
  filter(n > 5,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>% 
  visualise_bigrams()

# Counting and Correlating Pairs of Words with the widyr Package
austen_section_words <- austen_books() %>% 
  filter(book == "Pride & Prejudice") %>% 
  mutate(section = row_number() %/% 10) %>% 
  filter(section > 0) %>%
  unnest_tokens(word, text) %>% 
  filter(!word %in% stop_words$word)
  
austen_section_words

word_pairs <- austen_section_words %>% 
  pairwise_count(word, section, sort = TRUE)

word_pairs

word_pairs %>% 
  filter(item1 == "darcy")

words_cors <- austen_section_words %>% 
  group_by(word) %>% 
  filter(n() >= 20) %>% 
  pairwise_cor(word, section, sort = TRUE)

words_cors

words_cors %>% 
  filter(item1 == "pounds")

words_cors %>% 
  filter(item1 %in% c("elizabeth", "pounds",
                      "married", "pride")) %>% 
  group_by(item1) %>% 
  top_n(6) %>% 
  ungroup() %>% 
  mutate(item2 = reorder(item2, correlation)) %>% 
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# ------------------------------------------------------------------------
# Chapter 5. Converting to and from Nontidy Formats
# ------------------------------------------------------------------------



