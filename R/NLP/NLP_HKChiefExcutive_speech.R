# NLP/ML Speech Text Processing
# Hong Kong Chief Excutives' Speech
# Albert Baichen DU

# Load library
library(lubridate)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(hunspell)
library(ggwordcloud)
library(htmlwidgets)

# Q1 --------------
# Please see the submitted report

# Q2 --------------

## Load datasets-----
hsi <- readRDS("hsi.rds")
pori <- readRDS("pori.rds")
speech_lam_pdf <- readRDS("speech_lam_pdf.rds")
speech_lam_wp <- readRDS("speech_lam_wp.rds")
speech_tsang <- readRDS("speech_tsang.rds")
speech_tung <- readRDS("speech_tung.rds")

## Merge Ms Lam's speech-----
speech_lam <- rbind(speech_lam_pdf, speech_lam_wp) %>% 
  arrange(doc_id) %>% 
  mutate(chapter = 3)

## Add chapter id for Tsang and Tung to trace the docs
speech_tsang <- speech_tsang %>% 
  mutate(chapter = 2)

speech_tung <- speech_tung %>% 
  mutate(chapter = 1)

## Merge all datasets-----
speech <- rbind(speech_lam, speech_tsang) %>% 
  rbind(speech_tung) %>% 
  select(doc_id, title, date, text, speaker, chapter)

## Create a timepoint for pori surveys
pori <- pori %>% 
  mutate(survey_date = survey_start_date + (survey_end_date - survey_start_date)/2)

## Merge pori with speech ------
## Calculate the average support rate for each month for each CE and merge
speech <- speech %>% 
  mutate(year = year(date), month = month(date))
speech <- rename(speech, name = speaker)

pori_mean <- pori %>% 
  mutate(year = year(survey_date), month = month(survey_date)) %>% 
  group_by(year, month, name) %>% 
  summarise(support_mean = mean(support_rating), recognition_mean = mean(recognition_rate),
            raters_all = sum(number_of_raters))

speech <- speech %>% 
  left_join(pori_mean, by = c("year", "month")) 
meta <- rename(speech, name = name.x) %>% 
  select(doc_id, title, date, text, name, chapter, year, month, support_mean, 
         recognition_mean, raters_all)

## Merge hsi with meta------
### Monthly index
hsi_mean <- hsi %>% 
  mutate(year = year(date), month = month(date)) %>% 
  group_by(year, month) %>% 
  summarise(hsi_open_mean = mean(hsi_open, na.rm = TRUE),
            hsi_high_max = max(hsi_high, na.rm = TRUE),
            hsi_low_min = min(hsi_low, na.rm = TRUE), 
            hsi_close_mean = mean(hsi_close, na.rm = TRUE),
            hsi_adj_close_mean = mean(hsi_adj_close, na.rm = TRUE),
            hsi_volume_mean = mean(hsi_volume, na.rm = TRUE)
            )
### Daily index
hsi_daily <- hsi %>% 
  rename_at(vars(-date),function(x) paste0(x,"_daily"))

### Join them
meta <- meta %>% 
  left_join(hsi_mean, by = c("year", "month"))
meta <- meta %>% 
  left_join(hsi_daily, by = "date")

### Save dfs
saveRDS(speech, file = "speech.csv")
saveRDS(meta, file = "meta.csv")

## Remove intermediate dfs
rm(test1)
rm(speech_lam_pdf)
rm(speech_lam_wp)

# Q3 ------------
## For pori-------
### support rating by date
ggplot(pori)+
  geom_smooth(aes(x = survey_date, y = support_rating))+
  geom_point(aes(x = survey_date, y = support_rating))+
  labs(
    title = "Chief Executive's Public Support Rate over Time",
    subtitle = "Scatter Plot and Line Graph"
  )

### support rating by CEs
ggplot(pori)+
  geom_boxplot(aes(x = name, y = support_rating))+
  labs(
    title = "Chief Executive's Public Support Rate",
    subtitle = "Boxplot"
  )

### recognition by date
ggplot(pori)+
  geom_smooth(aes(x = survey_date, y = recognition_rate))+
  geom_point(aes(x = survey_date, y = recognition_rate))+
  labs(
    title = "Chief Executive's Recognition Rate over Time",
    subtitle = "Scatter Plot and Line Graph"
  )

### recognition by CEs
ggplot(pori)+
  geom_boxplot(aes(x = name, y = recognition_rate))+
  labs(
    title = "Chief Executive's Recognition Rate",
    subtitle = "Boxplot"
  )

### Number of raters by date 
ggplot(pori)+
  geom_smooth(aes(x = survey_date, y = number_of_raters))+
  geom_point(aes(x = survey_date, y = number_of_raters))+
  labs(
    title = "Participants of Chief Executive's Satisfaction Survey over Time",
    subtitle = "Scatter Plot and Line Graph"
  )
### Since it's pretty consistent, there may be no point plotting the raters
### by CEs.

### Standard Deviation by date (Opinion Polarization)
ggplot(pori, aes(x= survey_date, y =standard_error*(number_of_raters^2)))+
  geom_smooth()+
  geom_point()+
  labs(
    title = "Standard Deviation of Ratings over time",
    subtitle = "Scatter Plot and Line Graph"
  )+xlab("Date")+ylab("Standard Deviation")

pori %>% 
  group_by(name) %>% 
  summarise(n = n()) %>% 
  ggplot()+
  geom_col(aes(x = name, y = n))+
  labs(
    title = "Chief Executive's Public Opinion Survey",
    subtitle = "Bar Graph"
  )
### Summary statistics
summary(pori$support_rating)
summary(pori$recognition_rate)
cor(pori$support_rating, pori$recognition_rate)
cov(pori$support_rating, pori$recognition_rate)

## For hsi---------
ggplot(hsi)+
  geom_smooth(aes(x = date, y = hsi_adj_close), color = 'green')+
  geom_point(aes(x = date, y = hsi_adj_close), color = "blue", alpha = 0.1)+
  labs(
    title = "Hang Seng Index since 1996",
    subtitle = "Scatterplot and Line Graph" 
  )

ggplot(hsi)+
  geom_point(aes(x = date, y = hsi_volume), color = "blue", alpha = 0.1)+
  geom_smooth(aes(x = date, y = hsi_volume), color = 'green')+
  labs(
    title = "Hang Seng Index Volume since 1996",
    subtitle = "Scatterplot and Line Graph" 
  )

ggplot(hsi)+
  geom_point(aes(y = hsi_adj_close, x = hsi_volume), color = "blue", alpha = 0.1)+
  labs(
    title = "Hang Seng Volume and Closing Index since 1996",
    subtitle = "Scatterplot" 
  )

hsi %>% 
  group_by(date) %>% 
  mutate(change = hsi_close - hsi_open) %>% 
  ggplot()+
  geom_col(aes(x = date, y = change))+ 
  labs(
    title = 'Daily HSI Change since 1996',
    subtitle = "Bar Chart"
  )

### Summary Statistics
summary(hsi$hsi_adj_close, scientific = FALSE)
summary(hsi$hsi_volume, scientific = FALSE)
cov(hsi$hsi_volume, hsi$hsi_adj_close)
cor(hsi$hsi_volume, hsi$hsi_adj_close)
hsi %>% 
  group_by(date) %>% 
  summarise(change = hsi_close - hsi_open) %>% 
  summary(change)

## For the untokenized texts---------
### Number of speeches by CE
meta %>% 
  group_by(name) %>% 
  summarise(n = n()) %>% 
  ggplot()+
  geom_col(aes(x = name, y = n))+
  labs(
    title = "Number of Speeches Delivered by CE",
    subtitle = "Bar Chart"
  )

### Number of speeches by month
meta %>% 
  mutate(year_month = ym(str_c(year, month, sep = "-")))%>% 
  group_by(year_month) %>%  
  summarise(n = n()) %>% 
  ggplot()+
  geom_col(aes(x = year_month, y = n))+
  labs(
    title = "Number of CE Speeches by Month",
    subtitle = "Bar Chart"
  )

### The correlation between number of speech and public support in that month
test <- meta %>% 
  mutate(year_month = ym(str_c(year, month, sep = "-")))%>% 
  group_by(year_month) %>%  
  mutate(n = n())

test %>% 
  ggplot(aes(x = n, y = support_mean))+
  geom_point()+
  labs(
    title = "Number of Speech VS Average Public Support",
    subtitle = "Scatter Plot"
  )+xlab("number of speech per month")

### Summary Statistics
meta %>% 
  mutate(year_month = ym(str_c(year, month, sep = "-")))%>% 
  group_by(year_month) %>%  
  summarise(n = n()) %>% 
  summary()


# Q4 -----------
## Raw text data --------
library(tidytext)
speech_raw <- speech %>% 
  select(doc_id,title, date, text, name.x, chapter) %>% 
  rename(name = name.x) %>% 
  mutate(speech_id = doc_id) %>% 
  mutate(doc_id = row_number())
## Do some extra cleaning that was left over in previous steps
speech_lam <- speech_raw %>% 
  group_by(name) %>% 
  filter(name == "Lam") %>% 
  arrange(date) %>% 
  mutate(speech_id = row_number())

speech_tung <- speech_raw %>% 
  group_by(name) %>% 
  filter(name == "Tung") %>% 
  arrange(date) %>% 
  mutate(speech_id = row_number())

speech_tsang <- speech_raw %>% 
  group_by(name) %>% 
  filter(name == "Tsang") %>% 
  arrange(date) %>% 
  mutate(speech_id = row_number())

speech_raw <- rbind(speech_tung, speech_tsang)
speech_raw <- rbind(speech_raw, speech_lam)
write.csv(speech_raw, "speech_raw.csv")
speech_raw <- read.csv('speech_raw.csv')
speech_1 <- speech_raw %>% 
  mutate(doc_id = row_number())

saveRDS(speech_1, "speech_raw.rds")

## Some summary Statistics

### 1. Number of Documents
sum_ndocs <- nrow(speech_raw)

### 2. Number of Characters
sum_nchar <- speech_raw %>%
  transmute(n_characters = nchar(text))
summary(sum_nchar)

#### Plot it
sum_nchar %>%
  ggplot() + geom_histogram(aes(x = n_characters), bins = 100) +
  xlab("Number of characters") +
  labs(
    title = "Distribution of Speech Lengths",
    subtitle = str_c("Number of documents = ", sum_ndocs)
  )


## Unnest texts --------
speech_tokenized <- speech_raw %>% 
  unnest_tokens(word, text, token = "words")


## An overview of the speeches
speech_count <- speech_tokenized %>%
  group_by(doc_id, word) %>%
  count() %>% 
  arrange(desc(n))

speech_count

## Try to construct a dictionary --------
dictionary <- speech_count %>%
  group_by(word) %>%
  summarise(
    term_frequency = sum(n), 
    document_frequency = n())

### Some summary statistics
dictionary %>% 
  arrange(desc(term_frequency))

dictionary %>% 
  arrange(desc(document_frequency))

summary(dictionary$term_frequency)
summary(dictionary$document_frequency)

### Plot them
# 1. term frequency
p_tf_1 <- dictionary %>%
  ggplot() + geom_histogram(aes(x = term_frequency), bins = 100) +
  xlab("Term Frequency") + 
  labs(
    title = "Distribution of Post-Wrangling Term Frequencies",
    subtitle = "Histogram"
  )

p_tf_1

# 2. Document frequency
p_df_1 <- dictionary %>%
  ggplot() + geom_histogram(aes(x = document_frequency), bins = 100) +
  xlab("Document Frequency") + 
  labs(
    title = "Distribution of Post-Wrangling Document Frequencies",
    subtitle = "Histogram"
  )

p_df_1

## Diagnostics --------
### Diagnostic 1. Pre-wrangled most frequent words -----------

dictionary %>% 
  slice_max(term_frequency, n = 100) %>% 
  arrange(desc(term_frequency))

dictionary %>% 
  slice_max(term_frequency, n = 200) %>%
  ggplot(aes(label = word, size = term_frequency)) +
  geom_text_wordcloud() +
  labs(
    title = "Most Frequent Words (Pre-Wrangled)"
  ) +
  theme_minimal()

### Diagnostics 2: Pre-wrangled least frequent words ----
dictionary %>% 
  slice_min(term_frequency, n = 1) %>% 
  arrange(term_frequency)

dictionary %>% 
  slice_min(term_frequency, n = 1) %>%
  slice(1:200) %>%
  ggplot(aes(label = word, size = term_frequency)) +
  geom_text_wordcloud() +
  scale_radius(range = c(2, 2), limits = c(0, NA)) +
  labs(
    title = "Least Frequent Words (Pre-Wrangled)"
  ) +
  theme_minimal()
# There are too many numbers! which is useless. Considering removing them using regex

## Wrangling texts---------
### Make a copy of tokenized texts--------
speech_tokenized_t <- speech_tokenized

### 1. Are there any words that preced or follow underscores? ------
### Not necessary, just a conformation.

check_anomaly <- speech_tokenized %>%
  filter(str_detect(word, "^_") | str_detect(word, "_$"))
nrow(check_anomaly)
# Great! Nothing!

### 2. Remove all numbers------
speech_tokenized %>%
  filter(str_detect(word, "[:digit:]"))

speech_tokenized_t <- speech_tokenized_t %>%
  filter(!str_detect(word, "[:digit:]"))

### 3. Remove all "."s-------
speech_tokenized %>%
  filter(str_detect(word, "[A-Za-z']+\\."))

speech_tokenized %>%
  filter(str_detect(word, "\\.[A-Za-z']"))

speech_tokenized %>%
  filter(str_detect(word, "\\.[A-Za-z']+\\."))

speech_tokenized_t <- speech_tokenized_t %>%
  mutate(
    word = ifelse(
      str_detect(word, "[A-Za-z']+\\.") |
        str_detect(word, "\\.[A-Za-z']") | 
        str_detect(word, "\\.[A-Za-z']+\\."),
      str_replace_all(word, ".", ""), word))
speech_tokenized_t

### 4. Remove all possessive markers-------
speech_tokenized %>%
  filter(str_detect(word, "'s$"))

speech_tokenized_t <- speech_tokenized_t %>%
  mutate(
    word = ifelse(str_detect(word, "'s$"), str_replace(word, "'s$", ""), word)
  )

### 5. Starnadized cleaning-------
#### 5.1 Stop words-------
stop_words <- stop_words %>% 
  select(word) %>% 
  distinct()

speech_tokenized %>%
  semi_join(stop_words, by = "word")

speech_tokenized_t <- speech_tokenized_t %>%
  anti_join(stop_words, by = "word")

speech_tokenized_t %>%
  semi_join(stop_words, by = "word")
# They are gone!
stop_words_customized <- tibble(
  word = c("hong", "kong")
)
speech_tokenized %>%
  semi_join(stop_words_customized, by = "word")

speech_tokenized_t <- speech_tokenized_t %>%
  anti_join(stop_words_customized, by = "word")
#### 5.2 Stemming---------
speech_tokenized_t <- speech_tokenized_t %>%
  mutate(word = wordStem(word))

## 2nd Trial to Make Dictionary------
speech_count_t <- speech_tokenized_t %>%
  group_by(doc_id, word) %>%
  count()


dictionary_t <- speech_count_t %>%
  group_by(word) %>%
  summarise(
    term_frequency = sum(n), 
    document_frequency = n())

### Remove infrequent words
n_docs <- nrow(speech_raw)
threshold <- round(n_docs * 0.005, 0)

stop_words_infrequent <- dictionary_t %>% 
  filter(document_frequency < threshold)

speech_tokenized_t <- speech_tokenized_t %>%
  anti_join(stop_words_infrequent, by = "word")
#### Check again
speech_tokenized_t %>%
  semi_join(stop_words_infrequent, by = "word")

### Remake dic
speech_count_t <- speech_tokenized_t %>%
  group_by(doc_id, word) %>%
  count()

speech_count_t_byname <- speech_tokenized_t %>%
  group_by(doc_id, word, name) %>%
  count()

dictionary_t <- speech_count_t %>%
  group_by(word) %>%
  summarise(
    term_frequency = sum(n), 
    document_frequency = n())

## See if the cleaning process needs further rounds------

summary(dictionary_t$term_frequency)
summary(dictionary_t$document_frequency)

p_tf_2 <- dictionary_t %>%
  ggplot() + geom_histogram(aes(x = term_frequency), bins = 100) +
  xlab("Term Frequency") + 
  labs(
    title = "Distribution of Post-Wrangling Term Frequencies",
    subtitle = "Histogram"
  )
p_tf_2
ggsave("post wrangled term frequency.png", width = 5, height = 4)
p_df_2 <- dictionary_t %>%
  ggplot() + geom_histogram(aes(x = document_frequency), bins = 100) +
  xlab("Term Frequency") + 
  labs(
    title = "Distribution of Post-Wrangling Document Frequencies",
    subtitle = "Histogram"
  )
ggsave("post wrangled document frequency.png", width = 5, height = 4)

### 1.Most Frequent words------
dictionary_t %>% 
  slice_max(term_frequency, n = 100) %>% 
  arrange(desc(term_frequency))

dictionary_t %>% 
  slice_max(term_frequency, n = 200) %>%
  ggplot(aes(label = word, size = term_frequency)) +
  geom_text_wordcloud() +
  theme_minimal() +
  labs(
    title = "Most Frequent Words (Post-Wrangled)"
  )
ggsave("post wrangled most frequent words.png", width = 5, height = 4)
### 2. Least frequent words------
dictionary_t %>% 
  slice_min(term_frequency, n = 1) %>% 
  arrange(term_frequency)

dictionary_t %>% 
  slice_min(term_frequency, n = 100) %>%
  ggplot(aes(label = word, size = term_frequency)) +
  geom_text_wordcloud() +
  scale_radius(range = c(0, 8), limits = c(0, 8)) +
  theme_minimal() +
  labs(
    title = "Least Frequent Words (Post-Wrangled)"
  )
ggsave("post wrangled least frequent words.png", width = 8, height = 5)
## df-idf------
speech_count_t
dictionary_t

speech_count_t_tfidf <- speech_count_t %>%
  inner_join(dictionary_t, by = "word") %>%
  mutate(
    tf = n / term_frequency,
    idf = log(n_docs / document_frequency)
  ) %>%
  mutate(tfidf = tf * idf) %>%
  select(doc_id, word, tfidf)


## Save all datasets-----------
saveRDS(speech_tokenized_t, "speech_tokenized.rds")
saveRDS(dictionary_t, "dictionary.rds")
saveRDS(speech_count_t_byname,'speech_count_t_byname.rds')
saveRDS(speech_count_t, "word_count.rds")
saveRDS(speech_count_t_tfidf, "word_tfidf.rds")

# Q5---------
rm(list=ls())

## Load all libraries---------
library(tidyverse)
library(tidytext)
library(janeaustenr)
library(SnowballC)
library(hunspell)
library(ggwordcloud)
library(htmlwidgets)

## Load all datasets created in Q4-----------
speech_raw <- readRDS("speech_raw.rds")
speech_tokenized <- readRDS("speech_tokenized.rds")
dictionary <- readRDS("dictionary.rds")
word_count <- readRDS("word_count.rds")
word_tfidf <- readRDS("word_tfidf.rds")

speech_meta <- speech_raw %>%
  select(doc_id, chapter, name, speech_id)

word_count <- speech_meta %>% inner_join(word_count, by = "doc_id")
word_tfidf <- speech_meta %>% inner_join(word_tfidf, by = "doc_id")

## TEDA 1. The Corpus--------
sum_corpus <- word_count %>% 
  group_by(word) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))

sum_corpus %>% 
  slice_max(n, n = 100) %>%
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(shape = "circle") +
  scale_radius(range = c(0, 6), limits = c(0, NA)) +
  scale_color_gradient(low = "grey", high = "red") +
  theme_minimal() +
  labs(
    title = "Word Cloud of the Corpus",
    subtitle = "Most Frequent Words in CE's Speeches"
  )
ggsave("teda_describe_corpus.png", width = 5, height = 4)

## 2. Most Common Words Across the Corpus-----
tfidf_corpus <- word_tfidf %>% 
  group_by(word) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))

tfidf_corpus %>% 
  slice_max(n, n = 100) %>%
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(shape = "circle") +
  scale_radius(range = c(0, 6), limits = c(0, NA)) +
  scale_color_gradient(low = "grey", high = "red") +
  # Replace with your favorite color scheme: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
  theme_minimal() +
  labs(
    title = "Word Cloud of the Corpus",
    subtitle = "Most Common Words in CE's Speeches"
  )
ggsave("teda_describe_tfidf.png", width = 5, height = 4)

## 3. Most Common words of Lam's speech ----

sum_corpus_sub <- word_count %>%
  filter(name == "Lam") %>%
  ungroup() %>% 
  group_by(word) %>% 
  summarise(n = sum(n))

sum_corpus_sub %>% 
  slice_max(n, n = 100) %>%
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(shape = "circle") +
  scale_radius(range = c(3, 7), limits = c(0, NA)) +
  scale_color_gradient(low = "grey", high = "red") +
  labs(
    title = "Most Frequent Words",
    subtitle = "Word Cloud of Lam's Speech"
  ) +
  theme_minimal()
ggsave("teda_subset_lam.png", width = 5, height = 4)


## 4. Most Common words of Tung's speech ----

sum_corpus_sub <- word_count %>%
  filter(name == "Tung") %>%
  ungroup() %>% 
  group_by(word) %>% 
  summarise(n = sum(n))

sum_corpus_sub %>% 
  slice_max(n, n = 100) %>%
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(shape = "circle") +
  scale_radius(range = c(3, 7), limits = c(0, NA)) +
  scale_color_gradient(low = "grey", high = "red") +
  labs(
    title = "Most Frequent Words",
    subtitle = "Word Cloud of Tung's Speech"
  ) +
  theme_minimal()
ggsave("teda_subset_tung.png", width = 5, height = 4)

## 5. Most Common words of Tsang's speech ----

sum_corpus_sub <- word_count %>%
  filter(name == "Tsang") %>%
  ungroup() %>% 
  group_by(word) %>% 
  summarise(n = sum(n))

sum_corpus_sub %>% 
  slice_max(n, n = 100) %>%
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(shape = "circle") +
  scale_radius(range = c(3, 7), limits = c(0, NA)) +
  scale_color_gradient(low = "grey", high = "red") +
  labs(
    title = "Most Frequent Words",
    subtitle = "Word Cloud of Tsang's Speech"
  ) +
  theme_minimal()
ggsave("teda_subset_tsang.png", width = 5, height = 4)


## Compare speeches grouped by categorical variables--------
unique(word_count$name)

sum_corpus_sub <- word_count %>%
  filter(name %in% c("Lam", "Tsang", "Tung")) %>%
  group_by(name, word) %>% 
  summarise(n = sum(n))

### By one categorical variable ----
# Word Frequency
sum_corpus_sub %>% 
  slice_max(n, n = 50) %>%
  ggplot(aes(label = word, size = n, color = name, x = name)) +
  geom_text_wordcloud(shape = "circle") +
  scale_radius(range = c(2, 8), limits = c(0, NA)) +
  theme_minimal()+
  labs(
    title = "Most Frequent Words",
    subtitle = "Lam, Tsang, and Tung's Speech"
  )

ggsave("teda_compare_onecat_1.png", width = 6, height = 4)

sum_corpus_sub %>% 
  slice_max(n, n = 50) %>%
  ggplot(aes(label = word, size = n, color = name)) +
  geom_text_wordcloud(shape = "circle") +
  scale_radius(range = c(2, 8), limits = c(0, NA)) +
  theme_minimal() +
  facet_grid(~name)+
  labs(
    title = "Most Frequent Words",
    subtitle = "Lam, Tsang, and Tung's Speech"
  )

ggsave("teda_compare_onecat_2.png", width = 6, height = 4)

# Selected words frequency
sum_corpus_sub %>% 
  filter(word %in% c("china", "develop", "world", "govern", "economi", "financi", 
                  "econom", "technologi", "mainland", "peopl",
                  "bai")) %>% 
  group_by(name, word) %>% 
  ggplot() +
  geom_col(aes(x = word, y = n, fill = name), position = "dodge")
ggsave("selected frequent words by CE.png", width = 8, height = 5)

sum_corpus_sub %>% 
  filter(word %in% c("china", "develop", "world", "govern", "economi", "financi", 
                     "econom", "technologi", "mainland", "peopl",
                     "bai")) %>% 
  group_by(name, word) %>% 
  pivot_wider(names_from = word, values_from = n) %>% 
  write.csv("selected_wrods_frequency.csv")

sum_corpus_sub %>% 
  filter(word %in% c("china", "develop", "world", "govern", "economi", "financi", 
                     "econom", "technologi", "mainland", "peopl",
                     "bai")) %>% 
  group_by(name, word) %>% 
  pivot_wider(names_from = word, values_from = n) %>% 
  summary()
  
## tfidf
unique(word_tfidf$name)

sum_tfidf_sub <- word_tfidf %>%
  filter(name %in% c("Lam", "Tsang", "Tung")) %>%
  group_by(name, word) %>% 
  summarise(n = sum(tfidf))

## Most unique words for each CE
sum_tfidf_sub %>% 
  slice_max(n, n = 50) %>%
  ggplot(aes(label = word, size = n, color = name, x = name)) +
  geom_text_wordcloud(shape = "circle") +
  scale_radius(range = c(0, 3), limits = c(0, NA)) +
  theme_minimal()+
  labs(
    title = "Most Unique Words",
    subtitle = "Lam, Tsang, and Tung's Speech"
  )

ggsave("teda_compare_onecat_tfidf1.png", width = 6, height = 4)

sum_corpus_sub %>% 
  slice_max(n, n = 50) %>%
  ggplot(aes(label = word, size = n, color = name)) +
  geom_text_wordcloud(shape = "circle") +
  scale_radius(range = c(2, 8), limits = c(0, NA)) +
  theme_minimal() +
  facet_grid(~name)+
  labs(
    title = "Most Unique Words",
    subtitle = "Lam, Tsang, and Tung's Speech"
  )

ggsave("teda_compare_onecat_tfidf2.png", width = 6, height = 4)

### Two Cat Variables------
# In the first three speeches, what are the most frequent words
word_count %>%
  filter(speech_id %in% 1:3) %>%
  filter(chapter %in% 1:3) %>% 
  slice_max(n, n = 50) %>%
  ggplot(aes(label = word, size = n, color = name)) +
  geom_text_wordcloud(shape = "circle") +
  scale_radius(range = c(1, 6), limits = c(0, NA)) +
  theme_minimal() +
  facet_grid(speech_id~name)+
  labs(
    title = "Most Frequent Words in the First-Three Speeches",
    subtitle = "Lam, Tsang, and Tung"
  )

ggsave("teda_compare_twocat.png", width = 6, height = 4)

### Compare the initial speech of Tung and Lam
sum_lor <- word_count %>%
  filter(name %in% c("Lam", "Tung")) %>%
  group_by(name, word) %>%
  summarise(n = sum(n)) %>%
  group_by(name) %>%
  mutate(n = (n + 1) / (sum(n) + 1)) %>% 
  pivot_wider(names_from = "name", values_from = "n") %>%
  mutate(log_odds_ratio = log(`Lam` / `Tung`)) %>%
  select(word, log_odds_ratio)
sum_lor

write_csv(sum_lor, "teda_special_lor_lam_tung.csv")


sum_lor_top <- sum_lor %>% slice_max(log_odds_ratio, n = 100) %>% 
  mutate(type = "Top OR (Unique to Lam)")

sum_lor_btm <- sum_lor %>% slice_min(log_odds_ratio, n = 100) %>% 
  mutate(log_odds_ratio = abs(log_odds_ratio)) %>%
  mutate(type = "Bottom OR (Unique to Tung)")

# Combine top and bottom log odds ratios and visualize.
sum_lor_top %>% 
  bind_rows(sum_lor_btm) %>%
  ggplot(aes(label = word, color = type, x = type, size = log_odds_ratio)) +
  geom_text_wordcloud() +
  scale_radius(range = c(0, 7), limits = c(0, NA)) +
  labs(
    title = "What Words Distinguish the Two CEs?",
    subtitle = "Log Odds Ratio: Lam vs. Tung"
  ) +
  theme_minimal()  +
  theme(axis.title.x = element_blank()) 

ggsave("teda_special_lor_topbtm_1.png", width = 6, height = 4)

sum_lor_top %>% 
  bind_rows(sum_lor_btm) %>%
  ggplot(aes(label = word, color = type, size = log_odds_ratio)) +
  geom_text_wordcloud() +
  scale_radius(range = c(0, 7), limits = c(0, NA)) +
  labs(
    title = "What Words Distinguish the Two CEs?",
    subtitle = "Log Odds Ratio: Lam vs. Tung"
  ) +
  theme_minimal()  +
  facet_wrap(~type)

ggsave("teda_special_lor_topbtm_2.png", width = 6, height = 4)


# Q6.1----------------
rm(list=ls())

## Load packages------
library(tidyverse)
library(tidytext)
library(SnowballC)
library(textdata)

## Load Datasets ------
speech_raw <- readRDS("speech_raw.rds")
speech_tokenized <- readRDS("speech_tokenized.rds")
dictionary <- readRDS("dictionary.rds")
word_count <- readRDS("word_count.rds")
word_tfidf <- readRDS("word_tfidf.rds")
document_id <- readRDS("speech_raw.rds") %>% ungroup() %>% select(doc_id)
### Calculate document lengths--------
document_len <- word_count %>% 
  group_by(doc_id) %>% 
  summarise(doc_len = n())

### Load all dictionaries -------
dic_afinn <- lexicon_afinn()
dic_nrc_eil <- lexicon_nrc_eil()
dic_nrc_vad <- lexicon_nrc_vad()

dic_bing <- lexicon_bing()
dic_loughran <- lexicon_loughran()
dic_nrc <- lexicon_nrc()

### Fix the weird one --------
row_add <- names(dic_nrc_vad)
word <- row_add[1]
val <- as.numeric(row_add[2:4])

dic_nrc_vad <- lexicon_nrc_vad() %>%
  setNames(c("word", "valence", "arousal", "dominance")) %>%
  add_row(word = word, valence = val[2], arousal = val[3], dominance = val[4]) %>%
  pivot_longer(c("valence", "arousal", "dominance"), 
               names_to = "AffectDimension", values_to = "score")

### Stemming all dic -------
dic_afinn <- dic_afinn %>%
  mutate(word = wordStem(word)) %>% 
  group_by(word) %>% 
  summarise(value = mean(value))

dic_nrc_eil <- dic_nrc_eil %>% 
  mutate(term = wordStem(term)) %>% 
  group_by(term, AffectDimension) %>% 
  summarise(score = mean(score))

dic_nrc_vad <- dic_nrc_vad %>% 
  mutate(word = wordStem(word)) %>% 
  group_by(word, AffectDimension) %>% 
  summarise(score = mean(score))

dic_bing <- dic_bing %>% 
  mutate(word = wordStem(word)) %>% 
  distinct()

dic_loughran <- dic_loughran %>% 
  mutate(word = wordStem(word)) %>% 
  distinct()

dic_nrc <- dic_nrc %>% 
  mutate(word = wordStem(word)) %>% 
  distinct()

## Get document-level sentiment with categorical sentiment dictionaries ----

### BING ----

sent_bing_count <- word_count %>%
  inner_join(dic_bing, by = "word") %>%
  group_by(doc_id, sentiment) %>%
  summarise(score = sum(n)) %>%
  pivot_wider(names_from = "sentiment", values_from = "score", 
              values_fill = 0, names_prefix = "sent_bing_count_")

sent_bing_tfidf <- word_tfidf %>%
  inner_join(dic_bing, by = "word") %>%
  group_by(doc_id, sentiment) %>%
  summarise(score = sum(tfidf)) %>%
  pivot_wider(names_from = "sentiment", values_from = "score", 
              values_fill = 0, names_prefix = "sent_bing_tfidf_")

### LOUGHRAN ----

sent_loughran_count <- word_count %>%
  inner_join(dic_loughran, by = "word") %>%
  group_by(doc_id, sentiment) %>%
  summarise(score = sum(n)) %>%
  pivot_wider(names_from = "sentiment", values_from = "score", 
              values_fill = 0, names_prefix = "sent_loughran_count_")

sent_loughran_tfidf <- word_tfidf %>%
  inner_join(dic_loughran, by = "word") %>%
  group_by(doc_id, sentiment) %>%
  summarise(score = sum(tfidf)) %>%
  pivot_wider(names_from = "sentiment", values_from = "score", 
              values_fill = 0, names_prefix = "sent_loughran_tfidf_")
### NRC ----

sent_nrc_count <- word_count %>%
  inner_join(dic_nrc, by = "word") %>%
  group_by(doc_id, sentiment) %>%
  summarise(score = sum(n)) %>%
  pivot_wider(names_from = "sentiment", values_from = "score", 
              values_fill = 0, names_prefix = "sent_nrc_count_")

sent_nrc_tfidf <- word_tfidf %>%
  inner_join(dic_nrc, by = "word") %>%
  group_by(doc_id, sentiment) %>%
  summarise(score = sum(tfidf)) %>%
  pivot_wider(names_from = "sentiment", values_from = "score", 
              values_fill = 0, names_prefix = "sent_nrc_tfidf_")
## Get document-level sentiments with sentiment dictionaries with scores ----

### AFINN ----

sent_afinn_count <- word_count %>%
  inner_join(dic_afinn, by = "word") %>% 
  mutate(value_total = n * value) %>% # Multiple polarity score by word frequency
  group_by(doc_id) %>% # Group by all the meta data
  summarise(sent_afinn_count = sum(value_total))

sent_afinn_tfidf <- word_tfidf %>%
  inner_join(dic_afinn, by = "word") %>% 
  mutate(value_total = tfidf * value) %>% # Multiple polarity score by word frequency
  group_by(doc_id) %>% # Group by all the meta data
  summarise(sent_afinn_tfidf = sum(value_total))


### NRC EIL ----

sent_nrceil_count <- word_count %>%
  inner_join(dic_nrc_eil, by = c("word" = "term")) %>% 
  mutate(value_total = n * score) %>% # Multiple polarity score by word frequency
  group_by(doc_id, AffectDimension) %>% # Group by all the meta data
  summarise(value_sum = sum(value_total)) %>%
  pivot_wider(names_from = "AffectDimension", values_from = "value_sum", 
              values_fill = 0, names_prefix = "sent_nrceil_count_")

sent_nrceil_tfidf <- word_tfidf %>%
  inner_join(dic_nrc_eil, by = c("word" = "term")) %>% 
  mutate(value_total = tfidf * score) %>% # Multiple polarity score by word frequency
  group_by(doc_id, AffectDimension) %>% # Group by all the meta data
  summarise(value_sum = sum(value_total)) %>%
  pivot_wider(names_from = "AffectDimension", values_from = "value_sum", 
              values_fill = 0, names_prefix = "sent_nrceil_tfidf_")

### NRC VAD ----

sent_nrcvad_count <- word_count %>%
  inner_join(dic_nrc_vad, by = "word") %>% 
  mutate(value_total = n * score) %>% # Multiple polarity score by word frequency
  group_by(doc_id, AffectDimension) %>% # Group by all the meta data
  summarise(value_sum = sum(value_total)) %>%
  pivot_wider(names_from = "AffectDimension", values_from = "value_sum", 
              values_fill = 0, names_prefix = "sent_nrcvad_count_")

sent_nrcvad_tfidf <- word_tfidf %>%
  inner_join(dic_nrc_vad, by = "word") %>% 
  mutate(value_total = tfidf * score) %>% # Multiple polarity score by word frequency
  group_by(doc_id, AffectDimension) %>% # Group by all the meta data
  summarise(value_sum = sum(value_total)) %>%
  pivot_wider(names_from = "AffectDimension", values_from = "value_sum", 
              values_fill = 0, names_prefix = "sent_nrcvad_tfidf_")

## Put all these sentiment indicators together ----
sentiment_all <- list(
  document_id,
  sent_afinn_count, sent_afinn_tfidf,
  sent_bing_count, sent_bing_tfidf,
  sent_loughran_count, sent_loughran_tfidf,
  sent_nrc_count, sent_nrc_tfidf,
  sent_nrceil_count, sent_nrceil_tfidf,
  sent_nrcvad_count, sent_nrcvad_tfidf
) %>%
  reduce(left_join, by = "doc_id")

summary(sentiment_all)

## Sentiment index normalized------
sentiment_all <- sentiment_all %>%
  mutate_at(vars(starts_with("sent_")), ~replace_na(., 0))

sentiment_all_n <- sentiment_all %>% 
  inner_join(document_len, by = "doc_id") %>%
  rowwise() %>%
  mutate_at(vars(starts_with("sent")), ~. / doc_len) %>%
  rename_at(vars(starts_with("sent")), ~str_c(., "_n")) %>%
  select(-doc_len)

names(sentiment_all_n)

## Merge and export the results -----
sentiment_all_out <- sentiment_all %>%
  inner_join(sentiment_all_n, by = "doc_id")

write_csv(sentiment_all_out, "sentiment_analysis.csv")
write_csv(sentiment_all_n, 'sentiment_analysis_n.csv')
sent_all_n_tidy <- sentiment_all_n %>% 
  pivot_longer(cols = starts_with("sent_"), names_to = "sentiment", values_to = 'values')

sentiment_all_n <- read.csv('sentiment_analysis_n.csv')
### Plot NRC--------
sentiment_all_n %>% 
  select(doc_id, starts_with("sent_nrc_count")) %>% 
  pivot_longer(cols = starts_with("sent_nrc"), names_to = "sentiment", values_to = 'values') %>% 
  mutate(values = scale(values)) %>% 
  ggplot()+
  geom_smooth(aes(x = doc_id, y = values, color = sentiment))+
  labs(
    title = 'Sentiment of CE Speeches',
    subtitle = 'Dictionary: NRC'
  )+xlab('document (sorted by date)')
ggsave('sent_nrc.png', width = 10, height = 8)

sentiment_all_n %>% 
  select(doc_id, starts_with("sent_nrc_count")) %>% 
  pivot_longer(cols = starts_with("sent_nrc"), names_to = "sentiment", values_to = 'values') %>% 
  mutate(values = scale(values)) %>% 
  group_by(sentiment) %>% 
  summarise(
    mean = round(mean(values), digits = 4),
    median = round(median(values), digits = 4),
    sd = round(sd(values), digits = 4),
    min = round(min(values), digits = 4),
    max = round(max(values), digits = 4)
  ) %>% 
  tibble() %>% 
  write.csv('sent_nrc_count_summary.csv')

### Plot loughran--------
sentiment_all_n %>% 
  select(doc_id, starts_with("sent_loughran_count")) %>% 
  pivot_longer(cols = starts_with("sent_loughran"), names_to = "sentiment", values_to = 'values') %>% 
  mutate(values = scale(values)) %>% 
  ggplot()+
  geom_smooth(aes(x = doc_id, y = values, color = sentiment))+
  labs(
    title = 'Sentiment of CE Speeches',
    subtitle = 'Dictionary: Loughran'
  )+xlab('document (sorted by date)')
ggsave('sent_loughran.png', width = 10, height = 8)

sentiment_all_n %>% 
  select(doc_id, starts_with("sent_loughran_count")) %>% 
  pivot_longer(cols = starts_with("sent_loughran"), names_to = "sentiment", values_to = 'values') %>% 
  mutate(values = scale(values)) %>% 
  group_by(sentiment) %>% 
  summarise(
    mean = round(mean(values), digits = 4),
    median = round(median(values), digits = 4),
    sd = round(sd(values), digits = 4),
    min = round(min(values), digits = 4),
    max = round(max(values), digits = 4)
  ) %>% 
  tibble() %>% 
  write.csv('sent_loughran_count_summary.csv')

### Plot NRC tfidf--------
sentiment_all_n %>% 
  select(doc_id, starts_with("sent_nrc_tfidf")) %>% 
  pivot_longer(cols = starts_with("sent_nrc"), names_to = "sentiment", values_to = 'values') %>% 
  mutate(values = scale(values)) %>% 
  ggplot()+
  geom_smooth(aes(x = doc_id, y = values, color = sentiment))+
  labs(
    title = 'Sentiment of CE Speeches Normalized by TF-IDF',
    subtitle = 'Dictionary: NRC'
  )+xlab('document (sorted by date)')
ggsave('sent_nrc_tfidf.png', width = 10, height = 8)

sentiment_all_n %>% 
  select(doc_id, starts_with("sent_nrc_tfidf")) %>% 
  pivot_longer(cols = starts_with("sent_nrc"), names_to = "sentiment", values_to = 'values') %>% 
  mutate(values = scale(values)) %>% 
  group_by(sentiment) %>% 
  summarise(
    mean = round(mean(values), digits = 4),
    median = round(median(values), digits = 4),
    sd = round(sd(values), digits = 4),
    min = round(min(values), digits = 4),
    max = round(max(values), digits = 4)
  ) %>% 
  tibble() %>% 
  write.csv('sent_nrc_tfidf_summary.csv')

### Plot Loughran tfidf--------
sentiment_all_n %>% 
  select(doc_id, starts_with("sent_loughran_tfidf")) %>% 
  pivot_longer(cols = starts_with("sent_loughran"), names_to = "sentiment", values_to = 'values') %>% 
  mutate(values = scale(values)) %>% 
  ggplot()+
  geom_smooth(aes(x = doc_id, y = values, color = sentiment))+
  labs(
    title = 'Sentiment of CE Speeches Normalized by TF-IDF',
    subtitle = 'Dictionary: Loughran'
  )+xlab('document (sorted by date)')
ggsave('sent_loughran_tfidf.png', width = 10, height = 8)

sentiment_all_n %>% 
  select(doc_id, starts_with("sent_loughran_count")) %>% 
  pivot_longer(cols = starts_with("sent_loughran"), names_to = "sentiment", values_to = 'values') %>% 
  mutate(values = scale(values)) %>% 
  group_by(sentiment) %>% 
  summarise(
    mean = round(mean(values), digits = 4),
    median = round(median(values), digits = 4),
    sd = round(sd(values), digits = 4),
    min = round(min(values), digits = 4),
    max = round(max(values), digits = 4)
  ) %>% 
  tibble() %>% 
  write.csv('sent_loughran_iftdf_summary.csv')


# Q6.2----------------

rm(list=ls())
## Load packages ----

library(tidyverse)
library(tidytext)
library(ggwordcloud)
ggplot2::theme_set(theme_minimal()) 
library(tm)
library(topicmodels)
library(reshape2)

## Load cleaned text data ----

dictionary <- readRDS("dictionary.rds")
word_count <- readRDS("word_count.rds")
word_tfidf <- readRDS("word_tfidf.rds")
speech_tokenized <- readRDS("speech_tokenized.rds")
  
## Document matrix ------
dtm <- word_count %>% cast_dtm(doc_id, word, n)

## Fit Topic Models ----

### Set number of topics
K <- 20

#### Set random number generator seed
set.seed(1122)

#### compute the LDA model, inference via 1000 iterations of Gibbs sampling
m_tm <- LDA(dtm, K, method="Gibbs", control=list(iter = 500, verbose = 25))
saveRDS(m_tm, "model_topicmodel.rds")

## Retrieve results from topic models ----

m_tm <- readRDS("model_topicmodel.rds")
metadata <- readRDS("speech_raw.rds") %>% as_tibble() %>%
  select(doc_id, name, date)

### beta: How words map to topics
sum_tm_beta <- tidy(m_tm, matrix = "beta")

## gamma: How documents map on topics
sum_tm_gamma <- tidy(m_tm, matrix = "gamma") %>%
  rename("doc_id" = "document") %>%
  mutate(doc_id = as.integer(doc_id)) 

sum_tm_gamma <- sum_tm_gamma %>%
  inner_join(metadata, by = "doc_id")

sum_tm_gamma_w <- sum_tm_gamma %>%
  inner_join(metadata, by = "doc_id") %>%
  pivot_wider(names_from = "topic", values_from = "gamma", names_prefix = "topic_gamma_")


## Topics EDA ----

sum_tm_gamma %>%
  ggplot(aes(x = gamma)) +
  geom_density() +
  facet_wrap(~topic) +
  labs(
    title = "Topic Modeling Descriptive Statistics",
    subtitle = "Distribution of gamma"
  )

## Topics for human interpretation-----

### 1. Interpret topic meanings using top words ----

### Get top words associated with topics ----

TOP_N_WORD <- 10

speech_raw <- readRDS('speech_raw.rds')

topic_top_word <- sum_tm_beta %>%
  rename("word" = "term") %>%
  group_by(topic) %>%
  slice_max(beta, n = TOP_N_WORD) %>%
  arrange(topic, desc(beta))

write_csv(topic_top_word, "tm_topic_top_word.csv")

topic_top_word %>%
  mutate(word = reorder_within(word, beta, topic)) %>%
  ggplot(aes(y = word, x = beta)) +
  geom_bar(stat = "identity") +
  facet_wrap(~topic, scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Topic Modeling",
    subtitle = "Top words associated with each topic"
  )

ggsave("ie_tm_beta_bc.png", width = 10, height = 8)

# Present in word cloud, don't use it, very messy
topic_top_word %>%
  ggplot(aes(label = word, size = beta)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 8) +
  facet_grid(~factor(topic)) +
  labs(
    title = "Topic Modeling: Top words associated with each topic"
  )

ggsave("ie_tm_beta_wc.png", width = 20, height = 10)

## 2. Validate topic meanings using example documents ----
TOP_N_DOC = 10

### Load raw text to get the original text
document_raw <- readRDS("speech_raw.rds")

topic_top_doc <- sum_tm_gamma %>%
  group_by(topic) %>%
  slice_max(gamma, n = TOP_N_DOC) %>%
  arrange(desc(gamma)) %>%
  ungroup() %>%
  inner_join(document_raw, by = "doc_id") %>%
  arrange(topic, desc(gamma))

write_csv(topic_top_doc, "tm_topic_top_doc.csv")

## Assign names to recognized topics-------
topic_name <- tribble(
  ~ topic, ~ topic_name, ~ topic_description,
  1, "Public Health", "Health Infrastructure and Service",
  2, "Law and Legislation", "HK Law and Constitution",
  3, "Pearl River Delta", "Interconnection with Mainland Cities",
  4, "Economic Challenges", "Domestic and World Economy",
  5, "HK Government", "",
  6, "Cultural Affairs", "Arts, Creativity, and Cultural Events",
  7, "Higher Education", "University and Academic Appointment",
  8, "Mainland Business Opportunity", "",
  9, "Sustainable Development", "Environmental Issues",
  10, "Regional Financial Market", "",
  11, "Intermediate Education", "Schools",
  12, "Techonology", "Support for Technological Innovation",
  13, "Housing", "Housing Issues",
  14, "", "",
  15, "Belt and Road Initiative", "",
  16, "The Pandemic", "",
  17, "One Country, Two Systems", "",
  18, "Civil Servants", "",
  19, "Tourism", "",
  20, "Ceremonial Events", "Speeches at Ceremonies"
)

topic_name

library(lubridate)
# Clean gamma output
sum_tm_gamma_interpreted <- sum_tm_gamma %>%
  inner_join(topic_name, by = "topic") %>%
  inner_join(metadata, by = "doc_id") %>% 
  select(doc_id, topic, gamma, name.x, date.x, topic_name, topic_description) %>% 
  mutate(date = ymd(date.x), name = name.x) %>% 
  select(doc_id, topic, gamma, name, date, topic_name, topic_description)

# Check change of Civil servants topic

sum_tm_gamma_interpreted %>%
  filter(topic_name == "Civil Servants") %>%
  ggplot() +
  geom_point(aes(x = date, y = gamma))+
  facet_grid(.~name, scales = "free_x")+
  labs(
    title = "The Change of Topics Related to Civil Servants",
    subtitle = "Scatter Plot"
  )
ggsave("tm_civil_servants.png")



# 1C2S
sum_tm_gamma_interpreted %>%
  filter(topic_name == "One Country, Two Systems") %>%
  ggplot() +
  geom_point(aes(x = date, y = gamma))+
  facet_grid(.~name, scales = "free_x")+
  labs(
    title = "The Change of Topics Related to 'One Country Two System'",
    subtitle = "Scatter Plot"
  )
ggsave("tm_1c2s.png")

# Housing
sum_tm_gamma_interpreted %>%
  filter(topic_name == "Housing") %>%
  ggplot() +
  geom_point(aes(x = date, y = gamma))+
  facet_grid(.~name, scales = "free_x")+
  labs(
    title = "The Change of Topics Related to Housing Issues",
    subtitle = "Scatter Plot"
  )
ggsave("tm_housing.png")
#Pearl River Delta
# Housing
sum_tm_gamma_interpreted %>%
  filter(topic_name == "Pearl River Delta") %>%
  ggplot() +
  geom_point(aes(x = date, y = gamma))+
  facet_grid(.~name, scales = "free_x")+
  labs(
    title = "The Change of Topics Related to the Pearl River Delta",
    subtitle = "Scatter Plot"
  )
ggsave("tm_zhusanjiao.png")
#Ceremonial Events
sum_tm_gamma_interpreted %>%
  filter(topic_name == "Ceremonial Events") %>%
  ggplot() +
  geom_point(aes(x = date, y = gamma))+
  facet_grid(.~name, scales = "free_x")+
  labs(
    title = "The Change of Topics Related to the Ceremonial Events",
    subtitle = "Scatter Plot"
  )
ggsave("tm_ceremony.png")
## Output topic modeling results ----

## Produce visualization for *selected* topics of interest ----

### Visualization 1: Topics in bar charts ----

topic_top_word %>%
  inner_join(topic_name, by = "topic") %>% # FILTER HERE
  mutate(word = reorder_within(word, beta, topic)) %>%
  ggplot(aes(y = word, x = beta)) +
  geom_bar(stat = "identity") +
  facet_wrap(~topic+topic_name, scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Topic Modeling",
    subtitle = "Top words associated with selected topic"
  ) +
  theme_minimal()

ggsave("ie_tm_beta_bc_s.png", width = 10, height = 8)

## Still, don't use word cloud
# topic_top_word %>%
#   inner_join(topic_name, by = "topic") %>% # FILTER HERE
#   ggplot(aes(label = word, size = beta)) +
#   geom_text_wordcloud() +
#   scale_size_area(max_size = 8) +
#   facet_wrap(~factor(topic)) +
#   theme_minimal() +
#   labs(
#     title = "Topic Modeling: Top words associated with selected topics"
#   )
# 
# ggsave("ie_tm_beta_wc_s.png", width = 5, height = 4)

tm_out <- sum_tm_gamma %>%
  inner_join(topic_name, by = "topic") %>%
  group_by(doc_id, topic_name) %>%
  summarise(gamma = sum(gamma)) %>% 
  pivot_wider(names_from = "topic_name", values_from = "gamma", names_prefix = "tm_")

write_csv(tm_out, "tm_output.csv")




######################SKIP THIS PART############################################
speech_tokenized <- readRDS('speech_tokenized.rds')
## Topic modelling by CEs
word_count_lam <- speech_tokenized %>%
  filter(name == "Lam") %>%
  group_by(doc_id, name, word) %>%
  summarise(n = n())

word_count_tsang <- speech_tokenized %>%
  filter(name == "Tsang") %>%
  group_by(doc_id, name, word) %>%
  summarise(n = n())

word_count_tung <- speech_tokenized %>%
  filter(name == "Tung") %>%
  group_by(doc_id, name, word) %>%
  summarise(n = n())
## Perform topic modelling
dtm_lam <- word_count_lam %>% cast_dtm(doc_id, word, n)
### Set number of topics
K <- 10

#### Set random number generator seed
set.seed(1122)

#### compute the LDA model, inference via 1000 iterations of Gibbs sampling
m_tm_lam <- LDA(dtm_lam, K, method="Gibbs", control=list(iter = 500, verbose = 25))
saveRDS(m_tm_lam, "model_topicmodel_lam.rds")
### beta: How words map to topics
sum_tm_beta_lam <- tidy(m_tm_lam, matrix = "beta")

## gamma: How documents map on topics
sum_tm_gamma_lam <- tidy(m_tm_lam, matrix = "gamma") %>%
  rename("doc_id" = "document") %>%
  mutate(doc_id = as.integer(doc_id))


 ## Topics EDA

 sum_tm_gamma_lam %>%
   ggplot(aes(x = gamma)) +
   geom_density() +
   facet_wrap(~topic) +
   labs(
     title = "Topic Modeling Descriptive Statistics",
     subtitle = "Distribution of gamma of Lam's Speech"
   )

 ggsave("ie_tm_beta_wc_lam.png", width = 20, height = 10)
 ## Topics for human interpretation-----

 ### 1. Interpret topic meanings using top words ----

 ### Get top words associated with topics ----

 TOP_N_WORD <- 10

 speech_raw <- readRDS('speech_raw.rds')

 topic_top_word_lam <- sum_tm_beta_lam %>%
   rename("word" = "term") %>%
   group_by(topic) %>%
   slice_max(beta, n = TOP_N_WORD) %>%
   arrange(topic, desc(beta))

 write_csv(topic_top_word_lam, "tm_topic_top_word_lam.csv")

 topic_top_word_lam %>%
   mutate(word = reorder_within(word, beta, topic)) %>%
   ggplot(aes(y = word, x = beta)) +
   geom_bar(stat = "identity") +
   facet_wrap(~topic, scales = "free_y") +
   scale_y_reordered() +
   labs(
     title = "Topic Modeling",
     subtitle = "Top words associated with each topic"
   )

 ggsave("ie_tm_beta_bc_lam.png", width = 10, height = 8)

 # Present in word cloud, don't use it, very messy
 topic_top_word_lam %>%
   ggplot(aes(label = word, size = beta)) +
   geom_text_wordcloud() +
   scale_size_area(max_size = 8) +
   facet_grid(~factor(topic)) +
   labs(
     title = "Topic Modeling: Top words associated with each topic (Lam)"
   )

 ggsave("ie_tm_beta_wc_lam.png", width = 20, height = 10)

 ## 2. Validate topic meanings using example documents ----
 TOP_N_DOC = 10

 ### Load raw text to get the original text
 document_raw <- readRDS("speech_raw.rds")

 topic_top_doc_lam <- sum_tm_gamma_lam %>%
   group_by(topic) %>%
   slice_max(gamma, n = TOP_N_DOC) %>%
   arrange(desc(gamma)) %>%
   ungroup() %>%
   inner_join(document_raw, by = "doc_id") %>%
   arrange(topic, desc(gamma))

 write_csv(topic_top_doc_lam, "tm_topic_top_doc_lam.csv")

 ## Assign names to recognized topics-------
 topic_name_lam <- tribble(
   ~ topic, ~ topic_name, ~ topic_description,
   1, "Housing and LanD", "",
   2, "Construction", "",
   3, "Belt and Road Initiative", "",
   4, "Cultural Affairs", "",
   5, "Education", "",
   6, "National Security Law", "",
   7, "COVID-19 Pandemic", "",
   8, "The Greater Bay Area", "",
   9, "Technology", "",
   10, "HK Government", ""
   )

 topic_name_lam

 ## Output topic modeling results ----

 ## Produce visualization for *selected* topics of interest ----

 ### Visualization 1: Topics in bar charts ----

 topic_top_word_lam %>%
   inner_join(topic_name_lam, by = "topic") %>% # FILTER HERE
   mutate(word = reorder_within(word, beta, topic)) %>%
   ggplot(aes(y = word, x = beta)) +
   geom_bar(stat = "identity") +
   facet_wrap(~topic+topic_name, scales = "free_y") +
   scale_y_reordered() +
   labs(
     title = "Topic Modeling of Lam's Speech",
     subtitle = "Top words associated with selected topic"
   ) +
   theme_minimal()

 ggsave("ie_tm_beta_bc_s_lam.png", width = 10, height = 8)

 ## Still, don't use word cloud
  topic_top_word %>%
    inner_join(topic_name, by = "topic") %>% # FILTER HERE
    ggplot(aes(label = word, size = beta)) +
    geom_text_wordcloud() +
    scale_size_area(max_size = 8) +
    facet_wrap(~factor(topic)) +
    theme_minimal() +
    labs(
      title = "Topic Modeling: Top words associated with selected topics"
    )
 
  ggsave("ie_tm_beta_wc_s.png", width = 5, height = 4)

 tm_out_lam <- sum_tm_gamma_lam %>%
   inner_join(topic_name_lam, by = "topic") %>%
   group_by(doc_id, topic_name) %>%
   summarise(gamma = sum(gamma)) %>%
   pivot_wider(names_from = "topic_name", values_from = "gamma", names_prefix = "tm_")

 write_csv(tm_out_lam, "tm_output_lam.csv")









 ## Perform topic modelling
 dtm_tsang <- word_count_tsang %>% cast_dtm(doc_id, word, n)
 ### Set number of topics
 K <- 10

 #### Set random number generator seed
 set.seed(1122)

 #### compute the LDA model, inference via 1000 iterations of Gibbs sampling
 m_tm_tsang <- LDA(dtm_tsang, K, method="Gibbs", control=list(iter = 500, verbose = 25))
 saveRDS(m_tm_tsang, "model_topicmodel_tsang.rds")
 ### beta: How words map to topics
 sum_tm_beta_tsang <- tidy(m_tm_tsang, matrix = "beta")

 ## gamma: How documents map on topics
 sum_tm_gamma_tsang <- tidy(m_tm_tsang, matrix = "gamma") %>%
   rename("doc_id" = "document") %>%
   mutate(doc_id = as.integer(doc_id))

 ## Topics EDA ----

 sum_tm_gamma_tsang %>%
   ggplot(aes(x = gamma)) +
   geom_density() +
   facet_wrap(~topic) +
   labs(
     title = "Topic Modeling Descriptive Statistics",
     subtitle = "Distribution of gamma of Tsang's Speech"
   )

 ggsave("ie_tm_beta_wc_tsang.png", width = 20, height = 10)
 ## Topics for human interpretation-----

 ### 1. Interpret topic meanings using top words ----

 ### Get top words associated with topics ----

 TOP_N_WORD <- 10

 speech_raw <- readRDS('speech_raw.rds')

 topic_top_word_tsang <- sum_tm_beta_tsang %>%
   rename("word" = "term") %>%
   group_by(topic) %>%
   slice_max(beta, n = TOP_N_WORD) %>%
   arrange(topic, desc(beta))

 write_csv(topic_top_word_tsang, "tm_topic_top_word_tsang.csv")

 topic_top_word_tsang %>%
   mutate(word = reorder_within(word, beta, topic)) %>%
   ggplot(aes(y = word, x = beta)) +
   geom_bar(stat = "identity") +
   facet_wrap(~topic, scales = "free_y") +
   scale_y_reordered() +
   labs(
     title = "Topic Modeling of Tsang's Speech",
     subtitle = "Top words associated with each topic"
   )

 ggsave("ie_tm_beta_bc_tsang.png", width = 10, height = 8)

 # Present in word cloud, don't use it, very messy
 topic_top_word_tsang %>%
   ggplot(aes(label = word, size = beta)) +
   geom_text_wordcloud() +
   scale_size_area(max_size = 8) +
   facet_grid(~factor(topic)) +
   labs(
     title = "Topic Modeling: Top words associated with each topic (Tsang)"
   )

 ggsave("ie_tm_beta_wc_tsang.png", width = 20, height = 10)

 ## 2. Validate topic meanings using example documents ----
 TOP_N_DOC = 10

 ### Load raw text to get the original text
 document_raw <- readRDS("speech_raw.rds")

 topic_top_doc_tsang <- sum_tm_gamma_tsang %>%
   group_by(topic) %>%
   slice_max(gamma, n = TOP_N_DOC) %>%
   arrange(desc(gamma)) %>%
   ungroup() %>%
   inner_join(document_raw, by = "doc_id") %>%
   arrange(topic, desc(gamma))

 write_csv(topic_top_doc_tsang, "tm_topic_top_doc_tsang.csv")

 ## Assign names to recognized topics-------
 topic_name_tsang <- tribble(
   ~ topic, ~ topic_name, ~ topic_description,
   1, "Country and World", "",
   2, "HK Public Policies", "",
   3, "Mainland Business Opportunities", "",
   4, "Environment", "",
   5, "Hk Government", "",
   6, "Financial Market", "",
   7, "Technology", "",
   8, "Tourism", "",
   9, "Industies", "",
   10, "World Economy", ""
 )

 topic_name_tsang

 ## Output topic modeling results ----

 ## Produce visualization for *selected* topics of interest ----

 ### Visualization 1: Topics in bar charts ----

 topic_top_word_tsang %>%
   inner_join(topic_name_tsang, by = "topic") %>% # FILTER HERE
   mutate(word = reorder_within(word, beta, topic)) %>%
   ggplot(aes(y = word, x = beta)) +
   geom_bar(stat = "identity") +
   facet_wrap(~topic+topic_name, scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Topic Modeling of Tsang's Speech",
    subtitle = "Top words associated with selected topic"
  ) +
  theme_minimal()

ggsave("ie_tm_beta_bc_s_tsang.png", width = 10, height = 8)

## Still, don't use word cloud
# topic_top_word %>%
#   inner_join(topic_name, by = "topic") %>% # FILTER HERE
#   ggplot(aes(label = word, size = beta)) +
#   geom_text_wordcloud() +
#   scale_size_area(max_size = 8) +
#   facet_wrap(~factor(topic)) +
#   theme_minimal() +
#   labs(
#     title = "Topic Modeling: Top words associated with selected topics"
#   )
#
# ggsave("ie_tm_beta_wc_s.png", width = 5, height = 4)

tm_out_tsang <- sum_tm_gamma_tsang %>%
  inner_join(topic_name_tsang, by = "topic") %>%
  group_by(doc_id, topic_name) %>%
  summarise(gamma = sum(gamma)) %>%
  pivot_wider(names_from = "topic_name", values_from = "gamma", names_prefix = "tm_")

write_csv(tm_out_tsang, "tm_output_tsang.csv")







## Perform topic modelling
dtm_tung <- word_count_tung %>% cast_dtm(doc_id, word, n)
### Set number of topics
K <- 10

#### Set random number generator seed
set.seed(1122)

#### compute the LDA model, inference via 1000 iterations of Gibbs sampling
m_tm_tung <- LDA(dtm_tung, K, method="Gibbs", control=list(iter = 500, verbose = 25))
saveRDS(m_tm_tung, "model_topicmodel_tung.rds")
### beta: How words map to topics
sum_tm_beta_tung <- tidy(m_tm_tung, matrix = "beta")

## gamma: How documents map on topics
sum_tm_gamma_tung <- tidy(m_tm_tung, matrix = "gamma") %>%
  rename("doc_id" = "document") %>%
  mutate(doc_id = as.integer(doc_id))

## Topics EDA ----

sum_tm_gamma_tung %>%
  ggplot(aes(x = gamma)) +
  geom_density() +
  facet_wrap(~topic) +
  labs(
    title = "Topic Modeling Descriptive Statistics",
    subtitle = "Distribution of gamma of Tung's Speech"
  )

ggsave("ie_tm_beta_wc_tung.png", width = 20, height = 10)
## Topics for human interpretation-----

### 1. Interpret topic meanings using top words ----

### Get top words associated with topics ----

TOP_N_WORD <- 10

speech_raw <- readRDS('speech_raw.rds')

topic_top_word_tung <- sum_tm_beta_tung %>%
  rename("word" = "term") %>%
  group_by(topic) %>%
  slice_max(beta, n = TOP_N_WORD) %>%
  arrange(topic, desc(beta))

write_csv(topic_top_word_tung, "tm_topic_top_word_tung.csv")

topic_top_word_tung %>%
  mutate(word = reorder_within(word, beta, topic)) %>%
  ggplot(aes(y = word, x = beta)) +
  geom_bar(stat = "identity") +
  facet_wrap(~topic, scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Topic Modeling of Tung's Speech",
    subtitle = "Top words associated with each topic"
  )

ggsave("ie_tm_beta_bc_tung.png", width = 10, height = 8)

# Present in word cloud, don't use it, very messy
topic_top_word_tsang %>%
  ggplot(aes(label = word, size = beta)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 8) +
  facet_grid(~factor(topic)) +
  labs(
    title = "Topic Modeling: Top words associated with each topic (Tung)"
  )

ggsave("ie_tm_beta_wc_tung.png", width = 20, height = 10)

## 2. Validate topic meanings using example documents ----
TOP_N_DOC = 10

### Load raw text to get the original text
document_raw <- readRDS("speech_raw.rds")

topic_top_doc_tung <- sum_tm_gamma_tung %>%
  group_by(topic) %>%
  slice_max(gamma, n = TOP_N_DOC) %>%
  arrange(desc(gamma)) %>%
  ungroup() %>%
  inner_join(document_raw, by = "doc_id") %>%
  arrange(topic, desc(gamma))

write_csv(topic_top_doc_tung, "tm_topic_top_doc_tung.csv")

## Assign names to recognized topics-------
topic_name_tung <- tribble(
  ~ topic, ~ topic_name, ~ topic_description,
  1, "Public Health", "",
  2, "Technology", "",
  3, "HKSAR CE", "",
  4, "One Country, Two Systems", "",
  5, "Economy", "",
  6, "Mainland Business Opportunities", "",
  7, "The Society", "",
  8, "Education", "",
  9, "Financial Market", "",
  10, "World Economy", ""
)

topic_name_tung

## Output topic modeling results ----

## Produce visualization for *selected* topics of interest ----

### Visualization 1: Topics in bar charts ----

topic_top_word_tung %>%
  inner_join(topic_name_tsang, by = "topic") %>% # FILTER HERE
  mutate(word = reorder_within(word, beta, topic)) %>%
  ggplot(aes(y = word, x = beta)) +
  geom_bar(stat = "identity") +
  facet_wrap(~topic+topic_name, scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Topic Modeling of Tung's Speech",
    subtitle = "Top words associated with selected topic"
  ) +
  theme_minimal()

ggsave("ie_tm_beta_bc_s_tung.png", width = 10, height = 8)

## Still, don't use word cloud
# topic_top_word %>%
#   inner_join(topic_name, by = "topic") %>% # FILTER HERE
#   ggplot(aes(label = word, size = beta)) +
#   geom_text_wordcloud() +
#   scale_size_area(max_size = 8) +
#   facet_wrap(~factor(topic)) +
#   theme_minimal() +
#   labs(
#     title = "Topic Modeling: Top words associated with selected topics"
#   )
#
# ggsave("ie_tm_beta_wc_s.png", width = 5, height = 4)

tm_out_tsang <- sum_tm_gamma_tsang %>%
  inner_join(topic_name_tsang, by = "topic") %>%
  group_by(doc_id, topic_name) %>%
  summarise(gamma = sum(gamma)) %>%
  pivot_wider(names_from = "topic_name", values_from = "gamma", names_prefix = "tm_")

write_csv(tm_out_tsang, "tm_output_tsang.csv")
######################END OF SKIP############################################


# Q7------------
rm(list=ls())
.rs.restartR()

# Load general-purpose packages ----

library(tidyverse)
library(tidytext)

# Load text mining package ----

# install.packages("spacyr")
library(spacyr)

# library(reticulate)
# install_miniconda()

# # Run the code below for first run
# # Before you do, set memory to 3G... Otherwise it will fail.
# # This is a complicated package consuming LOTS of computing resources...
# # That's why we did not start with it (and you don't need it in may occassions)
spacy_install()

## https://spacy.io/universe/project/spacyr
## https://spacyr.quanteda.io/articles/using_spacyr.html

## Reminder: If you want to install this in your local machine:
## Intstalling this package might not be entirely straightforward
## Just install all packages (including miniconda) the system ask you to install.
## Look at the messages in the console

## SpaCy support the Chinese language as well.
### https://spacy.io/models/zh

### use spacy_download_langmodel() to download models you need
### Use with caution. Check if the models fit the type of text you work on

# Initialize SpaCy

spacy_initialize()


# Load documents ----

document_raw <- readRDS("speech_raw.rds")

# Move text data into a vector
texts_raw <- document_raw$text
# Name elements of the vector so that spacyr can recognize document IDs
names(texts_raw) <- document_raw$doc_id

rm(document_raw)

# Remove the dataset of raw document from space

# Parse text (sample) ----

texts_parsed_sample <- texts_raw[1:10] %>%
  spacy_parse(
    lemma = TRUE,
    entity = TRUE,
    pos = TRUE,
    tag = TRUE
  ) %>%
  as_tibble()

texts_parsed_sample

## An overview of what you get: https://spacy.io/usage/linguistic-features

## What do "tag" mean: https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html
## What do "pos" mean: https://universaldependencies.org/u/pos/


# Parse text (in batch) ----

## To make the workload manageable by the system (instead of crowding the memory)
## you will need to process the data in batch.

BATCH_SIZE = 20

n_batches <- ceiling(length(texts_raw) / BATCH_SIZE)

text_parsed_b <- list()

# Create folder to store interim restuls
dir.create("spacy_temp/")

for (j in 1:n_batches){
  i_min <- (j - 1) * BATCH_SIZE + 1
  i_max <- min(j * BATCH_SIZE, length(texts_raw))
  
  temp <- texts_raw[i_min:i_max] %>%
    spacy_parse(
      lemma = TRUE,
      entity = TRUE,
      pos = TRUE
    ) %>%
    as_tibble()
  
  saveRDS(temp, sprintf("spacy_temp/%s.rds", j))
  
  message(j, " of ", n_batches) # Print progress so that you can keep track
}

# Put interim output into a single dataset ----

## Read the stored interim results ----
fname_ls <- list.files("spacy_temp", full.names = TRUE)

texts_parsed_ls <- list()
for (i in seq_along(fname_ls)){
  texts_parsed_ls[[i]] <- readRDS(fname_ls[i])
  message(i, " of ", length(fname_ls))
}

## Merge into one table ----
texts_parsed_out <- reduce(texts_parsed_ls, bind_rows)

### token_id is useless when you process data in batch
texts_parsed_out <- texts_parsed_out %>% select(-token_id)

## Tidy doc_id ----

### Change doc_id to numeric if applicable
texts_parsed_out <- texts_parsed_out %>% mutate(doc_id = as.integer(doc_id))

# Take a look at the output data ----

texts_parsed_out

# Save results ----

saveRDS(texts_parsed_out, "data/document_spacy_annotate.rds")
write.csv(texts_parsed_out, "text_parsed_out.csv")
# Some Cleaning-------
remove(temp)
remove(text_parsed_b)
remove(texts_parsed_sample)
remove(texts_parsed_ls)


texts_parsed_out <- readRDS("data/document_spacy_annotate.rds")
# Add name for each doc-----
speech_raw <- readRDS('speech_raw.rds')
texts_parsed <- texts_parsed_out %>% 
  inner_join(speech_raw, by = "doc_id") %>% 
  select(doc_id, sentence_id, token, lemma, pos, entity, title, date, name, speech_id)
write.csv(texts_parsed, "text_parsed.csv")
# DEA------
texts_parsed %>% 
  ggplot()+
  geom_bar(aes(x = pos, fill = name), position = "dodge") +
  labs(
    title = "Part of Speech in CE's Speech",
    subtitle = "Bar Chart"
  )
ggsave("pos.png", width = 10, height = 8)

texts_parsed %>% 
  filter(entity %in% c("FAC_B", 'LOC_B', 'ORG_B', 'PRODUCT_I', "FAC_I", 'LOC_I',
                     'ORG_I', 'QUANTITY_B', 'CARDINAL_I', 'GPE_B', 'MONEY_B', 'PERCENT_B',
                     'QUANTITY_I', 'DATE_B', 'GPE_I', 'MONEY_I', 'TIME_B', 'DATE_I',
                     'LANGUAGE_B', 'NORP_B', 'PERSON_B', 'TIME_I', 'EVENT_B', 
                     'LAW_B', 'NORP_I', 'PERSON_I', 'WORK_OF_ART_B', 'EVENT_I',
                     'LAW_I', 'ORDINAL_B', 'PRODUCT_B', 'WORK_OF_ART_I')) %>% 
  ggplot()+
  geom_bar(aes(x = entity), position = "dodge")+
  facet_grid(.~name)+
  coord_flip()+
  labs(
    title = "Entity in the Speeches of CEs",
    subtitle = "Bar Chart"
  )
ggsave("entity_by_ce.png", width = 10, height = 8)


### Number of Sentence and Average Sentence Length----
### Number of Sentence
texts_parsed %>% 
  group_by(name, doc_id) %>% 
  summarise(number_sentence = max(sentence_id)) %>% 
  ggplot()+
  geom_col(aes(x = doc_id, y = number_sentence, fill = name))+
  geom_smooth(aes(x = doc_id, y = number_sentence))+
  labs(
    title = "Number of Sentences of CEs Speech"
  )
ggsave("number_of_sentence.png", width = 10, height = 8)
number_sentence <- texts_parsed %>% 
  group_by(name, doc_id) %>% 
  summarise(number_sentence = max(sentence_id))
write.csv(number_sentence, 'number_sentence.csv')

summary_number_sentence <- texts_parsed %>% 
  group_by(name, doc_id) %>% 
  summarise(number_sentence = max(sentence_id)) %>% 
  summarise(number_sentence_mean = mean(number_sentence),
            number_sentence_max = max(number_sentence),
            number_sentence_median = median(number_sentence),
            number_sentence_min = min(number_sentence))
write.csv(summary_number_sentence, "summary_number_sentence.csv")
# summary_number_sentence %>% 
#   ggplot()+
#   geom_col(aes(x = name, y = number_sentence_mean))
#   

### Sentence Length
word_count <- readRDS('word_count.rds')
document_len <- word_count %>% 
  group_by(doc_id) %>% 
  summarise(doc_len = n())

meaningful_sentence <- number_sentence %>% 
  inner_join(document_len, by = "doc_id") %>% 
  mutate(sentence_len = doc_len/number_sentence)

meaningful_sentence %>% 
  ggplot()+
  geom_col(aes(x = doc_id, y = sentence_len))+
  geom_smooth(aes(x = doc_id, y = sentence_len))+
  labs(
    title = "Meaningful Words in Sentences of CE's Speech"
  )
ggsave("meaningful_words_by_CE.png", width = 10, height = 8)


# Q7.3-----------
texts_parsed <- read.csv('text_parsed.csv')
speech_raw <- readRDS('speech_raw.rds')
df <- texts_parsed %>% 
  select(doc_id, sentence_id, token, lemma, pos, entity, date) %>% 
  inner_join(number_sentence, by = "doc_id")

entity_frequency <- subset(df, entity %in% c("FAC_B", 'LOC_B', 'ORG_B', 'PRODUCT_I', "FAC_I", 'LOC_I',
                                        'ORG_I', 'QUANTITY_B', 'CARDINAL_I', 'GPE_B', 'MONEY_B', 'PERCENT_B',
                                        'QUANTITY_I', 'DATE_B', 'GPE_I', 'MONEY_I', 'TIME_B', 'DATE_I',
                                        'LANGUAGE_B', 'NORP_B', 'PERSON_B', 'TIME_I', 'EVENT_B', 
                                        'LAW_B', 'NORP_I', 'PERSON_I', 'WORK_OF_ART_B', 'EVENT_I',
                                        'LAW_I', 'ORDINAL_B', 'PRODUCT_B', 'WORK_OF_ART_I')) %>% 
  group_by(doc_id, entity) %>% 
  summarise(frequency = max(cumsum(!is.na(entity)))) %>% 
  pivot_wider(names_from = entity, values_from = frequency)
  
pos_frequency <- subset(df, pos %in% c("ADJ", "ADP", "ADV",
                                                      "AUX", "CCONJ", "DET", "INTJ",
                                                      "NOUN", "NUM", "PART", "PRON",
                                                      "PROPN", "PUNCT", "SCONJ", "SPACE",
                                                      "SYM", "VERB", "X")) %>% 
  group_by(doc_id, pos) %>% 
  summarise(frequency = max(cumsum(!is.na(pos)))) %>% 
  pivot_wider(names_from = pos, values_from = frequency)
  

# pos_frequency <- df_for_regression %>% 
#   filter(pos %in% c("ADJ", "ADP", "ADV","AUX", "CCONJ", "DET", "INTJ",
#                      "NOUN", "NUM", "PART", "PRON","PROPN", "PUNCT", "SCONJ", 
#                     "SPACE","SYM", "VERB", "X")) %>% 
#   group_by(doc_id, pos) %>% 
#   summarise(frequency = max(cumsum(!is.na(pos)))) 
meta_number_sentence <- speech_raw %>% 
  select(doc_id,date) %>% 
  inner_join(number_sentence, by = "doc_id")
write.csv(meta_number_sentence, 'meta_number_sentence.csv')
meta_pos_frequency <- speech_raw %>% 
  select(doc_id, date, name) %>% 
  inner_join(pos_frequency, by = 'doc_id')
write.csv(meta_pos_frequency, 'meta_pos_frequency.csv')
meta_entity_frequency <- speech_raw %>% 
  select(doc_id,date, name) %>% 
  inner_join(entity_frequency, by = 'doc_id')
write.csv(meta_entity_frequency, 'meta_entity_frequency.csv')
## Running three regressions to predict who is delivering the speech-----
### Number of Sentence-----

library(nnet)

res_1 <- multinom(name ~ number_sentence,
                data = meta_number_sentence)
summary(res_1)
summary(res_1)$coefficients
summary(res_1)$standard.errors
z_1 <- summary(res_1)$coefficients/summary(res_1)$standard.errors
p_1 <- (1 - pnorm(abs(z_1), 0, 1))*2
p_1
confint(res, level = 0.95)

exp(summary(res_1)$coefficients)
exp(confint(res_1, level = 0.95))

### POS Frequency------

res_2 <- multinom(name ~ ADJ + ADP + ADV + AUX + CCONJ + DET + NOUN + NUM + PART + PRON + PROPN + PUNCT + SCONJ + SPACE + VERB + INTJ + X + SYM,
                data = meta_pos_frequency)
summary(res_2)
summary(res_2)$coefficients
summary(res_2)$standard.errors
z_2 <- summary(res_2)$coefficients/summary(res_2)$standard.errors
p_2 <- (1 - pnorm(abs(z_2), 0, 1))*2
p_2
confint(res_2, level = 0.95)

exp(summary(res_2)$coefficients)
exp(confint(res_2, level = 0.95))

### Entity Frequency------

res_3 <- multinom(name ~ DATE_B + DATE_I + GPE_B + GPE_I + ORG_B + ORG_I + PERSON_B + PERSON_I + TIME_B + TIME_I + MONEY_B + MONEY_I,
                  data = meta_entity_frequency)
summary(res_3)
summary(res_3)$coefficients
summary(res_3)$standard.errors
z <- summary(res_3)$coefficients/summary(res_3)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p
confint(res_3, level = 0.95)

exp(summary(res_3)$coefficients)
exp(confint(res_3, level = 0.95))
