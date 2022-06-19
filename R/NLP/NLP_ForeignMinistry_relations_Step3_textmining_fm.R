# Text Mining Chinese Words
# POLI3148 FP
# Albert Baichen Du
setwd("~/OneDrive/Documents/HKU/Year 3/Sem 1/POLI3148/final_project")
library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(stringr)

text <- read.csv("unnested.csv")
text <- text %>% 
  select(continent:words)

# Word count ------
text_count <- text %>% 
  group_by(country, doc_id, words) %>% 
  count()
# Construct Dictionary-------
dictionary <- text_count %>% 
  group_by(words) %>% 
  summarise(
    term_frequency = sum(n),
    document_frequency = n()
  )
summary(dictionary$term_frequency)
summary(dictionary$document_frequency)

p_tf_1 <- dictionary %>% 
  ggplot()+geom_histogram(aes(x = term_frequency), bins = 40)+
  xlab("Term Frequency")+
  labs(
    title = "DIstribution of Post-Wrangling Term Frequencies",
    subtitle = "Histogram"
  )
p_tf_1


# Diagnostics 1: Pre-wrangled most frequent words ----

dictionary %>% 
  slice_max(term_frequency, n = 100) %>% 
  arrange(desc(term_frequency))

dictionary %>% 
  slice_max(term_frequency, n = 200) %>% 
  ggplot(aes(label = words, size = term_frequency)) +
  geom_text_wordcloud() +
  labs(
    title = "Most Frequent Words (Pre-Wrangled)"
  ) +
  theme_minimal()
# This does not work, since wordcloud is not compatabile with zh-cn

# Diagnostic 2: Pre-wrangled least frequent words -----
dictionary %>% 
  slice_min(term_frequency, n = 1) %>% 
  arrange(term_frequency) %>% # Sort the rows by term frequency
  View()
# We will skip all wordcloud drawing. They won't work.

# Text data wrangling ------
## Duplicate the text data first-----
text_copy <- text

## Check some wierd words-----
check_anomaly <- text_copy %>%
  filter(str_detect(words, "^_") | str_detect(words, "_$"))

nrow(check_anomaly) # Nothing, Great!
check_anomaly


## Remove tokens that contain numbers ------
text_copy %>%
  filter(str_detect(words, "[:digit:]"))

text_copy <- text_copy %>%
  filter(!str_detect(words, "[:digit:]"))

## Remove tokens that contain English letters -----
text_copy <- text_copy %>%
  filter(!str_detect(words, "[A-Za-z]"))

# Remake the dictionary-----
text_count_2 <- text_copy %>%
  group_by(country, doc_id, words) %>%
  count()

dictionary_2 <- text_count_2 %>%
  group_by(words) %>%
  summarise(
    term_frequency = sum(n), 
    document_frequency = n())

year_month_count <- text_count_2 %>% 
  filter(words == '年'|words == "月")

year_month_freq <- year_month_count %>% 
  pivot_wider(names_from = "words", values_from = "n") %>% 
  mutate(year_month_freq = 年 + 月)

name_en <- read.csv("countries_en.csv")
name_zh <- read.csv('countries_zh.csv')
name_merged <- name_zh %>% 
  left_join(name_en, by = 'id') %>% 
  select(id, name.x, name.y)

final <- year_month_freq %>% 
  left_join(name_merged, by = c("country" = 'name.x')) %>% 
  select(country, doc_id, year_month_freq, name.y)

final$country <- as.character(final$country)

write_csv(final, 'final.csv')
