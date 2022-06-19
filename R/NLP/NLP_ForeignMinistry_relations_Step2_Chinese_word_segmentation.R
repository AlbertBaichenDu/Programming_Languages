# POLI3148 Final Project
# ALbert Baichen Du
# Segmenting Chinese Words

# install.packages("jiebaR")
library(jiebaR)
library(stringr)
library(tidyverse)
library(tidytext)
word_segr <- worker()

text_index <- read.csv("index_text_relation.csv")
doc_ids <- text_index$doc_id

dir.create("parsed_relation_text_seg")

i <- 1

text_ls <- c()


for (i in seq_along(doc_ids)){
  obj <- file(str_c("parsed_relation_text/", doc_ids[i], ".txt"), open = "r")
  
  lines <- readLines(obj)
  
  text_seg <- segment(lines, word_segr)
  
  text_seg_string <- str_c(text_seg, collapse = " ")
  
  text_ls[i] <- text_seg_string
  
  write(text_seg_string, sprintf("parsed_relation_text_seg/%s.txt", doc_ids[i]))
}

text_seg_csv <- text_index %>%
  mutate(description = text_ls)

text_seg_csv <- text_seg_csv %>% 
  rename(country = text, continent = id) %>% 
  select(continent, country, url_full, url_relation, doc_id, description)
write.csv(text_seg_csv, "text_seg.csv")

unnested <- unnest_tokens(text_seg_csv, words, description, token = "words")

unnested <- unnested %>% 
  select(continent, country, doc_id, words)

write.csv(unnested, "unnested.csv")
