# Web Scrapping using R
# Albert Baichen Du
# Last update: 2021-11-08

library(rvest)
library(tidyverse)
# Get all continents----- This needs human's input
url_aisa <- "https://www.fmprc.gov.cn/web/gjhdq_676201/gj_676203/yz_676205/"
url_africa <- "https://www.fmprc.gov.cn/web/gjhdq_676201/gj_676203/fz_677316/"
url_europe <- "https://www.fmprc.gov.cn/web/gjhdq_676201/gj_676203/oz_678770/"
url_northamerica <- "https://www.fmprc.gov.cn/web/gjhdq_676201/gj_676203/bmz_679954/"
url_southamerica <- "https://www.fmprc.gov.cn/web/gjhdq_676201/gj_676203/nmz_680924/"
url_oceania <- "https://www.fmprc.gov.cn/web/gjhdq_676201/gj_676203/dyz_681240/"

# Set up a function to get all urls-----
scraplinks <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes(".gubox_link a") %>%
    rvest::html_attr("href")
  # Extract the link text
  text_ <- webpage %>%
    rvest::html_nodes(".gubox_link a") %>%
    rvest::html_text()
  # Add the header url
  continent_url_ <- url
  return(tibble(text = text_, url = url_, continent_url = continent_url_))
}
# Save the results in separate continents
africa <- scraplinks(url_africa)
asia <- scraplinks(url_aisa)
europe <- scraplinks(url_europe)
northamerica <- scraplinks(url_northamerica)
oceania <- scraplinks(url_oceania)
southamerica <- scraplinks(url_southamerica)

# Combine all datasets into one
all <- bind_rows(africa, asia, europe, northamerica, oceania, southamerica, .id = "id")
# Recode the variables
all <- all %>% 
  mutate(id = recode(id, 
                     "1" = "africa",
                     "2" = "asia", 
                     "3" = "europe",
                     "4" = "north_america",
                     "5" = "oceania", 
                     "6" = "south_america"))

# The following part is a little bit messy, but it works! 

# Delete the "./"at the initial position
library(stringr)
all$url <- str_sub(all$url, 3, -2)

# Add the suffix of /1206x0_6xxxx
all <- all %>% 
  mutate(sub_sul = url)

a <- separate(all, url, c("prefix", "suffix"), sep = "_(?!.*_)")

a$suffix <- as.numeric(a$suffix)

a <- a %>% 
  mutate(suffix_2 = suffix+2)

a$suffix <- as.character(a$suffix_2)

a$prefix <- a$prefix %>% 
  paste("x0", sep = "")

a <- a %>% 
  unite(url_country_page, prefix, suffix_2, sep = "_")
a$sub_sul <- a$sub_sul %>% 
  paste("/", sep = "")
write.csv(a, "a.csv")

a <- a %>% 
  unite(url_country, continent_url, sub_sul, sep = "")

a <- a %>% 
  mutate(url_root = url_country)

a <- a %>% 
  unite(url_full, url_country, url_country_page, sep = "")

country <- a %>% 
  select(id, text, url_full, url_root)
country

write_csv(country, "country1.csv")
##################Excute everything above to get started####################
# Retrieve Pages --------
library(rvest)

rm(list=ls())

index <- read_csv("country1.csv") %>%
  mutate(doc_id = row_number())


doc_ids <- index$doc_id
urls <- index$url_full

dir.create("parsed_html")

i <- 1

for (i in seq_along(urls)){
  html <- read_html(urls[i])
  xml2::write_html(html, sprintf("parsed_html/%s.html", doc_ids[i]))
  message(i)
}

# Parse pages --------
library(rvest)

index <- read_csv("country1.csv") %>%
  mutate(doc_id = row_number())

dir.create("parsed_text")

doc_ids <- index$doc_id

i <- 1

text_ls <- c()

for (i in seq_along(doc_ids)){
  html <- read_html(str_c("parsed_html/", doc_ids[i], ".html"))
  
  text <- html %>%
    html_nodes(".cur+ li a") %>%
    html_attr("href")
  
  text_ls[i] <- text
  # Save as txt file
  write(text, sprintf("parsed_text/%s.txt", doc_ids[i]))
}

index_text <- index %>%
  mutate(description = text_ls)

write.csv(index_text, "index_text.csv")

index_text$description <- index_text$description %>% 
  str_sub(4)

country_relation <- index_text %>% 
  unite(url_relation, url_root, description, sep = "")

write.csv(country_relation, "country_relation.csv")

# Retrieve pages again-------

library(rvest)

rm(list=ls())

index <- read_csv("country_relation.csv") %>%
  mutate(doc_id = row_number())


doc_ids <- index$doc_id
urls <- index$url_relation

dir.create("parsed_relation_html")

i <- 1

for (i in seq_along(urls)){
  html <- read_html(urls[i])
  xml2::write_html(html, sprintf("parsed_relation_html/%s.html", doc_ids[i]))
  message(i)
}

# Parse Pages again-----

library(rvest)

index <- read_csv("country_relation.csv") %>%
  mutate(doc_id = row_number())

dir.create("parsed_relation_text")

doc_ids <- index$doc_id

i <- 1

text_ls <- c()

for (i in seq_along(doc_ids)){
  html <- read_html(str_c("parsed_relation_html/", doc_ids[i], ".html"))
  
  text <- html %>%
    html_nodes(xpath = '//*[@id="content"]') %>%
    html_text()
  
  text_ls[i] <- text
  # Save as txt file
  write(text, sprintf("parsed_relation_text/%s.txt", doc_ids[i]))
}

index_text_relation <- index %>%
  mutate(description = text_ls)

write.csv(index_text_relation, "index_text_relation.csv")

