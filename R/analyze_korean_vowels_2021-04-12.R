library(tidyverse)

vowels <- read_csv("result_vowels.csv", col_names = FALSE)

vowels %>%
  filter(X3 %in% c("i", "u", "o", "ae", "eo", "a")) -> vowels

colnames(vowels) <- c("speaker", "phrase", "segment", "duration", "F1_00", "F2_00", "F3_00","F1_05", "F2_05", "F3_05", "F1_10", "F2_10", "F3_10", "F1_15", "F2_15", "F3_15", "F1_20", "F2_20", "F3_20", "F1_25", "F2_25", "F3_25", "F1_30", "F2_30", "F3_30", "F1_35", "F2_35", "F3_35", "F1_40", "F2_40", "F3_40", "F1_45", "F2_45", "F3_45", "F1_50", "F2_50", "F3_50", "F1_55", "F2_55", "F3_55", "F1_60", "F2_60", "F3_60", "F1_65", "F2_65", "F3_65", "F1_70", "F2_70", "F3_70", "F1_75", "F2_75", "F3_75", "F1_80", "F2_80", "F3_80", "F1_85", "F2_85", "F3_85", "F1_90", "F2_90", "F3_90", "F1_95", "F2_95", "F3_95", "F1_100", "F2_100", "F3_100", "DeleteMe")
#vowels$F1_05 <- as.numeric(vowels$F1_05)

vowels %>% select(-DeleteMe) -> vowels

meta <- read_csv("Korean_Production_Wordlist_2021-03-01.csv")

left_join(vowels, meta, by = c("phrase" = "Carrier")) -> vowels
vowels %>% left_join(meta) -> vowels

vowels %>%
  pivot_longer(cols = starts_with("F", ignore.case = FALSE), names_to = c("formant", "timepoint"), names_sep="_") %>%
  mutate(timepoint = as.numeric(timepoint)) -> vowels
remove(test)
vowels %>% 
  pivot_wider(names_from = c("formant", "timepoint"), names_sep = "_", values_from = "value") ->test
vowels %>%
  filter(formant == "F2", vowel == "ae") %>%
  ggplot(aes(x = timepoint, y = value, group = phrase, color = prepalatal)) + geom_line()

vowels %>%
  group_by(speaker, vowel, prepalatal, formant, onset_poa) %>%
  filter(timepoint == 5, vowel == "a", onset_poa %in% c("p", "t", "k", "tc")) %>%
  summarise(meanformant = mean(value)) -> means %>%
  filter(formant == "F2", vowel == "ae") %>%
  ggplot(aes(x = timepoint, y = value, group = phrase, color = prepalatal)) + geom_line()

vowels %>%
  filter(formant == "F2", timepoint == 10, onset_poa %in% c("p", "t", "k", "tc"), prepalatal == "prepalatal") %>%
  ggplot(aes(x = onset_poa, y = value, fill = vowel)) + geom_boxplot("notch"=TRUE)# + facet_grid(prepalatal ~ .)
