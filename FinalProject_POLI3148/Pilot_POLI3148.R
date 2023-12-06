# environment -------------------------------------------------------------

library(tidyverse)
library(jiebaR)
library(tm)
library(topicmodels)
baidu_stopwords <- read.table("~/Documents/Documents - IreneLIU’s MacBook Air/23-24 Sem1/POLI3148_Data/FinalProject_POLI3148/baidu_stopwords.txt", quote="\"", comment.char="")
nvxing <- read_csv("Documents/Documents - IreneLIU’s MacBook Air/23-24 Sem1/POLI3148_Data/FinalProject_POLI3148/ScrapedData/POLI3148_nvxing_wtext.csv", 
                                             col_types = cols(TarAmount = col_number(), 
                                             RaisedAmount = col_number()))


# tidytext -- tidytextmining: https://www.tidytextmining.com/
# Tokenization tool: JiebaR
# Documentation: https://qinwenfeng.com/jiebaR/
# install.packages("jiebaR")


# Pre-processing ----------------------------------------------------------

nvxing$title_bio <- nvxing %>% unite("title_bio", Title:ShortBio)

## Tokenization -----------
text <- POLI3148_Playing1029$Body
length(text)

tokenizer <- jiebaR::worker()

customized_words <- c("李霞卿")

for (w in customized_words){
  jiebaR::new_user_word(tokenizer, w)
}

body_tokenized <- lapply(text, function(x) jiebaR::segment(x, tokenizer))
title_tokenized <- lapply(POLI3148_Playing1029$Title, function(x) jiebaR::segment(x, tokenizer))

# text_tokenized[1:10]

# Make a "term-documen matrix"

d_ext <- POLI3148_Playing1029 |>
  mutate(Title_tokenized = title_tokenized, .after = Title) |>
  mutate(Body_tokenize = body_tokenized, .after = Body)


d_ext_unnested <- d_ext |>
  select(`web-scraper-order`, Title, Body_tokenize) |>
  unnest(Body_tokenize) |>
  rename("word" = "Body_tokenize")

# Remove stop word using anti_join
d_ext_remove_sw <- d_ext_unnested |>
  anti_join(baidu_stopwords, by = c("word" = "V1"))



# Most frequent words
word_freq <- d_ext_remove_sw |>
  group_by(word) |>
  count() |>
  arrange(-n)


# Topic modeling