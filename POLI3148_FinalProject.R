# Environment -------------------------------------------------------------
library(stringr)
library(tidyverse)
library(jiebaR)
library(tm)
library(topicmodels)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(ggthemes)
theme_set(theme_map())
library(maps)
library(mapchina)
library(sf)
sf_use_s2(FALSE)

# Display Chinese text with ggplot
library(showtext)
showtext_auto()
baidu_stopwords <- read.table("baidu_stopwords.txt", quote="\"", comment.char="")
baidu_stopwords4topic <- read.table("baidu_stopwords_4topic.txt", quote="\"", comment.char="")
topicdictionary <- read_csv("topicdictionary.csv")

nvxing0 <- read_csv("ScrapedData/POLI3148_nvxing_wtext.csv", 
                                             col_types = cols(TarAmount = col_number(), 
                                             RaisedAmount = col_number()))
funv0 <- read_csv("ScrapedData/POLI3148_funv_text_[1-30].csv", 
                 col_types = cols(TarAmount = col_number(), 
                                  RaisedAmount = col_number()))
funv1 <- read_csv("ScrapedData/POLI3148_funv_text_[31-146].csv", 
                   col_types = cols(TarAmount = col_number(), 
                                    RaisedAmount = col_number()))
funv2 <- read_csv("ScrapedData/POLI3148_funv_text_[147-174].csv", 
                   col_types = cols(TarAmount = col_number(), 
                                    RaisedAmount = col_number()))
funv3 <- read_csv("ScrapedData/POLI3148_funv_text_[175-500].csv", 
                  col_types = cols(TarAmount = col_number(), 
                                   RaisedAmount = col_number()))

nvxing <- rbind(nvxing0, funv0, funv1, funv2, funv3)


# tidytext -- tidytextmining: https://www.tidytextmining.com/
# Tokenization tool: JiebaR
# Documentation: https://qinwenfeng.com/jiebaR/
# install.packages("jiebaR")

# Pre-processing ----------------------------------------------------------
## Data Wrangling ------
nvxing$titlebio <- paste(nvxing$Title, nvxing$ShortBio)
nvxing$AmountLog = log(nvxing$RaisedAmount)+1
nvxing <- nvxing[grepl("女|妇|母|妈|她|家暴|娘|姐|妹|妪", nvxing$titlebio), ]
nvxing <- nvxing[!grepl("男童|男孩", nvxing$titlebio), ]
nvxing <- nvxing[!grepl("男童|男孩", nvxing$titlebio), ]
## Quantile & binning -----
quantile(nvxing$RaisedAmount, probs = seq(0, 1, 0.1), na.rm = TRUE)
nvxing$DonationLevel <- cut(nvxing$RaisedAmount, breaks = c(0,45000,200000,79765755), labels = c("Low", "Medium", "High"))

# Identification =====

## Topic --------
# topic dictionary
health_dict <- paste(na.omit(topicdictionary$health), collapse = "|")
career_dict <- paste(na.omit(topicdictionary$career), collapse = "|")
rights_dict <- paste(na.omit(topicdictionary$rights), collapse = "|")
mental_dict <- paste(na.omit(topicdictionary$mental), collapse = "|")
disable_dict <- paste(na.omit(topicdictionary$disable), collapse = "|")
edu_dict <- paste(na.omit(topicdictionary$education), collapse = "|")
disable_dict <- paste(na.omit(topicdictionary$disable), collapse = "|")
repro_dict <- paste(na.omit(topicdictionary$maternal_reproductive), collapse = "|")

# topic identification
nvxing$topic <- ifelse(grepl(paste(paste(rights_dict), nvxing$titlebio), "rights",
                       ifelse(grepl(paste(career_dict), nvxing$titlebio), "career",
                              ifelse(grepl(paste(disable_dict), nvxing$titlebio), "disable",
                                     ifelse(grepl(paste(edu_dict), nvxing$titlebio), "education",
                                            ifelse(grepl(paste(mental_dict), nvxing$titlebio), "mental",
                                                   ifelse(grepl(health_dict), nvxing$titlebio), "health",
                              "others"))))))


## Administration Level --------

## Geographical Area --------

## NGO Types --------

# Text Processing =====
## Tokenization 
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


# Plot ======
## Donation Distribution by topic area (log) ------


## Sum of amount by topic area -------
ggplot(sum_raised, aes(x = topic_area, y = sum_raised, fill = topic_area)) +
  geom_col() +
  geom_text(aes(label = sum_raised), vjust = -0.5, color = "black") +
  labs(title = "Sum of Amount by Topic Areas / yuan",
       x = "Topic areas",
       y = "Sum of amount raised") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))

## Topic Area Distribution for Raised Amount by Group -----

## Proportion of Donation Scale by Administration Level -----

## Donation Scale by NGO Types (bar plot) -----

## Topic Area Raised Amount by Admin Level -----

## Topic Area Raised Amount by NGO Types -----

## Topic Area Raised Amount by Province -----

# Mapping ======

## GDP by province --------
# environment and data
library(readxl)
China_province_data <- read_excel("China_province_data.xlsx")
GDP_2019_province <- China_province_data |> filter(年份 == 2019)

# load china map 
library(mapchina)
map_china <- mapchina::china

# merge dataset by province
chinamap_province$Name_Province <- gsub("省|市|自治区", "", chinamap_province$Name_Province)
choro_GDP_2019 <- merge(GDP_2019_province, chinamap_province, by.x = "地区", by.y = "Name_Province")

# mapping
ggplot() + geom_sf(data = chinamap_province) +
  geom_sf(data = choro_GDP_2019, 
          aes(geometry = geometry,
              fill = GDP)) + 
  scale_fill_gradient(low = "lightblue", high = "black") 
## Donation by province --------

## Donation by city --------
