# Environment -------------------------------------------------------------
library(stringr)
library(tidyverse)
library(jiebaR)
library(tm)
library(topicmodels)
library(dplyr)
library(ggplot2)
library(ggthemes)
baidu_stopwords <- read.table("~/Documents/Documents - IreneLIU’s MacBook Air/23-24 Sem1/POLI3148_Data/FinalProject_POLI3148/baidu_stopwords.txt", quote="\"", comment.char="")
baidu_stopwords4topic <- read.table("~/Documents/Documents - IreneLIU’s MacBook Air/23-24 Sem1/POLI3148_Data/FinalProject_POLI3148/baidu_stopwords_4topic.txt", quote="\"", comment.char="")

nvxing0 <- read_csv("Documents/Documents - IreneLIU’s MacBook Air/23-24 Sem1/POLI3148_Data/FinalProject_POLI3148/ScrapedData/POLI3148_nvxing_wtext.csv", 
                                             col_types = cols(TarAmount = col_number(), 
                                             RaisedAmount = col_number()))
funv0 <- read_csv("Documents/Documents - IreneLIU’s MacBook Air/23-24 Sem1/POLI3148_Data/FinalProject_POLI3148/ScrapedData/POLI3148_funv_text_[1-30].csv", 
                 col_types = cols(TarAmount = col_number(), 
                                  RaisedAmount = col_number()))
funv1 <- read_csv("Documents/Documents - IreneLIU’s MacBook Air/23-24 Sem1/POLI3148_Data/FinalProject_POLI3148/ScrapedData/POLI3148_funv_text_[31-146].csv", 
                   col_types = cols(TarAmount = col_number(), 
                                    RaisedAmount = col_number()))
funv2 <- read_csv("Documents/Documents - IreneLIU’s MacBook Air/23-24 Sem1/POLI3148_Data/FinalProject_POLI3148/ScrapedData/POLI3148_funv_text_[147-174].csv", 
                   col_types = cols(TarAmount = col_number(), 
                                    RaisedAmount = col_number()))
funv3 <- read_csv("Documents/Documents - IreneLIU’s MacBook Air/23-24 Sem1/POLI3148_Data/FinalProject_POLI3148/ScrapedData/POLI3148_funv_text_[175-500].csv", 
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
nvxing$Amount4visual = log(nvxing$RaisedAmount)+1
nvxing <- nvxing[grepl("女|妇|母|妈|她|家暴|娘|姐|妹|妪", nvxing$titlebio), ]