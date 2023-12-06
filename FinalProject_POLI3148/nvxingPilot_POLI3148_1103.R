#environment
library(tidyverse)
library(jiebaR)
library(tm)
library(topicmodels)
POLI3148_nvxing_wtext <- read_csv("ScrapedData/POLI3148_nvxing_wtext.csv", 
      col_types = cols(TarAmount = col_integer(), 
      RaisedAmount = col_number(), StartTime = col_date(format = "%Y-%m-%d"), 
      EndTime = col_date(format = "%Y-%m-%d")))
stopwords <- read.csv("~/Documents/Documents - IreneLIU’s MacBook Air/23-24 Sem1/POLI3148_Data/FinalProject_POLI3148/baidu_stopwords.txt", sep="")

# Choropleth --------------------------------------------------------------
# Environment
library(ggthemes)
theme_set(theme_map())
library(sf)
sf_use_s2(FALSE)

# Display Chinese text with ggplot
library(showtext)
showtext_auto()

