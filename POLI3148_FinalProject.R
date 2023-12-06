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
admindict <- read_csv("admin_china.csv")
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
individualhealth_dict <- paste(na.omit(topicdictionary$health_individual), collapse = "|")
health_dict <- paste(na.omit(topicdictionary$health), collapse = "|")
career_dict <- paste(na.omit(topicdictionary$career), collapse = "|")
rights_dict <- paste(na.omit(topicdictionary$rights), collapse = "|")
mental_dict <- paste(na.omit(topicdictionary$mental), collapse = "|")
disable_dict <- paste(na.omit(topicdictionary$disable), collapse = "|")
edu_dict <- paste(na.omit(topicdictionary$education), collapse = "|")
disable_dict <- paste(na.omit(topicdictionary$disable), collapse = "|")
repro_dict <- paste(na.omit(topicdictionary$maternal_reproductive), collapse = "|")

# topic identification
nvxing$topic <- ifelse(grepl(paste(rights_dict), nvxing$titlebio), "rights", 
                       ifelse(grepl(paste(repro_dict), nvxing$titlebio), "maternal",
                              ifelse(grepl(paste(career_dict), nvxing$titlebio), "career",
                                     ifelse(grepl(paste(disable_dict), nvxing$titlebio), "disable",
                                            ifelse(grepl(paste(edu_dict), nvxing$titlebio), "education",
                                                   ifelse(grepl(paste(mental_dict), nvxing$titlebio), "mental",
                                                          ifelse(grepl(paste(individualhealth_dict), nvxing$titlebio), "health_individual",
                                                                 ifelse(grepl(paste(health_dict), nvxing$titlebio), "health",
                       "others"))))))))

## Administration Level --------
province_dict <- paste(na.omit(admindict$provincial), collapse = "|")
city_dict <- paste(na.omit(admindict$city), collapse = "|")

nvxing$adminlevel <- ifelse(grepl("国|中华", nvxing$Organizer), "national",
                            ifelse(grepl(paste(province_dict, collapse = "|"), nvxing$Organizer), "provincial",
                                   ifelse(grepl(paste(city_dict, collapse = "|"), nvxing$Organizer), "city",
                                          ifelse(grepl("县|区", nvxing$Organizer), "county",
                                                 "others")))) 

                       
## Geographical Area --------
nvxingmap_province <- nvxing |> filter(adminlevel == "provincial") |> 
  mutate(province = substr(Organizer, 1, 2))

nvxingmap_province$province <- gsub("内蒙", "内蒙古", nvxingmap_province$province)
nvxingmap_province <- subset(nvxingmap_province, !(province == "省直" | province == "中志"))

nvxingmap_city <- nvxing |> filter(adminlevel == "city") |> 
  mutate(city = substr(Organizer, 1, 2))

nvxingmap_city$city <- gsub("马鞍", "马鞍山", nvxingmap_city$city)
nvxingmap_city$city <- gsub("驻马", "驻马店", nvxingmap_city$city)
nvxingmap_city$city <- gsub("连云", "连云港", nvxingmap_city$city)
nvxingmap_city$city <- gsub("石家", "石家庄", nvxingmap_city$city)

nvxingmap_city <- subset(nvxingmap_city, !(city == "LG" | city == "中志" | city == "诸城"))

## NGO Types --------
nvxing$type <- ifelse(grepl("妇女发展|妇女儿童发展|妇女儿童基金会|中华|中国|慈善总会|妇联|妇女联合会", nvxing$Organizer), "GONGO",
                                                                          "NGO")
# Text Processing =====
## Tokenization 
text <- nvxing$ProjectDescription
length(text)

tokenizer <- jiebaR::worker()

customized_words <- c("李霞卿")

for (w in customized_words){
  jiebaR::new_user_word(tokenizer, w)
}

text_tokenized <- lapply(text, function(x) jiebaR::segment(x, tokenizer))
title_tokenized <- lapply(nvxing$titlebio, function(x) jiebaR::segment(x, tokenizer))

# text_tokenized[1:10]

# Make a "term-documen matrix"

d_ext <- nvxing |>
  mutate(Title_tokenized = title_tokenized, .after = titlebio) |>
  mutate(Text_tokenized = text_tokenized, .after = ProjectDescription)

d_ext_unnested <- d_ext |>
  select(`web-scraper-order`, Title, Title_tokenized, Text_tokenized, ) |>
  unnest(Text_tokenized) |>
  rename("word" = "Text_tokenized")

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
nvxing_topicplot <- nvxing |> filter(topic != "others")
nvxing_topicplot <- nvxing_topicplot |> filter(DonationLevel != "others")

## Donation Box-Dot by topic area (log) ------

ggplot(nvxing_topicplot, aes(x = topic, y = AmountLog)) +
  geom_boxplot(outlier.shape = NA, color = "red2") +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Sum of amount by topic area -------
ggplot(nvxing_topicplot, aes(x = topic, y = RaisedAmount, fill = topic)) +
  geom_col() +
  stat_summary(geom = "text", fun = sum, aes(label = round(..y..)), vjust = -0.5) +
  labs(title = "Sum of Amount by Topic Areas / yuan",
       x = "Topic areas",
       y = "Sum of amount raised") +
  scale_fill_tableau() + 
  theme_clean() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))

## Topic Area Distribution for Raised Amount by DonationBin -----
ggplot(nvxing_topicplot, aes(x = DonationLevel, y = RaisedAmount, fill = topic)) +
  geom_col(position = "fill") +
  labs(title = "Topic Area Distribution for Raised Amount",
       x = "Amount Level Bin",
       y = "Donation Raised") +
  scale_fill_tableau() + 
  theme_clean() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))
## Proportion of Donation Scale by Administration Level -----

ggplot(nvxing_topicplot, aes(x = adminlevel, y = RaisedAmount, fill = topic)) +
  geom_col(position = "stack") +
  labs(title = "Topic Distribution by AdminLevel",
       x = "Administration Leel",
       y = "Donation Raised") +
  scale_fill_tableau() + 
  theme_clean() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))

## Donation Scale by NGO Types (bar plot) -----

table(nvxing$type)  
pie(table(nvxing$type))

ggplot(nvxing_topicplot, aes(x = type, y = RaisedAmount, fill = topic)) +
  geom_col(position = "stack") +
  labs(title = "Topic Distribution by AdminLevel",
       x = "Administration Leel",
       y = "Donation Raised") +
  scale_fill_tableau() + 
  theme_clean() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))

## Raised Amount by NGO -----

ggplot(nvxing_topicplot, aes(x = type, y = DonationLevel, fill = DonationLevel)) +
  geom_col(position = "stack") +
  labs(title = "Project Level by NGO Type",
       x = "NGO Type",
       y = "Level of donation") +
  scale_fill_tableau() + 
  theme_void() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))

## Topic Area Raised Amount by Province -----

nvxingmap_province <- nvxingmap_province |> filter(topic != "others")
nvxingmap_province <- nvxingmap_province |> filter(topic != "health_individual")

ggplot(nvxingmap_province, aes(x = province, y = topic, fill = topic)) +
  geom_col(position = "stack") +
  labs(title = "Project Level by NGO Type",
       x = "NGO Type",
       y = "Level of donation") +
  scale_fill_tableau() + 
  theme_void() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Mapping ======

## GDP by province --------
# environment and data
library(readxl)
China_province_data <- read_excel("China_province_data.xlsx")
library(mapchina)
chinamap = china

# province-level map
chinamap_province = chinamap |>
  group_by(Code_Province, Name_Province) |>
  summarise(
    geometry = st_union(geometry))

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
  scale_fill_gradient(low = "lightblue", high = "black") +
  labs(title = "GDP Choropleth Map by Province")

## Donation by province --------

nvxingmap_province <- merge(nvxingmap_province, chinamap_province, 
                        by.x = "province", by.y = "Name_Province")
ggplot() + geom_sf(data = chinamap_province) +
  geom_sf(data = nvxingmap_province, 
          aes(geometry = geometry,
              fill = AmountLog)) + 
  scale_fill_gradient(low = "lightblue", high = "black") +
  labs(title = "Donation(log) by province")
## Donation by city --------

# city-level map

chinamap_perfecture = chinamap |>
  group_by(Code_Perfecture, Name_Perfecture) |>
  summarise(
    geometry = st_union(geometry))

chinamap_perfecture$Name_Perfecture <- gsub("市|自治州", "", chinamap_perfecture$Name_Perfecture)

nvxingmap_city <- merge(nvxingmap_city, chinamap_perfecture, 
                            by.x = "city", by.y = "Name_Perfecture")

ggplot() + geom_sf(data = chinamap_perfecture) +
  geom_sf(data = nvxingmap_city, 
          aes(geometry = geometry,
              fill = AmountLog)) + 
  scale_fill_gradient(low = "cyan", high = "black") +
  labs(title = "Donation(log) by province")