rm(list = ls())
library(rvest)
library(dplyr)
library(lubridate)
library(stringr)


htmls <- c("https://www.regjeringen.no/en/historical-archive/id115322/?isfilteropen=True&to=31.12.2020&from=01.01.2020&documenttype=aktuelt%2Ftalerogartikler&government=742671&utid=742672&term=COVID",
           "https://www.regjeringen.no/en/historical-archive/id115322/?from=01.01.2021&to=31.12.2021&isfilteropen=True&documenttype=aktuelt/talerogartikler&government=742671&utid=742672&term=COVID")

get_text = function(link) {
  
  subpage <- read_html(link)
  speech <- subpage %>% html_nodes(".article-body p") %>% html_text2()
  speech <- paste(speech[-1], collapse = " ")
  
  speech <- gsub("\\[.+?\\]|[\r\n]", "", speech)
  
}

nok <- tibble( titles = character(), dates = character(), speeches = character(), links = character())


for (i in c(1:length(htmls))) {
page <- read_html(htmls[i])
topics <- page %>% html_nodes('.title a') %>% html_text() 
topics <- gsub("[\r\n]+", "", topics)
topics <- gsub("\\s+", " ", topics)

dates <- page %>% html_nodes('.date') %>% html_text() 

links <- page %>% html_nodes('.title a') %>% html_attr('href') %>%
  paste("https://www.regjeringen.no", ., sep="")
speeches = sapply(links, get_text)
nok <- rbind(nok, tibble(topics, dates, speeches, links))
}


nok$dates <- dmy(nok$dates)

write.csv(nok, 'data/speeches/speeches_NOK.csv')



