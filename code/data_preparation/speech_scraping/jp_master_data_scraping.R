### For japanese speeches we will be using translations provided by the 
### prime minister website

rm(list = ls())
library(rvest)
library(dplyr)
library(lubridate)
library(stringr)


#1st page
htmls <- paste0('https://warp.ndl.go.jp/info:ndljp/pid/11547454/japan.kantei.go.jp/98_abe/statement/20200',
                c(1:9),'/index.html')

# creation of helper functions
get_text = function(link) {
  
  subpage <- read_html(link)
  speech <- subpage %>% html_nodes("#format") %>% html_text2() 
  speech <- gsub("\\[.+?\\]|[\r\n]", "", speech)

}

jp <- tibble( titles = character(), dates = character(), speeches = character(), links = character())

for (i in (1:length(htmls))){
  page <- read_html(htmls[i])
  topics <- page %>% html_nodes('.icon a') %>% html_text()
  dates <-  page %>% html_nodes('.icon') %>% html_text() %>% str_extract_all( "\\[[^\\]]+\\]")
  dates <- sapply(dates, function(x) {
    tryCatch(x[[2]], error = function(e) x)
  })
  links <- page %>% html_nodes('.icon a') %>% html_attr('href') %>% paste("https://warp.ndl.go.jp/", ., sep="")
  speeches = sapply(links, get_text)
  jp <- rbind(jp, tibble(topics, dates, speeches, links))
}
  

  
jp$dates <- mdy(jp$dates)

write.csv(jp, 'data/speeches/speeches_JP.csv')
