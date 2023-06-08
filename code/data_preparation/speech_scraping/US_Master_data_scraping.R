library(rvest)
library(dplyr)


#1st page
#html <- "https://www.gov.uk/search/news-and-communications?level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&people%5B%5D=boris-johnson&order=updated-oldest"
#2nd page
#html <- "https://www.gov.uk/search/news-and-communications?level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-oldest&page=2&people%5B%5D=boris-johnson"
#3rd page
#html <- "https://www.gov.uk/search/news-and-communications?level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-oldest&page=3&people%5B%5D=boris-johnson"
# 4th page
html <- "https://www.gov.uk/search/news-and-communications?level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-oldest&page=4&people%5B%5D=boris-johnson"
page <- read_html(html)

topics <- page %>% html_nodes('#js-results .govuk-link') %>% html_text()
dates <-  page %>% html_nodes('time') %>% html_text()
links <- page %>% html_nodes('#js-results .govuk-link') %>% html_attr("href") %>%
  paste("https://www.gov.uk", ., sep="")

get_text = function(link) {
  
  subpage <- read_html(link)
  speech <- subpage %>% html_nodes(".govspeak p") %>% html_text() %>% paste(collapse =" " )
}

speeches = sapply(links, get_text)

BoJo4 <- data_frame(topics, dates, speeches, links)
