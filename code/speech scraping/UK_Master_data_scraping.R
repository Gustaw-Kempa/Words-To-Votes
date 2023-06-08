library(rvest)
library(dplyr)
library(lubridate)


#1st page
htmls <- c("https://www.gov.uk/search/news-and-communications?level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&people%5B%5D=boris-johnson&order=updated-oldest",
"https://www.gov.uk/search/news-and-communications?level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-oldest&page=2&people%5B%5D=boris-johnson",
"https://www.gov.uk/search/news-and-communications?level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-oldest&page=3&people%5B%5D=boris-johnson",
"https://www.gov.uk/search/news-and-communications?level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-oldest&page=4&people%5B%5D=boris-johnson")

# creation of helper functions
get_text = function(link) {
  
  subpage <- read_html(link)
  speech <- subpage %>% html_nodes(".govspeak p") %>% html_text() %>% paste(collapse =" " )
}

uk <- tibble(
  topics = character(),
  dates = character(),
  speeches = character(),
  links = character()
)
for (i in 1:4){
  page <- read_html(htmls[i])
  topics <- page %>% html_nodes('#js-results .govuk-link') %>% html_text()
  dates <-  page %>% html_nodes('time') %>% html_text()
  links <- page %>% html_nodes('#js-results .govuk-link') %>% html_attr("href") %>%
    paste("https://www.gov.uk", ., sep="")
  speeches = sapply(links, get_text)
  uk <- rbind(uk,tibble(topics, dates, speeches, links))
}
uk$dates <- dmy(uk$dates)

write.csv(uk, 'data/speeches/speeches_UK.csv')


