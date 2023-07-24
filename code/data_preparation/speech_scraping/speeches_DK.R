rm(list = ls())
library(rvest)
library(dplyr)
library(lubridate)
library(stringr)
library(googleLanguageR)
# unique Google translate API key used for translation - the path needs to be changed
gl_auth(
  "/Users/gustawkempa/Desktop/Studia/Master/Master.Data/master-377713-d6f77365d167.json"
)


html <- "https://www.dansketaler.dk/taler/frederiksen-mette/"


get_text = function(link) {
  subpage <- read_html(link)
  speech <-
    subpage %>% html_nodes(".speech-article-content") %>% html_text2()
  
  speech <- gsub("\\[.+?\\]|[\r\n]", " ", speech)
  return(speech)
}

page <- read_html(html)
topics <- page %>% html_nodes('.speech-title') %>% html_text()


dates <- page %>% html_nodes('time') %>% html_text(trim = T)

# As the dates are in Danish we need to create a dictionary

month_dict <-
  c(
    "januar" = "01",
    "februar" = "02",
    "marts" = "03",
    "april" = "04",
    "maj" = "05",
    "juni" = "06",
    "juli" = "07",
    "august" = "08",
    "september" = "09",
    "oktober" = "10",
    "november" = "11",
    "december" = "12"
  )

translate_date <- function(date) {
  month <- month_dict[unlist(strsplit(date, " "))[2]]
  date <-
    paste(unlist(strsplit(date, " "))[1], month, unlist(strsplit(date, " "))[3])
}

dates <- sapply(dates, translate_date)

links <- page %>% html_nodes('.special') %>% html_attr("href") %>%
  paste("https://www.dansketaler.dk/", ., sep = "")

dates <- dmy(dates)

dk_temp <- tibble(topics, dates, links)
dk_temp <-
  dk_temp %>% filter(dates >= "2020-01-01") %>% filter(dates <= "2021-12-31")

# getting the speeches
speeches <- sapply(dk_temp$links, get_text)


# one of the speeches was doubled
speeches[[39]] <- speeches[[39]][1]


speeches <- unlist(speeches)


topics_EN <- gl_translate(t_string = dk_temp$topics, target = "en",
                          format = "text", model = "nmt")[1]
speeches_EN <- gl_translate(t_string = speeches, target = "en",
                            format = "text", model = "nmt")[1]



dk_final <- tibble (topics_dk = dk_temp$topics, dates = dk_temp$dates, speeches_dk = unlist(speeches), links = dk_temp$links, topics_EN = topics_EN, speeches_EN = speeches_EN)


write.csv(dk_final, 'data/speeches/speeches_DK.csv')


