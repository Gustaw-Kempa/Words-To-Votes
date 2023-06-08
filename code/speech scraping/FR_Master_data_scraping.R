library(rvest)
library(dplyr)
library(googleLanguageR)
# unique Google translate API key used for translation - the path needs to be changed
gl_auth("/Users/gustawkempa/Desktop/Studia/Master/Master.Data/master-377713-d6f77365d167.json")


#page addresses - to scrape all the text the code needs to be run for each sub-page
htmls <-c( "https://www.gouvernement.fr/discours-et-rapports?page=5&thematic=%2Fapi%2Fcontent_thematics%2F87&content_type%5B%5D=speech", 
           "https://www.gouvernement.fr/discours-et-rapports?page=4&thematic=%2Fapi%2Fcontent_thematics%2F87&content_type%5B%5D=speech",
           "https://www.gouvernement.fr/discours-et-rapports?page=3&thematic=%2Fapi%2Fcontent_thematics%2F87&content_type%5B%5D=speech",
"https://www.gouvernement.fr/discours-et-rapports?page=2&thematic=%2Fapi%2Fcontent_thematics%2F87&content_type%5B%5D=speech")
# creating helper functions to read speech text and dates
get_text = function(link) {
  
  subpage <- read_html(link)
  speech <- subpage %>% html_nodes(".fr-my-1w .fr-col") %>% html_text() %>% paste(collapse =" " )
  
}
get_date = function(link) {
  
  subpage <- read_html(link)
  date <- subpage %>% html_nodes(".fr-text--sm.fr-mb-5w") %>% html_text()
  return(date)
}

# creating an empty df to be populated with scrapped data:
fr <- tibble(
  topics = character(),
  dates = character(),
  speeches = character(),
  links = character()
)

for (i in 1:4){
page <- read_html(htmls[i])

topics <- page %>% html_nodes('.fr-card__link') %>% html_text()
dates <-  page %>% html_nodes('.views-field-field-docs-start-date-time-value.text-nowrap') %>% html_text()
links <- page %>% html_nodes('.fr-card__link') %>% html_attr("href") %>%
  paste("https://www.gouvernement.fr", ., sep="")


speeches = sapply(links, get_text)
dates = sapply(links, get_date)

fr_temp <- data.frame(topics, dates, speeches, links)

fr <- rbind(fr,fr_temp)
}

# then speeches need to be translated
topics_EN <- gl_translate(t_string = fr$topics, target = "en",
                          format = "text", model = "nmt")[1]
speeches_EN <- gl_translate(t_string = fr$speeches, target = "en",
                            format = "text", model = "nmt")[1]



fr$dates <- sub("Publié ", "", fr$dates)
fr$dates <- dmy(fr$dates)

france <- tibble (topics_FR = fr$topics, dates = fr$dates, speeches_FR = fr$speeches, links = fr$links, topics_EN = topics_EN, speeches_EN = speeches_EN)
write.csv(france, 'data/speeches/Translated_FR_speeches.csv')
