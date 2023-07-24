rm(list = ls())
library(rvest)
library(dplyr)
library(lubridate)
library(stringr)
library(googleLanguageR)
# unique Google translate API key used for translation - the path needs to be changed
gl_auth("/Users/gustawkempa/Desktop/Studia/Master/Master.Data/master-377713-d6f77365d167.json")


htmls <- paste0("https://www.casarosada.gob.ar/informacion/discursos?start=",seq(from =280, to = 600, by = 40))


# We will define a helper function to get the text:
get_text = function(link) {
  subpage <- read_html(link)
  speech <-
    subpage %>% html_nodes("p+ p") %>% html_text2()
  
  speech <- paste(speech, collapse = " ")
  return(speech)
}


# We will create a helper function to translate the dates
month_dict <- c(
  "Enero" = "01",
  "Febrero" = "02",
  "Marzo" = "03",
  "Abril" = "04",
  "Mayo" = "05",
  "Junio" = "06",
  "Julio" = "07",
  "Agosto" = "08",
  "Septiembre" = "09",
  "Octubre" = "10",
  "Noviembre" = "11",
  "Diciembre" = "12"
)


translate_date <- function(date) {
  month <- month_dict[unlist(strsplit(date, " "))[4]]
  date <-
    paste(unlist(strsplit(date, " "))[2], month, unlist(strsplit(date, " "))[6])
}



AR <- tibble(titles = character(), dates = character(), speeches = character(), links = character())



for (i in 6:length(htmls)) {

page <- read_html(htmls[i])
topics <- page %>% html_nodes('h3') %>% html_text2() 
topics <- topics[-1]

dates <- page %>% html_nodes('time') %>% html_text(trim = T)
dates <- sapply(dates, translate_date)

links <- page %>% html_nodes('a.panel') %>% html_attr("href") %>%
  paste("https://www.casarosada.gob.ar", ., sep = "")

speeches <- unlist(sapply(links, get_text))
speeches <- unname(speeches)
AR <- rbind(AR, tibble(titles = topics, dates, speeches, links))
}





AR$dates <- dmy(AR$dates)

# We will retain only the speeches mentioning COVID/CORONA
AR_covid <- AR[grep("covid|corona", AR$speeches, ignore.case = TRUE),]

# We need to delete the '(APLAUSOS)'

AR_covid$speeches <- str_replace_all(AR_covid$speeches, fixed("(APLAUSOS)"), "")


# We will translate the remaining speeches

topics_EN <- gl_translate(t_string = AR_covid$topics, target = "en",
                          format = "text", model = "nmt")[1]
speeches_EN <- gl_translate(t_string = AR_covid$speeches, target = "en",
                            format = "text", model = "nmt")[1]

AR_final <- tibble (topics_ES = AR_covid$titles, dates = AR_covid$dates, speeches_ES = AR_covid$speeches, links = AR_covid$links, topics_EN = topics_EN, speeches_EN = speeches_EN)



write.csv(AR_final, 'data/speeches/speeches_AR.csv')




