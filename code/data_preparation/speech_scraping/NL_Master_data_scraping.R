rm(list = ls())
library(rvest)
library(dplyr)
library(lubridate)
library(stringr)
library(googleLanguageR)
# unique Google translate API key used for translation - the path needs to be changed
gl_auth("/Users/gustawkempa/Desktop/Studia/Master/Master.Data/master-377713-d6f77365d167.json")



htmls <- c(
           "https://www.rijksoverheid.nl/documenten?trefwoord=corona&dateRange=specific&startdatum=01-01-2020&einddatum=31-12-2021&onderdeel=Ministerie+van+Algemene+Zaken&type=Mediatekst",
           "https://www.rijksoverheid.nl/documenten?trefwoord=corona&onderdeel=Ministerie%20van%20Algemene%20Zaken&type=Mediatekst&startdatum=01%2D01%2D2020&einddatum=31%2D12%2D2021&pagina=2",
           "https://www.rijksoverheid.nl/documenten?trefwoord=corona&onderdeel=Ministerie%20van%20Algemene%20Zaken&type=Mediatekst&startdatum=01%2D01%2D2020&einddatum=31%2D12%2D2021&pagina=3",
           "https://www.rijksoverheid.nl/documenten?trefwoord=corona&onderdeel=Ministerie%20van%20Algemene%20Zaken&type=Mediatekst&startdatum=01%2D01%2D2020&einddatum=31%2D12%2D2021&pagina=4",
           "https://www.rijksoverheid.nl/documenten?trefwoord=corona&onderdeel=Ministerie%20van%20Algemene%20Zaken&type=Mediatekst&startdatum=01%2D01%2D2020&einddatum=31%2D12%2D2021&pagina=5",
           "https://www.rijksoverheid.nl/documenten?trefwoord=corona&onderdeel=Ministerie%20van%20Algemene%20Zaken&type=Mediatekst&startdatum=01%2D01%2D2020&einddatum=31%2D12%2D2021&pagina=6",
           "https://www.rijksoverheid.nl/documenten?trefwoord=corona&onderdeel=Ministerie%20van%20Algemene%20Zaken&type=Mediatekst&startdatum=01%2D01%2D2020&einddatum=31%2D12%2D2021&pagina=7",
           "https://www.rijksoverheid.nl/documenten?trefwoord=corona&onderdeel=Ministerie%20van%20Algemene%20Zaken&type=Mediatekst&startdatum=01%2D01%2D2020&einddatum=31%2D12%2D2021&pagina=8",
           "https://www.rijksoverheid.nl/documenten?trefwoord=corona&onderdeel=Ministerie%20van%20Algemene%20Zaken&type=Mediatekst&startdatum=01%2D01%2D2020&einddatum=31%2D12%2D2021&pagina=9")




get_text = function(link) {
  
  subpage <- read_html(link)

  nodes <- subpage %>% html_nodes(" h2,p") 
 # we are looking for the borders between two first h2's
  locs <- grep("<h2>",  as.character(nodes), fixed = TRUE)
  speech <- nodes %>% html_text(trim = T)
  if (length(locs)>1){
  speech <- paste(speech[(locs[1]+1):(locs[2]-1)], collapse = " ")
  }
}


nl <- tibble( titles = character(), dates = character(), speeches = character(), links = character())

for (i in 1:length(htmls)) {
page <- read_html(htmls[i])
topics <- page %>% html_nodes('h3') %>% html_text() 
topics <- gsub("[\r\n]+", "", topics)
topics <- gsub("\\s+", " ", topics)

dates <- page %>% html_nodes('.meta') %>% html_text() 
dates <- gsub(".*\\|\\s*", "", dates)

links <- page %>% html_nodes('.publication') %>% html_attr('href') %>% 
paste("https://www.rijksoverheid.nl", ., sep="")
speeches = sapply(links, get_text)
nl <- rbind(nl, tibble(topics, dates, speeches, links))

}


nl$dates <- dmy(nl$dates)


# Now we need to delete all the websites without text
nl <- subset(nl, speeches != 'Verantwoordelijk Zie ook')
# We also need to delete interviews
nl <- subset(nl, !grepl('^(JONKER|DE WINTHER|WESTER)', speeches))



# Now we will translate the file

topics_EN <- gl_translate(t_string = nl$topics, target = "en",
                          format = "text", model = "nmt")[1]
speeches_EN <- gl_translate(t_string = nl$speeches, target = "en",
                            format = "text", model = "nmt")[1]



nl_final <- tibble (topics_nl = nl$topics, dates = nl$dates, speeches_nl = nl$speeches, links = nl$links, topics_EN = topics_EN, speeches_EN = speeches_EN)


write.csv(nl_final, 'data/speeches/speeches_NL.csv')
