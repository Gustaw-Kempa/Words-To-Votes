rm(list = ls())

library(rvest)
library(dplyr)
library(lubridate)
library(stringr)
library(googleLanguageR)
# unique Google translate API key used for translation - the path needs to be changed
gl_auth("/Users/gustawkempa/Desktop/Studia/Master/Master.Data/master-377713-d6f77365d167.json")


# links to separate chosen speeches by German chancellor

links <- c('https://www.bundesregierung.de/breg-de/aktuelles/rede-von-bundeskanzlerin-merkel-zur-uebergabe-der-2-euro-gedenkmuenze-brandenburg-am-14-februar-2020-in-berlin-1722152',
           'https://www.bundesregierung.de/breg-de/aktuelles/rede-von-bundeskanzlerin-merkel-zur-eroeffnung-des-11-integrationsgipfels-am-2-maerz-2020-in-berlin-1726948',
           'https://www.bundesregierung.de/breg-de/aktuelles/rede-von-bundeskanzlerin-merkel-im-rahmen-der-covid-19-geberkonferenz-am-4-mai-2020-in-berlin-videokonferenz--1750234',
           'https://www.bundesregierung.de/breg-de/aktuelles/rede-von-bundeskanzlerin-merkel-anlaesslich-der-wiederauffuellungskonferenz-der-impfallianz-gavi-am-4-juni-2020-videokonferenz--1757954',
           'https://www.bundesregierung.de/breg-de/aktuelles/rede-von-bundeskanzlerin-merkel-anlaesslich-des-festakts-zum-70-jubilaeum-des-zentralrats-der-juden-in-deutschland-am-15-september-2020-1786986',
           'https://www.bundesregierung.de/breg-de/aktuelles/rede-von-bundeskanzlerin-merkel-anlaesslich-der-uebergabe-des-nationalen-integrationspreises-am-5-oktober-2020-1796026',
           'https://www.bundesregierung.de/breg-de/aktuelles/rede-von-bundeskanzlerin-merkel-bei-der-verleihung-des-deutschen-sozialpreises-2020-durch-die-bundesarbeitsgemeinschaft-der-freien-wohlfahrtspflege-am-26-oktober-2020-1804634',
           'https://www.bundesregierung.de/breg-de/aktuelles/regierungserklaerung-von-bundeskanzlerin-merkel-1746554',
           'https://www.bundesregierung.de/breg-de/aktuelles/regierungserklaerung-von-bundeskanzlerin-merkel-1762594',
           'https://www.bundesregierung.de/breg-de/aktuelles/regierungserklaerung-von-bundeskanzlerin-merkel-1820778',
           'https://www.bundesregierung.de/breg-de/aktuelles/regierungserklaerung-von-bundeskanzlerin-merkel-1807160',
           'https://www.bundesregierung.de/breg-de/aktuelles/regierungserklaerung-von-bundeskanzlerin-merkel-1881860',
           'https://www.bundesregierung.de/breg-de/aktuelles/rede-von-bundeskanzlerin-merkel-bei-der-hannover-messe-2021-am-12-april-2021-per-video--1888516',
           'https://www.bundesregierung.de/breg-de/aktuelles/rede-von-bundeskanzlerin-merkel-anlaesslich-der-nationalen-luftfahrtkonferenz-2021-am-18-juni-2021-videokonferenz--1933590',
           'https://www.bundesregierung.de/breg-de/aktuelles/regierungserklaerung-von-bundeskanzlerin-merkel-1937200',
           'https://www.bundesregierung.de/breg-de/aktuelles/rede-von-bundeskanzlerin-merkel-anlaesslich-des-festakts-zum-75-jubilaeum-des-landes-nordrhein-westfalen-am-23-august-2021-in-duesseldorf-1953496',
           'https://www.bundesregierung.de/breg-de/aktuelles/regierungserklraeung-von-bundeskanzlerin-merkel-am-25-august-2021-1956202',
           'https://www.bundesregierung.de/breg-de/aktuelles/rede-von-bundeskanzlerin-merkel-anlaesslich-der-eroeffnungsfeier-des-who-hub-for-pandemic-and-epidemic-intelligence-am-1-september-2021-in-berlin-1956872',
           'https://www.bundesregierung.de/breg-de/aktuelles/rede-von-bundeskanzlerin-merkel-anlaesslich-der-uebergabe-des-jahresberichts-des-nationalen-normenkontrollrats-am-16-september-2021-in-berlin-1960640'
           )

res <- tibble( titles = character(), dates = character(), speeches_DE = character(), links = character())

for (i in 1:length(links)) {
  res_temp <- matrix("", ncol = 4, nrow =1)
  res_temp[,1] <- links[i]
  page <- read_html(res_temp[,1])
  res_temp[,2] <- page %>% html_nodes('.bpa-teaser-title-text-inner') %>% html_text()
  res_temp[,2] <- gsub("\\n", "", res_temp[,2])
  res_temp[,3] <-  page %>% html_nodes('.bpa-government-declaration-place-date') %>% html_text()
  res_temp[,3] <- gsub("^.*?(\\d{1,2}\\.\\s\\w+\\s\\d{4}).*$", "\\1", res_temp[,3])
  speech <- page %>% html_nodes('.bpa-richtext p') %>% html_text()
  res_temp[,4] <- paste(speech[-c(1:5)], collapse = " ")
  res <- rbind(res, res_temp)
}

data_DE <- res
colnames(data_DE) <- c("links", "titles", "dates", "speeches_DE")

topics_EN <- gl_translate(t_string = data_DE$titles, target = "en",
                          format = "text", model = "nmt")[1]
speeches_EN <- gl_translate(t_string = data_DE$speeches_DE, target = "en",
                            format = "text", model = "nmt")[1]


months <- str_extract(data_DE$dates, "\\s(.*)\\s")
months <- gl_translate(t_string = months, target = "en",
                       format = "text", model = "nmt")[1]


data_DE$dates <- gsub("\\s(.*)\\s", "", data_DE$dates)
data_DE$dates<- paste(months$translatedText,data_DE$dates,sep = "." )
data_DE$dates <- mdy(data_DE$dates)


data <- cbind(data_DE,topics_EN ,speeches_EN)

colnames(data) <- c("links", "topics", "dates", "speeches_DE", "topics_EN", "speeches_EN")
write.csv(data, 'data/speeches/speeches_DE_translated.csv')





