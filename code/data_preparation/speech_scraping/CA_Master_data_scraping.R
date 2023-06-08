library(rvest)
library(dplyr)
library(lubridate)
# links to separate chosen speeches by Canadian PM
links <- c('https://pm.gc.ca/en/news/speeches/2020/03/11/prime-ministers-remarks-canadas-response-covid-19',
'https://pm.gc.ca/en/news/speeches/2020/03/20/prime-ministers-remarks-announcing-canadas-plan-mobilize-industry-fight',
'https://pm.gc.ca/en/news/speeches/2020/03/26/prime-ministers-remarks-updating-canadians-covid-19-situation',
'https://pm.gc.ca/en/news/speeches/2020/04/05/prime-ministers-remarks-thanking-workers-and-all-canadians-helping-their',
'https://pm.gc.ca/en/news/speeches/2020/04/10/prime-ministers-remarks-updating-canadians-covid-19-situation',
'https://pm.gc.ca/en/news/speeches/2020/04/15/prime-ministers-remarks-expansion-canada-emergency-response-benefit-and',
'https://pm.gc.ca/en/news/speeches/2020/04/23/prime-ministers-remarks-new-support-covid-19-medical-research-and-vaccine',
'https://pm.gc.ca/en/news/speeches/2020/04/29/prime-ministers-remarks-updating-canadians-measures-taken-response-covid',
'https://pm.gc.ca/en/news/speeches/2020/05/08/prime-ministers-remarks-measures-place-help-canadians-during-covid-19',
'https://pm.gc.ca/en/news/speeches/2020/05/20/prime-ministers-remarks-help-available-business-owners-affected-covid-19',
'https://pm.gc.ca/en/news/speeches/2020/05/27/prime-ministers-remarks-supports-announced-help-canadians-during-covid-19',
'https://pm.gc.ca/en/news/speeches/2020/06/04/prime-ministers-remarks-support-seniors-and-new-modelling-covid-19',
'https://pm.gc.ca/en/news/speeches/2020/06/19/prime-ministers-remarks-measures-support-canadians-and-businesses-during',
'https://pm.gc.ca/en/news/speeches/2020/06/29/prime-ministers-remarks-ongoing-covid-19-updates',
'https://pm.gc.ca/en/news/speeches/2020/07/16/prime-ministers-remarks-safe-restart-agreement',
'https://pm.gc.ca/en/news/speeches/2020/08/26/prime-ministers-remarks-announcing-safe-return-class-fund',
'https://pm.gc.ca/en/news/speeches/2020/09/23/prime-ministers-remarks-addressing-canadians-covid-19-situation',
'https://pm.gc.ca/en/news/speeches/2020/10/05/prime-ministers-remarks-updating-canadians-support-available-them-during',
'https://pm.gc.ca/en/news/speeches/2020/10/23/prime-ministers-remarks-rapid-testing-and-funding-advance-development',
'https://pm.gc.ca/en/news/speeches/2020/11/10/prime-ministers-remarks-covid-19-situation-and-support-veterans',
'https://pm.gc.ca/en/news/speeches/2020/11/24/prime-ministers-remarks-updating-canadians-covid-19-situation-and-support',
'https://pm.gc.ca/en/news/speeches/2020/12/07/prime-ministers-remarks-announcing-early-delivery-pfizer-biontech-covid-19',
'https://pm.gc.ca/en/news/speeches/2020/12/15/prime-ministers-remarks-updating-canadians-latest-developments-covid-19',
'https://pm.gc.ca/en/news/speeches/2021/01/05/prime-ministers-remarks-updating-canadians-covid-19-situation-vaccines-and',
'https://pm.gc.ca/en/news/speeches/2021/01/19/prime-ministers-remarks-updating-canadians-covid-19-situation-and',
'https://pm.gc.ca/en/news/speeches/2021/01/26/prime-ministers-remarks-updating-canadians-covid-19-situation-and-support',
'https://pm.gc.ca/en/news/speeches/2021/02/05/prime-ministers-remarks-updating-canadians-covid-19-situation-and-vaccine',
'https://pm.gc.ca/en/news/speeches/2021/03/03/prime-ministers-remarks-updating-canadians-vaccine-deliveries-and',
'https://pm.gc.ca/en/news/speeches/2021/03/11/prime-ministers-remarks-national-day-observance-covid-19',
'https://pm.gc.ca/en/news/speeches/2021/03/29/prime-ministers-remarks-united-nations-joint-press-conference-financing',
'https://pm.gc.ca/en/news/speeches/2021/04/16/prime-ministers-remarks-updating-canadians-covid-19-public-health',
'https://pm.gc.ca/en/news/speeches/2021/05/07/prime-ministers-remarks-covid-19-response-canada-and-internationally',
'https://pm.gc.ca/en/news/speeches/2021/06/18/prime-ministers-remarks-covid-19-situation',
'https://pm.gc.ca/en/news/speeches/2021/07/19/prime-ministers-remarks-updating-canadians-covid-19-situation-and'
)


res <- tibble( titles = character(), dates = character(), speeches = character(), links = character())

for (i in 1:length(links)) {
  res_temp <- matrix("", ncol = 4, nrow =1)
  res_temp[,1] <- links[i]
  page <- read_html(res_temp[,1])
  title <- page %>% html_nodes('h1') %>% html_text()
  res_temp[,2] <- title[3]
  res_temp[,3] <-  page %>% html_nodes('.inline-date') %>% html_text()
#  res_temp[,3] <- gsub("^.*?(\\d{1,2}\\.\\s\\w+\\s\\d{4}).*$", "\\1", res_temp[,3])
  speech <- page %>% html_nodes('.content-news-article p') %>% html_text()
  res_temp[,4] <- paste(speech[-1], collapse = " ")
  #res_temp[,4] <- paste(speech, collapse = " ")
  res <- rbind(res, res_temp)
}

data_CA <- tibble(res)
colnames(data_CA) <- c("links", "titles", "dates", "speeches")
data_CA$dates <- mdy(data_CA$dates)

write.csv(data_CA, 'data/speeches/speeches_CA.csv')

