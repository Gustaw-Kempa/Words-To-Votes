rm(list = ls())
library(jsonlite)
library(dplyr)
library(lubridate) 
library(readr)

# for now for/against boris johnson can be changed
UK <- fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/GB-parliament")$polls
USA <- read.csv('https://projects.fivethirtyeight.com/polls/data/generic_ballot_polls_historical.csv')
DE <- fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/DE-parliament")$polls
FR <- fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/FR-presidential-approval")$polls
IT <- fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/IT-parliament")$polls
ES <- fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/ES-parliament")$polls
# Source -> https://canadianpolling.ca - needs to downloaded by hand
CA <- fromJSON("data/polls/CA_polls.json")





UK <- tibble(UK[,2][1],UK[c(1,3,5)])
UK$date <- ymd(UK$date)
UK$week <- strftime(UK$date, format = "%y%V")
UK <- UK %>% na.omit()
write_csv(UK, "data/polls/UK_polls.csv")




USA <- tibble(USA[,c(13,3,9,38)])
USA$end_date <- mdy(USA$end_date)
USA$week <- strftime(USA$end_date, format = "%y%V")
write_csv(USA, "data/polls/USA_polls.csv")

ES <- tibble(ES[,2][2],ES[c(1,3,5)])
ES$date <- ymd(ES$date)
ES$week <- strftime(ES$date, format = "%y%V")
ES <- ES %>% na.omit()
write_csv(ES, "data/polls/ES_polls.csv")


FR <-tibble(FR[,2][1],FR[c(1,3,5)])
FR$date <- ymd(FR$date)
FR$week <- strftime(FR$date, format = "%y%V")
FR <- FR %>% na.omit()
write_csv(FR, "data/polls/FR_polls.csv")

# Conte was independent, but we can try to track his support by the proxy of 5 stars movement
IT <-tibble(IT[,2][8],IT[c(1,3,5)])
IT$date <- ymd(IT$date)
IT$week <- strftime(IT$date, format = "%y%V")
IT <- IT %>% na.omit()
write_csv(IT, "data/polls/IT_polls.csv")


DE <-  tibble(DE[,2][1],DE[c(1,3,5)])
DE$date <- ymd(DE$date)
DE$week <- strftime(DE$date, format = "%y%V")
DE <- DE %>% na.omit()
write_csv(DE, "data/polls/DE_polls.csv")



CA <- CA %>% filter (jurisdiction == "Canada")
LPA_support <- lapply(CA[,8], function(x) x[x[,1]=='LPC',2])
CA$field <- ymd(CA$field)
CA$week <- strftime(CA$field, format = "%y%V")
CA <- tibble(unlist(LPA_support), CA$field, CA$company,CA$n, CA$week)
colnames(CA) <- c("support", "date", "firm", "sample_size", "week")
write_csv(CA, "data/polls/CA_polls.csv")


##### VISUALIZATIONS #####
library(ggplot2)
library(data.table)
library(stringr)
# Adding a row with country name
CA$country <- "CA"
USA$country <- "USA"
UK$country <- "UK"
IT$country <- "IT"
DE$country <- "DE"
ES$country <- "ES"
FR$country <- "FR"



# binding the rows

combined_data <- rbindlist (list (CA[,c(1,2,6)], USA[,c(4,1,6)], UK[,c(1,2,6)], IT[,c(1,2,6)], 
                                  DE[,c(1,2,6)], ES[,c(1,2,6)], FR[,c(1,2,6)]),use.names = F)
combined_data <- combined_data[year(combined_data$date)>=2020]
combined_data <- combined_data[year(combined_data$date)<2022]

# creation of weekly averages for plotting 

combined_data <- combined_data %>% group_by(month(date), year(date), country) %>% summarise(support = mean(support))
combined_data$`month(date)` <-  str_pad(combined_data$`month(date)`, 2, pad = '0')
combined_data$month <- paste0(combined_data$`year(date)`,'.', combined_data$`month(date)`)
# creation of ggplot


ggplot(combined_data, aes(x = month, y = support, color = country, group = country)) +
  geom_line(linewidth = 1.2) +
  scale_color_discrete() + # apply discrete color scale
  labs(title = "Support Percentages Over Time (2020-2021)", x = "Date", y = "Support %") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
