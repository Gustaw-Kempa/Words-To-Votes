rm(list = ls())
library(jsonlite)
library(dplyr)
library(lubridate)
library(readr)
library(rvest)

# We will be using trends for the leader's party - kalman smoothing
UK <-
  fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/GB-parliament")$trends$kalmanSmooth
# For USA we only have the daily averages avaliable
USA <-
  read.csv('https://projects.fivethirtyeight.com/polls/data/generic_ballot_averages.csv')
DE <-
  fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/DE-parliament")$trends$kalmanSmooth
FR <-
  fromJSON(
    "https://www.politico.eu/wp-json/politico/v1/poll-of-polls/FR-presidential-approval"
  )$trends$kalmanSmooth
IT <-
  fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/IT-parliament")$trends$kalmanSmooth
ES <-
  fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/ES-parliament")$trends$kalmanSmooth
NL <-
  fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/NL-parliament-p")$trends$kalmanSmooth
DK <-
  fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/DK-parliament")$trends$kalmanSmooth
NOK <-
  fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/NO-parliament")$trends$kalmanSmooth

# Source -> https://canadianpolling.ca - needs to downloaded by hand
CA <- fromJSON("data/polls/CA_polls.json")




# We need to download the Japan polls from Wikipedia

url <-
  "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2021_Japanese_general_election"

tables <- url %>%
  read_html(url) %>%
  html_elements('.wikitable') %>%
  html_table()

jap2021 <- tables[[1]]
# deleting the first, second and last row
jap2021 <-
  jap2021[-c(1, 2, 19, 29, length(jap2021$`Fieldwork date`)), ]


regex_pattern <- "\\b(\\d{1,2})(?:–|-)(\\d{1,2}) ([A-Za-z]{3})\\b"

# Function to transform the dates into the desired format
transform_date <- function(date_str) {
  result <- gsub(regex_pattern, "\\2 \\3", date_str)
  return(result)
}

jap2021$`Fieldwork date` <-
  sapply(jap2021$`Fieldwork date`, transform_date)
jap2021$`Fieldwork date` <- unname(jap2021$`Fieldwork date`)

jap2021$`Fieldwork date` <- paste(jap2021$`Fieldwork date`, "2021")


jap2021$`Fieldwork date` <- dmy(jap2021$`Fieldwork date`)

# we do the same for 2020
jap2020 <- tables[[2]]

# deleting the first, second and last row
jap2020 <-
  jap2020[-c(1, 2, 30, 31, 34, length(jap2020$`Fieldwork date`)), ]


jap2020$`Fieldwork date` <-
  sapply(jap2020$`Fieldwork date`, transform_date)
jap2020$`Fieldwork date` <- unname(jap2020$`Fieldwork date`)

# for those that didnt work
jap2020$`Fieldwork date`[9] <- "17 11"
jap2020$`Fieldwork date`[63] <- "13 04"

jap2020$`Fieldwork date` <- paste(jap2020$`Fieldwork date`, "2020")


jap2020$`Fieldwork date` <- dmy(jap2020$`Fieldwork date`)

JP <-
  rbind(
    tibble(
      date = jap2020$`Fieldwork date`,
      LDP = as.numeric(jap2020$LDP)
    ),
    tibble(
      date = jap2021$`Fieldwork date`,
      LDP = as.numeric(jap2021$LDP)
    )
  )


JP$week <- strftime(JP$date, format = "%y%V")


write_csv(JP, "data/polls/JP_polls.csv")


UK <- tibble(UK[, 2][1], UK[1])
UK$date <- ymd(UK$date)
UK$week <- strftime(UK$date, format = "%y%V")
UK <- UK %>% na.omit()
write_csv(UK, "data/polls/UK_polls.csv")




USA <- USA %>% filter (candidate == 'Republicans')
USA <- tibble(USA[, c(2, 5)])
colnames(USA) <- c("rep", "date")
USA$date <- ymd(USA$date)
USA$week <- strftime(USA$date, format = "%y%V")

write_csv(USA, "data/polls/USA_polls.csv")

ES <- tibble(ES[, 2][2], ES[1])
ES$date <- ymd(ES$date)
ES$week <- strftime(ES$date, format = "%y%V")
ES <- ES %>% na.omit()
write_csv(ES, "data/polls/ES_polls.csv")


FR <- tibble(FR[, 2][1], FR[1])
FR$date <- ymd(FR$date)
FR$week <- strftime(FR$date, format = "%y%V")
FR <- FR %>% na.omit()
write_csv(FR, "data/polls/FR_polls.csv")

# Conte was independent, but we can try to track his support by the proxy of 5 stars movement
IT <- tibble(IT[, 2][8], IT[1])
IT$date <- ymd(IT$date)
IT$week <- strftime(IT$date, format = "%y%V")
IT <- IT %>% na.omit()
write_csv(IT, "data/polls/IT_polls.csv")


DE <-  tibble(DE[, 2][1], DE[1])
DE$date <- ymd(DE$date)
DE$week <- strftime(DE$date, format = "%y%V")
DE <- DE %>% na.omit()
write_csv(DE, "data/polls/DE_polls.csv")



CA <- CA %>% filter (jurisdiction == "Canada")
LPA_support <- lapply(CA[, 8], function(x)
  x[x[, 1] == 'LPC', 2])
CA$field <- ymd(CA$field)
CA$week <- strftime(CA$field, format = "%y%V")
CA <-
  tibble(unlist(LPA_support), CA$field, CA$company, CA$n, CA$week)
colnames(CA) <- c("support", "date", "firm", "sample_size", "week")
write_csv(CA, "data/polls/CA_polls.csv")


NL <-  tibble(NL[, 2][1], NL[1])
NL$date <- ymd(NL$date)
NL$week <- strftime(NL$date, format = "%y%V")
NL <- NL %>% na.omit()
write_csv(NL, "data/polls/NL_polls.csv")

DK <-  tibble(DK[, 2][1], DK[1])
DK$date <- ymd(DK$date)
DK$week <- strftime(DK$date, format = "%y%V")
DK <- DK %>% na.omit()
write_csv(DK, "data/polls/DK_polls.csv")

NOK <-  tibble(NOK[, 2][2], NOK[1])
NOK$date <- ymd(NOK$date)
NOK$week <- strftime(NOK$date, format = "%y%V")
NOK <- NOK %>% na.omit()
write_csv(NOK, "data/polls/NOK_polls.csv")





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

combined_data <-
  rbindlist (list (CA[, c(1, 2, 6)], USA[, c(4, 1, 6)], UK[, c(1, 2, 6)], IT[, c(1, 2, 6)],
                   DE[, c(1, 2, 6)], ES[, c(1, 2, 6)], FR[, c(1, 2, 6)]), use.names = F)
combined_data <- combined_data[year(combined_data$date) >= 2020]
combined_data <- combined_data[year(combined_data$date) < 2022]

# creation of weekly averages for plotting

combined_data <-
  combined_data %>% group_by(month(date), year(date), country) %>% summarise(support = mean(support))
combined_data$`month(date)` <-
  str_pad(combined_data$`month(date)`, 2, pad = '0')
combined_data$month <-
  paste0(combined_data$`year(date)`, '.', combined_data$`month(date)`)
# creation of ggplot


ggplot(combined_data,
       aes(
         x = month,
         y = support,
         color = country,
         group = country
       )) +
  geom_line(linewidth = 1.2) +
  scale_color_discrete() + # apply discrete color scale
  labs(title = "Support Percentages Over Time (2020-2021)", x = "Date", y = "Support %") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


combined_data <- combined_data %>%
  arrange(country, `year(date)`, `month(date)`) %>%
  group_by(country) %>%
  mutate(first_diff = support - lag(support))

ggplot(combined_data, aes(x = first_diff, fill = country, group = country)) +
  geom_density(color = "black", alpha = 0.3) +
  labs(title = "Distributions of First Differences of Support",
       x = "First Difference of Support",
       y = "Frequency") + xlim(-12, 12)
scale_fill_discrete()

export <- combined_data %>% group_by(country) %>% summarise(
  support_m = mean(support, na.rm = TRUE),
  sd = sd(support, na.rm = T),
  diff = mean(first_diff, na.rm = T)
)

xtable(export)


aTSA::adf.test(combined_data$first_diff)
