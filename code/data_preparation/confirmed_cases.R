rm(list = ls())
library(tidyverse)
library(lubridate)

# the dataset needs to be downloaded from https://www.kaggle.com/datasets/antgoldbloom/covid19-data-from-john-hopkins-university
global_data <- read_delim('data/cases/john_hopkins_covid.csv'
                 )[-1,]


colnames(global_data) <- c('date', colnames(global_data)[-1])
global_data$date <- mdy(global_data$date)

global_data$week <- strftime(global_data$date, format = "%y%V")


global_data[, -1] <- lapply(global_data[-1], as.numeric)
global_data <- global_data[,-1]


weekly_global_data <- global_data %>% group_by(week) %>% summarise_all(sum, na.omi = TRUE)



write_csv(weekly_global_data, 'data/cases/global_confirmed_cases.csv')






#### Data visualization ####

# calculating sums in all countires
uk_cases <-
 tibble(cases= weekly_global_data$`United Kingdom...266` + weekly_global_data$`United Kingdom...267` + weekly_global_data$`United Kingdom...268` +
  weekly_global_data$`United Kingdom...269` + weekly_global_data$`United Kingdom...270` + weekly_global_data$`United Kingdom...271` +
  weekly_global_data$`United Kingdom...272` + weekly_global_data$`United Kingdom...273` + weekly_global_data$`United Kingdom...274` +
  weekly_global_data$`United Kingdom...275` + weekly_global_data$`United Kingdom...276` + weekly_global_data$`United Kingdom...277` +
  weekly_global_data$`United Kingdom...278` + weekly_global_data$`United Kingdom...279` + weekly_global_data$`United Kingdom...280`,
  country = 'UK', week = weekly_global_data$week)

us_cases <-  tibble (cases = weekly_global_data$US, country = 'USA', week = weekly_global_data$week)
spain_cases <- tibble(cases = weekly_global_data$Spain,  country = 'ES', week = weekly_global_data$week)

fr_cases <- tibble( cases =
  weekly_global_data$France...122 + weekly_global_data$France...123 + weekly_global_data$France...124 +
  weekly_global_data$France...125 +
  weekly_global_data$France...126 + weekly_global_data$France...127 + weekly_global_data$France...128 + weekly_global_data$France...129 +
  weekly_global_data$France...130 + weekly_global_data$France...131 + weekly_global_data$France...132 + weekly_global_data$France...133,
  country = 'FR', week = weekly_global_data$week)

de_cases <- tibble(cases = weekly_global_data$Germany,   country = 'DE', week = weekly_global_data$week)

it_cases <- tibble(cases = weekly_global_data$Italy,   country = 'IT', week = weekly_global_data$week)

ca_cases <-tibble(cases = weekly_global_data$Canada...42 + weekly_global_data$Canada...43 + weekly_global_data$Canada...44 + weekly_global_data$Canada...45+
  weekly_global_data$Canada...46 + weekly_global_data$Canada...47 + weekly_global_data$Canada...48 + weekly_global_data$Canada...49 + weekly_global_data$Canada...50 +
  weekly_global_data$Canada...51 + weekly_global_data$Canada...52 + weekly_global_data$Canada...53 + weekly_global_data$Canada...54 + weekly_global_data$Canada...57,
  country = 'CA', week = weekly_global_data$week)

es_cases <- tibble(cases = weekly_global_data$Spain, country = 'ES', week = weekly_global_data$week)
world_cases <- rbind(ca_cases, it_cases, de_cases, es_cases, fr_cases, us_cases, uk_cases)
world_cases$date <- strptime(paste0(world_cases$week,1),format = "%y%W%w")

# changing granularity to monthly
world_cases <- world_cases %>% group_by(year(date), month(date), country) %>% summarise(cases = sum(cases))
# adding padding to months
world_cases$`month(date)` <- str_pad(world_cases$`month(date)`, 2, pad = '0')

world_cases$month <- paste0(world_cases$`year(date)`,'.', world_cases$`month(date)`)
world_cases <- na.omit(world_cases)
world_cases <- world_cases[world_cases$`year(date)`<2022,]


ggplot(world_cases, aes(x = month, y = cases, color = country, group = country)) +
  geom_line(linewidth = 1) +
  scale_color_discrete() + # apply discrete color scale
  labs(title = "Monthly COVID-19 cases over time", x = "Date", y = "Confirmed monthly cases") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::number_format(scale = 1/1000000, accuracy = 1, suffix = "M"))



