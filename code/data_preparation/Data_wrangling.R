rm(list = ls())
library(jsonlite) #to read poll data
library(tidyverse) #general data wrangling
library(lubridate) #making the dates easier
library(BBmisc) # data scaling and normalizing
library(reshape2) # melting data for plots



# adding COVID cases data to the model
cases_data <- read_csv('data/cases/global_confirmed_cases.csv')


# UK ####


UK_speeches  <-
  read_delim("data/speeches/speeches_UK.csv")

#fixing the dates in dataset
UK_speeches$dates <- ymd(UK_speeches$dates)

df <- tibble()
fragm <- c()
for (i in 1:nrow(UK_speeches)) {
  # Dividing the speeches into 5-sentence sequences
  
  speech_fragment <-
    unlist(strsplit(
      UK_speeches$speeches[i],
      "(?<=[[:punct:]])\\s(?=[A-Z])",
      perl = T
    ))
  fragm <- c()
  for (j in 1:floor(length(speech_fragment) / 5)) {
    fragm[j] <-
      paste(
        speech_fragment[j * 5 - 4],
        speech_fragment[j * 5 - 3],
        speech_fragment[j * 5 - 2],
        speech_fragment[j * 5 - 1],
        speech_fragment[j * 5]
      )
    fragm[j] <- gsub('NA', '', fragm[j])
    fragm[j] <- gsub('\n', '', fragm[j])
  }
  df_temp <-
    data.frame(
      fragment = fragm,
      fragment_no = c(1:length(fragm)),
      date = UK_speeches$dates[i]
    )
  df <- rbind.data.frame(df, df_temp)
}
UK_speeches <- df


# binding the results
UK_res <-
  tibble(
    sentiment = read.csv("data/emotions/UK/sent_temp.csv")[, 2],
    happiness = read.csv('data/emotions/UK/hap_temp.csv')[, 2],
    persuasion = read.csv('data/emotions/UK/pers_temp.csv')[, 2],
    anger = read.csv('data/emotions/UK/ang_temp.csv')[, 2],
    fear = read.csv('data/emotions/UK/fear_temp.csv')[, 2],
    surprise = read.csv('data/emotions/UK/surp_temp.csv')[, 2],
    informativeness = read.csv('data/emotions/UK/inf_temp.csv')[, 2]
    
  )
# Creation of final dataset to the model - with the means and standard deviations of each variable
UK_final <-
  tibble(UK_speeches, UK_res) %>% group_by(date) %>% summarise(
    mean_sentiment = mean(sentiment),
    mean_informativeness = mean(informativeness),
    mean_persuasion = mean(persuasion),
    mean_anger = mean(anger),
    mean_surprise = mean(surprise),
    mean_happiness = mean(happiness),
    mean_fear = mean(fear),
    sd_sentiment = sd(sentiment),
    sd_informativeness = sd(informativeness),
    sd_fear = sd(fear),
    sd_persuasion = sd(persuasion),
    sd_anger = sd(anger),
    sd_surprise = sd(surprise),
    sd_happiness = sd(happiness)
  )

# Replacing NA's in columns with SD where there is only one chunk of text
UK_final[, c(9:15)] <-
  apply(UK_final[, c(9:15)], 2, function(x)
    replace(x, is.na(x), 0))
# adding the week number (format year, week no)
UK_final$week <- strftime(UK_final$date, format = "%y%V")

# reading the polls data

UK_polls <- read_csv('data/polls/UK_polls.csv')
UK_polls <-
  UK_polls %>% group_by(week) %>% summarise(support = mean(Con))
UK_polls <- UK_polls %>% mutate(
  diff0 = support - lag(support),
  diff1 = lead(support) - support,
  diff2 = lead(support, n = 2) - lead(support, n =
                                        1)
)
UK_final$week <- as.numeric(UK_final$week)
# joining the two dataframes


UK_final <-
  UK_final %>% left_join (UK_polls, by = 'week') %>% na.omit()
uk_cases <-
  cases_data$`United Kingdom...266` + cases_data$`United Kingdom...267` + cases_data$`United Kingdom...268` +
  cases_data$`United Kingdom...269` + cases_data$`United Kingdom...270` + cases_data$`United Kingdom...271` +
  cases_data$`United Kingdom...272` + cases_data$`United Kingdom...273` + cases_data$`United Kingdom...274` +
  cases_data$`United Kingdom...275` + cases_data$`United Kingdom...276` + cases_data$`United Kingdom...277` +
  cases_data$`United Kingdom...278` + cases_data$`United Kingdom...279` + cases_data$`United Kingdom...280`


UK_final <-
  UK_final %>% left_join(tibble(week = cases_data$week, cases = uk_cases), by =
                           'week')

UK_final$country <- "UK"



rm(list = setdiff(ls(), c("UK_final", "cases_data")))


# USA ####

USA_speeches  <-
  read_delim("data/speeches/speeches_USA.tsv")[,-1]
#fixing the dates in dataset
USA_speeches$dates <- mdy(USA_speeches$dates)

df <- tibble()
fragm <- c()
for (i in 1:nrow(USA_speeches)) {
  #we need to check how many groups to create
  
  speech_fragment <-
    unlist(strsplit(
      USA_speeches$speeches[i],
      "(?<=[[:punct:]])\\s(?=[A-Z])",
      perl = T
    ))
  fragm <- c()
  for (j in 1:floor(length(speech_fragment) / 5)) {
    fragm[j] <-
      paste(
        speech_fragment[j * 5 - 4],
        speech_fragment[j * 5 - 3],
        speech_fragment[j * 5 - 2],
        speech_fragment[j * 5 - 1],
        speech_fragment[j * 5]
      )
    fragm[j] <- gsub('NA', '', fragm[j])
    fragm[j] <- gsub('\n', '', fragm[j])
  }
  df_temp <-
    data.frame(
      fragment = fragm,
      fragment_no = c(1:length(fragm)),
      date = USA_speeches$dates[i]
    )
  df <- rbind.data.frame(df, df_temp)
}
USA_speeches <- df


# binding the results
USA_res <- read_csv("data/emotions/USA/USA_emotions.csv")

USA_final <-
  tibble(USA_speeches, USA_res) %>% group_by(date) %>% summarise(
    mean_sentiment = mean(sentiment),
    mean_informativeness = mean(informativeness),
    mean_persuasion = mean(persuasion),
    mean_anger = mean(anger),
    mean_surprise = mean(surprise),
    mean_happiness = mean(happiness),
    mean_fear = mean(fear),
    sd_sentiment = sd(sentiment),
    sd_informativeness = sd(informativeness),
    sd_fear = sd(fear),
    sd_persuasion = sd(persuasion),
    sd_anger = sd(anger),
    sd_surprise = sd(surprise),
    sd_happiness = sd(happiness)
  )
# Replacing NA's in columns with SD where there is only one chunk of text
USA_final[, c(9:15)] <-
  apply(USA_final[, c(9:15)], 2, function(x)
    replace(x, is.na(x), 0))

# adding the week number (format year, week no)
USA_final$week <- strftime(USA_final$date, format = "%y%V")

# reading the polls data

USA_polls <- read_csv('data/polls/USA_polls.csv')
USA_polls <-
  USA_polls %>% group_by(week) %>% summarise(support = mean(rep))
USA_polls <- USA_polls %>%  mutate(
  diff0 = support - lag(support),   diff1 = lead(support) - support,
  diff2 = lead(support, n = 2) - lead(support, n =
                                        1)
)
USA_final$week <- as.numeric(USA_final$week)
# joining the two dataframes

USA_final <-
  USA_final %>% left_join (USA_polls, by = 'week') %>% na.omit()

USA_final <-
  USA_final %>% left_join(tibble(week = cases_data$week, cases = cases_data$US), by =
                            'week')

USA_final$country <- "USA"

rm(list = setdiff(ls(), c("USA_final", "UK_final", "cases_data")))

# ES ####


ES_speeches  <-
  read_delim("data/speeches/speeches_ES_translated")
#fixing the dates in dataset
colnames(ES_speeches) = c('id',
                          'links',
                          'topics',
                          'date',
                          'speeches_ES',
                          'len',
                          'topics_EN',
                          'speeches_EN')
ES_speeches$date <- ymd(ES_speeches$date)



df <- tibble()
fragm <- c()
for (i in 1:nrow(ES_speeches)) {
  #we need to check how many groups to create
  
  speech_fragment <-
    unlist(strsplit(
      ES_speeches$speeches_EN[i],
      "(?<=[[:punct:]])\\s(?=[A-Z])",
      perl = T
    ))
  fragm <- c()
  for (j in 1:floor(length(speech_fragment) / 5)) {
    fragm[j] <-
      paste(
        speech_fragment[j * 5 - 4],
        speech_fragment[j * 5 - 3],
        speech_fragment[j * 5 - 2],
        speech_fragment[j * 5 - 1],
        speech_fragment[j * 5]
      )
    fragm[j] <- gsub('NA', '', fragm[j])
    fragm[j] <- gsub('\n', '', fragm[j])
  }
  df_temp <-
    data.frame(
      fragment = fragm,
      fragment_no = c(1:length(fragm)),
      date = ES_speeches$date[i]
    )
  df <- rbind.data.frame(df, df_temp)
}
ES_speeches <- df


# binding the results
ES_res <- read_csv("data/emotions/ES/ES_emotions.csv")

ES_final <- tibble(ES_speeches, ES_res) %>% na.omit()


ES_final <- ES_final %>%
  group_by(date) %>%
  summarise(
    mean_sentiment = mean(sentiment),
    mean_informativeness = mean(informativeness),
    mean_persuasion = mean(persuasion),
    mean_anger = mean(anger),
    mean_surprise = mean(surprise),
    mean_happiness = mean(happiness),
    mean_fear = mean(fear),
    sd_sentiment = sd(sentiment),
    sd_informativeness = sd(informativeness),
    sd_fear = sd(fear),
    sd_persuasion = sd(persuasion),
    sd_anger = sd(anger),
    sd_surprise = sd(surprise),
    sd_happiness = sd(happiness),
    .groups = 'keep'
  )




# Replacing NA's in columns with SD where there is only one chunk of text
ES_final[, c(9:15)] <-
  apply(ES_final[, c(9:15)], 2, function(x)
    replace(x, is.na(x), 0))


# adding the week number (format year, week no)
ES_final$week <- strftime(ES_final$date, format = "%y%V")

# reading the polls data

ES_polls <- read_csv('data/polls/ES_polls.csv')
ES_polls <-
  ES_polls %>% group_by(week) %>% summarise(support = mean(PSOE))
ES_polls <- ES_polls %>%  mutate(
  diff0 = support - lag(support),   diff1 = lead(support) - support,
  diff2 = lead(support, n = 2) - lead(support, n =
                                        1)
)
ES_final$week <- as.numeric(ES_final$week)
# joining the two dataframes

ES_final <-
  ES_final %>% left_join (ES_polls, by = 'week') %>% na.omit()

ES_final <-
  ES_final %>% left_join(tibble(week = cases_data$week, cases = cases_data$Spain), by =
                           'week')

ES_final$country <- "ES"

rm(list = setdiff(ls(), c(
  "USA_final", "UK_final", "ES_final", "cases_data"
)))


# FR ####
?read_delim

FR_speeches  <-
  data <- read_delim("data/speeches/Translated_FR_speeches.csv")[,-c(1:2)]
#fixing the dates in dataset
FR_speeches$dates <- ymd(FR_speeches$dates)

df <- tibble()
fragm <- c()
for (i in 1:nrow(FR_speeches)) {
  #we need to check how many groups to create
  
  speech_fragment <-
    unlist(strsplit(
      FR_speeches$speeches_EN[i],
      "(?<=[[:punct:]])\\s(?=[A-Z])",
      perl = T
    ))
  fragm <- c()
  for (j in 1:floor(length(speech_fragment) / 5)) {
    fragm[j] <-
      paste(
        speech_fragment[j * 5 - 4],
        speech_fragment[j * 5 - 3],
        speech_fragment[j * 5 - 2],
        speech_fragment[j * 5 - 1],
        speech_fragment[j * 5]
      )
    fragm[j] <- gsub('NA', '', fragm[j])
    fragm[j] <- gsub('\n', '', fragm[j])
  }
  df_temp <-
    data.frame(
      fragment = fragm,
      fragment_no = c(1:length(fragm)),
      date = FR_speeches$dates[i]
    )
  df <- rbind.data.frame(df, df_temp)
}
FR_speeches <- df


# binding the results
FR_res <- read_csv("data/emotions/FR/FR_emotions.csv")

FR_final <-
  tibble(FR_speeches, FR_res) %>% group_by(date) %>% summarise(
    mean_sentiment = mean(sentiment),
    mean_informativeness = mean(informativeness),
    mean_persuasion = mean(persuasion),
    mean_anger = mean(anger),
    mean_surprise = mean(surprise),
    mean_happiness = mean(happiness),
    mean_fear = mean(fear),
    sd_sentiment = sd(sentiment),
    sd_informativeness = sd(informativeness),
    sd_fear = sd(fear),
    sd_persuasion = sd(persuasion),
    sd_anger = sd(anger),
    sd_surprise = sd(surprise),
    sd_happiness = sd(happiness)
  )

# Replacing NA's in columns with SD where there is only one chunk of text
FR_final[, c(9:15)] <-
  apply(FR_final[, c(9:15)], 2, function(x)
    replace(x, is.na(x), 0))

# adding the week number (format year, week no)
FR_final$week <- strftime(FR_final$date, format = "%y%V")

# reading the polls data

FR_polls <- read_csv('data/polls/FR_polls.csv')
FR_polls <-
  FR_polls %>% group_by(week) %>% summarise(support = mean(macron_approve))
FR_polls <- FR_polls %>%  mutate(
  diff0 = support - lag(support),   diff1 = lead(support) - support,
  diff2 = lead(support, n = 2) - lead(support, n =
                                        1)
)
FR_final$week <- as.numeric(FR_final$week)
# joining the two dataframes

FR_final <-
  FR_final %>% left_join (FR_polls, by = 'week') %>% na.omit()

# data needs to be aggregated
fr_cases <-
  cases_data$France...122 + cases_data$France...123 + cases_data$France...124 +
  cases_data$France...125 +
  cases_data$France...126 + cases_data$France...127 + cases_data$France...128 + cases_data$France...129 +
  cases_data$France...130 + cases_data$France...131 + cases_data$France...132 + cases_data$France...133


FR_final <-
  FR_final %>% left_join(tibble(week = cases_data$week, cases = fr_cases), by =
                           'week')

FR_final$country <- "FR"

rm(list = setdiff(
  ls(),
  c("USA_final", "UK_final", "ES_final", "FR_final", "cases_data")
))

# DE ####

DE_speeches  <-
  data <- read_delim("data/speeches/speeches_DE_translated.csv")
#fixing the dates in dataset
DE_speeches$dates <- ymd(DE_speeches$dates)

df <- tibble()
fragm <- c()
for (i in 1:nrow(DE_speeches)) {
  #we need to check how many groups to create
  
  speech_fragment <-
    unlist(strsplit(
      DE_speeches$speeches_EN[i],
      "(?<=[[:punct:]])\\s(?=[A-Z])",
      perl = T
    ))
  fragm <- c()
  for (j in 1:floor(length(speech_fragment) / 5)) {
    fragm[j] <-
      paste(
        speech_fragment[j * 5 - 4],
        speech_fragment[j * 5 - 3],
        speech_fragment[j * 5 - 2],
        speech_fragment[j * 5 - 1],
        speech_fragment[j * 5]
      )
    fragm[j] <- gsub('NA', '', fragm[j])
    fragm[j] <- gsub('\n', '', fragm[j])
  }
  df_temp <-
    data.frame(
      fragment = fragm,
      fragment_no = c(1:length(fragm)),
      date = DE_speeches$dates[i]
    )
  df <- rbind.data.frame(df, df_temp)
}
DE_speeches <- df


# binding the results
DE_res <- read_csv("data/emotions/DE/DE_emotions.csv")

DE_final <-
  tibble(DE_speeches, DE_res) %>% group_by(date) %>% summarise(
    mean_sentiment = mean(sentiment),
    mean_informativeness = mean(informativeness),
    mean_persuasion = mean(persuasion),
    mean_anger = mean(anger),
    mean_surprise = mean(surprise),
    mean_happiness = mean(happiness),
    mean_fear = mean(fear),
    sd_sentiment = sd(sentiment),
    sd_informativeness = sd(informativeness),
    sd_fear = sd(fear),
    sd_persuasion = sd(persuasion),
    sd_anger = sd(anger),
    sd_surprise = sd(surprise),
    sd_happiness = sd(happiness)
  )

# Replacing NA's in columns with SD where there is only one chunk of text
DE_final[, c(9:15)] <-
  apply(DE_final[, c(9:15)], 2, function(x)
    replace(x, is.na(x), 0))

# adding the week number (format year, week no)
DE_final$week <- strftime(DE_final$date, format = "%y%V")

# reading the polls data

DE_polls <- read_csv('data/polls/DE_polls.csv')
DE_polls <-
  DE_polls %>% group_by(week) %>% summarise(support = mean(Union))
DE_polls <- DE_polls %>%  mutate(
  diff0 = support - lag(support),   diff1 = lead(support) - support,
  diff2 = lead(support, n = 2) - lead(support, n =
                                        1)
)
DE_final$week <- as.numeric(DE_final$week)
DE_polls$week <- as.numeric(DE_polls$week)
# joining the two dataframes

DE_final <-
  DE_final %>% left_join (DE_polls, by = 'week') %>% na.omit()

# data needs to be aggregated


DE_final <-
  DE_final %>% left_join(tibble(week = cases_data$week, cases = cases_data$Germany), by =
                           'week')

DE_final$country <- "DE"

rm(list = setdiff(
  ls(),
  c("USA_final", "UK_final", "ES_final", "FR_final", "DE_final" ,"cases_data")
))

# IT ####

IT_speeches  <-
  data <- read_delim("data/speeches/speeches_IT_translated.csv")
#fixing the dates in dataset
IT_speeches$dates <- ymd(IT_speeches$dates)

df <- tibble()
fragm <- c()
for (i in 1:nrow(IT_speeches)) {
  #we need to check how many groups to create
  
  speech_fragment <-
    unlist(strsplit(
      IT_speeches$speeches_EN[i],
      "(?<=[[:punct:]])\\s(?=[A-Z])",
      perl = T
    ))
  fragm <- c()
  for (j in 1:floor(length(speech_fragment) / 5)) {
    fragm[j] <-
      paste(
        speech_fragment[j * 5 - 4],
        speech_fragment[j * 5 - 3],
        speech_fragment[j * 5 - 2],
        speech_fragment[j * 5 - 1],
        speech_fragment[j * 5]
      )
    fragm[j] <- gsub('NA', '', fragm[j])
    fragm[j] <- gsub('\n', '', fragm[j])
  }
  df_temp <-
    data.frame(
      fragment = fragm,
      fragment_no = c(1:length(fragm)),
      date = IT_speeches$dates[i]
    )
  df <- rbind.data.frame(df, df_temp)
}
IT_speeches <- df


# binding the results
IT_res <- read_csv("data/emotions/IT/IT_emotions.csv")[c(1:length(IT_speeches$fragment)),]

IT_final <-
  tibble(IT_speeches, IT_res) %>% group_by(date) %>% summarise(
    mean_sentiment = mean(sentiment),
    mean_informativeness = mean(informativeness),
    mean_persuasion = mean(persuasion),
    mean_anger = mean(anger),
    mean_surprise = mean(surprise),
    mean_happiness = mean(happiness),
    mean_fear = mean(fear),
    sd_sentiment = sd(sentiment),
    sd_informativeness = sd(informativeness),
    sd_fear = sd(fear),
    sd_persuasion = sd(persuasion),
    sd_anger = sd(anger),
    sd_surprise = sd(surprise),
    sd_happiness = sd(happiness)
  )

# Replacing NA's in columns with SD where there is only one chunk of text
IT_final[, c(9:15)] <-
  apply(IT_final[, c(9:15)], 2, function(x)
    replace(x, is.na(x), 0))

# adding the week number (format year, week no)
IT_final$week <- strftime(IT_final$date, format = "%y%V")

# reading the polls data

IT_polls <- read_csv('data/polls/IT_polls.csv')
IT_polls <-
  IT_polls %>% group_by(week) %>% summarise(support = mean(M5S))
IT_polls <- IT_polls %>%  mutate(
  diff0 = support - lag(support),   diff1 = lead(support) - support,
  diff2 = lead(support, n = 2) - lead(support, n =
                                        1)
)
IT_final$week <- as.numeric(IT_final$week)
IT_polls$week <- as.numeric(IT_polls$week)
# joining the two dataframes

IT_final <-
  IT_final %>% left_join (IT_polls, by = 'week') %>% na.omit()

# data needs to be aggregated


IT_final <-
  IT_final %>% left_join(tibble(week = cases_data$week, cases = cases_data$Italy), by =
                           'week')

IT_final$country <- "IT"

rm(list = setdiff(
  ls(),
  c("USA_final", "UK_final", "ES_final", "FR_final", "DE_final", "IT_final" ,"cases_data")
))


# CA ####

CA_speeches  <-
  data <- read_delim("data/speeches/speeches_CA.csv")
#fixing the dates in dataset
CA_speeches$dates <- ymd(CA_speeches$dates)

df <- tibble()
fragm <- c()
for (i in 1:nrow(CA_speeches)) {
  #we need to check how many groups to create
  
  speech_fragment <-
    unlist(strsplit(
      CA_speeches$speeches[i],
      "(?<=[[:punct:]])\\s(?=[A-Z])",
      perl = T
    ))
  fragm <- c()
  for (j in 1:floor(length(speech_fragment) / 5)) {
    fragm[j] <-
      paste(
        speech_fragment[j * 5 - 4],
        speech_fragment[j * 5 - 3],
        speech_fragment[j * 5 - 2],
        speech_fragment[j * 5 - 1],
        speech_fragment[j * 5]
      )
    fragm[j] <- gsub('NA', '', fragm[j])
    fragm[j] <- gsub('\n', '', fragm[j])
  }
  df_temp <-
    data.frame(
      fragment = fragm,
      fragment_no = c(1:length(fragm)),
      date = CA_speeches$dates[i]
    )
  df <- rbind.data.frame(df, df_temp)
}
CA_speeches <- df


# binding the results
CA_res <- read_csv("data/emotions/CA/CA_emotions.csv")

CA_final <-
  tibble(CA_speeches, CA_res) %>% group_by(date) %>% summarise(
    mean_sentiment = mean(sentiment),
    mean_informativeness = mean(informativeness),
    mean_persuasion = mean(persuasion),
    mean_anger = mean(anger),
    mean_surprise = mean(surprise),
    mean_happiness = mean(happiness),
    mean_fear = mean(fear),
    sd_sentiment = sd(sentiment),
    sd_informativeness = sd(informativeness),
    sd_fear = sd(fear),
    sd_persuasion = sd(persuasion),
    sd_anger = sd(anger),
    sd_surprise = sd(surprise),
    sd_happiness = sd(happiness)
  )

# Replacing NA's in columns with SD where there is only one chunk of text
CA_final[, c(9:15)] <-
  apply(CA_final[, c(9:15)], 2, function(x)
    replace(x, is.na(x), 0))

# adding the week number (format year, week no)
CA_final$week <- strftime(CA_final$date, format = "%y%V")

# reading the polls data

CA_polls <- read_csv('data/polls/CA_polls.csv')
CA_polls <-
  CA_polls %>% group_by(week) %>% summarise(support = mean(support))
CA_polls <- CA_polls %>%  mutate(
  diff0 = support - lag(support),   diff1 = lead(support) - support,
  diff2 = lead(support, n = 2) - lead(support, n =
                                        1)
)
CA_final$week <- as.numeric(CA_final$week)
CA_polls$week <- as.numeric(CA_polls$week)
# joining the two dataframes

CA_final <-
  CA_final %>% left_join (CA_polls, by = 'week') %>% na.omit()

# data needs to be aggregated


CA_cases <- cases_data$Canada...42 + cases_data$Canada...43 + cases_data$Canada...44 + cases_data$Canada...45+
  cases_data$Canada...46 + cases_data$Canada...47 + cases_data$Canada...48 + cases_data$Canada...49 + cases_data$Canada...50 +
   cases_data$Canada...51 + cases_data$Canada...52 + cases_data$Canada...53 + cases_data$Canada...54 + cases_data$Canada...57


CA_final <- CA_final %>% left_join(tibble(week = cases_data$week, cases = CA_cases), by ='week')



CA_final$country <- "CA"

rm(list = setdiff(
  ls(),
  c("USA_final", "UK_final", "ES_final", "FR_final", "DE_final", "IT_final" , "CA_final" ,"cases_data")
))









model_data <- rbind(USA_final, UK_final, ES_final, FR_final, DE_final, IT_final, CA_final)





write_csv(model_data, "data/model_data.csv")




#### VISUALIZATIONS ####
library(reshape2)
plotting_df <- model_data[,c(1:8,22)]
colnames(plotting_df) <- c('Date', 'Sentiment', 'Informativeness', 'Persuasion',
                           'Anger', "Surprise", "Happiness", "Fear", "Country")

plotting_df$Date <- ymd(plotting_df$Date)

plotting_df <- reshape2::melt(plotting_df, id.vars =(c(1,9)))


ggplot(data = plotting_df, aes(x = Date, y = value, color = variable, group = variable)) + 
  facet_wrap(~Country, ncol = 1) +  # Make the plots stack vertically
  geom_line() +   
  scale_color_discrete() + 
  theme_minimal()  + xlab(NULL) + ylab(NULL) + ylim(c(0,5)) + 
  theme(
    legend.title = element_blank(), 
    legend.position = "bottom",  # Move the legend to the bottom
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(color = guide_legend(direction = "horizontal")) +  # Make the legend horizontal
  scale_x_date(date_labels = "%m/%y", date_breaks = "2 months") +
  labs(title = "Features extracted from the transcripts")



