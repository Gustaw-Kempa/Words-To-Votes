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





# JP ####

JP_speeches  <-
  data <- read_delim("data/speeches/speeches_JP.csv")
#fixing the dates in dataset
JP_speeches$dates <- ymd(JP_speeches$dates)

df <- tibble()
fragm <- c()
for (i in 1:nrow(JP_speeches)) {
  #we need to check how many groups to create
  
  speech_fragment <-
    unlist(strsplit(
      JP_speeches$speeches[i],
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
      date = JP_speeches$dates[i]
    )
  df <- rbind.data.frame(df, df_temp)
}
JP_speeches <- df


# binding the results
JP_res <- read_csv("data/emotions/JP/JP_emotions.csv")

JP_final <-
  tibble(JP_speeches, JP_res) %>% group_by(date) %>% summarise(
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
JP_final[, c(9:15)] <-
  apply(JP_final[, c(9:15)], 2, function(x)
    replace(x, is.na(x), 0))

# adding the week number (format year, week no)
JP_final$week <- strftime(JP_final$date, format = "%y%V")

# reading the polls data

JP_polls <- read_csv('data/polls/JP_polls.csv')
JP_polls <-
  JP_polls %>% group_by(week) %>% summarise(support = mean(LDP))
JP_polls <- JP_polls %>%  mutate(
  diff0 = support - lag(support),   diff1 = lead(support) - support,
  diff2 = lead(support, n = 2) - lead(support, n =
                                        1)
)
JP_final$week <- as.numeric(JP_final$week)
JP_polls$week <- as.numeric(JP_polls$week)
# joining the two dataframes

JP_final <-
  JP_final %>% left_join (JP_polls, by = 'week') %>% na.omit()

# data needs to be aggregated

JP_cases <- cases_data$Japan


JP_final <- JP_final %>% left_join(tibble(week = cases_data$week, cases = JP_cases), by ='week')



JP_final$country <- "JP"

rm(list = setdiff(
  ls(),
  c("USA_final", "UK_final", "ES_final", "FR_final", "DE_final", "IT_final" , "CA_final", "JP_final" ,"cases_data")
))


# DK ####

DK_speeches  <-
  data <- read_delim("data/speeches/speeches_DK.csv")
#fixing the dates in dataset
DK_speeches$dates <- ymd(DK_speeches$dates)

df <- tibble()
fragm <- c()
for (i in 1:nrow(DK_speeches)) {
  #we need to check how many groups to create
  
  speech_fragment <-
    unlist(strsplit(
      DK_speeches$speeches_EN[i],
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
      date = DK_speeches$dates[i]
    )
  df <- rbind.data.frame(df, df_temp)
}
DK_speeches <- df


# binding the results
DK_res <- read_csv("data/emotions/DK/DK_emotions.csv")

DK_final <-
  tibble(DK_speeches, DK_res) %>% group_by(date) %>% summarise(
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
DK_final[, c(9:15)] <-
  apply(DK_final[, c(9:15)], 2, function(x)
    replace(x, is.na(x), 0))

# adding the week number (format year, week no)
DK_final$week <- strftime(DK_final$date, format = "%y%V")

# reading the polls data

DK_polls <- read_csv('data/polls/DK_polls.csv')
DK_polls <-
  DK_polls %>% group_by(week) %>% summarise(support = mean(A))
DK_polls <- DK_polls %>%  mutate(
  diff0 = support - lag(support),   diff1 = lead(support) - support,
  diff2 = lead(support, n = 2) - lead(support, n =
                                        1)
)
DK_final$week <- as.numeric(DK_final$week)
DK_polls$week <- as.numeric(DK_polls$week)
# joining the two dataframes

DK_final <-
  DK_final %>% left_join (DK_polls, by = 'week') %>% na.omit()

# data needs to be aggregated

DK_cases <- cases_data$Denmark...105 + cases_data$Denmark...106 + cases_data$Denmark...107


DK_final <- DK_final %>% left_join(tibble(week = cases_data$week, cases = DK_cases), by ='week')



DK_final$country <- "DK"

rm(list = setdiff(
  ls(),
  c("USA_final", "UK_final", "ES_final", "FR_final", "DE_final", "IT_final" , "CA_final", "JP_final" , "DK_final","cases_data")
))


# NOK ####

NOK_speeches  <-
  data <- read_delim("data/speeches/speeches_NOK.csv")
#fixing the dates in dataset
NOK_speeches$dates <- ymd(NOK_speeches$dates)

df <- tibble()
fragm <- c()
for (i in 1:nrow(NOK_speeches)) {
  #we need to check how many groups to create
  
  speech_fragment <-
    unlist(strsplit(
      NOK_speeches$speeches[i],
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
      date = NOK_speeches$dates[i]
    )
  df <- rbind.data.frame(df, df_temp)
}
NOK_speeches <- df


# binding the results
NOK_res <- read_csv("data/emotions/NOK/NOK_emotions.csv")

NOK_final <-
  tibble(NOK_speeches, NOK_res) %>% group_by(date) %>% summarise(
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
NOK_final[, c(9:15)] <-
  apply(NOK_final[, c(9:15)], 2, function(x)
    replace(x, is.na(x), 0))

# adding the week number (format year, week no)
NOK_final$week <- strftime(NOK_final$date, format = "%y%V")

# reading the polls data

NOK_polls <- read_csv('data/polls/NOK_polls.csv')
NOK_polls <-
  NOK_polls %>% group_by(week) %>% summarise(support = mean(H))
NOK_polls <- NOK_polls %>%  mutate(
  diff0 = support - lag(support),   diff1 = lead(support) - support,
  diff2 = lead(support, n = 2) - lead(support, n =
                                        1)
)
NOK_final$week <- as.numeric(NOK_final$week)
NOK_polls$week <- as.numeric(NOK_polls$week)
# joining the two dataframes

NOK_final <-
  NOK_final %>% left_join (NOK_polls, by = 'week') %>% na.omit()

# data needs to be aggregated

NOK_cases <- cases_data$Norway


NOK_final <- NOK_final %>% left_join(tibble(week = cases_data$week, cases = NOK_cases), by ='week')



NOK_final$country <- "NOK"

rm(list = setdiff(
  ls(),
  c("USA_final", "UK_final", "ES_final", "FR_final", "DE_final", "IT_final" , "CA_final", "JP_final" ,"DK_final", "NOK_final","cases_data")
))




# NL ####

NL_speeches  <-
  data <- read_delim("data/speeches/speeches_NL.csv")
#fixing the dates in dataset
NL_speeches$dates <- ymd(NL_speeches$dates)

df <- tibble()
fragm <- c()
for (i in 1:nrow(NL_speeches)) {
  #we need to check how many groups to create
  
  speech_fragment <-
    unlist(strsplit(
      NL_speeches$speeches_EN[i],
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
      date = NL_speeches$dates[i]
    )
  df <- rbind.data.frame(df, df_temp)
}
NL_speeches <- df


# binding the results
NL_res <- read_csv("data/emotions/NL/NL_emotions.csv")

NL_final <-
  tibble(NL_speeches, NL_res) %>% group_by(date) %>% summarise(
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
NL_final[, c(9:15)] <-
  apply(NL_final[, c(9:15)], 2, function(x)
    replace(x, is.na(x), 0))

# adding the week number (format year, week no)
NL_final$week <- strftime(NL_final$date, format = "%y%V")

# reading the polls data

NL_polls <- read_csv('data/polls/NL_polls.csv')
NL_polls <-
  NL_polls %>% group_by(week) %>% summarise(support = mean(VVD))
NL_polls <- NL_polls %>%  mutate(
  diff0 = support - lag(support),   diff1 = lead(support) - support,
  diff2 = lead(support, n = 2) - lead(support, n =
                                        1)
)
NL_final$week <- as.numeric(NL_final$week)
NL_polls$week <- as.numeric(NL_polls$week)
# joining the two dataframes

NL_final <-
  NL_final %>% left_join (NL_polls, by = 'week') %>% na.omit()

# data needs to be aggregated

NL_cases <- cases_data$Netherlands...198 + cases_data$Netherlands...199 + cases_data$Netherlands...200 +
  cases_data$Netherlands...201 + cases_data$Netherlands...202


NL_final <- NL_final %>% left_join(tibble(week = cases_data$week, cases = NL_cases), by ='week')



NL_final$country <- "NL"

rm(list = setdiff(
  ls(),
  c("USA_final", "UK_final", "ES_final", "FR_final", "DE_final", "IT_final" , "CA_final", "JP_final" ,"DK_final","NOK_final",  "NL_final","cases_data")
))







model_data <- rbind(USA_final, UK_final, ES_final, FR_final, DE_final, IT_final,
                    CA_final, JP_final, NOK_final, DK_final, NL_final)





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



