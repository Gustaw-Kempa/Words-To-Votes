rm(list = ls())

library(tidyverse) #general data wrangling
library(openai) # access to GPT-3
library(lubridate) #making the dates easier

# A unique private openAI API key needs to be provided:
# Sys.setenv(OPENAI_API_KEY = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')


#select the name of the country to be analysed
country <- "CA" 


data <- read_delim(paste0("data/speeches/speeches_", country, ".csv"))



prompts <-tibble(prompts = c("Evaluate the sentiment and its strength in the the text. Output only a number from -5 (very negative) to 5 (very positive) with 0.1 increments. Text:'",
             "Detect the emotion of happiness in the following text. Evaluate strength of the emotion on a scale. Output only a number from 0 (no happiness) to 5 (very happy) with 0.1 increments. Text: '",
             "Detect the persuasion in the following text. Evaluate strength of the emotion on a scale. Output only a number from 0 (no persuasion) to 5 (very persuasive) with 0.1 increments. Text: '",
             "Detect the emotion of anger in the following text. Evaluate strength of the emotion on a scale. Output only a number from 0 (no anger) to 5 (very angry) with 0.1 increments. Text: '",
             "Detect the emotion of fear in the following text. Evaluate strength of the emotion on a scale. Output only a number from 0 (no fear) to 5 (very fearful) with 0.1 increments. Text:'",
             "Detect the emotion of surprise in the following text. Evaluate strength of the emotion on a scale. Output only a number from 0 (not surprised) to 5 (very surprised) with 0.1 increments. Text: '",
             "Detect the informativeness in the following text. Evaluate its strength on a scale. Output only a number from 0 (no informativeness) to 5 (very informative) with 0.1 increments. Text: '"
)
, emotions = c("sentiment", "happiness", "persuasion", "anger", "fear", "surprise", "informativeness")
)

#fixing the dates in dataset
 data$dates <- ymd(data$dates)
#count the number of words in each speech - would be better to count tokens instead - will be implemented :)
data$count <- str_count(data$speeches, "\\w+")

#a loop splitting the speeches into 400 word groups (with a 50-word overlap)
df <- tibble()
fragm <- c()
for (i in 1:nrow(data)) {
  #we need to check how many groups to create
  
  speech_fragment <-
    unlist(strsplit(data$speeches[i], "(?<=[[:punct:]])\\s(?=[A-Z])", perl = T))
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
  
  #we need to set bouandries for the fragment
  #speech_fragment <- word(data$speeches[i], max(0, (n-1)*350-50), min(-1,(n-t)*350))
  #speech_fragment <- word(data$speeches[i], max(0, (n-1)*400-50), min(data$count[i],n*400))
  df_temp <-
    data.frame(
      fragment = fragm,
      fragment_no = c(1:length(fragm)),
      date = data$dates[i]
    )
  df <- rbind.data.frame(df, df_temp)
}


# Implementing the gpt-3.5-turbo in the completions
res <- matrix(NA, nrow = nrow(df), ncol = nrow(prompts))
colnames(res) <- prompts$emotions
prompt_chat <- c("Evaluate the sentiment and its strength in the the text. Output only a number from -5 (very negative) to 5 (very positive) with 0.1 increments. Do not provide any explanation.",
                                  "Detect the emotion of happiness in the following text. Evaluate strength of the emotion on a scale. Output only a number from 0 (no happiness) to 5 (very happy) with 0.1 increments. Do not provide any explanation. ",
                                  "Detect the persuasion in the following text. Evaluate strength of the emotion on a scale. Output only a number from 0 (no persuasion) to 5 (very persuasive) with 0.1 increments. Do not provide any explanation.",
                                  "Detect the emotion of anger in the following text. Evaluate strength of the emotion on a scale. Output only a number from 0 (no anger) to 5 (very angry) with 0.1 increments. Do not provide any explanation.",
                                  "Detect the emotion of fear in the following text. Evaluate strength of the emotion on a scale. Output only a number from 0 (no fear) to 5 (very fearful) with 0.1 increments. Do not provide any explanation.",
                                  "Detect the emotion of surprise in the following text. Evaluate strength of the emotion on a scale. Output only a number from 0 (not surprised) to 5 (very surprised) with 0.1 increments. Do not provide any explanation.",
                                  "Detect the informativeness in the following text. Evaluate its strength on a scale. Output only a number from 0 (no informativeness) to 5 (very informative) with 0.1 increments. Do not provide any explanation."
)

for (j in 1:length(prompt_chat)) {
  for (i in which(is.na(res[,j]))) {
    tryCatch({

      
      ans <- create_chat_completion(
  model = "gpt-3.5-turbo", temperature = 0, 
  messages = list(
    list("role" = "system",
         "content" = prompt_chat[j]),
    list("role" = "user",
         "content" = df$fragment[i])
  )
)

scores <- readr::parse_number(ans$choices$message.content)
res[i,j] <- scores
print(paste(i,j, " score:", scores, Sys.time()))
Sys.sleep(10)


    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  write.csv(res, paste0("data/temp/temp_res_", country,j, ".csv" ))
}

res[res>5]

country_data <- as_tibble(res)

write_csv(country_data, paste0("data/emotions/",country, "/",country, "_emotions.csv"))



