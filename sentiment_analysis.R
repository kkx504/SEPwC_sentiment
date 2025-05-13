suppressPackageStartupMessages({
  suppressWarnings(library(sentimentr))
  suppressWarnings(library(tidytext))
  suppressWarnings(library(lubridate))
  suppressWarnings(library(dplyr))
  suppressWarnings(library(tidyr))
  suppressWarnings(library(argparse))
  suppressWarnings(library(ggpubr))
  suppressWarnings(library(tidyverse))
  suppressWarnings(library(textdata))
})

load_data<-function(filename, stringAsfunction = FALSE) { #we need this to do string manipulation
  data <- read.csv("../data/toots.csv") 
  
  #need to remove any html
  data <- data %>% 
    mutate_if(is.character, ~gsub("<[^>]+>", " ", .)) %>% #we use an anonymous function to ensure the code passes through each element, . represents the current thing being operated on
    mutate_if(is.character, ~gsub("\\s+", " ", .)) #gemini suggested removing multiple white spaces to not mess anything up

  #change to date time format
  data$created_at <- ymd_hms(data$created_at)

  #only english language
  data <- data[data$language == "en", ] #needs the comma at the end to ensure we keep ALL columns but only rows with 'en'

  #ensuring id column is character class
  data$id <- as.character(data$id) #as.character turns it into character vector
    
  return(data)
}



word_analysis<-function(toot_data, emotion) {
  library(tidyverse)
  library(textdata)

  
  #need to group all rows together and seperate into just word data then group them by different sentiments, count how many sentiments for each and display these
  #1. combine the text rows into single string
  toot_data <- read.csv("../data/toots.csv", colClasses = c("id" = "character")) 
  
  #code from gemini to rejumble my code and keep id and created_at
  word_data <- toot_data %>%
    unnest_tokens(word, content) %>%
    select(id, created_at, word) %>% # Keep id and created_at
    mutate(word = str_remove_all(word, "[[:punct:]]")) %>%
    mutate(word = str_to_lower(word)) %>%
    filter(word != "") 
  
  #4. use nrs lexicon - need to cite
  nrc_lexicon <- get_sentiments("nrc") %>% 
    filter(sentiment != ("positive")) %>% #we only want emotions
    filter(sentiment != ("negative")) 
  #join words with lexicon using inner join
  words_with_sentiment <- inner_join(word_data, nrc_lexicon, by = "word", relationship = "many-to-many") %>% 
    filter(sentiment == emotion)
  
  word_data <- tail(words_with_sentiment %>% arrange(desc(id)), 9) #9 because the bottom 9 have those ids
  print(word_data)

  
  emotion_words_count <- words_with_sentiment %>%
   group_by(word, sentiment) %>% 
    count(sort=TRUE) %>% 
    head(10)
   print(emotion_words_count)
  
  
    return(word_data)
}

sentiment_analysis<-function(toot_data) {

    return()

}

main <- function(args) {

}


if(sys.nframe() == 0) {

  # main program, called via Rscript
  parser = ArgumentParser(
                    prog="Sentiment Analysis",
                    description="Analyse toots for word and sentence sentiments"
                    )
  parser$add_argument("filename",
                    help="the file to read the toots from")
  parser$add_argument("--emotion",
                      default="anger",
                      help="which emotion to search for")
  parser$add_argument('-v', '--verbose',
                    action='store_true',
                    help="Print progress")
  parser$add_argument('-p', '--plot',
                    help="Plot something. Give the filename")
  
  args = parser$parse_args()  
  main(args)
}
