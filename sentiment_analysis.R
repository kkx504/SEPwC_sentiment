suppressPackageStartupMessages({
library(sentimentr)
library(tidytext)
library(lubridate)
library(dplyr)
library(tidyr)
library(argparse)
library(ggpubr)
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

user_input <- readline("Enter something: ")
print(paste("You entered:", user_input))


word_analysis<-function(toot_data, emotion) {
  library(tidyverse)
  library(textdata)
  #want to create columns called sentiment and word
  
  #need to group all rows together and seperate into just word data then group them by different sentiments, count how many sentiments for each and display these
  #1. combine the text rows into single string
  toot_data <- read.csv("../data/toots.csv")
  
  all_text <- paste(toot_data$content, collapse = " ") #from gemini
  #2.split into wordss
  words <- strsplit(all_text, split = " ")[[1]]
  #3. clean the words (no punctuation) 
  clean_words <- str_remove_all(words, "[[:punct:]]") 
  clean_words <- str_to_lower(clean_words)
  #4. use nrs lexicon - need to cite
  nrc_lexicon <- get_sentiments("nrc") %>% 
    filter(sentiment != ("positive")) %>% #we only want emotions
    filter(sentiment != ("negative")) 
  #join words with lexicon using inner join
  words_with_sentiment <- inner_join(tibble(word = clean_words), nrc_lexicon, by = "word")  #need to use tibble because inner join only used dataframes and we are using a string at the moment
    print(words_with_sentiment)
    
  emotion_words <- words_with_sentiment %>%
      filter(sentiment == emotion) %>%
      count(word, sort = TRUE)
  print(emotion_words)

    return(emotion_words)
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
