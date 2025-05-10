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
  data <- read.csv(filename) 
  
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
  #want to create columns called sentiment and word
  
  #need to group all rows together and seperate into just word data then group them by different sentiments, count how many sentiments for each and display these
  #1. combine the text rows into single string
  toot_data <- read_csv("toots.csv")
  all_text <- paste(toot_data$content, collapse = " ") #from gemini
  #2.split into wordss
  words <- strsplit(all_text, split = " ")
  #3. clean the words (no punctuation) 
  clean_words <- str_remove_all(words, "[[:punct:]]")
  #4. use nrs lexicon - need to cite
  #join words with lexicon using inner join
  #group by sentiment
  #count senitments
  #has columns id, sentiment, created_at and word

  
  get_sentiments("nrc") 
   %>% 
    group_by(content)
  mutate(sentiment = !!emotion)
  select(toot_data, id, sentiment, created_at, word)

 
  #no more than 10 rows
  #descending order based on count column
class(toot_data)
    return()
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
