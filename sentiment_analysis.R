suppressPackageStartupMessages({
  suppressWarnings(library(dplyr))
  suppressWarnings(library(sentimentr))
  suppressWarnings(library(tidytext))
  suppressWarnings(library(lubridate))
  suppressWarnings(library(tidyr))
  suppressWarnings(library(argparse))
  suppressWarnings(library(ggpubr))
  suppressWarnings(library(tidyverse))
  suppressWarnings(library(textdata))
})

load_data<-function(filename, stringAsfunction = FALSE, verbose = FALSE) { #we need this to do string manipulation
  if (verbose) message("Reading data...")
  data <- read.csv("../data/toots.csv") 
  
  if (verbose) message("Cleaning data...")
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
    
  if (verbose) message("Data successfully loaded and cleaned")
  return(data)
}



word_analysis<-function(toot_data, emotion, verbose = FALSE) {
  library(tidyverse)
  library(textdata)

  toot_data <- read.csv("../data/toots.csv", colClasses = c("id" = "character"))  #making sure content of id is read as a character
 
  if (verbose) message("Analysing data for ", emotion)
  word_data <- toot_data %>% #layout from gemini
    unnest_tokens(word, content) %>% #splitting content column into words
    select(id, created_at, word) %>% # Keep id and created_at
    filter(word != "") 
  
  #using NRC lexicon: Mohammed, Saif M; Turney, Peter; 2011; http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
  nrc_lexicon <- get_sentiments("nrc") %>% 
    filter(sentiment != ("positive")) %>% #we only want emotions
    filter(sentiment != ("negative")) 
  
  #join words with lexicon using inner join
  words_with_sentiment <- inner_join(word_data, nrc_lexicon, by = "word", relationship = "many-to-many") %>% #joining my word column with the lexicon
    filter(sentiment == emotion)
  
  word_data <- tail(words_with_sentiment %>% arrange(desc(id)), 9) #9 because the bottom 9 have the desired IDs
  
  if (verbose) message("word_analysis complete.")
    return(word_data)
}


#emotion_words_count <- words_with_sentiment %>%
   #group_by(word, sentiment) %>% 
    #count(sort=TRUE) %>% 
    #head(10)
   #print(emotion_words_count)

sentiment_analysis<-function(toot_data) {
  library(dplyr)
  library(tidytext)
  #Take the output of loaded data as input
  toot_data <- read.csv("../data/toots.csv", colClasses = c("id" = "character"))
  #Perform sentiment analysis with the three methods
  sentiment_data <- toot_data %>% 
    unnest_tokens(word, content) %>% 
    select(id, created_at, word) %>% 
    filter(word != "") 
  #1.afinn
  afinn_sentiment <- (get_sentiments("afinn")) %>% 
    inner_join(sentiment_data, by = "word", relationship = "many-to-many") %>% 
    group_by(id, created_at) %>% 
    summarise(sentiment = sum(value)) %>% 
    mutate(method = "afinn")
  print(afinn_sentiment)
  #2.nrc
  nrc_sentiment <- (get_sentiments("nrc")) %>% 
    inner_join(sentiment_data, by = "word", relationship = "many-to-many") %>% 
    group_by(id, created_at) %>% 
    summarise(sentiment = paste(unique(sentiment))) %>% 
    mutate(method = "nrc")
  print(nrc_sentiment)
  #3.bing
  bing_sentiment <- (get_sentiments("bing")) %>% 
    inner_join(sentiment_data, by = "word", relationship = "many-to-many") %>% 
    group_by(id, created_at) %>% 
    summarise(positive_count = sum(sentiment == "positive", na.rm = TRUE),
              negative_count = sum(sentiment == "negative", na.rm = TRUE),
              sentiment = ifelse(positive_count > negative_count, "positive",
                                 ifelse(negative_count > positive_count, "negative", "neutral")
              )) %>%
    mutate(method = "bing")
  print(bing_sentiment)
  
  all_lexicons <- bind_rows(afinn_sentiment, nrc_sentiment, bing_sentiment)
  print(all_lexicons)

  #Return a data structure with atleast ID, method, created_at and sentiment
  #Method column to identify which of the three methods was used for each sentiment score
  #Only return these three and all should be present
  #The function should preserve the IDs and the unique IDs in the output should match th IDS
  
    return(all_lexicons)

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
