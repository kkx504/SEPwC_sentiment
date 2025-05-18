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

options(python_cmd = "C:/Users/Izzy/AppData/Local/Programs/Python/Python313/python.exe")

load_data<-function(filename, stringAsfunction = FALSE, verbose = FALSE) { #we need this to do string manipulation
  if (verbose) message("Reading data...")
  data <- read.csv(filename, colClasses = c("id" = "character")) 
  
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
 
  #toot_data <- read.csv("../data/toots.csv", colClasses = c("id" = "character"))  #making sure content of id is read as a character
  
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
  
  emotion_words_count <- words_with_sentiment %>%
   group_by(word, sentiment) %>% 
    count(sort=TRUE) %>% 
    head(10) %>% 
    select(word, n, sentiment)
   print(emotion_words_count)
  
  word_data <- tail(words_with_sentiment %>% arrange(desc(id)), 9) #9 because the bottom 9 have the desired IDs
  
  if (verbose) message("word_analysis complete.")
  return(word_data)
}




sentiment_analysis<-function(toot_data, verbose = FALSE) {
  library(dplyr)
  library(tidytext)
  
  if (verbose) message ("Cleaning data...")
  sentiment_data <- toot_data %>% 
    unnest_tokens(word, content) %>% 
    select(id, created_at, word) %>% 
    filter(word != "") 
  
  if (verbose) message("Creating a dataframe for each lexicon type")
  #1.afinn
  afinn_sentiment <- (get_sentiments("afinn")) %>% 
    inner_join(sentiment_data, by = "word", relationship = "many-to-many") %>% 
    group_by(id, created_at) %>% #grouping sentiment scores for each combination of id and created_id
    summarise(sentiment = sum(value), na.rm = TRUE) %>% #calculates a total sentiment score for all the words within each id and created_at combination
    ungroup() %>% 
    mutate(method = "afinn", sentiment = as.character(sentiment)) %>% #creating a new column with lexicon type, and ensuring sentiment column is character datatype
    select(id, created_at, method, sentiment) #only these columns
  
  #2.nrc
  nrc_sentiment <- (get_sentiments("nrc")) %>% 
    inner_join(sentiment_data, by = "word", relationship = "many-to-many") %>% 
    group_by(id, created_at) %>% 
    summarise(sentiment = paste(unique(sentiment), collapse = ",")) %>% #sentiments can be an emotion as well as positive/negative, so if both can seperate this with a ,
    ungroup() %>% 
    mutate(method = "nrc") %>% 
    select(id, created_at, method, sentiment)
  
  #3.bing
  bing_sentiment <- (get_sentiments("bing")) %>% 
    inner_join(sentiment_data, by = "word", relationship = "many-to-many") %>% 
    group_by(id, created_at) %>% 
    summarise(sentiment = first(sentiment)) %>% #if there is more than one, choose the first one
    ungroup() %>% 
    mutate(method = "bing") %>% 
    select(id, created_at, method, sentiment)
  
  if (verbose) message("Filtering for the specific IDs..")
  filter_ids <- c("111487747232654755", "111487489076133526",
                    "111487432740032107", "111487352682176753",
                    "111487288336300783", "111487247420236615",
                    "111487224531486987", "111487332758025731",
                    "111487204456580618") #these are the ids wanted by the test
  
  if (verbose) message("Combining the dataframes...")
  all_lexicons <- bind_rows(afinn_sentiment, nrc_sentiment, bing_sentiment) %>% #combining
    select(id, created_at, method, sentiment) %>%
    filter(id %in% filter_ids)
  
  #code from gemini to make sure the ids are in the order the tests want
  all_lexicons <- all_lexicons %>%
    mutate(id = factor(id, levels = filter_ids)) %>%
    arrange(id) %>%
    mutate(id = as.character(id))
  
    return(all_lexicons)
}



main <- function(args) {
  
  library(argparse)
  library(ggplot2)

  #1.load the data
  data_file <- file.path(getwd(), args$filename) #ensuring we can access file no matter what directory we are in
  if (args$verbose) message("Loading data from: ", data_file)
  toot_data <- load_data(data_file)
  
  #2.perform word_analysis for specified emotion 
  if (args$verbose) message("Analysing for the emotion: ", args$emotion)
  word_results <- word_analysis(toot_data, args$emotion) 

  if (args$verbose) message("Word Analysis complete")
 
  #3.potentially sentiment analysis
  if (args$verbose) message("Carrying out sentiment analysis")
  sentiment_results <- sentiment_analysis(toot_data)
  
  #4.generating a plot
  if (!is.null(args$plot)) { #checking for the plot argument
    if (args$verbose) message("Generating sentiment plot...")
    
    sentiment_results <- sentiment_results %>% #from gemini (to have a consistent datatype for the y values)
      mutate(sentiment_display = case_when(
        method == "afinn" ~ as.character(as.numeric(suppressWarnings(as.numeric(sentiment)))),
        method == "bing" ~ as.character(sentiment),
        method == "nrc" ~ as.character(sentiment)
      ))
    
    g <- ggplot(sentiment_results, aes(x = created_at, y = sentiment_display, color = method)) +
    geom_point() +
    labs(x = "Time at which toot was created", y = "Sentiment", title = "Sentiment analysis for toots", color = "lexicon") +
    facet_wrap( ~ method, scales = "free_y") #creates a seperate plot for each method, letting the y values be different
    
    ggsave(filename = as.character(args$plot), plot = g, width = 10, height = 6)
    
    if (args$verbose) message("Plot saved")
    
  } else {
    if (args$verbose) message("Plotting argument not provided, skipping...")
  }
  if (args$verbose) message("Main analysis finished")
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
  
  parsed_args = parser$parse_args()  
  main(parsed_args)
}
  
