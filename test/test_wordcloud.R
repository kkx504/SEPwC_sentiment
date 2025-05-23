suppressPackageStartupMessages({
  suppressWarnings(library(testthat))
})

# load in the script you want to test
source("../sentiment_analysis.R")
test_data <- "data/test_toots.csv"

# tests --------------------
# check the get_plot_limit function
test_that("load_data", {
  
  cleaned_data <- load_data(test_data)
  
  #expect no html in content
  expect_false(any(grepl("<[^>]+>",cleaned_data$content)))
  # expect create_at column to be a date time
  expect_true(is.timepoint(cleaned_data$created_at))
  # expect all languages to be "en"
  expect_true(all(cleaned_data$language=="en"))
  # expect id to be a character class
  expect_type(cleaned_data$id, "character")
  
})

test_that("word_analysis", {
  
  cleaned_data <- load_data(test_data)
  word_data <- word_analysis(cleaned_data, "joy")
  
  # expect id, character, etc as columns
  expect_true("id" %in% colnames(word_data))
  expect_true("sentiment" %in% colnames(word_data))
  expect_true("created_at" %in% colnames(word_data))
  expect_true("word" %in% colnames(word_data))
  
  # expect 10 or fewer rows
  expect_true(length(word_data$id) <= 10)
  
  # expect count column to be sorted
  expect_true(all(sort(word_data$n, decreasing=T) == word_data$n))
  
  # specific tests (e.g. range of ID, etc)
  # for joy we expect ids 111487432740032107 and 111487288336300783
  expect_true(all(unique(word_data$id) == c("111487432740032107","111487288336300783")))
  
})

test_that("sentiment_analysis", {
  
  cleaned_data <- load_data(test_data)
  sentiment_data <- sentiment_analysis(cleaned_data)
  
  # exepct a method colum
  expect_true("id" %in% colnames(sentiment_data))
  expect_true("sentiment" %in% colnames(sentiment_data))
  expect_true("created_at" %in% colnames(sentiment_data))
  expect_true("method" %in% colnames(sentiment_data))
  
  # expect method to contain afinn, nrc and bing only (and all of these!)
  methods <- unique(sentiment_data$method)
  expect_true(all(methods==c("afinn", "nrc", "bing")))
  
  # specific tests (e.g. range of IDs, n rows, counts etc)
  expected_ids <- c("111487747232654755", "111487489076133526", 
                    "111487432740032107", "111487352682176753",
                    "111487288336300783", "111487247420236615",
                    "111487224531486987", "111487332758025731",
                    "111487204456580618")
  expect_true(all(unique(sentiment_data$id) == expected_ids))
  
})

test_that("check main", {
  args <- c("filename" = "data/test_toots.csv",
            "output"   = "test.pdf",
            "emotion"  = "anger")
  args <- data.frame(t(args))
  main(args)
  expect_gt(file.info("test.pdf")$size,1000)
})



if (requireNamespace("lintr")) {
  library(lintr)
  
  context("linting script")
  test_that("Coding style", {
    output<-lintr::lint("../sentiment_analysis.R")
    expect_lt(length(output),500)
    expect_lt(length(output),400)
    expect_lt(length(output),250)
    expect_lt(length(output),100)
    expect_lt(length(output),50)
    expect_lt(length(output),10)
    expect_equal(length(output),0)
  })
}


test_that("wordcloud", {
  
  cleaned_data <- load_data(test_data)
  output_filename <- "test_wordcloud.html"
  
  #check it runs
  expect_no_error(wordcloud(cleaned_data, "joy", output_filename))
   
  #check if file created
  expect_true(file.exists(output_filename))
  
  #testing that the file size has content (greater than 0)
  expect_gt(file.size(output_filename), 0)
})