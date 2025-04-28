# Load required libraries
library(httr)
library(jsonlite)
library(syuzhet)

# Function to fetch stock news from NewsAPI
fetch_stock_news <- function(stock_symbol, api_key) {
  url <- paste0("https://newsapi.org/v2/everything?q=", stock_symbol, "&language=en&pageSize=5&apiKey=", api_key)
  response <- GET(url)
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  return(data$articles)
}

# Function to analyze sentiment of a vector of article titles
analyze_sentiment <- function(article_titles) {
  sentiment_scores <- get_sentiment(article_titles, method = "syuzhet")
  avg_score <- mean(sentiment_scores, na.rm = TRUE)
  
  if (avg_score > 0) return(1)
  else if (avg_score < 0) return(-1)
  else return(0)  
}

# Set your API key
api_key <- "2ffc1c4ea2e84fb6b9f6fafd242e4cddgjkfl,fkuvtg"

# Read stock symbols (assumed in a column named 'x')
sp500_symbols <- read.csv("symbols.csv", stringsAsFactors = FALSE)$x

# Preallocate sentiment vector
sentiment <- vector("numeric", length(sp500_symbols))

# Loop through symbols
for (i in seq_along(sp500_symbols)) {
  sym <- sp500_symbols[i]
  print(paste("Getting sentiment for symbol:", sym))
  
  articles <- fetch_stock_news(sym, api_key)
  
  if (!is.null(articles) && "title" %in% names(articles) && length(articles$title) > 0) {
    sentiment[i] <- analyze_sentiment(articles$title)
  } else {
    sentiment[i] <- 0  # Neutral if no articles
  }
}

# Save sentiment results with symbols
sentiment_df <- data.frame(symbol = sp500_symbols, sentiment = sentiment)
save(sentiment_df, file = "sentiments.RData")

# Optional: Save as CSV for easier viewing
write.csv(sentiment_df, "sentiments.csv", row.names = FALSE)

# Clear R environment and console
rm(list = ls())
cat("\014")
