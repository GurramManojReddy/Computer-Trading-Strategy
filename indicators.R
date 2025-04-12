defaultscalinglength <- 10000

genIndicators <- function(sym) {
  print(paste('Generating Indicators for symbol:', sym))
  stock <- subset(sp500_data, symbol == sym)
  stock.xts <- xts(stock[, c(3:7)], order.by = stock$date)             
  stock.xts$momentum1 <- diff(stock.xts$close)  
  stock.xts$accel <- diff(stock.xts$momentum1)
  
  stock.xts$momentum10 <- tryCatch({
    momentum(stock.xts$close, n = 10)
  }, warning = function(w) { NA }, error = function(e) { NA })
  
  stock.xts$momentum20 <- tryCatch({
    momentum(stock.xts$close, n = 20)
  }, warning = function(w) { NA }, error = function(e) { NA })
  
  macd <- tryCatch({
    MACD(stock.xts$close, maType = "EMA")
  }, warning = function(w) { NULL }, error = function(e) { NULL })
  if (is.null(macd)) {
    stock.xts$macdDiff <- NA
  } else {
    stock.xts$macdDiff <- macd[,1] - macd[,2]
  }
  
  stock.xts$sma5 <- tryCatch({
    SMA(stock.xts$close, n = 5)
  }, warning = function(w) { NA }, error = function(e) { NA })
  stock.xts$sma10 <- tryCatch({
    SMA(stock.xts$close, n = 10)
  }, warning = function(w) { NA }, error = function(e) { NA })
  stock.xts$sma20 <- tryCatch({
    SMA(stock.xts$close, n = 20)
  }, warning = function(w) { NA }, error = function(e) { NA })
  stock.xts$sma40 <- tryCatch({
    SMA(stock.xts$close, n = 40)
  }, warning = function(w) { NA }, error = function(e) { NA })
  stock.xts$sma60 <- tryCatch({
    SMA(stock.xts$close, n = 60)
  }, warning = function(w) { NA }, error = function(e) { NA })
  stock.xts$sma100 <- tryCatch({
    SMA(stock.xts$close, n = 100)
  }, warning = function(w) { NA }, error = function(e) { NA })
  
  stock.xts$sma5.lag1 <- stats::lag(stock.xts$sma5, k = 1)
  stock.xts$sma10.lag1 <- stats::lag(stock.xts$sma10, k = 1)
  stock.xts$sma20.lag1 <- stats::lag(stock.xts$sma20, k = 1)
  stock.xts$sma40.lag1 <- stats::lag(stock.xts$sma40, k = 1)
  stock.xts$sma60.lag1 <- stats::lag(stock.xts$sma60, k = 1)
  
  stock.xts$cross510 <- stock.xts$sma5 / stock.xts$sma10
  stock.xts$cross520 <- stock.xts$sma5 / stock.xts$sma20
  stock.xts$cross540 <- stock.xts$sma5 / stock.xts$sma40
  stock.xts$cross1020 <- stock.xts$sma10 / stock.xts$sma20
  stock.xts$cross1040 <- stock.xts$sma10 / stock.xts$sma40
  stock.xts$cross1060 <- stock.xts$sma10 / stock.xts$sma60  
  stock.xts$cross10100 <- stock.xts$sma10 / stock.xts$sma100  
  stock.xts$cross2040 <- stock.xts$sma20 / stock.xts$sma40
  stock.xts$cross2060 <- stock.xts$sma20 / stock.xts$sma60
  stock.xts$cross20100 <- stock.xts$sma20 / stock.xts$sma100      
  
  bb <- tryCatch({
    BBands(HLC(stock.xts), n = 20, maType = "SMA", sd = 2)
  }, warning = function(w) { NULL }, error = function(e) { NULL })
  if (is.null(bb)) {
    stock.xts$dn <- NA
    stock.xts$mavg <- NA
    stock.xts$up <- NA
    stock.xts$range <- NA
  } else {
    stock.xts$dn <- bb$dn
    stock.xts$mavg <- bb$mavg
    stock.xts$up <- bb$up
    stock.xts$range <- bb$up - bb$dn
  }
  
  stock.xts$rsi5 <- tryCatch({
    RSI(stock.xts$close, n = 5)
  }, warning = function(w) { NA }, error = function(e) { NA })
  stock.xts$rsi10 <- tryCatch({
    RSI(stock.xts$close, n = 10)
  }, warning = function(w) { NA }, error = function(e) { NA })
  stock.xts$rsi20 <- tryCatch({
    RSI(stock.xts$close, n = 20)
  }, warning = function(w) { NA }, error = function(e) { NA })
  
  stock.xts <- na.omit(stock.xts)
  scaled.xts <- NULL
  lengthforscaling <- min(defaultscalinglength, nrow(stock.xts))
  if (lengthforscaling > 0) {
    maxs <- apply(stock.xts[1:lengthforscaling, ], 2, max)
    mins <- apply(stock.xts[1:lengthforscaling, ], 2, min)
    scaled.xts <- tryCatch({
      scale(stock.xts[1:lengthforscaling, ], center = mins, scale = maxs - mins)
    }, warning = function(w) { NULL }, error = function(e) { NULL })
  }
  
  if (!is.null(scaled.xts)) { 
    if (lengthforscaling < nrow(stock.xts)) {
      startscale <- lengthforscaling + 1
      stopscale <- nrow(stock.xts)
      for (i in startscale:stopscale) {
        ws <- i - lengthforscaling + 1
        maxs <- apply(stock.xts[ws:i, ], 2, max)
        mins <- apply(stock.xts[ws:i, ], 2, min)
        temp.xts <- tryCatch({
          scale(stock.xts[ws:i, ], center = mins, scale = maxs - mins)
        }, warning = function(w) { NULL }, error = function(e) { NULL })
        nextrow <- nrow(temp.xts)
        scaled.xts <- rbind(scaled.xts, temp.xts[nextrow, ])
      }
    }
    scaled.xts$cross510up <- ifelse(stock.xts$sma5.lag1 < stock.xts$sma10.lag1 & stock.xts$sma5 > stock.xts$sma10, 1, 0)
    scaled.xts$cross510dn <- ifelse(stock.xts$sma5.lag1 > stock.xts$sma10.lag1 & stock.xts$sma5 < stock.xts$sma10, 1, 0)
    scaled.xts$cross520up <- ifelse(stock.xts$sma5.lag1 < stock.xts$sma20.lag1 & stock.xts$sma5 > stock.xts$sma20, 1, 0)
    scaled.xts$cross520dn <- ifelse(stock.xts$sma5.lag1 > stock.xts$sma20.lag1 & stock.xts$sma5 < stock.xts$sma20, 1, 0)
    scaled.xts$cross540up <- ifelse(stock.xts$sma5.lag1 < stock.xts$sma40.lag1 & stock.xts$sma5 > stock.xts$sma40, 1, 0)
    scaled.xts$cross540dn <- ifelse(stock.xts$sma5.lag1 > stock.xts$sma40.lag1 & stock.xts$sma5 < stock.xts$sma40, 1, 0)
    scaled.xts$cross1020up <- ifelse(stock.xts$sma10.lag1 < stock.xts$sma20.lag1 & stock.xts$sma10 > stock.xts$sma20, 1, 0)
    scaled.xts$cross1020dn <- ifelse(stock.xts$sma10.lag1 > stock.xts$sma20.lag1 & stock.xts$sma10 < stock.xts$sma20, 1, 0)
    scaled.xts$cross1040up <- ifelse(stock.xts$sma10.lag1 < stock.xts$sma40.lag1 & stock.xts$sma10 > stock.xts$sma40, 1, 0)
    scaled.xts$cross1040dn <- ifelse(stock.xts$sma10.lag1 > stock.xts$sma40.lag1 & stock.xts$sma10 < stock.xts$sma40, 1, 0)
    scaled.xts$cross510   <- stock.xts$cross510
    scaled.xts$cross520   <- stock.xts$cross520
    scaled.xts$cross540   <- stock.xts$cross540
    scaled.xts$cross1020  <- stock.xts$cross1020
    scaled.xts$cross1040  <- stock.xts$cross1040
    scaled.xts$cross1060  <- stock.xts$cross1060
    scaled.xts$cross10100 <- stock.xts$cross10100
    scaled.xts$cross2040  <- stock.xts$cross2040
    scaled.xts$cross2060  <- stock.xts$cross2060
    scaled.xts$cross20100 <- stock.xts$cross20100
    scaled.xts$rsi5      <- stock.xts$rsi5
    scaled.xts$rsi10     <- stock.xts$rsi10
    scaled.xts$rsi20     <- stock.xts$rsi20
    scaled.xts$nextreturn <- (lead(as.vector(stock.xts$close), 1) - lead(as.vector(stock.xts$open), 1)) / lead(as.vector(stock.xts$open), 1) + 1
    scaled.xts$nextopen   <- lead(as.vector(stock.xts$open), 1)
    scaled.xts$nextclose  <- lead(as.vector(stock.xts$close), 1)
    
    # Convert back to a data frame and add symbol and date
    stock_df <- data.frame(scaled.xts)
    date <- as.Date(rownames(stock_df))
    stock_df <- cbind(symbol = sym, date, stock_df)
    
    # Correct merge: include both symbol and date to avoid duplicates
    temp <- unique(sp500_data[, c("symbol", "date", "adjusted")])
    stock_df <- merge(stock_df, temp, by = c("symbol", "date"))
    
    stock_df$dow <- as.factor(weekdays(stock_df$date, abbreviate = TRUE))
    
    # Replace infinite values with NA
    for (i in 1:ncol(stock_df)) {
      inflist <- which(is.infinite(stock_df[, i]))
      if (length(inflist) > 0) {
        stock_df[inflist, i] <- NA
      }
    }
    stock_df <- na.omit(stock_df)
    rownames(stock_df) <- seq(1, nrow(stock_df), 1)
  } else {
    stock_df <- NULL
  }
  
  return(stock_df)
}

# Load the saved data
load("Data.Rdata")  # Do not use source()
indicators <- NULL
sp500_symbols <- read.csv("symbols.csv", stringsAsFactors = FALSE)$x
for (sym in sp500_symbols) {
  temp <- genIndicators(sym)
  indicators <- rbind(indicators, temp)
}

# Save everything using a conventional R data file extension
save(indicators, file = "indicators.RData")

# Clearing the Variables
rm(list = ls())
cat("\014")