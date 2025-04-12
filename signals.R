load("indicators.rdata")
days<-unique(indicators$date)
days<-days[order(days)]
longthreshold<-1.015
shortthreshold<-0.985
windowsize<-25                       # rolling training days for random forest
longestindicator<-100
yearsBacktest<-1
teststart<-as.Date("2024-04-01")
datastart<-which(days==teststart)-windowsize-longestindicator
dataend<-which(days==teststart)+ yearsBacktest*252+1
trainstart<-datastart+windowsize+longestindicator
trainend<-dataend-1


genSignals <- function(stock) {
  results <- NULL
  tickers <- unique(stock$symbol)  # assuming column is named 'ticker'
  for (currday in trainstart:trainend) {
    current_date <- days[currday]
    print(paste("Processing day:", current_date))
    for (t in tickers) {
      substock <- subset(stock, symbol == t)
      from <- days[currday - windowsize]
      to <- days[currday - 1]
      # Train only on this stock’s past data
      train <- subset(substock, date >= from & date <= to)[3:ncol(stock)]
      if (nrow(train) < 5) next  # skip if insufficient data
      rf.model=ranger(nextreturn~.-nextopen -nextclose,data=train,num.trees=500)
      rsq<-round(mean(rf.model$r.squared),3)
      # print(paste("RSQ:",rsq, "for symbol: ", t))
      # Predict only for the current stock on current date
      test <- subset(substock, date == current_date)
      if (nrow(test) == 0) next
      test$rsq <- rsq
      test$prediction <- predict(rf.model, test)$predictions
      test$buy<-NA
      test$sell<-NA
      test$short<-ifelse(test$prediction<shortthreshold,1,0)
      test$long<-ifelse(test$prediction>longthreshold,1,0)
      test$buy<-ifelse(test$long==1,test$nextopen,
                        ifelse(test$short==1,test$nextclose,NA))
      test$sell<-ifelse(test$long==1,test$nextclose,
                         ifelse(test$short==1,test$nextopen,NA))
      results <- rbind(results, test)
    }
  }
  return(results)
}


predictions<- genSignals(indicators)
colnames(predictions)

# Save everything using a conventional R data file extension
save(predictions, file = "signals.RData")

# Clearing the Variables
rm(list = ls())
cat("\014")