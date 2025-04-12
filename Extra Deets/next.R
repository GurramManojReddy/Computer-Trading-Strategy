#Global Variables
windowsize<-25
longthreshold<-1.015
shortthreshold<-0.985
defaultscalinglength<-10000


genIndicators=function(sym){
  print(paste('Generating Indicators for symbol: ',sym))
  stock<-unique(subset(sp500_data,sp500_data$symbol==sym))  
  stock.xts<-xts(stock[,c(3:7)],stock$date)             
  stock.xts$momentum1<-diff(stock.xts$close)  
  stock.xts$accel<-diff(stock.xts$momentum1)
  stock.xts$momentum10<-tryCatch({
    stock.xts$momentum10<-momentum(stock.xts$close,n=10)    
  }, warning=function(w) {stock.xts$momentum10<-NA }, error=function(e) {stock.xts$momentum10<-NA})
  stock.xts$momentum20<-tryCatch({
    stock.xts$momentum20<-momentum(stock.xts$close,n=20)    
  }, warning=function(w) {stock.xts$momentum20<-NA }, error=function(e) {stock.xts$momentum20<-NA})
  macd<-tryCatch({
    macd<-MACD(stock.xts$close,maType="EMA")              
  }, warning=function(w) {macd<-NULL }, error=function(e) {macd<-NULL})
  if (is.null(macd)) {
    stock.xts$macdDiff<-NA
  } else stock.xts$macdDiff<-macd[,1]-macd[,2]                    
  stock.xts$sma5<-tryCatch({
    stock.xts$sma5<-SMA(stock.xts$close,n=5)        
  }, warning=function(w) {stock.xts$sma5<-NA }, error=function(e) {stock.xts$sma5<-NA})
  stock.xts$sma10<-tryCatch({
    stock.xts$sma10<-SMA(stock.xts$close,n=10)        
  }, warning=function(w) {stock.xts$sma10<-NA }, error=function(e) {stock.xts$sma10<-NA})
  stock.xts$sma20<-tryCatch({
    stock.xts$sma20<-SMA(stock.xts$close,n=20)        
  }, warning=function(w) {stock.xts$sma20<-NA }, error=function(e) {stock.xts$sma20<-NA})
  stock.xts$sma40<-tryCatch({
    stock.xts$sma40<-SMA(stock.xts$close,n=40)        
  }, warning=function(w) {stock.xts$sma40<-NA }, error=function(e) {stock.xts$sma40<-NA})
  stock.xts$sma60<-tryCatch({
    stock.xts$sma60<-SMA(stock.xts$close,n=60)        
  }, warning=function(w) {stock.xts$sma60<-NA }, error=function(e) {stock.xts$sma60<-NA})
  stock.xts$sma100<-tryCatch({
    stock.xts$sma100<-SMA(stock.xts$close,n=100)        
  }, warning=function(w) {stock.xts$sma100<-NA }, error=function(e) {stock.xts$sma100<-NA})
  stock.xts$sma5.lag1<-stats::lag(stock.xts$sma5,k=1)
  stock.xts$sma10.lag1<-stats::lag(stock.xts$sma10,k=1)
  stock.xts$sma20.lag1<-stats::lag(stock.xts$sma20,k=1)
  stock.xts$sma40.lag1<-stats::lag(stock.xts$sma40,k=1)
  stock.xts$sma60.lag1<-stats::lag(stock.xts$sma60,k=1)
  stock.xts$cross510<-stock.xts$sma5/stock.xts$sma10
  stock.xts$cross520<-stock.xts$sma5/stock.xts$sma20
  stock.xts$cross540<-stock.xts$sma5/stock.xts$sma40
  stock.xts$cross1020<-stock.xts$sma10/stock.xts$sma20
  stock.xts$cross1040<-stock.xts$sma10/stock.xts$sma40
  stock.xts$cross1060<-stock.xts$sma10/stock.xts$sma60  
  stock.xts$cross10100<-stock.xts$sma10/stock.xts$sma100  
  stock.xts$cross2040<-stock.xts$sma20/stock.xts$sma40
  stock.xts$cross2060<-stock.xts$sma20/stock.xts$sma60
  stock.xts$cross20100<-stock.xts$sma20/stock.xts$sma100      
  bb<-tryCatch({
    bb<-BBands(HLC(stock.xts), n = 20, maType="SMA", sd = 2)   # sometimes calls to BBands will crash the system   
  }, warning=function(w) {bb<-NULL }, error=function(e) {bb<-NULL})
  if (is.null(bb)) {                                          # we won't trade if there are no BBands 
    stock.xts$dn<-NA
    stock.xts$mavg<-NA
    stock.xts$up<-NA
    stock.xts$range<-NA
  } else {                                                    # otherwise create the indicators from the bb object
    stock.xts$dn<-bb$dn                                       # note we consider trading in period t based on the 
    stock.xts$mavg<-bb$mavg                                   # the band calculations in period t-1 (hence the lag)
    stock.xts$up<-bb$up
    stock.xts$range<-bb$up-bb$dn                              # keep track of band range/width as a measure of risk
  }
  stock.xts$rsi5<-tryCatch({
    stock.xts$rsi5<-RSI(stock.xts$close,n=5)        
  }, warning=function(w) {stock.xts$rsi5<-NA }, error=function(e) {stock.xts$rsi5<-NA})
  stock.xts$rsi10<-tryCatch({
    stock.xts$rsi10<-RSI(stock.xts$close,n=10)      
  }, warning=function(w) {stock.xts$rsi10<-NA }, error=function(e) {stock.xts$rsi10<-NA})
  stock.xts$rsi20<-tryCatch({
    stock.xts$rsi20<-RSI(stock.xts$close,n=20)        
  }, warning=function(w) {stock.xts$rsi20<-NA }, error=function(e) {stock.xts$rsi20<-NA})
  stock.xts<-na.omit(stock.xts)
  scaled.xts<-NULL
  lengthforscaling<-min(defaultscalinglength,nrow(stock.xts))
  if (lengthforscaling>0) {
    maxs <- apply(stock.xts[c(1:lengthforscaling),], 2, max)
    mins <- apply(stock.xts[c(1:lengthforscaling),], 2, min)
    scaled.xts<-tryCatch({
      scaled.xts<-scale(stock.xts[c(1:lengthforscaling),],center = mins, scale = maxs - mins)        # sometimes calls to TTR functions will crash the system   
    }, warning=function(w) {scaled.xts<-NULL }, error=function(e) {scaled.xts<-NULL})
  }
  if (!is.null(scaled.xts)){ 
    if (lengthforscaling<nrow(stock.xts$close)) {
      startscale<-lengthforscaling+1
      stopscale<-nrow(stock.xts)
      for (i in c(startscale:stopscale)){
        ws<-i-lengthforscaling+1
        maxs <- apply(stock.xts[c(ws:i),], 2, max)
        mins <- apply(stock.xts[c(ws:i),], 2, min)
        temp.xts<-tryCatch({
          temp.xts<-scale(stock.xts[c(ws:i),],center = mins, scale = maxs - mins)        # sometimes calls to TTR functions will crash the system   
        }, warning=function(w) {temp.xts<-NULL }, error=function(e) {temp.xts<-NULL})
        nextrow<-nrow(temp.xts)
        scaled.xts<-rbind(scaled.xts,temp.xts[nextrow,])
      }
    }
    scaled.xts$cross510up<-ifelse(stock.xts$sma5.lag1<stock.xts$sma10.lag1&
                                    stock.xts$sma5>stock.xts$sma10,1,0)
    scaled.xts$cross510dn<-ifelse(stock.xts$sma5.lag1>stock.xts$sma10.lag1&
                                    stock.xts$sma5<stock.xts$sma10,1,0)
    scaled.xts$cross520up<-ifelse(stock.xts$sma5.lag1<stock.xts$sma20.lag1&
                                    stock.xts$sma5>stock.xts$sma20,1,0)
    scaled.xts$cross520dn<-ifelse(stock.xts$sma5.lag1>stock.xts$sma20.lag1&
                                    stock.xts$sma5<stock.xts$sma20,1,0)
    scaled.xts$cross540up<-ifelse(stock.xts$sma5.lag1<stock.xts$sma40.lag1&
                                    stock.xts$sma5>stock.xts$sma40,1,0)
    scaled.xts$cross540dn<-ifelse(stock.xts$sma5.lag1>stock.xts$sma40.lag1&
                                    stock.xts$sma5<stock.xts$sma40,1,0)
    scaled.xts$cross1020up<-ifelse(stock.xts$sma10.lag1<stock.xts$sma20.lag1&
                                     stock.xts$sma10>stock.xts$sma20,1,0)
    scaled.xts$cross1020dn<-ifelse(stock.xts$sma10.lag1>stock.xts$sma20.lag1&
                                     stock.xts$sma10<stock.xts$sma20,1,0)
    scaled.xts$cross1040up<-ifelse(stock.xts$sma10.lag1<stock.xts$sma40.lag1&
                                     stock.xts$sma10>stock.xts$sma40,1,0)
    scaled.xts$cross1040dn<-ifelse(stock.xts$sma10.lag1>stock.xts$sma40.lag1&
                                     stock.xts$sma10<stock.xts$sma40,1,0)
    scaled.xts$cross510<-stock.xts$cross510
    scaled.xts$cross520<-stock.xts$cross520
    scaled.xts$cross540<-stock.xts$cross540
    scaled.xts$cross1020<-stock.xts$cross1020
    scaled.xts$cross1040<-stock.xts$cross1040
    scaled.xts$cross1060<-stock.xts$cross1060
    scaled.xts$cross10100<-stock.xts$cross10100
    scaled.xts$cross2040<-stock.xts$cross2040
    scaled.xts$cross2060<-stock.xts$cross2060
    scaled.xts$cross20100<-stock.xts$cross20100
    scaled.xts$rsi5<-stock.xts$rsi5
    scaled.xts$rsi10<-stock.xts$rsi10
    scaled.xts$rsi20<-stock.xts$rsi20
    scaled.xts$nextreturn<-(lead(as.vector(stock.xts$close),1)-lead(as.vector(stock.xts$open),1))/lead(as.vector(stock.xts$open),1)+1
    scaled.xts$nextopen<-lead(as.vector(stock.xts$open),1)
    scaled.xts$nextclose<-lead(as.vector(stock.xts$close),1)
    stock<-data.frame(scaled.xts)                                # convert back to dataframe object
    date<-as.Date(rownames(stock))                              
    stock<-cbind(sym,date,stock)
    names(stock)[1]<-"symbol"
    temp<-unique(sp500_data[,c(1,8)]) 
    stock<-merge(stock,temp)
    stock$dow<-as.factor(weekdays(stock$date,abbreviate=TRUE))
    for (i in (1:ncol(stock))){
      inflist<-which(is.infinite(stock[,i]))
      if (!is.null(inflist)) {
        for (j in inflist)
          stock[j,i]<-NA
      }
    }
    stock<-na.omit(stock)
    
    rownames(stock)<-seq(1,nrow(stock),1)
  } else stock<-NULL
  return(stock)
}

genPredictions=function(stock){
  results<-NULL
  for (currday in c(trainstart:(trainend))) {
    print(paste("processing day",as.Date(days[currday])))
    from<-days[currday-windowsize]
    to<-days[currday-1]
    train<-subset(stock,stock$date>=from&stock$date<=to)[3:ncol(stock)]
    rf.model=ranger(nextreturn~.-nextopen -nextclose,data=train,num.trees=500)
    rsq<-round(mean(rf.model$r.squared),3)
    print(paste("RSQ:",rsq))
    preds<-subset(stock,stock$date==as.Date(days[currday]))    
    preds$rsq<-rep(rsq,nrow(preds))
    preds$prediction<-predict(rf.model,preds)$predictions
    if (is.null(results)){
      results<-preds} else results<-rbind(results,preds)
  }
  return(results)
}


genSignals=function(stock){
  stock$buy<-NA
  stock$sell<-NA
  stock$short<-ifelse(stock$prediction<shortthreshold,1,0)
  stock$long<-ifelse(stock$prediction>longthreshold,1,0)
  stock$buy<-ifelse(stock$long==1,stock$nextopen,
                    ifelse(stock$short==1,stock$nextclose,NA))
  stock$sell<-ifelse(stock$long==1,stock$nextclose,
                     ifelse(stock$short==1,stock$nextopen,NA))
  return(list(stock))
}


closePositions=function(day,equity,position){  # Define a function to close positions
  cash<-0  # Initialize cash to 0
  closed<-NULL  # Initialize closed positions to NULL
  if (!is.null(position)) {  # Check if there are any positions
    longposition<-subset(position,type=="Long")  # Subset long positions
    shortposition<-subset(position,type=="Short")  # Subset short positions
    candidates<-subset(signals,signals$date==day&  # Check for short exit signals
                         (signals$short.exit==1))[,c(1,2,6)]  # Extract symbol, date, and price
    names(candidates)[2]<-"closedate"  # Rename date column to closedate
    names(candidates)[3]<-"outprice"  # Rename price column to outprice
    closeshort<-merge(shortposition,candidates,by="symbol")  # Close short positions
    candidates<-subset(signals,signals$date==day&  # Check for long exit signals
                         (signals$long.exit==1))[,c(1,2,6)]    
    names(candidates)[2]<-"closedate"  # Rename date column to closedate
    names(candidates)[3]<-"outprice"  # Rename price column to outprice
    closelong<-merge(longposition,candidates,by="symbol")  # Close long positions
    closed<-rbind(closeshort,closelong)  # Combine closed short and long positions
    if (nrow(closed)>0) {  # Check if there are any closed positions
      closed$closecash<-closed$outprice*closed$position  # Calculate closing cash
      closed$sellprice<-ifelse(closed$type=="Long",closed$outprice,closed$sellprice)  # Update sell price
      closed$buyprice<-ifelse(closed$type=="Short",closed$outprice,closed$buyprice)  # Update buy price
      closed$profit<-(closed$sellprice-closed$buyprice)*abs(closed$position)  # Calculate profit
      cash<-sum(closed$closecash) - nrow(closed)*1 + 0.005*closed$position  # Calculate aggregate cash including transaction cost
    } else closed<-NULL  # If no closed positions, set closed to NULL
  }
  return(list(closed=closed,cashin=cash))  # Return closed positions and cash
}


openPositions=function(day,equity,position){  # Define a function to open positions
  cash=0  # Initialize cash to 0
  transcost<-0  # Initialize transaction cost to 0
  opened<-NULL  # Initialize opened positions to NULL
  if (!is.null(position)) {  # Check if there are any existing positions
    longposition<-subset(position,type=="Long")[,c(1,2)]  # Subset long positions
    names(longposition)[2]<-"dummy"  # Rename column to dummy
    shortposition<-subset(position,type=="Short")[,c(1,2)]  # Subset short positions
    names(shortposition)[2]<-"dummy"  # Rename column to dummy
    candidates<-subset(signals,signals$date==day&  # Check for short entry signals
                         signals$short.entry==1)
    temp<-merge(candidates,shortposition,by="symbol",all.x=TRUE)  # Merge candidates with short positions
    openshort<-subset(temp,is.na(dummy))  # Check for short positions to open
    if (nrow(openshort)>0) {  # Check if there are any short positions to open
      openshort<-openshort[,c(1:ncol(openshort)-1)]  # Remove dummy column
      openshort$type<-"Short"  # Set type to Short
    } else {openshort<-NULL}  # If no short positions, set to NULL
    candidates<-subset(signals,signals$date==day&  # Check for long entry signals
                         signals$long.entry==1)
    temp<-merge(candidates,longposition,by="symbol",all.x=TRUE)  # Merge candidates with long positions
    openlong<-subset(temp,is.na(dummy))  # Check for long positions to open
    if (nrow(openlong)>0) {
      openlong<-openlong[,c(1:ncol(openlong)-1)]  # Remove dummy column
      openlong$type<-"Long"  # Set type to Long
    } else {openlong<-NULL}  # If no long positions, set to NULL
    opened<-rbind(openlong,openshort)  # Combine short and long positions
    if (!is.null(opened)) {  # Check if opened positions is not NULL
      if (nrow(opened)==0) opened<-NULL  # If no rows, set to NULL
    }
  } else {
    opened<-subset(signals,signals$date==day&  # If no positions, check for all signals
                     (signals$short.entry==1|signals$long.entry==1)) 
    if (nrow(opened)==0) {opened<-NULL} else {  # If no signals, set to NULL
      opened$type<-ifelse(opened$short.entry==1,  # Set type based on entry signal
                          "Short","Long")}
  }
  if (!is.null(opened)) {  # Check if opened positions is not NULL
    opened$buyprice<-ifelse(opened$type=="Long",opened$open,NA)  # Set buy price
    opened$sellprice<-ifelse(opened$type=="Short",opened$open,NA)  # Set sell price
    opened<-opened[order(opened$range),]  # Sort by range
    # Separate long and short trades
    long_trades <- subset(opened, type == "Long")
    short_trades <- subset(opened, type == "Short")
    # Apply maxlong and maxshort constraints
    if (nrow(long_trades) > maxLong) {
      long_trades <- long_trades[1:maxLong,]
    }
    if (nrow(short_trades) > maxShort) {
      short_trades <- short_trades[1:maxShort,]
    }
    # Re-combine after applying constraints
    opened <- rbind(long_trades, short_trades)
    numtrades <- nrow(opened)  # Count the number of trades
    
    if(numtrades > 0){  # If there are trades
      softmax_scores <- softmax(1/opened$range)  # Normalize trade amounts based on risk
      tradeamounts <- softmax_scores * equity  # Calculate trade amounts
      tradeamounts <- pmin(tradeamounts, maxTrade)  # Limit trade amounts to maxTrade
      opened$position<-ifelse(opened$type=="Long",  # Set position size
                              trunc(tradeamounts/opened$open),    # Round down to nearest whole number
                              -trunc(tradeamounts/opened$open))   # Negative position for shorts
      opened$opencash<-ifelse(opened$type=="Long",  # Update cash position
                              opened$buyprice*opened$position,0)
      opened$opencash<-ifelse(opened$type=="Short",
                              opened$sellprice*opened$position,opened$opencash)
      opened<-subset(opened,opened$position!=0)  # Remove rows with zero position
      cash<-sum(opened$opencash) - (numtrades*1 + 0.005*opened$position)  # Calculate total cash
    } else {opened<-NULL}  # If no trades, set opened to NULL
  } 
  return(list(opened=opened,cashout=cash))  
}

# Function to apply trading rules based on signals and positions
applyRules=function(currdate,equity,position){
  netopen<-position                                        # netopen will hold all open positions after any close orders
  close.results<-closePositions(currdate,equity,position)  # close any orders for which we have positions and signals
  if (!is.null(close.results$closed)) {                    # Did we actually close out any positions
    temp<-close.results$close[,c(1,2)]                     # if we we need to remove them from our open positions
    names(temp)[2]<-"dummy"                                # we need one field to check if it is empty after the merge
    temp<-merge(position,temp,by="symbol",all.x=TRUE)      # and we don't want to generate duplicate columns, hence dummy
    netopen<-subset(temp,is.na(temp$dummy))                # so if dummy is NA, then the position is not closed
    netopen<-netopen[,c(1:ncol(netopen)-1)]                # get rid of the dummy column
    equity<-equity+close.results$cashin                    # update our equity position with the cash from closing
  }
  open.results<-openPositions(currdate,equity,netopen)     # now check for opening new positions
  return(list(open=open.results$opened,close=close.results$closed,
              posnetofcloses=netopen,cashin=close.results$cash,cashout=open.results$cash))
}


# Function to calculate portfolio statistics
portfolioStats <- function(trades, pvalue, tdays) {
  tradedays <- length(unique(trades$date))  # Count unique trading days
  totaldays <- length(tdays)  # Count total days in analysis
  pctdaystraded <- tradedays / totaldays  # Calculate % of days with trades
  totaltrades <- nrow(trades)  # Count total trades
  pdiff <- c(0, diff(pvalue))  # Calculate daily price difference
  preturn <- pdiff/pvalue + 1  # Calculate daily return on each day
  shorttrades <- sum(trades$type == "Short")  # Count short trades
  longtrades <- sum(trades$type == "Long")  # Count long trades
  # Calculate returns for each trade
  trades$return_long <- ifelse(trades$type == "Long", (trades$outprice / trades$buyprice) - 1, NA)  # Long returns
  trades$return_short <- ifelse(trades$type == "Short", (trades$sellprice / trades$outprice) - 1, NA)  # Short returns
  trades$return <- ifelse(trades$type == "Long", trades$return_long, trades$return_short)  # Overall returns
  # Calculate % of winning trades
  pct_winning_long <- sum(trades$return_long > 0, na.rm = TRUE) / longtrades  # % of winning long trades
  pct_winning_short <- sum(trades$return_short > 0, na.rm = TRUE) / shorttrades  # % of winning short trades
  # Calculate average returns
  avg_return_long <- mean(trades$return_long, na.rm = TRUE)  # Average long return
  avg_return_short <- mean(trades$return_short, na.rm = TRUE)  # Average short return
  # Calculate overall % of winning trades
  overall_pct_winning <- sum(trades$return > 0, na.rm = TRUE) / totaltrades  # Overall % of winning trades
  cumreturn <- cumprod(preturn)  # Cumulative return
  maxreturn <- cummax(cumreturn)  # Maximum return
  maxdraw <- min((cumreturn - maxreturn) / maxreturn) * 100  # Maximum drawdown %
  drawdown_lengths <- rle((cumreturn - maxreturn) / maxreturn < 0)  # Drawdown period lengths
  maxstreak <- max(drawdown_lengths$lengths[drawdown_lengths$values])  # Longest drawdown period
  meanreturn <- mean(preturn, na.rm = TRUE) - 1  # Mean daily return
  sharpe <- meanreturn / sd(preturn, na.rm = TRUE) * sqrt(252)  # Sharpe ratio
  maxy <- max(c(max(cumreturn, na.rm = TRUE), max(maxreturn, na.rm = TRUE), max(preturn, na.rm = TRUE)))  # Max value for y-axis
  # Calculate average number of open positions per day
  avg_open_positions <- mean(table(trades$date))  # Average open positions
  # Calculate average duration for which a position is open
  if ("closedate" %in% names(trades) && "date" %in% names(trades)) {
    trades$duration <- as.numeric(as.Date(trades$closedate) - as.Date(trades$date))  # Calculate duration
    avg_open_duration <- mean(trades$duration, na.rm = TRUE)  # Average open duration
  } else {
    avg_open_duration <- NA  # Set to NA if necessary data is not available
  }
  # Plot the portfolio results
  max_data_value <- max(cumreturn, maxreturn, preturn, na.rm = TRUE)  # Get the maximum data value
  ylim_upper <- max_data_value * 1.02  # Extend the limit by 5% above the maximum value
  plot(tdays, cumreturn, type = "l", col = "black", lwd = 2,
       xlab = "Time Period", ylab = "Portfolio Return",
       main = "Portfolio Results", ylim = c(0.95, ylim_upper),
       xaxt = "n", cex.lab = 0.75, cex.axis = 0.75, cex.main = 0.85)
  # Add lines for maximum return and portfolio return
  lines(tdays, maxreturn, col = "red", lwd = 2)
  lines(tdays, preturn, col = "blue", lwd = 2)
  # Customize the x-axis with date formatting
  axis.Date(1, at = seq(min(tdays), max(tdays), by = "3 months"),
            format = "%b %Y", cex.axis = 0.8)
  # Add gridlines for better readability
  abline(h = seq(0.5, ylim_upper, by = 0.1), col = "lightgray", lty = "dotted")
  abline(v = seq(min(tdays), max(tdays), by = "3 months"), col = "lightgray", lty = "dotted")
  # Add a legend to the plot
  legend("topleft", legend = c("Cumulative Return", "Max Return", "Portfolio Return"),
         col = c("black", "red", "blue"), lwd = 2, bty = "n", cex = 0.75)
  # Compile performance measures into a list
  performance <- list(
    totaltrades = totaltrades,
    longtrades = longtrades,
    pct_winning_long = pct_winning_long * 100,
    avg_return_long = avg_return_long * 100,
    shorttrades = shorttrades,
    pct_winning_short = pct_winning_short * 100,
    avg_return_short = avg_return_short * 100,
    overall_pct_winning = overall_pct_winning * 100,
    cumreturn = cumreturn[length(cumreturn)],
    meanreturn = meanreturn,
    sharpe = sharpe,
    maxdraw = maxdraw,
    maxdraw_period = maxstreak,
    avg_open_positions = avg_open_positions,
    avg_open_duration = avg_open_duration
  )
  return(performance) 
}


# ********************************  RUN STRATEGY **********************************
sym<-"A"
indicators<-NULL                               # we will put all OHLC data and our generated
for (sym in sp500_symbols) {                         # indicaters into a dataframe named "indicators"
  temp<-genIndicators(sym)                     # by looping through all the symbols in our
  if (!is.null(temp)) {                        # restricted universe. Need to ensure we have indicators  
    if (is.null(indicators)) {                   
      indicators<-temp} else
        indicators<-rbind(indicators,temp)
  }
}

predictions<-data.frame(genPredictions(indicators))
