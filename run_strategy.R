initialequity<-25000                 # starting money
maxtradepct<-0.10                    # maximum value of any single trade
maxdaytrades<-10                     # maximum trades in one day

sentiments <- read.csv("sentiments.csv")

genTrades=function(candidates,equity){
  cashin<-0
  cashout<-0
  transcost<-0
  numtrades<-0
  maxtrade<-maxtradepct*equity
  if (nrow(candidates)>0) {  # there are potential candidates 1 or more
    candidates<-candidates[order(-candidates$prediction),]  # sort them by decreasing predicted returns
    numtrades<-nrow(candidates)  
    if (numtrades>maxdaytrades) {                     # make sure we don't exceed the # of trades allowed
      candidates<-candidates[c(1:maxdaytrades),]
      numtrades<-maxdaytrades
    }
    tradeamount<-max(min(maxtrade,equity/numtrades),0) # invest equally, but not more than we have
    cashout<-0
    # now figure out how much cash we have used to invest in our new positions
    if (numtrades>0) {
      candidates$position<-NA
      for (i in 1:numtrades){
        candidates$position[i]<-trunc(tradeamount/candidates$buy[i])
        cashout<-cashout+candidates$position[i]*candidates$buy[i]
        cashin<-cashin+candidates$position[i]*candidates$sell[i]
      }
      candidates<-subset(candidates,candidates$position>0)
    }
  } else candidates<-NULL
  transcost<-2*numtrades+0.01*sum(candidates$position)
  return(list(trades=candidates,cashin=cashin,cashout=cashout,transcost=transcost))  
}

# **************************************** APPLY RULES ********************************************************************

applyRules <- function(day, equity) {
  # Load sentiment data if it hasn't been loaded yet
  if (!exists("sentiment_data")) {
    sentiment_data <- read.csv("sentiments.csv")
  }
  
  cashin <- 0
  cashout <- 0
  transcost <- 0
  
  # Get long candidates for the day
  candidates <- subset(signals, signals$date == day & signals$long == 1)
  
  # Filter candidates based on sentiment (only those with sentiment >= 0)
  if (nrow(candidates) > 0) {
    candidates <- candidates %>%
      inner_join(sentiment_data, by = "symbol") %>%
      filter(sentiment >= 0)
  }
  
  # Generate trades for long positions
  longs <- genTrades(candidates, equity)
  equity <- equity + longs$cashin - longs$cashout - longs$transcost
  
  # Get short candidates for the day
  candidates <- subset(signals, signals$date == day & signals$short == 1)
  candidates$prediction <- 2 - candidates$prediction
  
  # Filter short candidates based on sentiment
  if (nrow(candidates) > 0) {
    candidates <- candidates %>%
      inner_join(sentiment_data, by = "symbol") %>%
      filter(sentiment >= 0)
  }
  
  # Generate trades for short positions
  shorts <- genTrades(candidates, equity)
  
  cashin = longs$cashin + shorts$cashin
  cashout = longs$cashout + shorts$cashout + longs$transcost + shorts$transcost
  transcost = longs$transcost + shorts$transcost
  
  return(list(long = longs$trades, 
              short = shorts$trades, 
              cashin = cashin, 
              cashout = cashout, 
              transcost = transcost))
}

applyRules_without_sentiment=function(day,equity){
  cashin<-0
  cashout<-0
  transcost<-0
  candidates<-subset(signals,signals$date==day&signals$long==1)
  longs<-genTrades(candidates,equity)
  equity<-equity+longs$cashin-longs$cashout-longs$transcost
  candidates<-subset(signals,signals$date==day&signals$short==1)
  candidates$prediction<-2-candidates$prediction
  shorts<-genTrades(candidates,equity)
  cashin=longs$cashin+shorts$cashin
  cashout=longs$cashout+shorts$cashout+longs$transcost+shorts$transcost
  transcost=longs$transcost+shorts$transcost
  return(list(long=longs$trades,short=shorts$trades,cashin=cashin,cashout=cashout,transcost=transcost))
}


# ****************************** CALCULATE PORTFOLIO STATISTICS ************************************************
# Calculate various portfolio statistics such as period returns, cumulative returns, number of trades,
# max drawdown period, max drawdown percent, and annualized sharpe ratio.  Much more can be done
# here.

portfolioStats=function(trades,pvalue,tdays){
  trades$actreturn<-(trades$sell-trades$buy)/trades$buy+1
  avgwin<-mean(subset(trades,trades$sell>trades$buy)$actreturn)
  avgloss<-mean(subset(trades,trades$sell<trades$buy)$actreturn)   
  tradedays<-length(unique(trades$date))
  transcost<-2*nrow(trades)+0.01*sum(trades$position,na.rm=TRUE)
  totaldays<-length(tdays)
  pctdaystraded<-tradedays/totaldays
  totaltrades<-nrow(trades)  
  pdiff<-c(diff(pvalue),0)  
  preturn<-pdiff/pvalue+1
  shorts<-subset(trades,trades$short==1)
  shorttrades<-nrow(shorts)
  pctshortwins=nrow(subset(shorts,shorts$actreturn>1))/shorttrades
  avgshortreturn=mean(shorts$actreturn)
  shorttrades<-nrow(subset(trades,trades$short==1))
  longs<-subset(trades,trades$long==1)
  longtrades<-nrow(longs)
  pctlongwins=nrow(subset(longs,longs$actreturn>1))/longtrades
  avglongreturn=mean(longs$actreturn)
  cumreturn<-rep(1,length(totaldays))
  maxvalue<-cumreturn
  maxreturn<-cumreturn
  for (i in c(1:totaldays)){
    cumreturn[i]<-prod(preturn[c(1:i)],na.rm=TRUE)
    maxreturn[i]<-max(cumreturn[c(1:i)],na.rm=TRUE)
    maxvalue[i]<-max(pvalue[c(1:i)],na.rm=TRUE)
  }
  down<-pvalue-maxvalue
  downpct<-(pvalue-maxvalue)/maxvalue
  streak<-0
  maxstreak<-0
  for (i in c(1:totaldays)){
    streak<-ifelse(down[i]<0,streak+1,0)
    maxstreak<-ifelse(streak>maxstreak,streak,maxstreak)
  }
  maxy<-max(cumreturn+0.2)
  miny<-min(cumreturn-0.01)
  plot(tdays,cumreturn,type="l",col="black",lwd=2,xlab="Time Period",ylim=c(miny,maxy),ylab="Portfolio Return",main="Portfolio Results")
  lines(tdays,maxreturn,co=2,lw=2)
  lines(tdays,preturn,co=4,lw=2)
  cumreturn<-cumreturn[totaldays]
  avgreturn<-mean(preturn,na.rm=TRUE)
  sreturn<-preturn-1
  sharpe<-mean(sreturn,na.rm=TRUE)/sd(sreturn,na.rm=TRUE)*sqrt(252)
  maxdraw<-min(down)
  maxdrawpct<-min(downpct)*100
  performance<-list(totaltrades=totaltrades,avgwin=avgwin,avgloss=avgloss,longtrades=longtrades,pctlongwins=pctlongwins,shorttrades=shorttrades,pctshortwins=pctshortwins,cumreturn=cumreturn,
                    avgportfolioreturn=avgreturn,avglongreturn=avglongreturn,avgshortreturn=avgshortreturn,sharpe=sharpe,maxdraw=maxdraw,
                    maxdrawpct=maxdrawpct,drawlength=maxstreak,transcost=transcost)
  return(performance)
}



load("signals.RData")
signals<-na.omit(predictions)

tdays<-unique(signals$date)                    # Now process (apply rules) for each trading day in
closed<-NULL                                   # "closed" positions as we proceed.  
pvalue<-rep(initialequity,length(tdays))       # Each day we will keep track of our portfolio value
currentcash<-initialequity                     # that includes current cash, plus our investments.
startday<-1 
stopday<-length(tdays) 
for (day in startday:stopday) {                 # start of our loop
  currdate<-tdays[day]
  print(currdate)                              # simple update to screen on our progress
  results<-applyRules_without_sentiment(currdate,currentcash)    # our state variables are the date and cash available
  currentcash<-currentcash+results$cashin-results$cashout  # update our cash position at end of day
  pvalue[day]<-currentcash
  if (!is.null(closed)) {
    closed<-rbind(closed,results$long,results$short)
  } else closed<-rbind(results$long,results$short)
}  

performance<-portfolioStats(closed,pvalue[startday:stopday],tdays[startday:stopday]) ; performance

