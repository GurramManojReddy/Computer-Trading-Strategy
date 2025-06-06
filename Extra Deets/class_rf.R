# ****************************************************************************************
# 
# RANDOM FOREST STRATEGY -> for a set of equities, over a trading period, long stocks
# that exceed a certain predicted return threshold and short those that fall below
# a certain predicted return threshold.  This is a daily strategy, exiting at close
#
# **************SET WORKING DIRECTORY AND CLEAR ENVIRONMENT ******************************
library(rstudioapi)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
options(scipen=999)
cat("\014")

# ***************GET DATA AND SET TRADING DATE RANGE **********************************

library(quantmod)
library(dplyr)
library(robustHD)
library(ranger)
load("universe.rdata")
days<-unique(universe$date)
days<-days[order(days)]
yearsBacktest<-1
windowsize<-25                       # rolling training days for random forest
longestindicator<-100
teststart<-as.Date("2015-01-02")
datastart<-which(days==teststart)-windowsize-longestindicator
dataend<-which(days==teststart)+yearsBacktest*252+1
trainstart<-datastart+windowsize+longestindicator
trainend<-dataend-1
universe<-subset(universe,universe$date>=days[datastart]&universe$date<=days[dataend])
symbols<-unique(universe$symbol)
numsymbols<-length(symbols)
initialequity<-25000                 # starting money
maxtradepct<-0.10                    # maximum value of any single trade
maxdaytrades<-10                     # maximum trades in one day
defaultscalinglength<-10000
longthreshold<-1.015
shortthreshold<-0.985


# ************************************** GENERATE INDICATORS **********************************************************
sym<-"A"
genIndicators=function(sym){
  print(paste('Generating Indicators for symbol: ',sym))
  stock<-unique(subset(universe,universe$symbol==sym))  
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
    temp<-unique(universe[,c(1,8)]) 
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

applyRules=function(day,equity){
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

# ********************************  RUN STRATEGY **********************************
sym<-"A"
indicators<-NULL                               # we will put all OHLC data and our generated
for (sym in symbols) {                         # indicaters into a dataframe named "indicators"
  temp<-genIndicators(sym)                     # by looping through all the symbols in our
  if (!is.null(temp)) {                        # restricted universe. Need to ensure we have indicators  
    if (is.null(indicators)) {                   
      indicators<-temp} else
        indicators<-rbind(indicators,temp)
  }
}

predictions<-data.frame(genPredictions(indicators))

signals<-data.frame(genSignals(predictions))
signals<-na.omit(signals)

tdays<-unique(signals$date)                    # Now process (apply rules) for each trading day in
closed<-NULL                                   # "closed" positions as we proceed.  
pvalue<-rep(initialequity,length(tdays))       # Each day we will keep track of our portfolio value
currentcash<-initialequity                     # that includes current cash, plus our investments.
startday<-1 
stopday<-length(tdays) 
for (day in startday:stopday) {                 # start of our loop
  currdate<-tdays[day]
  print(currdate)                              # simple update to screen on our progress
  results<-applyRules(currdate,currentcash)    # our state variables are the date and cash available
  currentcash<-currentcash+results$cashin-results$cashout  # update our cash position at end of day
  pvalue[day]<-currentcash
  if (!is.null(closed)) {
    closed<-rbind(closed,results$long,results$short)
  } else closed<-rbind(results$long,results$short)
}  

performance<-portfolioStats(closed,pvalue[startday:stopday],tdays[startday:stopday]) ; performance

