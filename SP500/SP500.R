library(quantmod)
library(BatchGetSymbols)
library(data.table)
library(ggplot2)
library(dplyr)
library(readr)
library(directlabels)
library(wesanderson)



first.date <- as.Date("2019-12-31")
last.date <- Sys.Date()
vday.date <- as.Date("2020-02-14")


###################### Use Quantmod to get stock Prices ########################

df.SP500 <- data.table(GetSP500Stocks())

getSymbols("^GSPC", from=first.date)

SP500 = data.table("Dt" = index(GSPC), 
                   "Price" = as.vector(GSPC[,4]), 
                   "Comp"= "SP500")
SP500$Dt = as.Date(SP500$Dt)

Tic <- new.env()

for(i in 82:nrow(df.SP500)){
        getSymbols(Symbols = df.SP500$Tickers[i], env = Tic, auto.assign = TRUE, from=first.date, src = "yahoo")
        Temp = Tic[[df.SP500$Tickers[i]]]
        SP500 = rbind(SP500,
                      data.table("Dt" = index(Temp), 
                                 "Price" = as.vector(Temp[,4]),
                                 "Comp" = df.SP500$Tickers[i])
                      )
        rm(Temp)
     }
# length(unique(SP500$Comp))
# nrow(SP500) / length(unique(SP500$Comp))
# length(Tic)

############ Load SP500 csv file #####################################

setwd("C:/Users/mohit/OneDrive - University of California/R/Corona")
SP500 = data.table(read_csv("SP500.csv"))
SP500$Dt = as.Date(SP500$Dt, format = "%d-%b-%y")
str(SP500)

SP500 = SP500[!is.na(Price),]
SP500[, Price.Adj := Price/first(Price)*100, by=TICKER]
# SP500[, Wt := 0]
# SP500[Comp=="SP500", Wt := 1]

SP500.summary = SP500.Merge %>% group_by(GICS.Sector, TICKER) %>% 
        summarise(Price.Adj.max = max(Price.Adj[Dt <= vday.date]),
                  Price.Adj.min = min(Price.Adj[Dt >= vday.date]),
                  Price.Adj.end = last(Price.Adj),
                  Waterfall = 100-Price.Adj.min/Price.Adj.max*100
                  )

SP500.summary = data.table(SP500.summary)
# SP500.summary[is.na(SP500$Waterfall),]
# SP500.summary = data.table(SP500.summary[!is.na(SP500.summary$Waterfall),])
sum(SP500.summary$Waterfall<=50)
sum(SP500.summary$Price.Adj.min<=50)
sum(SP500.summary$Price.Adj.end<=50)
median(SP500.summary$Waterfall)

# write.csv(SP500, "SP500.csv", row.names = F)

##################### All Stocks with MCap #######################

Stocks.All = data.table(read_csv("Stocks.csv"))
Stocks.All = Stocks.All[which(SHRCD %in% c(10,11)) & which(EXCHCD %in% c(1,2,3)),]
Stocks.All[, Mcap := abs(PRC)*SHROUT/10^6]
Stocks.All = Stocks.All[order(Mcap, decreasing = TRUE),]

#################### Merge All Stocks with SP 500 ###################

Stocks.All = merge(Stocks.All[,c("TICKER","Mcap")], df.SP500[,c("Tickers", "Company", "GICS.Sector")],  by.x="TICKER", by.y = "Tickers")
SP500.Merge = merge(SP500, Stocks.All, by = "TICKER")

SP500.100B = SP500.Merge[Mcap >= 100,]
# SP500.Top200 = SP500.Top200[order(Mcap, decreasing = TRUE)]


############# Graphs #############################

CBP <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00","#000000", "#CC79A7", "#FF00FF","#003366")
col1 = colours()[c(1:11)*50]
length(CBP)

# Stock Price Moves - SP500 
ggplot(SP500.Merge, aes(x=Dt, y=Price.Adj, group=TICKER, colour=GICS.Sector)) + geom_line() + geom_vline(xintercept = vday.date, colour = "red") + theme(legend.position = "bottom", legend.box = "horizontal") + xlab("") + ggtitle("Performance of S&P500 Stocks", subtitle = "From 31st Dec till date") + geom_dl(data=subset(SP500.Merge, (Dt==max(Dt) & ( Mcap >300))), aes(label=TICKER, colour=GICS.Sector), method = list("last.points"))  + scale_colour_manual(values = CBP)
# + geom_text(data=subset(SP500.Merge, (Dt==max(Dt) & ( Mcap >300))), aes(label=TICKER), hjust=0) +  expand_limits(x = max(SP500$Dt) + 0.03)



# Stock Price Moves - SP500 Above 100 B
ggplot(SP500.100B, aes(x=Dt, y=Price.Adj, group=Comp, colour=Comp)) + geom_line() + geom_vline(xintercept = vday.date, colour = "red") + theme(legend.position = "none") + xlab("") + ggtitle("Performance of Stocks with Market Cap above 100B", subtitle = "From 31st Dec till date") + geom_dl(aes(label=Comp), method="last.points") + geom_dl(aes(label=Comp, colour=Comp), method = list("last.points"))

ggplot(SP500.100B, aes(x=Dt, y=Price.Adj, group=TICKER, colour=GICS.Sector)) + geom_line() + geom_vline(xintercept = vday.date, colour = "red") + theme(legend.position = "bottom") + xlab("") + ggtitle("Performance of Stocks with MCap above 100B", subtitle = "From 31st Dec till date") + geom_label(data=subset(SP500.100B, (Dt==max(Dt) & ( Mcap >300))), aes(label=TICKER), hjust=0) +  expand_limits(x = max(SP500$Dt) + 0.03)

# Stock Price Moves - Top200
ggplot(SP500.Top200) + geom_line(aes(x=Dt, y=Price.Adj, group=Comp, colour=Comp)) + theme(legend.position = "none") + geom_vline(xintercept = vday.date, colour = "red") + xlab("")
# + gghighlight::gghighlight(Price.Adj[Dt==last.date] > 120 )
# + geom_label(data = subset(SP500.Top200, Dt==last.date), aes(label=Comp, colour=Comp))

# Histogram of Summary
ggplot(SP500.summary, aes(x=Waterfall, colour=GICS.Sector)) + geom_histogram(fill="white", alpha=0.5, position = "dodge") + geom_vline(aes(xintercept=median(Waterfall)),color="blue", linetype="dashed", size=1) + scale_colour_manual(values = CBP) + xlab("")  + ggtitle("Histogram of max Fall (YTD) in Stock Prices") + theme(legend.position = "bottom", legend.box = "horizontal")
