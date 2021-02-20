library(quantmod)
library(BatchGetSymbols)
library(data.table)
library(ggplot2)
library(dplyr)
library(readr)
library(directlabels)
library(wesanderson)
library(ggrepel)

setwd('C:/Users/mohit/OneDrive - University of California/R/SP500')

first.date <- as.Date("2019-12-31")
last.date <- as.Date("2020-06-08")
top.date <- as.Date("2020-02-14")
bottom.date <- as.Date("2020-03-23")

###################### Use Quantmod, BatchGetSymbols to get stock Prices ########################

df.SP500 <- data.table(GetSP500Stocks())
Stocks <- read_csv('Stocks.csv')

# getSymbols('^GSPC', from=first.date, to=last.date+1)

SP = BatchGetSymbols(tickers=df.SP500$Tickers, first.date = first.date, last.date = last.date+1)
# SP1 = data.table(SP[[1]])
SP2 = data.table(SP[[2]])

############ Merge all data tables ##############

SPX = merge(Stocks[,c("TICKER","SHROUT")], SP2[,c("price.close", "ref.date", "ticker")],  by.x='TICKER', by.y = 'ticker', all.y=TRUE)
SPX = merge(SPX, df.SP500[,c("Tickers","Company","GICS.Sector")], by.x='TICKER', by.y='Tickers', all.x = TRUE)
SPX = data.table(SPX)

# View(SPX[TICKER=='^GSPC'])
# SPX[TICKER=='^GSPC', Company := 'SP500']
# SPX[TICKER=='^GSPC', GICS.Sector := 'SP500']
# SPX[TICKER=='^GSPC', SHROUT := 10^6]

colnames(SPX) = c("TICKER","SHROUT","Price","Dt","Company","GICS.Sector")
SPX = SPX[!is.na(Price),]
SPX[, Price.Adj := Price/(Price[Dt == first.date])*100, by=TICKER]
SPX[, Price.Adj_min := Price/(Price[Dt == bottom.date])*100, by=TICKER]
SPX[, MCap := Price*SHROUT/10^6]
SPX = SPX[order(TICKER, Dt),]

###################### SPX Sector - Weighted Avg ###############

SPX_S = summarise(group_by(SPX, GICS.Sector, Dt),
                  Price_Sec = mean(Price.Adj),
                  Price_Sec_min = mean(Price.Adj_min)
                  # Price_Sec = sum(Price.Adj*MCap, na.rm=T)/sum(MCap, na.rm=T),
                  # Price_Sec_min = sum(Price.Adj_min*MCap, na.rm=T)/sum(MCap, na.rm=T)
                  )
# SPX_S = rbind(SPX_S, SP500[,-'Price'])

CBP <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00","#000000", "#CC79A7", "#FF00FF","#003366", "indianred4")


############# Graphs ######################

P1 = ggplot(SPX, aes(x=Dt, y=Price.Adj, group=TICKER, colour=GICS.Sector)) + 
     geom_line() + 
     scale_colour_manual(values = CBP) +
     theme(legend.position="bottom") + 
  geom_text_repel(data=subset(SPX[order(Price.Adj, decreasing = T),], Dt==last.date)[1:10], aes(label=TICKER), hjust=0, show.legend = F) +
     xlab('') + ylab('') + ggtitle('Adjusted Price Movement (YTD) of all S&P500 Stocks', subtitle = 'All Stock Prices at 100 on 31-Dec-2019') +
  guides(colour=guide_legend(nrow=2,byrow=TRUE)) + 
  geom_vline(xintercept = c(top.date,bottom.date), colour = c("blue",'red'), linetype='dashed') +
  geom_hline(yintercept = 100, linetype='dashed')

SPX_end = subset(SPX[order(Price.Adj_min, decreasing = T),], Dt==last.date)[1:10]

P2 = ggplot(SPX, aes(x=Dt, y=Price.Adj_min, group=TICKER, colour=GICS.Sector)) + 
  geom_line() + 
  scale_colour_manual(values = CBP) + 
  theme(legend.position="bottom") + 
  geom_text_repel(data=SPX_end, aes(label=TICKER), hjust=0, show.legend = F) +
  geom_text_repel(data=subset(SPX, Dt==first.date & (TICKER %in% SPX_end$TICKER)), aes(label=TICKER), hjust=0, show.legend = F) +
  xlab('') + ylab('') + ggtitle('Adjusted Price Movement (YTD) of all S&P500 Stocks', subtitle = 'All Stock Prices at 100 on 23-Mar-2020') +
  guides(colour=guide_legend(nrow=2,byrow=TRUE)) + 
  geom_vline(xintercept = c(top.date,bottom.date), colour = c("blue",'red'), linetype='dashed') +
  geom_hline(yintercept = 100, linetype='dashed')

## Sectoral Performance ##
P3 = ggplot(SPX_S, aes(x=Dt, y=Price_Sec, group=GICS.Sector, colour=GICS.Sector)) + 
        geom_line() +
        scale_colour_manual(values = CBP) + 
        theme(legend.position="none") + 
        xlab('') + ylab('') + ggtitle('Adjusted Price Movement (YTD) - Sectoral', subtitle = 'All Stock Prices at 100 on 31-Dec-2019') +
        geom_label_repel(data=subset(SPX_S, Dt==last.date), aes(label=GICS.Sector), hjust=0) +
  geom_vline(xintercept = c(top.date,bottom.date), colour = c("blue",'red'), linetype='dashed') +
  geom_hline(yintercept = 100, linetype='dashed')
  # geom_line(data=GSPC, aes(x=index(GSPC), y=GSPC.Close, linetype='dashed', size=0.5))

P4 = ggplot(SPX_S, aes(x=Dt, y=Price_Sec_min, group=GICS.Sector, colour=GICS.Sector)) +
     geom_line() + 
     scale_colour_manual(values = CBP) + 
     theme(legend.position="none") + 
     xlab('') + ylab('') + ggtitle('Adjusted Price Movement (YTD) - Sectoral', subtitle = 'All Stock Prices at 100 on 23-Mar-2020') +
     geom_label_repel(data=subset(SPX_S, Dt==first.date), aes(label=GICS.Sector), hjust=0) +
     geom_label_repel(data=subset(SPX_S, Dt==last.date), aes(label=GICS.Sector), hjust=0) +
  geom_vline(xintercept = c(top.date,bottom.date), colour = c("blue",'red'), linetype='dashed') +
  geom_hline(yintercept = 100, linetype='dashed')


## Histogram ##
P5 = ggplot(subset(SPX, Dt==last.date), aes(x=Price.Adj, fill=GICS.Sector, colour=GICS.Sector)) + 
        geom_histogram(alpha=0.5, position=position_dodge2(padding=0.5)) +
        scale_fill_manual(values = CBP) + scale_colour_manual(values = CBP) + 
        theme(legend.position="bottom") + 
        xlab('') + ylab('') + ggtitle('Adjusted Price of all S&P500 Stocks', subtitle = 'All Stock Prices at 100 on 31-Dec-2019') +
  geom_vline(xintercept = 0, linetype='dashed')
        # geom_vline(subset(SPX, Dt==last.date), aes(x=Price.Adj_min[GICS.Sector=='SP500']))

