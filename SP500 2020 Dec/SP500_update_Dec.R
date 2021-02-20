####### Library ######
# library(quantmod)
library(BatchGetSymbols)
library(data.table)
library(ggplot2)
# library(plotly)
library(ggrepel)
# library(tidyverse)

##### Setting dates #########
first.date <- as.Date("2019-12-31")
last.date <- as.Date("2020-12-31")
# top.date <- as.Date("2020-02-14")
# bottom.date <- as.Date("2020-03-23")

##### Getting data #########
df.SP500 = data.table(GetSP500Stocks())
SP = BatchGetSymbols(tickers=c(df.SP500$Tickers, "SPY", "SPYG", "SPYV", "ZM"), first.date = first.date)
# SP1 = data.table(SP[[1]])
SP2 = data.table(SP[[2]])[,c('ref.date','ticker','price.adjusted')]
SP3 = merge(SP2, df.SP500[,c("Tickers","Company","GICS.Sector")], by.x='ticker', by.y='Tickers', all.x = TRUE)

########### Data Manipulation ##########

SP3[ticker=="SPY", Company := ".S&P500 ETF"]
SP3[ticker=="SPYG", Company := ".S&P500 Growth ETF"]
SP3[ticker=="SPYV", Company := ".S&P500 Value ETF"]
SP3[ticker=="ZM", Company := "Zoom Video Communications Inc"]
SP3[, price.adj := price.adjusted/first(price.adjusted)*100, by=ticker]

########## Graphs #########

# CBP <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00","#000000", "#CC79A7", "#FF00FF","#003366", "indianred4")

P1 = ggplot(data = subset(SP3), aes(x=ref.date, y=price.adj, group=ticker, color=GICS.Sector)) + 
        geom_line(alpha=0.5, size=0.5) + 
        theme(legend.position="bottom") +
        # scale_colour_manual(values = CBP) +
        geom_label_repel(data = subset(SP3, (price.adj>200 |price.adj<50)  & ref.date==last(ref.date)), mapping = aes(x=ref.date, y=price.adj, label=Company), show.legend = FALSE) +
        scale_y_log10() + xlab("") + ylab("") +
        # geom_hline(yintercept = 100, linetype='dashed') + 
        guides(colour=guide_legend(nrow=2, byrow=TRUE))
        # theme_minimal()

P2 = P1 + geom_line(data = subset(SP3, ticker %in% c("SPY", "SPYG", "SPYV")), mapping = aes(x=ref.date, y=price.adj), color = "black", alpha=2, linetype="dotted", size=2, show.legend = FALSE) + 
        geom_label(data = subset(SP3, ticker %in% c("SPY", "SPYG", "SPYV") & ref.date==last(ref.date)), mapping = aes(x=ref.date, y=price.adj, label=ticker), color="black")
P2


SP = c()
P3 = ggplot(data = subset(SP3, ticker %in% SP), mapping = aes(x=ref.date, y=price.adj, group=ticker, color=ticker)) + 
        geom_line(show.legend=FALSE) +
        # geom_label_repel(data = subset(SP3, ticker %in% c() & ref.date==last(ref.date)), mapping = aes(x=ref.date, y=price.adj, label=ticker), show.legend = FALSE) + 
        # scale_y_log10() + 
        xlab("") + ylab("") + 
        scale_y_log10() +
        theme_minimal()

SP = c("SPY", "SPYG", "SPYV")
P3.0 = P3 + geom_line(data = subset(SP3, ticker %in% SP), mapping = aes(x=ref.date, y=price.adj, group=ticker, color=ticker), show.legend=FALSE) +
        geom_label_repel(data = subset(SP3, ticker %in% SP & ref.date==last(ref.date)), mapping = aes(x=ref.date, y=price.adj, label=Company), show.legend = FALSE) 
        
SP = c(SP, "FB", "AAPL", "AMZN", "NFLX", "GOOGL")
P3.1 = P3 + geom_line(data = subset(SP3, ticker %in% SP), mapping = aes(x=ref.date, y=price.adj, group=ticker, color=ticker), show.legend = FALSE) +
        geom_label_repel(data = subset(SP3, ticker %in% SP & ref.date==last(ref.date)), mapping = aes(x=ref.date, y=price.adj, label=Company, color=ticker), show.legend = FALSE)

SP = c(SP, "ZM", "TSLA")
P3.2 = P3 + geom_line(data = subset(SP3, ticker %in% SP), mapping = aes(x=ref.date, y=price.adj, group=ticker, color=ticker), show.legend = FALSE) +
        geom_label_repel(data = subset(SP3, ticker %in% SP & ref.date==last(ref.date)), mapping = aes(x=ref.date, y=price.adj, label=Company, color=ticker), show.legend = FALSE)
        
P3.3 = P3 + geom_line(data = SP3, mapping = aes(x=ref.date, y=price.adj, group=ticker, color=ticker), show.legend = FALSE, alpha=0.5) +
        geom_label_repel(data = subset(SP3, (price.adj>200 |price.adj<50) & ref.date==last(ref.date)), mapping = aes(x=ref.date, y=price.adj, label=Company, color=ticker), show.legend = FALSE)

P3.4 = P3.3 + geom_line(data = subset(SP3, ticker %in% c("SPY", "SPYG", "SPYV")), mapping = aes(x=ref.date, y=price.adj, group=ticker), show.legend = FALSE, linetype = "dotted", alpha=2) +
        geom_label_repel(data = subset(SP3, ticker %in% SP & ref.date==last(ref.date)), mapping = aes(x=ref.date, y=price.adj, label=Company, color=ticker), show.legend = FALSE)

## Plotly Graph
# P1a = ggplotly(P1)
# 
# G1 = plot_ly(data = SP3,
#              x=~ref.date,
#              y=~price.adj, 
#              color=~GICS.Sector,
#              alpha=0.5,
#              # text=~Company,
#              type="scatter",
#              name = ~Company,
#              mode="lines"
#              # hoverinfo="text"
# )