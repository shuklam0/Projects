library(quantmod)
library(data.table)
library(BatchGetSymbols)
library(ggplot2)
library(plotly)
library(ggrepel)
library(car)

Tk = "^GSPC"
SP = BatchGetSymbols::BatchGetSymbols(tickers = Tk, 
                                      first.date = "1949-12-25", 
                                      last.date = "2021-09-30",
                                      type.return = "log",
                                      freq.data = "daily"
                                      )[[2]]
SP = SP %>% data.table

SP1 = SP[endpoints(SP$ref.date, on = "month")]
SP1[, Mth := month(ref.date)]
SP1[, Yr := year(ref.date)]

SP1[, Ret := log(price.close) - shift(log(price.close))]
SP1 = SP1[-1]

################# Plots #####################

SP1[, outlier:= Ret %in% boxplot.stats(Ret)$out, by=factor(Mth)]

P1 = ggplot(SP1, aes(x=month.abb[Mth], y=Ret, group=Mth, fill=factor(Mth), label=Yr)) +
  # geom_hline(yintercept = mean(SP1$Ret), na.rm = TRUE, linetype="dotted", size=1.5) +
  geom_boxplot(show.legend = FALSE) +
  # geom_dotplot(show.legend = FALSE) +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(labels = scales::percent) +
  geom_label_repel(data = SP1[outlier==TRUE], mapping = aes(x=month.abb[Mth], y=Ret), show.legend = FALSE) +
  theme_minimal() + xlab("") + ylab("") + ggtitle("S&P500 Monthly Returns (since 1950)")
  
P1 %>% ggplotly %>% hide_legend
