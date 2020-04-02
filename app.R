library(shiny)
library(quantmod)
library(BatchGetSymbols)
library(ggplot2)
library(data.table)
library(dplyr)
library(plotly)
library(ggpubr)


getSymbols("^DJI", from=as.Date("1900-01-01"))
getSymbols("^GSPC", from=as.Date("1900-01-01"))

frm_dt = as.Date("1946-01-01")

DJ = data.table("Dt" = index(DJI), "DJI" = as.vector(DJI$DJI.Close))
# DJ = DJ[Dt>=frm_dt]
DJ[, High := cummax(DJI)]
DJ[, Drawdown := 1-DJI/High]

SP = data.table("Dt" = index(GSPC), "Cl" = as.vector(GSPC$GSPC.Close))
# SP = SP[Dt>=frm_dt]
SP[, High := cummax(Cl)]
SP[, Drawdown := 1-Cl/High]

# Dow Jones
g1 = ggplot(DJ) + geom_line(aes(x=Dt, y=DJI, col="Dow Jones Index")) + theme_bw() + scale_y_log10() + geom_line(aes(x=Dt, y=High)) + theme(legend.position = "none") + xlab("") + ylab("Index Value") + ggtitle("Dow Jones")
recessions.trim = recessions.df[recessions.df$Trough>min(DJ$Dt),]
g2 = g1 + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=max(DJ$DJI)), fill='pink', alpha=0.4)
g3 = g2 + geom_point(data =DJ[DJ$Drawdown>DJ$Drawdown[nrow(DJ)]], aes(x=Dt, y=DJI, col="current level", alpha=0.1))

g4 = ggplot(DJ) + geom_line(aes(x=Dt, y=Drawdown)) + theme_bw() + ylim(rev(range(DJ$Drawdown))) + ggtitle("Dow Jones Drawdown")
g5 = g4 + geom_line(data =DJ[DJ$Drawdown>DJ$Drawdown[nrow(DJ)]], aes(x=Dt, y=Drawdown, col="current level"))

g6 = g4 + geom_hline(yintercept = max(DJ$Drawdown[(nrow(DJ)-251):nrow(DJ)]), col="red")

# SP500
F1 = ggplot(SP) + geom_line(aes(x=Dt, y=Cl)) + theme_bw() + scale_y_log10() + geom_line(aes(x=Dt, y=High, color="blue")) + theme(legend.position = "none") + xlab("") + ylab("Index Value") + ggtitle("S&P 500")
recessions.trim2 = recessions.df[recessions.df$Trough>min(SP$Dt),]
F2 = F1 + geom_rect(data=recessions.trim2, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=max(SP$Cl)), fill='pink', alpha=0.4)
F3 = F2 + geom_point(data =SP[SP$Drawdown>SP$Drawdown[nrow(SP)]], aes(x=Dt, y=Cl, col="current level"))

F4 = ggplot(SP) + geom_line(aes(x=Dt, y=Drawdown)) + theme_bw() + ylim(rev(range(SP$Drawdown))) + geom_rect(data=recessions.trim2, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=max(SP$Drawdown)), fill='pink', alpha=0.4) + xlab("") + ggtitle("S&P 500 Drawdown")
F5 = F4 + geom_line(data =SP[SP$Drawdown>SP$Drawdown[nrow(SP)]], aes(x=Dt, y=Drawdown, col="current level"))
F6 = F4 + geom_hline(yintercept = max(SP$Drawdown[(nrow(SP)-251):nrow(SP)]), col="red")


# unrate = getSymbols('UNRATE',src='FRED', auto.assign=F) 
# unrate.df = data.frame(date=time(unrate), coredata(unrate) )

recessions.df = read.table(textConnection(
     "Peak, Trough
     1857-06-01, 1858-12-01
     1860-10-01, 1861-06-01
     1865-04-01, 1867-12-01
     1869-06-01, 1870-12-01
     1873-10-01, 1879-03-01
     1882-03-01, 1885-05-01
     1887-03-01, 1888-04-01
     1890-07-01, 1891-05-01
     1893-01-01, 1894-06-01
     1895-12-01, 1897-06-01
     1899-06-01, 1900-12-01
     1902-09-01, 1904-08-01
     1907-05-01, 1908-06-01
     1910-01-01, 1912-01-01
     1913-01-01, 1914-12-01
     1918-08-01, 1919-03-01
     1920-01-01, 1921-07-01
     1923-05-01, 1924-07-01
     1926-10-01, 1927-11-01
     1929-08-01, 1933-03-01
     1937-05-01, 1938-06-01
     1945-02-01, 1945-10-01
     1948-11-01, 1949-10-01
     1953-07-01, 1954-05-01
     1957-08-01, 1958-04-01
     1960-04-01, 1961-02-01
     1969-12-01, 1970-11-01
     1973-11-01, 1975-03-01
     1980-01-01, 1980-07-01
     1981-07-01, 1982-11-01
     1990-07-01, 1991-03-01
     2001-03-01, 2001-11-01
     2007-12-01, 2009-06-01"), sep=',',
     colClasses=c('Date', 'Date'), header=TRUE)


H1 = ggarrange(F2, g2, F6, g6, ncol=2, nrow=2)

ui <- fluidPage(
     titlePanel("",windowTitle = "Index Drawdowns"),
     mainPanel(
          plotOutput("P1")
)
)

server <- function(input, output) {
     output$P1 <- renderPlot(H1)
}
shinyApp(ui = ui, server = server)