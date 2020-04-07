library(shiny, quietly = T, warn.conflicts = F)
library(quantmod, quietly = T, warn.conflicts = F)
library(BatchGetSymbols, quietly = T, warn.conflicts = F)
library(ggplot2, quietly = T, warn.conflicts = F)
library(data.table, quietly = T, warn.conflicts = F)
library(dplyr, quietly = T, warn.conflicts = F)
library(plotly, quietly = T, warn.conflicts = F)
library(ggpubr, quietly = T, warn.conflicts = F)


getSymbols("^DJI", from=as.Date("1900-01-01"))
getSymbols("^GSPC", from=as.Date("1900-01-01"))
getSymbols("^BSESN", from=as.Date("1900-01-01"))

frm_dt = as.Date("1946-01-01")

DJ = data.table("Dt" = index(DJI), "Cl" = as.vector(DJI$DJI.Close))
# DJ = DJ[Dt>=frm_dt]
DJ[, High := cummax(Cl)]
DJ[, Drawdown := 1-Cl/High]

SP = data.table("Dt" = index(GSPC), "Cl" = as.vector(GSPC$GSPC.Close))
SP = SP[Dt>=frm_dt]
SP[, High := cummax(Cl)]
SP[, Drawdown := 1-Cl/High]

Sensex = data.table("Dt" = index(BSESN), "Cl" = as.vector(BSESN$BSESN.Close))
Sensex = Sensex[!is.na(Cl),]
# SP = SP[Dt>=frm_dt]
Sensex[, High := cummax(Cl)]
Sensex[, Drawdown := 1-Cl/High]

# Remove All xts files
rm(DJI, GSPC, BSESN)

recessions.df = data.table("Peak"=as.Date(c('1926-10-01','1929-08-01','1937-05-01','1945-02-01','1948-11-01','1953-07-01','1957-08-01','1960-04-01','1969-12-01','1973-11-01','1980-01-01','1981-07-01','1990-07-01','2001-03-01','2007-12-01', '2020-01-01')),"Trough"=as.Date(c('1927-11-01','1933-03-01','1938-06-01','1945-10-01','1949-10-01','1954-05-01','1958-04-01','1961-02-01','1970-11-01','1975-03-01','1980-07-01','1982-11-01','1991-03-01','2001-11-01','2009-06-01', '2020-06-30')))

# Dow Jones
g1 = ggplot(DJ) + geom_line(aes(x=Dt, y=Cl, col="Dow Jones Index")) + theme_bw() + scale_y_log10() + geom_line(aes(x=Dt, y=High)) + theme(legend.position = "none") + xlab("") + ylab("Index Value") + ggtitle("Dow Jones")
recessions.trim = recessions.df[recessions.df$Trough>min(DJ$Dt),]
g2 = g1 + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=max(DJ$Cl)), fill='pink', alpha=0.4)
g3 = g2 + geom_point(data =DJ[DJ$Drawdown>DJ$Drawdown[nrow(DJ)]], aes(x=Dt, y=Cl, col="current level", alpha=0.1))

g4 = ggplot(DJ) + geom_line(aes(x=Dt, y=Drawdown)) + theme_bw() + ylim(c(0.6,0)) + ggtitle("Dow Jones Drawdown") + xlab("") + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=0.6), fill='pink', alpha=0.4) + xlab("") + ggtitle("Dow Jones Drawdown")
g5 = g4 + geom_line(data =DJ[DJ$Drawdown>DJ$Drawdown[nrow(DJ)]], aes(x=Dt, y=Drawdown, col="current level"))

g6 = g4 + geom_hline(yintercept = max(DJ$Drawdown[(nrow(DJ)-251):nrow(DJ)]), col="red") + geom_hline(yintercept = DJ$Drawdown[nrow(DJ)], col="blue")

# SP500
F1 = ggplot(SP) + geom_line(aes(x=Dt, y=Cl, color="SP500")) + theme_bw() + scale_y_log10() + geom_line(aes(x=Dt, y=High)) + theme(legend.position = "none") + xlab("") + ylab("Index Value") + ggtitle("S&P 500")
recessions.trim2 = recessions.df[recessions.df$Trough>min(SP$Dt),]
F2 = F1 + geom_rect(data=recessions.trim2, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=max(SP$Cl)), fill='pink', alpha=0.4)
F3 = F2 + geom_point(data =SP[SP$Drawdown>SP$Drawdown[nrow(SP)]], aes(x=Dt, y=Cl, col="current level"))

F4 = ggplot(SP) + geom_line(aes(x=Dt, y=Drawdown)) + theme_bw() +ylim(c(0.6,0))+ geom_rect(data=recessions.trim2, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=0.6), fill='pink', alpha=0.4) + xlab("") + ggtitle("S&P 500 Drawdown")
F5 = F4 + geom_line(data =SP[SP$Drawdown>SP$Drawdown[nrow(SP)]], aes(x=Dt, y=Drawdown, col="current level"))
F6 = F4 + geom_hline(yintercept = max(SP$Drawdown[(nrow(SP)-251):nrow(SP)]), col="red") + geom_hline(yintercept = SP$Drawdown[nrow(SP)], col="blue")

# Sensex
S1 = ggplot(Sensex) + geom_line(aes(x=Dt, y=Cl, col="Sensex")) + theme_bw() + scale_y_log10() + geom_line(aes(x=Dt, y=High)) + theme(legend.position = "none") + xlab("") + ylab("Index Value") + ggtitle("Sensex")
recessions.trimS = recessions.df[recessions.df$Trough>min(Sensex$Dt),]
S2 = S1 + geom_rect(data=recessions.trimS, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=max(Sensex$Cl)), fill='pink', alpha=0.4)
S3 = S2 + geom_point(data =DJ[DJ$Drawdown>DJ$Drawdown[nrow(DJ)]], aes(x=Dt, y=DJI, col="current level", alpha=0.1))

S4 = ggplot(DJ) + geom_line(aes(x=Dt, y=Drawdown)) + theme_bw() + ylim(c(0.6,0)) + ggtitle("Sensex Drawdown") + xlab("") + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=0.6), fill='pink', alpha=0.4) + xlab("") + ggtitle("Sensex Drawdown")
S5 = S4 + geom_line(data =DJ[DJ$Drawdown>DJ$Drawdown[nrow(DJ)]], aes(x=Dt, y=Drawdown, col="current level"))

S6 = S4 + geom_hline(yintercept = max(Sensex$Drawdown[(nrow(Sensex)-251):nrow(Sensex)]), col="red") + geom_hline(yintercept = Sensex$Drawdown[nrow(Sensex)], col="blue")


H1 = ggarrange(F2, g2, S2, F6, g6, S6, ncol=3, nrow=2)

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
