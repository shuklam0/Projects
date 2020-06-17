library(quantmod)
library(readxl)
library(readr)
library(data.table)
library(ggplot2)
library(roll)
library(devtools)
library(XML)

setwd('C:/Users/mohit/OneDrive - University of California/R/SPX vs NSE/')

# INR.old = read_csv('BankWise.csv'); INR.new = read_csv('USD_INR.csv')
# INR = data.table(rbind(read_csv('USD_INR.csv'), read_csv('BankWise.csv')))

# Load the NSE 500 Total Return Index
NSE500TRI = data.table(read_csv('NSE500TRI.csv'), stringsAsFactors = F)
colnames(NSE500TRI) = c("Date","NSE500TRI")
NSE500TRI[, Date := as.Date(Date, format = '%d-%b-%Y')]

# Load the currency files
INR.old = data.table(readHTMLTable('BankWise.xls', stringsAsFactors = F)[[1]])
colnames(INR.old) = c("Date","USD")
INR.old[, Date := as.Date(Date, format = '%d/%m/%Y')]
INR.old[, USD := as.numeric(USD)]

INR.new = data.table(readHTMLTable('USD_INR.xls', stringsAsFactors = F)[[2]])
colnames(INR.new) = c("Date","USD")
INR.new[, Date := as.Date(Date, format = '%d %b %Y')]
INR.new[, USD := as.numeric(USD)]

INR = rbind(INR.new, INR.old[-1])
rm(INR.new, INR.old) # Remove old files

# Combine USD & NSE500 file
NSE500 = merge(NSE500TRI, INR, by = 'Date', all = T)
NSE500 = NSE500[order(Date),]
NSE500[, NSE500TRD := NSE500TRI/USD]

# Load S&P 500 Total Return Index
getSymbols("^SP500TR", from=as.Date("1900-01-01"))
SP500 = data.table("Date" = index(SP500TR), "SP500TR" = as.vector(SP500TR$SP500TR.Close))

# write.csv(SP500, "SP500.csv", col.names = F)
# Combine all files
NSE500 = data.table(merge(NSE500, SP500, by="Date", all = T))
NSE500[, NSE500TRD := NSE500TRD/max(NSE500TRD, na.rm = T)*max(SP500TR, na.rm = T)]
NSE500 = NSE500[!is.na(NSE500TRD/SP500TR),]
# Remove the other files
rm(SP500TR, INR, NSE500TRI, SP500)