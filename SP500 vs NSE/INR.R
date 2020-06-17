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

############################# Return calculations ########################

# Daily Returns 
NSE500[, Ret_SP500 := SP500TR/shift(SP500TR)-1]
NSE500[, Ret_NSE500D := NSE500TRD/shift(NSE500TRD)-1]

# Drawdown
NSE500[, DD_SP500 := 1- SP500TR/cummax(SP500TR)]
NSE500[, DD_NSE500D := 1- NSE500TRD/cummax(NSE500TRD)]

# Rolling Correlation between returns - 21 days (1M) and 252 days (1Y)
NSE500[, Corr21 := roll_cor(x=Ret_SP500, y=Ret_NSE500D, width = 21)]
NSE500[, Corr252 := roll_cor(x=Ret_SP500, y=Ret_NSE500D, width = 252)]

# Segregating the time periods
NSE500[, DD_max := rollmax(NSE500$DD_SP500, k=252*3, fill = "extend")]
NSE500[, DD_min := rollmax(NSE500$SP500TR, k=252*3, fill = 'extend')]
NSE500[, EP := (DD_max==DD_SP500 & DD_SP500>=0.2) | (DD_min==SP500TR)]
NSE500[, Pd := shift(paste0("P",cumsum(EP)))]; NSE500[is.na(Pd), Pd := "P0"]

# Finally, retaining only the required columns
NSE500 = NSE500[,c("Date", "SP500TR", "NSE500TRD", "Ret_SP500", "Ret_NSE500D", "DD_SP500", "DD_NSE500D", "Corr21", "Corr252", "EP", "Pd")]


################# Regression Analysis between daily returns ###############
out1 = lm(data=NSE500, formula=Ret_NSE500D ~ Ret_SP500)
summary(out1)
T1 = paste0("y = ", round(out1$coefficients[1],4), " + ", round(out1$coefficients[2],4),"*x", "; R^2 = ", round(summary(out1)$r.squared,4))
out2 = lm(data=NSE500, formula=Ret_NSE500D ~ Ret_SP500 + Pd)
summary(out2)

# Daily Regression for each period
out.P1 = lm(data=NSE500[Pd=="P1"], formula=Ret_NSE500D ~ Ret_SP500)
out.P2 = lm(data=NSE500[Pd=="P2"], formula=Ret_NSE500D ~ Ret_SP500)
out.P3 = lm(data=NSE500[Pd=="P3"], formula=Ret_NSE500D ~ Ret_SP500)
out.P4 = lm(data=NSE500[Pd=="P4"], formula=Ret_NSE500D ~ Ret_SP500)

Tex = c()
Tex[1] = paste0("P1: y = ", round(out.P1$coefficients[1],4), " + ", round(out.P1$coefficients[2],4),"*x", "; R^2 = ", round(summary(out.P1)$r.squared,4))
Tex[2] = paste0("P2: y = ", round(out.P2$coefficients[1],4), " + ", round(out.P2$coefficients[2],4),"*x", "; R^2 = ", round(summary(out.P2)$r.squared,4))
Tex[3] = paste0("P3: y = ", round(out.P3$coefficients[1],4), " + ", round(out.P3$coefficients[2],4),"*x", "; R^2 = ", round(summary(out.P3)$r.squared,4))
Tex[4] = paste0("P4: y = ", round(out.P4$coefficients[1],4), " + ", round(out.P4$coefficients[2],4),"*x", "; R^2 = ", round(summary(out.P4)$r.squared,4))

print(Tex)

################################# Graphs ################################


P1 = ggplot(NSE500[-1], aes(x=Ret_SP500, y=Ret_NSE500D)) + 
        geom_point(aes(group=Date, colour=Date)) +
        theme_minimal() + xlab("SP500TR Daily Return") + ylab("Nifty500TR Dollar Daily Return") + ggtitle("Regression Analysis") + 
        stat_smooth(method = "lm", formula='y~x', se = T, show.legend = F) + 
        geom_text(aes(-0.1,0.1, label=T1)) + 
     geom_hline(yintercept = 0, linetype="dashed", size=0) + geom_vline(xintercept = 0, linetype="dashed", size=0)

# Returns
P2 = ggplot(NSE500, aes(x=Date)) + 
        geom_line(aes(y=SP500TR, col="SP500TR")) + 
        geom_line(aes(y=NSE500TRD, col="NSE500TR")) + 
        theme_minimal() + xlab("") + ylab("") + ggtitle("Total Return Index - Historical Comparison") + scale_y_log10() + 
        geom_text(data = NSE500[1,c("Date","SP500TR","NSE500TRD")], aes(x=Date, y=SP500TR, label="SP500TR", col="SP500TR"), hjust = 0.1) + 
        geom_text(data = NSE500[1,c("Date","SP500TR","NSE500TRD")], aes(x=Date, y=NSE500TRD, label="NSE500TRD", col="NSE500TR"), hjust = 0.1) + 
        theme(legend.position = "none") + 
        geom_smooth(aes(x=Date, y=NSE500TRD, col="NSE500TRD"), method="lm", formula='y~x', se = T, show.legend = F, linetype='dotted')
        # theme(legend.position = c(0.7, 0.2),legend.direction = "horizontal", legend.title = element_blank())


# Drawdown with Correlation
P3 = ggplot(NSE500, aes(x=Date)) + 
        geom_area(aes(y=DD_SP500, fill="SP500TR", alpha=0.2)) + 
        geom_area(aes(y=DD_NSE500D, fill="NSE500TR", alpha=0.2)) + 
        theme_minimal() + xlab("") + ylab("") + ggtitle("Drawdown Index with Rolling Correlation: 252 days") +
        geom_line(aes(y=Corr252, col="Rolling Correlation: 252 days"), col="navyblue",  linetype="dashed") +
        theme(legend.position = "top", legend.direction = "horizontal", legend.title = element_blank()) +
        guides(alpha = FALSE)
        # geom_line(aes(y=Corr21, col="Rolling Correlation: 21 Days")) +
        # theme(legend.position = "none")

# Daily Return with Correlation
P3a = ggplot(NSE500[-1], aes(x=Date)) + 
        geom_col(aes(y = Ret_NSE500D - Ret_SP500, fill="NSE500D - SP500")) +
        geom_col(aes(y = Ret_NSE500D - Ret_SP500*out1$coefficients[2], fill=paste0("NSE500D - ",round(out1$coefficients[2],4),"*SP500"))) +
        theme(legend.position = "top", legend.title = element_blank()) +
        theme_minimal() + 
        xlab("") + ylab("") + ggtitle("Return difference between NSE500 & S&P500")

# P2 + geom_col(data=NSE500[EP==T,], aes(x=Date, y=Inf),show.legend = F, width = 1)

# Regression in distinct time periods
P4 = ggplot(NSE500[-1], aes(x=Ret_SP500, y=Ret_NSE500D, group=Pd, colour=Pd)) + geom_point() + 
        theme_minimal() + xlab("SP500TR Daily Return") + ylab("Nifty500TR Dollar Daily Return") + ggtitle("Regression Analysis for each period") + 
        stat_smooth(method = "lm", formula='y~x', se = T, show.legend = F) + 
        # geom_text(aes(-0.1,0.1, label=T1)) + 
        geom_hline(yintercept = 0, linetype="dashed", size=0) + geom_vline(xintercept = 0, linetype="dashed", size=0) +
        geom_text(aes(-0.1,0.1, label=T1)) +
        geom_text(aes(-0.1,0.08, label=Tex[1])) +
        geom_text(aes(-0.1,0.06, label=Tex[2])) +
        geom_text(aes(-0.1,0.04, label=Tex[3])) +
        geom_text(aes(-0.1,0.02, label=Tex[4]))
        # theme(aspect.ratio = 1)

# Total Return Index in different periods
P5 = ggplot(NSE500) +
        theme_minimal() + xlab("") + ylab("") + ggtitle("Total Return Index") + scale_y_log10() + theme(legend.position = 'none') +
        geom_line(aes(x=Date, y=SP500TR, group=Pd, col="SP500TR")) +
        geom_smooth(aes(x=Date, y=SP500TR, group=Pd, col="SP500TR"), method="lm", formula='y~x', se = T, show.legend = F, linetype='dotted') +
        geom_line(aes(x=Date, y=NSE500TRD, group=Pd, col="NSE500TRD")) +
        geom_smooth(aes(x=Date, y=NSE500TRD, group=Pd, col="NSE500TRD"), method="lm", formula='y~x', se = T, show.legend = F, linetype='dotted') +
        geom_label(data = NSE500[1,c("Date","SP500TR","NSE500TRD")], aes(x=Date, y=SP500TR, label="SP500TR", col="SP500TR"), hjust = 0.1) + 
        geom_label(data = NSE500[1,c("Date","SP500TR","NSE500TRD")], aes(x=Date, y=NSE500TRD, label="NSE500TRD", col="NSE500TRD"), hjust = 0.1)
        # geom_vline(xintercept = NSE500[EP==T]$Date, linetype="dashed", size=0)


# Total Return Index in different periods
# P5 = P2 + 

# Banded Rows
NSE500.endpoints = subset(NSE500, EP)[,c("Date", "Pd")]
NSE500.endpoints = data.table(NSE500.endpoints, 'Ed' = shift(NSE500.endpoints$Date, -1))[-nrow(NSE500.endpoints)]

P6 = P5 +
        geom_rect(data=NSE500.endpoints, aes(xmin=Date, xmax=Ed, ymin=0, ymax=Inf), fill=c('orange1','green1','orange2','green2','orange3'), alpha=0.2)
        # geom_vline(xintercept = NSE500[EP==T]$Date, linetype="dashed", size=0)




# Output
P1; P2; P3; P4; P5; P6


############################# Part 2 ######################

NSE500m = NSE500[endpoints(Date),]
NSE500m = data.table(NSE500m)
NSE500m[, Ret_SP500 := log(SP500TR) - shift(log(SP500TR))]
NSE500m[, Ret_NSE500D := log(NSE500TRD) - shift(log(NSE500TRD))]
NSE500m[, b := roll_cov(Ret_SP500, Ret_NSE500D, 36)/roll_var(Ret_SP500, 36)]

mout1 = lm(data=NSE500m[-1], formula=Ret_NSE500D ~ Ret_SP500)
summary(mout1)
TM1 = paste0("y = ", round(mout1$coefficients[1],4)*12, " + ", round(mout1$coefficients[2],4),"*x", "; R^2 = ", round(summary(mout1)$r.squared,4))

M1 =  ggplot(NSE500m[-1], aes(x=Ret_SP500, y=Ret_NSE500D)) + 
        geom_point(aes(group=Date, colour=Date)) +
        theme_minimal() + xlab("SP500TR Daily Return") + ylab("Nifty500TR Dollar Daily Return") + ggtitle("Regression Analysis") + 
        stat_smooth(method = "lm", formula='y~x', se = T, show.legend = F) + 
        geom_text(aes(-0.1,0.1, label=TM1)) + 
        geom_hline(yintercept = 0, linetype="dashed", size=0) + geom_vline(xintercept = 0, linetype="dashed", size=0)

M2 = ggplot(NSE500m, aes(x=Date)) + 
        # geom_line(aes(x=Date, y=b)) +
        geom_area(aes(y=DD_SP500, fill="SP500TR", alpha=0.2)) + 
        geom_area(aes(y=DD_NSE500D, fill="NSE500D", alpha=0.2)) + 
        geom_line(aes(y=b, col="Rolling Beta - past 36 months"), col="navyblue",  linetype="dashed") +
        theme(legend.position = "top", legend.direction = "horizontal", legend.title = element_blank()) +
        guides(alpha = FALSE)

