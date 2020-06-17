library(quantmod)
library(ggplot2)
library(data.table)

Symb = c("^DJI","^GSPC","^BSESN", "^VIX", "INR=X")
S_TR = c("^SP500TR")
from_Dt = as.Date("1900-01-01")

getSymbols(Symb, from=from_Dt)
getSymbols(S_TR, from=from_Dt)

Ind = rbind(
     # data.table("Dt" = index(GSPC), "Cl" = as.vector(GSPC[,4]), "Index" = "S&P_500"),
     data.table("Dt" = index(DJI), "Cl" = as.vector(DJI[,4]), "Index" = "Dow_Jones"),
     data.table("Dt" = index(BSESN), "Cl" = as.vector(BSESN[,4]), "Index" = "Sensex"),
     data.table("Dt" = index(`INR=X`), "Cl" = as.vector(`INR=X`[,4]), "Index" = "INR")
     # data.table("Dt" = index(VIX), "Cl" = as.vector(VIX[,4]), "Index" = "VIX")
)
# Ind = Ind[!is.na(Cl),] ; Ind[, High := cummax(Cl)]; Ind[, Drawdown := 1-Cl/High]; Ind[, Ret := Cl/shift(Cl)-1, by=Index]

rm(GSPC, DJI, BSESN, `INR=X`, VIX, SP500TR)

# Indices Plot
# p1 = ggplot(Ind[which(Index %in% c("Dow Jones","BSE Sensex")),], aes(x=Dt, y=Cl, group=Index, colour=Index)) + geom_line() + scale_y_log10() + geom_smooth(method = "lm") + theme_minimal() + xlab('') + ylab('')
p1 = ggplot(Ind[Index!="INR",], aes(x=Dt, y=Cl, group=Index, colour=Index)) + geom_line() + scale_y_log10() + geom_smooth(method = "lm") + theme_minimal() + xlab('') + ylab('')


# Index Return
p2 = ggplot(Ind[Index=="Dow_Jones",], aes(x=Dt, y=Ret, group=Index, colour=Index)) + geom_line() + theme_minimal() + xlab('') + ylab('')

# VIX Plot
p3 = ggplot(Ind[Index=="VIX"], aes(x=Dt, y=Cl)) + geom_line() + xlab('') + ylab('') + theme_bw() + ggtitle("VIX Index") + geom_smooth()


# Combined Plots
# library(ggpubr)
# ggarrange(p1, p3, ncol=1)

library(reshape2)

# Ind1 = data.table(Ind1)
# Ind1 = Ind1[!is.na(`Dow Jones`) & !is.na(VIX),]
# Ind1[, Ret_DJ := `Dow Jones`/shift(`Dow Jones`)-1]
# cor(Ind1$VIX[-1], Ind1$Ret_DJ[-1])
Ind1 = data.table(dcast(Ind,Dt~Index, value.var = "Cl"))
Ind1 = Ind1[!is.na(Sensex) & !is.na(INR) & !is.na(Dow_Jones),]
Ind1[, Sensex_USD := Sensex/INR]
Ind3 = data.table(Ind1)
Ind1[, Sensex_USD := Sensex_USD/max(Sensex_USD)*max(Dow_Jones)]; Ind1[, Sensex :=  Sensex/max(Sensex)*max(Dow_Jones)]
# Ind1[, INRUSD := 1/INR]; Ind1[, INRUSD := INRUSD/max(INRUSD)*max(Dow_Jones)] 
# Ind1[, INR:= INR/max(INR)*max(Dow_Jones)]

Ind2 = melt(Ind1[,c("Dt", "Dow_Jones", "Sensex", "Sensex_USD")], id="Dt")

# Comaprative Plot
p4 = ggplot(Ind2, aes(x=Dt, y=value, group=variable, colour=variable)) + geom_line() + 
        scale_y_log10() + theme_minimal() + xlab('') + ylab('') + 
        geom_smooth(method = "lm") # Lines
        # theme(legend.title = element_text(size=12, color = "salmon", face="bold"), legend.justification=c(0,1), legend.position=c(0.7, 0.6), legend.background = element_blank(), legend.key = element_blank())

########### Modifying the Index #################

Ind1[, Sensex_alpha := Sensex_USD/shift(Sensex_USD) - Dow_Jones/shift(Dow_Jones)]
# hist(Ind1$Sensex_alpha, breaks = 100)
m1 = mean(Ind1$Sensex_alpha[-1])
m2 = sd(Ind1$Sensex_alpha[-1])

m1/m2

cor(Ind1$Dow_Jones, Ind1$Sensex_USD)

p5 = ggplot() + geom_col(data=Ind1[Sensex_alpha>0], aes(x=Dt, y=Sensex_alpha), fill="green") + geom_col(data=Ind1[Sensex_alpha<=0], aes(x=Dt, y=Sensex_alpha), fill="red") + theme_minimal() + ylim(-0.1,0.1)

# Histogram
p6 = ggplot(Ind1[-1], aes(x=Sensex_alpha*252)) + geom_histogram(bins=100, fill="orange") +
        theme_minimal() + xlab("") + ylab("") + 
        geom_vline(aes(xintercept=mean(Sensex_alpha*252)), colour="blue")


######## Regression ##########


Ind3[, Ret_DJI := Dow_Jones/shift(Dow_Jones)-1]
Ind3[, Ret_Sensex_USD := Sensex_USD/shift(Sensex_USD)-1]
Ind3 = Ind3[-1]

p7 = ggplot(Ind3, aes(x=Ret_DJI, y=Ret_Sensex_USD)) + geom_point() +
        theme_minimal() + 
        # xlab("") + ylab("") + 
        geom_smooth(method = "lm") # Lines

out1 = lm(data = Ind3[-1,], formula = Ret_Sensex_USD ~ Ret_DJI)
summary(out1)
