library(readr)
library(readxl)
library(ggplot2)
library(plotly)
library(ggrepel)
library(data.table)

# getwd()
IB = read_csv('IPL_batting.csv') %>% data.table
IB2 = read_csv('IPL_bats.csv') %>% data.table

P1 = ggplot(IB, aes(x=Ave, y=SR, label=Player, size=Runs, color=Team)) + 
  geom_point(alpha = 0.5) +
  # scale_size(range = c(1, 10), name="Runs") +
  geom_text_repel(size=3) + 
  theme_minimal() +
  # theme(text = element_text(size = 10)) +
  theme(legend.position = "none")
  # theme(text=element_text(family="Arial", face="plain", size=12/.pt)) +
# ggplotly(P1)
# P1

# IB2 = data.table::data.table(IB2)
IB2[, Ave := as.numeric(Ave)]
P2 = ggplot(subset(IB2, Runs>0), aes(x=Runs, y=SR, label=Player, size=Ave, color=Team)) +
  geom_point(alpha = 0.5) +
  geom_text_repel(size=3) + 
  theme_minimal() +
  theme(legend.position = "none")
# P2

IB3 = subset(IB2, Runs>0 & SR>0 & Ave>0)
P3 = ggplot(IB3, aes(x=Ave, y=SR, size=Runs, color=Team)) +
  geom_point(alpha = 0.2) +
  xlim(0,70) + ylim(0,300) +
  xlab('Average') + ylab('Strike Rate') + ggtitle('IPL 2022: Batting', subtitle = 'Size of the circle is indicative of the Runs scored') +
  # geom_text_repel(size=3) +
  # scale_y_continuous(breaks = seq(0, 300, by = 50)) +
  # scale_y_continuous(minor_breaks = seq(0,300,y=5)) + 
  theme_minimal() +
  # theme(panel.grid.minor.y = element_line(color = 2, size = 0.25, linetype = 1)) +
  theme(legend.position = "none")
# panel.grid.minor = element_line(colour = "grey70", size = 0.2)
IB4 = subset(IB3, Runs>150 | SR>200 | Ave>40)
P4 = P3 + 
  geom_point(data = IB4, alpha = 0.5) +
  geom_text_repel(data = IB4, inherit.aes = TRUE, aes(label=paste0(Player," (", Runs, ")")), size=3)
P4
# ggplotly(P4)

# 
# P5 = symbols(IB3$Ave, IB3$SR, circles = sqrt(IB3$Runs/pi))
# P5
