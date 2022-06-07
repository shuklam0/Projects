library(readr)
library(readxl)
library(ggplot2)
library(plotly)
library(ggrepel)
library(data.table)

# getwd()
IB = read_csv('IPL_bowl.csv') %>% data.table
IB[, Ave := as.numeric(Ave)]
IB[, SR := as.numeric(SR)]
IB3 = subset(IB, Wkts>0)

P3 = ggplot(IB3, aes(x=Ave, y=Econ, size=Wkts, color=Team)) +
  geom_point(alpha = 0.2) +
  # xlim(0,70) + ylim(0,300) +
  xlab('Bowling Average') + ylab('Economy Rate') + ggtitle('IPL 2022: Bowling', subtitle = 'Size of the circle is indicative of the Wickets Taken') +
  # geom_text_repel(size=3) +
  # scale_y_continuous(breaks = seq(0, 300, by = 50)) +
  # scale_y_continuous(minor_breaks = seq(0,300,y=5)) + 
  theme_minimal() +
  # theme(panel.grid.minor.y = element_line(color = 2, size = 0.25, linetype = 1)) +
  theme(legend.position = "none")
# panel.grid.minor = element_line(colour = "grey70", size = 0.2)
IB4 = subset(IB3, Wkts>15 | Econ<7 | Ave<20)
P4 = P3 + 
  geom_point(data = IB4, alpha = 0.5) +
  geom_text_repel(data = IB4, inherit.aes = TRUE, aes(label=paste0(Player," (", Wkts, ")")), size=3)
P4
# ggplotly(P4)
