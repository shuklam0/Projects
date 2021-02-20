
# Using other files
source('global.R')


# Library for saving data
library(data.table)
library(tidyverse)
# library(COVID19)
# library(countrycode)


# Shiny Packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)



# Valueboxes 
# Total Cases
Box.T = valueBox(value = format(sum(COVID19[date == last(date), "confirmed"]), big.mark = ","), 
                 subtitle =  "Total Confirmed Cases", 
                 icon = icon("viruses"),
                 color = "light-blue",
                 width = 3, href="https://www.worldometers.info/coronavirus/")

# Total Deaths
Box.D = valueBox(value = format(sum(COVID19[date == last(date), "deaths"]), big.mark = ","), 
                 subtitle =  "Total Deaths", 
                 icon = icon("skull-crossbones"),
                 color = "red",
                 width = 3)

# Total Recovered
Box.R = valueBox(value = format(sum(COVID19[date == last(date), "recovered"]), big.mark = ","), 
                 subtitle =  "Total Recovered",
                 icon = icon("shield-virus"),
                 color = "green",
                 width = 3)

# Total Active
Box.A = valueBox(value = format(sum(COVID19[date == last(date), "active"]), big.mark = ","), 
                 subtitle =  "Total Cases Currently Active", 
                 icon = icon("hospital"),
                 color = "orange",
                 width = 3)

# Baxa = dashboardPage(skin = "black",
#                      header = dashboardHeader(disable = TRUE),
#                      sidebar = dashboardSidebar(disable = TRUE),
#                      body = dashboardBody(fluidRow(Box.T, Box.D, Box.R, Box.A), height=4, collapse=TRUE, disable=TRUE)
#                      )

Baxa = fluidRow(Box.T, Box.D, Box.R, Box.A, color = "teal")

