#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Libraries for Data
library(COVID19)
library(data.table)
library(countrycode)
library(tidyverse)

# Library for Graph
library(ggplot2)
library(ggrepel)
library(leaflet)
library(markdown)
library(gganimate)

# Shiny Packages
library(shiny)
library(plotly)
library(shinyanimate)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(shinyWidgets)
library(shinyflags)
library(leaflet)

# Using other files
source('global.R')
source('box.R')

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Title on Page & Window
    titlePanel(title = "", windowTitle = "COVID19 Dashboard by Mohit Shukla"),
    # Current Date
    h5(paste("Latest date: ", max(COVID19$date)), align="right"),
    
    # ValueBoxes & InfoBoxes
    Baxa,
    
    # 
    # hr(),
    
    # 1. Navigation bar: Text left of Tabs 
    navbarPage("Coronavirus Dashboard",
               # Tab1: Panel with the Global Stats
               tabPanel("Summary",
                        sidebarLayout(
                            sidebarPanel(
                                
                                # Select Countries
                                multiInput(inputId = "Ct", label = "Select Countries", choices = sort(unique(as.vector(COVID19$country))), selected = unique(D2$country), choiceNames = NULL, choiceValues = NULL),
                                
                                # Select Type of Data
                                selectInput(inputId = "ty", label = "Select Data", choices = c("confirmed", "recovered", "deaths", "active"), selected = "confirmed")
                            ),
                            mainPanel(
                                box(title = paste0("Total Cases"), width = 12,
                                    column(checkboxInput("CB", "y-axis (Total Numbers) in logarithmic scale", value = TRUE),
                                           width=12),
                                    plotlyOutput(outputId = "Plot1", inline = TRUE),
                            ))))
)))