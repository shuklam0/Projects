#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    COVID19.P1 = reactive({
        COVID19.long[, CAT := ifelse(country %in% input$Ct, country, ".Rest of World")] %>%
            subset(variable == input$ty)
    })
    
    output$Plot1 = renderPlotly({
        # Plot A: Total Cases Output
        PlotA = COVID19.long[, CAT := ifelse(country %in% input$Ct, country, ".Rest of World")] %>%
            subset(variable == input$ty) %>%
            group_by(date, CAT) %>%
            summarize(cases=sum(value)) %>%
            subset(cases>0) %>% 
            plot_ly(
                type="scatter", 
                x=~date, 
                y=~cases, 
                color=~CAT, 
                mode="lines", 
                fill=~CAT, 
                stackgroup="all", 
                height=600,
                text=~CAT,
                legendgroup="A") %>% 
            layout(title = paste0("Total ", input$ty))
        
        if(input$CB){PlotA = PlotA %>% layout(yaxis = list(type="log"))}
        
        # Create Plot B: Country Cases vs Date
        PlotB = COVID19.long %>%
            subset(variable == input$ty) %>% 
            plot_ly(
            x=~date,
            y=~value, 
            type='scatter', 
            mode='lines', 
            color=~continent, 
            alpha=0.1, 
            height=600,
            text=~paste0(country,", ", continent),
            legendgroup="B",
            showlegend=FALSE
            # hoverinfo=~paste('<b>%{date}</b>','<br><i>Confirmed: <i> %{confirmed}')
        )
        
        if(!is.null(input$Ct)){PlotB = PlotB %>% 
            add_trace(
            data = COVID19.long %>% subset(variable == input$ty & country %in% input$Ct), 
            x=~date, 
            y=~value, 
            type='scatter', 
            mode='lines', 
            color=~country,
            # text=~country, ", ", format(confirmed, big.mark = ",")),
            showlegend=TRUE,
            # hoverinfo="text", 
            legendgroup="B",
            alpha=1)}
        
        if(input$CB){PlotB = PlotB %>% layout(yaxis = list(type="log"))}
        
        subplot(PlotA, PlotB, nrows = 2, shareX = TRUE)
        
    })
})
