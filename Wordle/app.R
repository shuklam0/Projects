######### Function

library(shiny)
library(tidyverse)
library(words)


Wordle_remove <- function(Wrds, x){
  if(length(x)>0){
    x = x %>% str_to_upper
    Wrds = str_to_upper(Wrds)
    x = str_replace_all(x, "[^[:alnum:]]", "")
    x = str_split(x,'') %>% unlist
    x = paste0(x, collapse = '|')
    Wrds = Wrds[!grepl(x, Wrds, ignore.case = TRUE)]
  }
  Wrds
}

Wordle_keep <- function(Wrds, x){
  if(length(x)>0){
    x = x %>% str_to_upper
    Wrds = str_to_upper(Wrds)
    x = str_replace_all(x, "[^[:alnum:]]", "")
    x = str_split(x,'') %>% unlist
    for (i in x){
      Wrds = Wrds[grepl(i, Wrds, ignore.case = TRUE)]
    } 
  }
  Wrds
}

Wordle_pos <- function(Wrds, x){
  x = x %>% str_to_upper
  Wrds = str_to_upper(Wrds)
  Wrds = str_to_upper(Wrds)
  x = str_replace_all(x, "[^[:alnum:]]", "")
  x = str_split(x,'') %>% unlist
  i = 1
  while (i < length(x)){
    x[i+1] = as.integer(x[i+1])
    Wrds = Wrds[substr(Wrds, x[i+1], x[i+1])==x[i]]
    i=i+2
  }
  Wrds
}

Wordle_pos_not <- function(Wrds, x){
  x = x %>% str_to_upper
  Wrds = str_to_upper(Wrds)
  x = str_replace_all(x, "[^[:alnum:]]", "")
  x = str_split(x,'') %>% unlist
  i = 1
  while (i < length(x)){
    x[i+1] = as.integer(x[i+1])
    Wrds = Wrds[substr(Wrds, x[i+1], x[i+1])!=x[i]]
    i=i+2
  }
  Wrds
}

############# Page

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Wordle Help"),

    # Sidebar with a list of resulting words 
    sidebarLayout(
      
      sidebarPanel(
        h2("Input the Letters"),
        selectizeInput("keep", "Letters to Keep", LETTERS, selected = NULL, multiple = TRUE, options = NULL),
        selectizeInput("remove", "Letters to Remove", LETTERS, selected = NULL, multiple = TRUE, options = NULL),
        selectizeInput("pos", "Letters with positions", c(LETTERS, seq(1:5)), selected = NULL, multiple = TRUE, options = NULL),
        selectizeInput("pos_not", "Letters not in positions", c(LETTERS,seq(1:5)), selected = NULL, multiple = TRUE, options = NULL)
      ),
      
      mainPanel(
        h2("List of Filtered Words"),
        textOutput("Words_filtered")
      )

    )
)


######### Server


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$Words_filtered <- renderText({
      as.vector(words::words[words$word_length==5,]$word) %>% str_to_upper %>% 
        Wordle_keep(input$keep) %>% 
        Wordle_remove(input$remove) %>% 
        Wordle_pos(input$pos) %>% 
        Wordle_pos_not(input$pos_not)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
