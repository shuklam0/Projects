library(words)
library(tidyverse)
library(shiny)

`%!in%` <- Negate(`%in%`)
L = 4       # min length of words

W = words::words
W$word = W$word %>% str_to_upper
W = W[W$word_length>=max(L,1),1] %>% unlist

ui <- fluidPage(
  titlePanel(title = "Spelling Bee Help", windowTitle = "Spelling Bee Help"),
  
  sidebarLayout(
    sidebarPanel(
      h1("Let's Solve Spelling Bee"),
      
      selectizeInput(inputId="L2", label=h3("Select Central Letter"), LETTERS, selected=NULL, multiple=FALSE, options=NULL),
      selectizeInput(inputId="L1", label=h3("Choose 6 Other Letters"), LETTERS, selected=NULL, multiple=TRUE, options=NULL)
      ),
    
    mainPanel(
      h2("List of Filtered Words"),
      textOutput("OUTS")
      )
    )
  )

server <- function(input, output, session) {
  
      output$OUTS <- renderText({
        L3 = LETTERS[which(LETTERS %!in% c(input$L1,input$L2))]
        W = W[!grepl(paste0(L3, collapse = '|'), W, ignore.case = TRUE)]
        W = W[grepl(input$L2, W, ignore.case = TRUE)]
        W
      })

}

# Run the application 
shinyApp(ui = ui, server = server)