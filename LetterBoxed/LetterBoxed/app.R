library(shiny)
library(shinydashboard)
library(words)
library(tidyverse)

`%!in%` <- Negate(`%in%`)
L=3 # min length

W = words::words
W$word = W$word %>% str_to_upper
W = W[W$word_length>=max(L,1),1] %>% unlist

Wrd_all <- function(W, L5){ 
  W1 = c()
  for (Wrd in W) {
    X = strsplit(Wrd,"") %>% unlist
    Y = TRUE
    Z = (match(X,L5)-1) %/% 3
    
    for (i in 2:length(Z)) {
      if (Z[i-1] == Z[i]) {
        Y = FALSE
        break
      }
    }
    
    if (Y == TRUE) {
      W1 = c(Wrd, W1)
    }
  }
  W1
}

Unq_chr = function(Wrd){
  W1 = str_split(Wrd, "") %>% unlist
  W2 = unique(W1)
  length(W2)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NYT LetterBoxed Help"),

    sidebarLayout(
        sidebarPanel(
          h2("Input the Letters"),
          
          fluidRow(
            box(width=12, 
            splitLayout(
            selectizeInput(inputId="L1", label=h4("TOP"), LETTERS, selected=NULL, multiple=TRUE, options=NULL),
            selectizeInput(inputId="L2", label=h4("LEFT"), LETTERS, selected=NULL, multiple=TRUE, options=NULL),
            selectizeInput(inputId="L3", label=h4("RIGHT"), LETTERS, selected=NULL, multiple=TRUE, options=NULL),
            selectizeInput(inputId="L4", label=h4("BOTTOM"), LETTERS, selected=NULL, multiple=TRUE, options=NULL),
            )),
            
            # selectizeInput(label=h3("Select the First Word"), inputId="output.Out1", choices=L5(), selected=NULL, multiple=FALSE, options=NULL)
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h2("List of all possible Words"),
          textOutput("Out1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  L5 = reactive({c(input$L1, input$L2, input$L3, input$L4)})
  L6 = reactive({LETTERS[which(LETTERS %!in% L5())]})
  # W = W[!grepl(paste0(L6, collapse = '|'), W, ignore.case = TRUE)]

    output$Out1 <- renderText({
      # W = W[!grepl(L6(), W, ignore.case = TRUE)]
      W = W[!grepl(paste0(L6(), collapse = '|'), W, ignore.case = TRUE)]
      
      W1 = Wrd_all(W, L5())
      WL1 = lapply(W1, Unq_chr) %>% unlist
      WL2 = lapply(W1, str_length) %>% unlist
      Wx = data.table::data.table(W1, WL1, WL2)
      Wx[, Wr := WL1/WL2]
      Wx = Wx[order(WL1, Wr, decreasing = TRUE),]
      unique(Wx$W1)

      # W1
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
