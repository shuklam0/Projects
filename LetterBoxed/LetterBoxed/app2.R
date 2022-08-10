library(shiny)
library(shinydashboard)
library(words)
library(tidyverse)

`%!in%` <- Negate(`%in%`)
L=3 # min length
W = words::words
W = W[W$word_length>=max(L,1),1] %>% str_to_upper %>% unlist

L5 = c('S', 'T', 'M', 'E', 'W', 'I', 'C', 'D', 'L', 'O', 'N', 'U')

Wrd_all <- function(W, L5){
  L5 = L5 %>% str_to_upper
  L6 = LETTERS[LETTERS %!in% L5]
  W = W[!grepl(paste0(L6, collapse = '|'), W, ignore.case = TRUE)] %>% str_to_upper
  
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
    
    if (Y) {
      W1 = c(Wrd, W1)
    }
  }
  
  WL1 = lapply(W1, Unq_chr) %>% unlist
  WL2 = lapply(W1, str_length) %>% unlist
  Wx = data.table::data.table(W1, WL1, WL2)
  Wx[, Wr := WL1/WL2]
  Wx = Wx[order(WL1, Wr, decreasing = TRUE),]
  
  W1 = unique(Wx$W1) %>% unlist
  W1
}

Unq_chr = function(Wrd){
  Wrd %>% 
    str_split(pattern = "") %>% 
    unlist %>% 
    unique %>% 
    length
}

Wrd_next <- function(Wx, W=unlist(words::words$word)){
  Lx = Wx %>% str_split(pattern = "") %>% unlist %>% last
  W[str_sub(W, 1, 1)==Lx] %>% unlist
}

Wrd_prev <- function(Wx, W=unlist(words::words$word)){
  Lx = Wx %>% str_split(pattern = "") %>% unlist %>% first
  W[str_sub(W, -1, -1)==Lx] %>% unlist
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NYT Letter Boxed Help"),
    sidebarLayout(
      sidebarPanel(
    uiOutput("sidebarOutput"),
    uiOutput("sidebarOutput2")),
    mainPanel(
      uiOutput("mainPanel"))
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$sidebarOutput <- renderUI({
      fluidRow(
        box(width=12, title = h2("Input the Letters"),
            splitLayout(
              selectizeInput(inputId="L1", label=h4("TOP"), choices=LETTERS, selected=NULL, multiple=TRUE, options=NULL),
              selectizeInput(inputId="L2", label=h4("LEFT"), choices=LETTERS, selected=NULL, multiple=TRUE, options=NULL),
              selectizeInput(inputId="L3", label=h4("RIGHT"), choices=LETTERS, selected=NULL, multiple=TRUE, options=NULL),
              selectizeInput(inputId="L4", label=h4("BOTTOM"), choices=LETTERS, selected=NULL, multiple=TRUE, options=NULL),
            )))
    })
  
  output$sidebarOutput2 <- renderUI({
    # box(
      selectizeInput(inputId="Lx", label=h2("Select the First Word"), choices=Wrd_all(W, L5()), selected=NULL, multiple=FALSE, options=NULL)
        # footer = span(textOutput("message"), style="color:red"))
      # footer = paste0("<font color=\"#FF0000\"><b>", "Previous", "</b></font>", "<font color=\"#0000FF\"><b>", "Next", "</b></font>"))
    })
  
  L5 = reactive({c(input$L1, input$L2, input$L3, input$L4)})

    output$mainPanel <- renderText({
      W1 = Wrd_all(W, L5())
      if(!is.null(input$Lx)){
        Xb = Wrd_prev(input$Lx, Wrd_all(W, L5()))
        Xe = Wrd_next(input$Lx, Wrd_all(W, L5()))
        
        for(i in which(W1 %in% Xb)){
          W1[i] = paste0("<font color=\"#FF0000\"><b>", W1[i], "</b></font>")
        }
        
        for(j in which(W1 %in% Xe)){
          W1[j] = paste0("<font color=\"#0000FF\"><b>", W1[j], "</b></font>")
        }
      }
      
      paste0(W1, collapse = " ")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
