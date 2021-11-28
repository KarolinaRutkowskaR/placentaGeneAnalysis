library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)

data <- read.csv("Placenta_data.csv", sep = ";")
GBChoices <- as.list(names(data))
names(GBChoices) <- paste(names(data),map(data,~length(unique(.x))))

ui <- fluidPage(h1("Placenta gene analysis", align = "center"),
  theme = shinytheme("sandstone"),
  sidebarLayout(
    sidebarPanel(
     selectInput(
       inputId = "gene", 
       label = "Gene type", 
       choices = GBChoices
     ),
     selectInput(
       inputId =  "placenta", 
       label = "Placenta type", 
       choices = names(select_if(data,is.numeric))
     )
    ),
    mainPanel(
     plotOutput("plot"),
     downloadButton(
       outputId = "mydownload", 
       label = "Download Table", 
       style="margin: 16px 0; background: lightgrey; padding: 8px; color: #000"
     ),
     dataTableOutput("datatable")
    )
  )
)

server <- function(input, output) {
  updateData <- reactive(
    data %>% group_by(!!! rlang::syms(input$gene)) %>% summarise_if(is.numeric, sum)
  )
  
  output$plot <- renderPlot({ updateData() %>% 
    ggplot(aes(x=!! rlang::sym(input$gene), y=!! rlang::sym(input$placenta), fill=!! rlang::sym(input$gene))) +
    geom_col()}
  )

  output$mydownload = downloadHandler(
    filename = "Placenta.csv",
    content = function(file) {
      write.csv(updateData(), file)
    }
  )
  
  output$datatable <- DT::renderDataTable(updateData())
}

shinyApp(ui = ui, server = server)
