library(shiny)
library(ggplot2)

library(quantmod) 
nasdaq_stocks = stockSymbols(exchange="NASDAQ")
nyse_stocks = stockSymbols(exchange="NYSE")
amex_stocks = stockSymbols(exchange="AMEX")
combined_array = rbind(nyse_stocks,nasdaq_stocks,amex_stocks)

library(dplyr)
fin_table=filter(combined_array, Sector=="Finance")
fin_table$MarketCap <- as.numeric(
  sub("\\$(\\d+(\\.\\d+)?)[A-Z]?", "\\1", fin_table$MarketCap)) * 
  ifelse(gsub("[^A-Z]", "", fin_table$MarketCap) == "M", 1e6,
         ifelse(gsub("[^A-Z]", "", fin_table$MarketCap) == "B", 1e9, 1.0))

##### SERVER #####

# Define server logic for random distribution application
server <- function(input, output) {
  
  fin_table2 = fin_table[sample(nrow(fin_table), 1052), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(fin_table2[, input$show_vars, drop = FALSE])
  })
  }

##### UI #####

ui <- fluidPage(
  title = "Examples of DataTables",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "fin_table"',
        checkboxGroupInput("show_vars", "Columns in fin_tables to show:",
                           names(fin_table), selected = names(fin_table))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("fin_table", DT::dataTableOutput("mytable1"))
      )
    )
  )
)


##### Run #####
shinyApp(ui = ui, server = server)
