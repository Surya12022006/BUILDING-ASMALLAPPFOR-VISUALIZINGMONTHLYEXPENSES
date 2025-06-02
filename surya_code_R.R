# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Define UI for the application
ui <- fluidPage(
 
  # Application title
  titlePanel("Monthly Expense Visualizer"),
 
  # Sidebar layout with input and output definitions
  sidebarLayout(
   
    # Sidebar panel for inputs
    sidebarPanel(
      textInput("category", "Expense Category:", ""),
      numericInput("amount", "Expense Amount:", value = 0, min = 0),
      actionButton("add", "Add Expense"),
      hr(),
      fileInput("file", "Upload CSV File:", accept = ".csv"),
      actionButton("load", "Load File"),
      hr(),
      downloadButton("downloadData", "Download Data as CSV"),
      hr(),
      actionButton("reset", "Reset Data")
    ),
   
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Expense Table", tableOutput("expenseTable")),
       
        tabPanel("Bar Chart", plotlyOutput("barChart")),
       
        tabPanel("Line Chart", plotlyOutput("lineChart")),
       
        tabPanel("Histogram", plotlyOutput("histogram")),
       
        tabPanel("Expense Summary", verbatimTextOutput("expenseSummary"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
 
  # Reactive values to store expenses data
  expenses <- reactiveVal(data.frame(Category = character(), Amount = numeric(), stringsAsFactors = FALSE))
 
  # Observe add button to add new expenses
  observeEvent(input$add, {
    req(input$category != "", input$amount > 0)
    new_expenses <- rbind(expenses(), data.frame(Category = input$category, Amount = input$amount))
    expenses(new_expenses)
  })
 
  # Observe reset button to clear the expenses data
  observeEvent(input$reset, {
    expenses(data.frame(Category = character(), Amount = numeric(), stringsAsFactors = FALSE))
  })
 
  # Observe file upload and load the data
  observeEvent(input$load, {
    req(input$file)
    file_data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    req(all(c("Category", "Amount") %in% names(file_data)))
    expenses(file_data)
  })
 
  # Render expense table
  output$expenseTable <- renderTable({
    expenses()
  })
 
  # Render bar chart visualization
  output$barChart <- renderPlotly({
    req(nrow(expenses()) > 0)
   
    plot_data <- expenses()
    plot <- ggplot(plot_data, aes(x = Category, y = Amount, fill = Category)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Monthly Expenses - Bar Chart", x = "Category", y = "Amount")
   
    ggplotly(plot)
  })
 
  # Render line chart visualization
  output$lineChart <- renderPlotly({
    req(nrow(expenses()) > 0)
   
    plot_data <- expenses()
    plot_data$Index <- seq_len(nrow(plot_data))
    plot <- ggplot(plot_data, aes(x = Index, y = Amount, group = 1, color = Category)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = "Monthly Expenses - Line Chart", x = "Index", y = "Amount")
   
    ggplotly(plot)
  })
 
  # Render histogram visualization
  output$histogram <- renderPlotly({
    req(nrow(expenses()) > 0)
   
    plot_data <- expenses()
    plot <- ggplot(plot_data, aes(x = Amount)) +
      geom_histogram(binwidth = 10, fill = "blue", color = "white") +
      theme_minimal() +
      labs(title = "Monthly Expenses - Histogram", x = "Amount", y = "Frequency")
   
    ggplotly(plot)
  })
 
  # Render expense summary (descriptive statistics)
  output$expenseSummary <- renderPrint({
    req(nrow(expenses()) > 0)
   
    expense_data <- expenses()
   
    summary_stats <- expense_data %>%
      summarise(
        Total_Amount = sum(Amount),
        Average_Amount = mean(Amount),
        Max_Expense = max(Amount),
        Min_Expense = min(Amount)
      )
   
    print("Expense Summary:")
    print(summary_stats)
  })
 
  # Download data as CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("expenses", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(expenses(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)