library(shiny)
library(tidyverse)

ui <- navbarPage(

  theme = "yeti",
  tags$title(" q"),

  div(
    tags$header(p(" Liquor Sales", style="font-size:40px"),
                p("group 6", style="font-size:30px")),
    align = "center", style="color:#ffffff; background-color: #4d728d"),

  tabPanel("Data Import",
           sidebarLayout(
             sidebarPanel(
               fileInput("file","Upload your CSV File", multiple = FALSE)
               ),
             mainPanel(uiOutput("tb1")))),

          tabPanel("Time",
                    sidebarLayout(sidebarPanel(
                      selectInput("storenames", "Store Names",
                                  choices = c("z","b","c"), selected = "z"),
                      selectInput("category", "Category",
                                  choices = c("z","b","c"), selected = "z")),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Time Plot", plotOutput("timeplot"))
                         )))),

  tabPanel("Sptial",
           sidebarLayout(sidebarPanel(
             selectInput("year", "Years",
                         choices = c("z","b","c"), selected = "z"),
             selectInput("category", "Category",
                         choices = c("z","b","c"), selected = "z")),
             mainPanel(
               tabsetPanel(
                 tabPanel("Bar Plot", plotOutput("barplot"))
                 ))))
)



server <- function(input, output) {

  df <- reactive({
    inFile <- input$file

    if (is.null(inFile))
      return(NULL)

    read.csv(inFile)

  })

  output$table <- renderTable({
    if(is.null(df())){return ()}
    df()
  })
  output$tb1 <- renderUI({
    tableOutput("table")
  })

  df1 <- reactive({df()%>%
      filter( StoreName== input$storenames, Category== input$category )})

  df2 <- reactive({df()%>%
      filter( Year== input$year, Category== input$category )})

  output$timeplot <- renderPlot({
    ggplot(data = df1(), aes(x = Date, y = Sale (Dollars))) +
      geom_line() +
      theme_bw() +
      ggtitle("Times and Sales for", input$category, "in year of ", input$year)
    })

  output$barplot <- renderPlot({
    ggplot(data = df2(), aes(x = input$category, fill = input$year, weight=Sale (Dollars))) +
      geom_bar(stat = "count") +
      theme_bw() +
      ggtitle(paste("In Year of ", input$year, " liquor is ", input$category))
  })
}


shinyApp(ui = ui, server = server)

