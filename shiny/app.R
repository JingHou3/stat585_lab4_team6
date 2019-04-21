library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(leaflet)

IA_Liquor <- read_csv("../data/story-sales.zip") %>% 
  mutate(Date = lubridate::mdy(Date))

store_info <- IA_Liquor %>% 
  group_by(`Store Number`) %>% 
  filter(grepl("\\(",`Store Location`)) %>% 
  summarise(`Store Name` = unique(`Store Name`)[1], `Store Location` = unique(`Store Location`)[1]) %>% 
  mutate(Lat = purrr::map_chr(`Store Location`, .f = function(x){x %>% 
      str_extract_all("(?<=\\()[-0-9.]{1,}") %>% 
      unlist()}) %>% 
        as.numeric(), 
      Lon = purrr::map_chr(`Store Location`, .f = function(x){x %>% 
          str_extract_all( "[-0-9.]{1,}(?=\\))") %>% 
          unlist()}) %>% 
        as.numeric())

ui <- navbarPage(

  theme = "yeti",
  tags$title(" "),

  div(
    tags$header(p(" Liquor Sales in Iowa", style="font-size:40px"),
                p("group 6", style="font-size:30px")),
    align = "center", style="color:#ffffff; background-color: #4d728d"),

          tabPanel("Temporal",
                    sidebarLayout(sidebarPanel(
                      selectInput("storename", "Store Name",
                                  choices = sort(unique(IA_Liquor$`Store Name`)), selected = "Hy-Vee  #2 / Ames"),
                      selectInput("catgry_temp", "Category",
                                  choices = sort(unique(IA_Liquor$`Category Name`)), selected = "TENNESSEE WHISKIES")),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Temporal Plot", plotOutput("temporal_plot"))
                         )))),

  tabPanel("Sptial",
           sidebarLayout(sidebarPanel(
             selectInput("year", "Year",
                         choices = 2012:2019, selected = 2018),
             selectInput("catgry_sp", "Category",
                         choices = sort(unique(IA_Liquor$`Category Name`)), selected = "TENNESSEE WHISKIES")),
             mainPanel(
               tabsetPanel(
                 tabPanel("Spatial Plot", leaflet::leafletOutput("leaflet_plot"))
                 ))))
)



server <- function(input, output) {
  store_id <- reactive({
    IA_Liquor$`Store Number`[which(IA_Liquor$`Store Name` == input$storename)[1]]
  })
  catgry_temp_id <- reactive({
    IA_Liquor$Category[which(IA_Liquor$`Category Name` == input$catgry_temp)[1]]
  })
  
  catgry_sp_id <- reactive({
    IA_Liquor$Category[which(IA_Liquor$`Category Name` == input$catgry_sp)[1]]
  })

  Temporal_subset <- reactive({
    IA_Liquor %>% 
      filter(`Category` == catgry_temp_id()) %>% 
      filter(`Store Number` == store_id()) %>% 
      arrange(Date) %>% 
      group_by(Date) %>% 
      summarise(`Sale (Dollars)` = `Sale (Dollars)` %>% sum(.,na.rm = T))
  })
  
  Spatial_subset <- reactive({
    IA_Liquor %>% 
      filter(Category == catgry_sp_id()) %>% 
      filter(Date %>% lubridate::year() == input$year) %>% 
      group_by(`Store Number`) %>% 
      summarise( `Sale (Dollars)` = `Sale (Dollars)` %>% sum(.,na.rm = T)) %>% 
      dplyr::right_join(store_info) %>% 
      filter(!is.na(`Sale (Dollars)`))
  })

  output$temporal_plot <- renderPlot({
    ggplot(data = Temporal_subset(), aes(x = Date, y = `Sale (Dollars)`)) +
      geom_line() +
      theme_bw() +
      scale_x_date(date_labels= "%y-%m-%d", breaks = "3 month") +
      ggtitle(paste0("Sales for ", input$catgry_temp, " in ", input$storename, sep="")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

  output$leaflet_plot <- leaflet::renderLeaflet({
    leaflet::leaflet(data = Spatial_subset()) %>% leaflet::addTiles() %>%
      leaflet::addCircleMarkers(~ Lon, ~ Lat, popup = ~paste("Store Name: ", Spatial_subset()$`Store Name`, "<br>",
                                                            "Sale (Dollars): ", Spatial_subset()$`Sale (Dollars)`, "<br>"), 
                                clusterOptions = leaflet::markerClusterOptions())
  })
}


shinyApp(ui = ui, server = server)

