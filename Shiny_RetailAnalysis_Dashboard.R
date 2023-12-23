library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)
library(magrittr)
library(scales)
library(DT)
library(tinytex)
library(knitr)

combined_df <- read_csv("sales_final.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Retail Sales Performance"),
  
  dashboardSidebar(
    dateRangeInput('dateRange',
                   label = 'Date Range',
                   start = as.Date('2011-10-11'), end = as.Date('2012-09-03')),
    radioButtons("GraphType", "Retail Sales Graph Type:",
                 c("Cumulative" = "cum",
                   "Daily" = "day",
                   "Logarithmic" = "Log_Graph")
    ),
    sliderInput(inputId = "bins",
                label = "Bin Size of Histogram:",
                min = 1,
                max = 5,
                value = 1)
  ),
  
  dashboardBody(
    fluidRow(
      valueBoxOutput("Retail_fig"),
      valueBoxOutput("AOV_fig"),
      valueBoxOutput("A30_user")
    ),
    
    fluidRow(
      box(
        title = "Retail Sales Value ", background = "maroon", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
        plotlyOutput("Retail_Plot", height = 200))
    ),
    fluidRow(
      box(
        title = "Product by Sub Categories", background = "olive", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
        column(12, (DT::dataTableOutput("topCat")))
      ),
      box(
        title = "Days to Delivery", background = "navy", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
        plotlyOutput("deliveryTime"),
      ), 
    ),
  fluidRow(
      box(
        title = "Margin by Priority of Order", background = "olive", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
        plotlyOutput("ProfitMargin")
      ),
      box(
        title = "Region wise Sales of Product", background = "navy", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
        plotlyOutput("regionwisesales"),
      ), 
    )
  )
)

server <- function(input, output){
  
  
  delivered_df <- combined_df %>%
    filter(order_status == "delivered")
  
  
  output$Retail_fig <- renderValueBox({
    deliveredToDate <- delivered_df %>%
      filter(Senddate <= input$dateRange[2] & Senddate >= input$dateRange[1])
    valueBox(
      paste0("$", prettyNum(ceiling(sum(deliveredToDate$SaleValue)), big.mark = ",")),
      "Total Retail Sales",
      color = "teal",
      icon = icon("dollar-sign"))
  })
  
  output$AOV_fig <- renderValueBox({
    deliveredToDate <- delivered_df %>%
      filter(Senddate <= input$dateRange[2] & Senddate >= input$dateRange[1])
    valueBox(
      paste0("$", round(sum(deliveredToDate$SaleValue)/length(unique(deliveredToDate$CustomerID)),digits = 2)),
      "Average Order Value",
      color = "green",
      icon = icon("shopping-basket")
    )
    
  })
  
  output$A30_user <- renderValueBox({
    UserToDate <- combined_df %>%
      filter(Senddate <= input$dateRange[2] & Senddate >= (input$dateRange[2] - 30))
    valueBox(
      prettyNum(length(unique(UserToDate$OrderID)),big.mark = ","),
      "Customers",
      color = "light-blue",
      icon = icon("user-alt")
    )
    
  })
  
  
  output$Retail_Plot <- renderPlotly({
    
    dailyRetail <- delivered_df %>%
      filter(Senddate <= input$dateRange[2] & Senddate >= input$dateRange[1]) %>%
      group_by(Senddate) %>%
      summarize(price_sum = sum(SaleValue))
    
    
    if (input$GraphType %in% c("cum","Log_Graph")) {
      Retail <- cumsum(dailyRetail$price_sum)/1000
    }
    else if (input$GraphType == "day") {
      Retail <- (dailyRetail$price_sum)/1000
    }
    
    p <- ggplot(dailyRetail, aes(x= Senddate, y= Retail, group=1,
                              text = paste('Date : ', as.Date(Senddate),
                                           "<br>Retail: $ ", prettyNum(Retail * 1000, big.mark = ",")))) + 
      
      geom_line(color = "dodgerblue4") +
      (if (input$GraphType == "Log_Graph") { 
        scale_y_log10()}
      )+
      labs(x = "Date", y = "Retail ($'000)" ) +
      theme(plot.background = element_rect(fill = "ghostwhite"),
            panel.background = element_rect(fill = "ghostwhite"))
    
    ggplotly(p, tooltip = c("text"))
    
  })
  
  output$GSTotalBalanceLineHc <- renderPlotly({
    
    deliverydays <- delivered_df %>%
      filter(Senddate <= input$dateRange[2] & Senddate >= input$dateRange[1])
    
    p <- ggplot(deliverydays, aes(x = DaystoDelivery)) +
      geom_histogram(binwidth = input$bins)+
      xlim(0,10) + 
      labs(x = "Days to Delivery", y = "# of Orders" ) +
      theme(plot.background = element_rect(fill = "ghostwhite"),
            panel.background = element_rect(fill = "ghostwhite"))
    
    ggplotly(p)
    
  })
  
  
  output$deliveryTime <- renderPlotly({
    
    deliverydays <- delivered_df %>%
      filter(Senddate <= input$dateRange[2] & Senddate >= input$dateRange[1])
    
    p <- ggplot(deliverydays, aes(x = DaystoDelivery)) +
      geom_histogram(binwidth = input$bins)+
      xlim(0,10) + 
      labs(x = "Days to Delivery", y = "# of Orders" ) +
      theme(plot.background = element_rect(fill = "ghostwhite"),
            panel.background = element_rect(fill = "ghostwhite"))
    
    ggplotly(p)
  })
  
  
  output$topCat <- DT::renderDataTable(
    
    (dailyRetailbyCat <- delivered_df %>%
       filter(Senddate <= input$dateRange[2] & Senddate >= input$dateRange[1]) %>%
       group_by("Category" = ProductSubCategory) %>%
       summarize("Retail ($ '000s)" = ceiling(sum(SaleValue)/1000))),
    rownames = FALSE,
    options = list(lengthMenu = c(5,10,15), pageLength =5)
  )
  
  
  output$regionwisesales <- renderPlotly({
    
    (regionsales <- delivered_df %>%
      filter(Senddate <= input$dateRange[2] & Senddate >= input$dateRange[1]) %>%
       group_by("Category" = ProductSubCategory))
    
    p <- ggplot(regionsales, aes(x = Region)) +
      geom_bar(aes(fill = ProductSubCategory), position = position_stack(reverse = TRUE)) +
      theme(legend.position = "top") + 
      labs(x = "Regions", y = "# of Orders" ) +
      theme(plot.background = element_rect(fill = "ghostwhite"),
            panel.background = element_rect(fill = "ghostwhite"))
    
    ggplotly(p)
  })
  
  output$ProfitMargin  <- renderPlotly({
    
    (Profitsales <- delivered_df %>%
      filter(Senddate <= input$dateRange[2] & Senddate >= input$dateRange[1])%>%
       group_by("City" = City))
    
    p <- ggplot(Profitsales, aes(x = Margin, y= Priority)) +
      geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
      theme(legend.position = "top") + 
      labs(x = "Margin per of Orders", y = "Priority of Order" ) +
      theme(plot.background = element_rect(fill = "ghostwhite"),
            panel.background = element_rect(fill = "ghostwhite"))
    
    ggplotly(p)
  })
  
}
shinyApp(ui, server)