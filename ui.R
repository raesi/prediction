
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rCharts)

shinyUI(fluidPage(

  # Application title
  titlePanel("Sales Dashboard"),

tabPanel("Explore the Data",
         sidebarPanel(
           sliderInput("timeline", 
                       "Timeline:", 
                       min = 0,
                       max = 33,
                       value = c(0, 33),
                       sep=""),
           selectInput("shopid", "ShopId", sort(unique(sales_train$shop_id)), selected = NULL, multiple = FALSE,
                       selectize = TRUE, width = NULL, size = NULL),
           selectInput("category", "Item Category", sort(unique(sales_train$item_category_name_translated)), selected = NULL, multiple = FALSE,
                       selectize = TRUE, width = NULL, size = NULL)
         )
      ,
mainPanel(
  tabsetPanel(
  tabPanel(
    p("Main Dashboard"),
           h4('Sales Unit by Month', align = "center"),
           h5('Please hover over each point to see the Month and Total Unit of Sales.', 
              align ="center"),
           showOutput("setsByYear", "nvd3"),
           h4('Sales Unit by Product Group', align = "center"),
           h5('Please hover over each point to see the ProductGroup and Total Unit of Sales.', 
              align ="center"),
           showOutput("themesByYear", "nvd3"),
           h4('Sales Unit by Shop Number', align = "center"),
           h5('Please hover over each point to see the Shop Number and Total Unit of Sales.', 
              align ="center"),
           showOutput("shopesByYear", "nvd3"),
           h4('Sales Unit per Item by Shop', align = "center"), 
           showOutput("piecesByYear", "nvd3")),
    tabPanel( p(icon("line-chart"), "Visualize the Data"),
              h4('Sales Unit by Month Group by Shop and Category', align = "center"),
              showOutput("piecesByYearAvg", "nvd3") )
  )
))

))
