
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

# Load data processing file
source("main.R")

shinyServer(function(input, output) {

  dataTableBySetYear <- reactive({
    groupByMonth(sales_train, input$timeline[1], 
                   input$timeline[2])
  })
  
  output$setsByYear <- renderChart({
    plotSetsCountByYear(dataTableBySetYear())
  })
  
  dataTableByProductGroup <- reactive({
    groupByProductGroup(sales_train, input$timeline[1],
                        input$timeline[2])

  })
  
  output$themesByYear <- renderChart({
    plotThemesCountByYear(dataTableByProductGroup())
  })
  
  dataTableByshopid <- reactive({
    groupByShopid(sales_train, input$timeline[1],
                        input$timeline[2])
    
  })
  
  output$shopesByYear <- renderChart({
    plotsalesbyshops(dataTableByshopid())
  })

  dataTableByshopid1 <- reactive({
    groupByShopidByItem(sales_train, input$timeline[1],
                  input$timeline[2])
    
  })
  
  output$piecesByYear <- renderChart({
  
    plotPiecesByYear(dataTableByshopid1())
  })
  
  
  dataTableByShopCategoryId <- reactive({
    avgShopCatgory(sales_train, input$timeline[1],
                        input$timeline[2],input$shopid,input$category)
    
  })
  
  output$piecesByYearAvg <- renderChart({
    
    plotPiecesByYearAvg(dataTableByShopCategoryId())
  })

})
