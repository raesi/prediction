library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(tidyr)




dir()
options(scipen  = 100)

# read in the data from the input folder
sales_train <- fread("input/sales_train_v2.csv")
items_translated <- fread("input/items-translated.csv")
items <- fread("input/items.csv")
items_categories <- fread("input/item_categories-translated.csv")
shops <- fread("input/shops-translated.csv")


items <- items[,-1]
sales_train$date = as.Date(sales_train$date,format = "%d.%m.%Y")
sales_train <- sales_train  %>%
          mutate(year = year(date)) %>%
          mutate(month = month(date)) %>%
          left_join(items) %>%
          left_join(items_categories) %>%
          left_join(items_translated) 

sum(is.na(sales_train)) #0

sales_train <- tidyr::separate(sales_train , item_category_name_translated , 
                               into = c('product_group','product_line'),sep = '-',remove = FALSE ,extra = "merge")%>%
                               dplyr::filter(!product_group %in% c("ÐšÐ½Ð¸Ð³Ð¸ ","PC ","MAC Games ","ÐšÐ½Ð¸Ð³Ð¸ ")) %>%
                               dplyr::filter(!item_id %in% c(22110,10696,8435))
          


unique(sales_train$item_category_name_translated)
# number of unique items 
length(unique(sales_train$item_id))
# number of active items since July 2015
length(unique(sales_train[sales_train$date > "2015-07-01",]$item_id))

# # explore one of the items
# df_5037 <- sales_train[sales_train$item_id == 5037,]
# df_5037 <- df_5037 %>% 
#                     mutate(shop_id = as.factor(shop_id)) %>%
#                     group_by(date_block_num,shop_id,item_id,date) %>%
#                     summarise(item_cnt_day = sum(item_cnt_day)) %>%
#                     filter(shop_id== 25)
#                  
# 
# df_5037 <- df_5037[-3]
# 
# ggplot(data = df_5037 , aes(x = date,y = item_cnt_day)) + 
# geom_line()
# 
# # explore total data
# df_total <- sales_train %>%
#           group_by(date_block_num,shop_id,item_id) %>%
#           summarise(item_cnt_day = sum(item_cnt_day)) 
# 
# 
# 
#           
# ggplot(data = df_total , aes(x = date_block_num , y = item_cnt_day )) + 
#           geom_col() + 
#           scale_x_continuous(breaks =  unique(sales_train$date_block_num))
# 
# ggplot(data = df_total , aes(x = shop_id , y = item_cnt_day )) + 
#           geom_col() + 
#           scale_x_continuous(breaks =  unique(sales_train$shop_id))



#################
#' Aggregate dataset only by month
#' 
#' @param dt data.table
#' @param minMonth
#' @param maxMonth
#' @return data.table
#'
groupByMonth <- function(dt, minMonth, maxMonth ) {
       
          result <- dt %>% filter(date_block_num >= minMonth, date_block_num <= maxMonth) %>%
                    group_by(date_block_num) %>%
                    summarise(total_sales = sum(item_cnt_day))
          
          return(result)
}


#' Plot number of sets by year
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel number of sets
#' @return setsByYear plot
plotSetsCountByYear <- function(dt, dom = "setsByYear", 
                                xAxisLabel = "Month",
                                yAxisLabel = "Sales by Unit ") {
          setsByYear <- nPlot(
                    total_sales ~ date_block_num,
                    data = dt,
                    type = "stackedAreaChart",
                    dom = dom, width = 650
          )
          setsByYear$chart(margin = list(left = 100))
          setsByYear$chart(color = c('purple', 'blue', 'green'))
          setsByYear$chart(tooltipContent = "#! function(key, x, y, e){ 
                   return '<h5><b>Month</b>: ' + e.point.date_block_num + '<br>' + '<b>Total Sales</b>: ' 
                   + e.point.total_sales + '<br>'
                   + '</h5>'
} !#")
          setsByYear$yAxis(axisLabel = yAxisLabel, width = 80)
          setsByYear$xAxis(axisLabel = xAxisLabel, width = 70)
          setsByYear 
}
#' Aggregate dataset product group
#' 
#' @param dt data.table
#' @param minMonth
#' @param maxMonth
#' @return data.table
#'
groupByProductGroup <- function(dt, minMonth, maxMonth ) {
          
          result <- dt %>% filter(date_block_num >= minMonth, date_block_num <= maxMonth) %>%
                    group_by(product_group) %>%
                    summarise(total_sales = sum(item_cnt_day))
          
          return(result)
}

#' Plot sales by product group
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel number of themes
#' @return themesByYear plot
plotThemesCountByYear <- function(dt, dom = "themesByYear", 
                                  xAxisLabel = "Product Group",
                                  yAxisLabel = "Sales") {
          themesByYear <- nPlot(
                    total_sales ~ product_group,
                    data = dt,
                    type = "multiBarChart",
                    dom = dom, width = 650 
          )
          themesByYear$chart(margin = list(left = 100))
          themesByYear$yAxis(axisLabel = yAxisLabel, width = 80)
          themesByYear$xAxis(axisLabel = xAxisLabel, width = 80, rotateLabels = -70, height = 400)
          themesByYear$chart(tooltipContent = "#! function(key, x, y, e){ 
                     return '<h5><b>Product Group</b>: ' + e.point.product_group + '<br>' + '<b>Total Sales</b>: ' + e.point.total_sales + '<br>'
                     + '</h5>'
} !#")
          themesByYear
}

#' Aggregate dataset by shop id
#' 
#' @param dt data.table
#' @param minMonth
#' @param maxMonth
#' @return data.table
#'
groupByShopid <- function(dt, minMonth, maxMonth ) {
          
          result <- dt %>% filter(date_block_num >= minMonth, date_block_num <= maxMonth) %>%
                    group_by(shop_id) %>%
                    summarise(total_sales = sum(item_cnt_day))
          
          return(result)
}

#' Plot sales by product group
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel number of themes
#' @return shopesByYear plot
plotsalesbyshops <- function(dt, dom = "shopesByYear", 
                                  xAxisLabel = "Shop Number",
                                  yAxisLabel = "Sales") {
          shopesByYear <- nPlot(
                    total_sales ~ shop_id,
                    data = dt,
                    type = "multiBarChart",
                    dom = dom, width = 650 
          )
          shopesByYear$chart(margin = list(left = 100))
          shopesByYear$yAxis(axisLabel = yAxisLabel, width = 80)
          shopesByYear$xAxis(axisLabel = xAxisLabel, width = 80, rotateLabels = -70, height = 400)
          shopesByYear$chart(tooltipContent = "#! function(key, x, y, e){ 
                     return '<h5><b>Shop Number</b>: ' + e.point.shop_id + '<br>' + '<b>Total Sales</b>: ' + e.point.total_sales + '<br>'
                     + '</h5>'
} !#")
          shopesByYear
}

groupByShopidByItem <- function(dt, minMonth, maxMonth ) {
          
          result <- dt %>% filter(date_block_num >= minMonth, date_block_num <= maxMonth) %>%
                    group_by(item_id,shop_id) %>%
                    summarise(total_sales = sum(item_cnt_day)) %>%
                    mutate(total_sales = as.numeric(total_sales))%>%
                    filter(total_sales>100)%>%
                    ungroup()
          
          return(result)
}

#' Plot number of pieces by year
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel number of pieces
#' @return plotPiecesByYear plot
plotPiecesByYear <- function(dt, dom = "piecesByYear", 
                             xAxisLabel = "Shop Number", 
                             yAxisLabel = "Sales") {
    
          piecesByYear <- nPlot(
                    total_sales ~ shop_id,
                    data = dt,
                    type = "scatterChart",
                    dom = dom, width = 650
          )
          piecesByYear$chart(margin = list(left = 100), 
                             showDistX = TRUE,
                             showDistY = TRUE)
          piecesByYear$chart(color = c('green', 'orange', 'blue'))
          piecesByYear$chart(tooltipContent = "#! function(key, x, y, e){ 
                     return '<h5><b>Set Name</b>: ' + e.point.name + '<br>'
                     + '<b>Set ID</b>: ' + e.point.setId + '<br>'
                     + '<b>Theme</b>: ' + e.point.theme
                     + '</h5>'
} !#")
          piecesByYear$yAxis(axisLabel = yAxisLabel, width = 80)
          piecesByYear$xAxis(axisLabel = xAxisLabel, width = 70)
          #     piecesByYear$chart(useInteractiveGuideline = TRUE)
          piecesByYear
    
}

avgShopCatgory <- function(dt, minMonth, maxMonth, shopId ,categoryId) {
          
          result <- dt %>% filter(date_block_num >= minMonth, date_block_num <= maxMonth) %>%
                    filter(shop_id == shopId,item_category_name_translated == categoryId)%>%
                    group_by(shop_id,item_category_name_translated,date_block_num) %>%
                    summarise(avg_sales = sum(item_cnt_day)) %>%
                    ungroup()
          str(result)
          return(result)
}

' Plot number of average pieces by year
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel number of pieces
#' @return themesByYear plot
plotPiecesByYearAvg <- function(dt, dom = "piecesByYearAvg", 
                                xAxisLabel = "Month",
                                yAxisLabel = "Sales") {
   
          piecesByYearAvg <- nPlot(
                    avg_sales ~ date_block_num,
                    data = dt,
                    type = "lineChart",
                    dom = dom, width = 650
          )
          piecesByYearAvg$chart(margin = list(left = 100))
          #piecesByYearAvg$chart(color = c('orange', 'blue', 'green'))
          piecesByYearAvg$yAxis(axisLabel = yAxisLabel, width = 80)
          piecesByYearAvg$xAxis(axisLabel = xAxisLabel, width = 70)
          piecesByYearAvg
}

