#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)


data <- read.csv("2017FL_performance.csv")

data %>% drop_na()

data %>% 
  select(DAY_OF_MONTH,ARR_DELAY,DEP_DELAY,CANCELLED) %>% 
  group_by(DAY_OF_MONTH) %>%
  summarise(avg_dep_delay = mean(DEP_DELAY, na.rm = TRUE),
            avg_arr_delay = mean(ARR_DELAY, na.rm = TRUE),
            avg_cancel = mean(CANCELLED))
            
      

colnames(data)[1] <- "days"
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2] 
    cancel <- data %>% 
      select(days,ARR_DELAY,DEP_DELAY,CANCELLED) %>% 
      group_by(days) %>%
      filter(days == input$days) %>%
      summarise(avg_cancel = mean(CANCELLED))
    
    cancel <- cbind("dynamic",cancel)
    colnames(cancel)[1] <- "type"
    
    total_avg <- data %>% 
      select(days,ARR_DELAY,DEP_DELAY,CANCELLED) %>% 
      summarise(avg_cancel = mean(CANCELLED))
    
    total_avg <- cbind(input$days,total_avg)
    colnames(total_avg)[1] <- "days"
    total_avg <- cbind("static",total_avg)
    colnames(total_avg)[1] <- "type"
    
    result <- rbind(cancel,total_avg)
    result <- data.frame(result)
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    #days <- seq(min(data$DAY_OF_MONTH), max(data$DAY_OF_MONTH), length.out = input$days +1) 
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    mx <- t(as.matrix(result[-1]))
    colours = c("red","blue") 
    
    ggplot(data = result,aes(x = factor(type), y = avg_cancel)) + geom_bar(stat = "identity")
    
    #barplot(mx, beside = TRUE,col = colours)
    print(result)   
    g<-ggplot(data = result,aes(x = factor(type), y = avg_cancel)) + geom_bar(stat = "identity")
    g
    
    
  })
  
})
