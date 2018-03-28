library(shiny)
library(tidyverse)
library(ggfortify)
library(forecast)

## Given Data
flights <- read.csv('data/flights.csv.gz')
arr_delay <- flights %>% arrange(FL_DATE) %>% group_by(FL_DATE) %>% summarise(mean(ARR_DELAY, na.rm = TRUE))
names(arr_delay) <- c('FL_DATE', 'AVE_DELAY')
data(AirPassengers)

## Data For Assignment 
data <- read.csv("2017FL_performance.csv")

data %>% drop_na()

data %>% 
  select(DAY_OF_MONTH,ARR_DELAY,DEP_DELAY,CANCELLED) %>% 
  group_by(DAY_OF_MONTH) %>%
  summarise(avg_dep_delay = mean(DEP_DELAY, na.rm = TRUE),
            avg_arr_delay = mean(ARR_DELAY, na.rm = TRUE),
            avg_cancel = mean(CANCELLED))



colnames(data)[1] <- "days"


ui <- fluidPage(
  fluidRow(
    column(6,
           plotOutput("plot1", click = "plot1_click")
    ),
    column(5,
           br(), br(), br(),
           htmlOutput("x_value"),
           verbatimTextOutput("selected_rows")
    )),
  fluidRow(
    column(6,
           plotOutput("plot2", click = "plot2_click")
    ),
    column(5,
           br(), br(), br(),
           radioButtons("decomp", "Plot type:",
                        c("Observed" = "obs",
                          "Trend" = "trend",
                          "Seasonal" = "seas",
                          "Random" = "rand"))
    )),
  mainPanel(
    tabsetPanel(type = "tabs",
      tabPanel("Starter Code"
               ),
      tabPanel("New Addition")
    )
  )
  
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    autoplot(as.ts(arr_delay$AVE_DELAY)) + xlab("Day of the Year") + ylab("Average Delay") + ggtitle("Arrival Delays in Florida (2017)")
  })
  
  # Print the name of the x value
  output$x_value <- renderText({
    if (is.null(input$plot1_click$x)) return("")
    else {
      #lvls <- levels(ToothGrowth$supp)
      #name <- lvls[round(input$plot1_click$x)]
      HTML("You've selected day <code>", round(input$plot1_click$x), "</code>",
           "<br><br>Here is more information about that day:")
    }
  })
  
  # Print the rows of the data frame which match the x value
  output$selected_rows <- renderPrint({
    if (is.null(input$plot1_click$x)) return()
    else {
      as.data.frame(arr_delay[round(input$plot1_click$x),])
    }
  })
  
  output$plot2 <- renderPlot({
    ts_air = ts(AirPassengers, frequency = 12)
    decompose_air = decompose(ts_air, "multiplicative")
    plot_type <- switch(input$decomp,
                        obs = ts_air,
                        trend = as.ts(decompose_air$trend),
                        seas = as.ts(decompose_air$seasonal),
                        rand = as.ts(decompose_air$random),
                        ts_air)
    plot(plot_type, ylab="Passengers (Thousands)", xlab="Time (Month)")
  })
  
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
    
    mx <- t(as.matrix(result[-1]))
    colours = c("red","blue") 
    
    ggplot(data = result,aes(x = factor(type), y = avg_cancel)) + geom_bar(stat = "identity")

    print(result)   
    g<-ggplot(data = result,aes(x = factor(type), y = (avg_cancel*100))) + 
      geom_bar(stat = "identity") + 
      ggtitle("Percentage of Flights Cancelled for a given day vs to Average Monthly Cancel Rate")  + 
      ylab("Cancelation Percentage") +
      xlab("Dynamic = Selected Day, Static = Monthly Rate")
    
    g
}

shinyApp(ui, server) 
