#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
md = read.csv('./Predicted_Values.csv') 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$distPlot <- renderPlot({
    date = input$Date
    data_select = md[which(md$Date == date),]
    data_select$ReadinessIndex = 100*(data_select$values + 3.890635)/(5.249426+3.890635)
    
    p <- ggplot(data_select, aes(x=as.factor(PlayerID), y=ReadinessIndex, fill=ReadinessIndex)) +
      scale_fill_gradient(low = "Red", high = "Green") +
      geom_bar(stat="identity") + 
      ggtitle('Player Readiness Index') +
      xlab('PlayerID') +
      ylab('Readiness Index Score') +
      theme(plot.title = element_text(hjust = 0.5))
    
    p
  })
  
  output$distTable <- renderTable({
    date = input$Date
    data_select = md[which(md$Date == date),]
    data_select$ReadinessIndex = 100*(data_select$values + 3.890635)/(5.249426+3.890635)
    
    data_select[,c('PlayerID', 'Date', 'ChronicLoad','FocusRating','AAAI_t1','AcuteLoad','DailyLoad','ReadinessIndex')]
  })
  
})
