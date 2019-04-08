#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(tidyr)
md = read.csv('./Predicted_Values.csv')
# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Theme
  theme = shinytheme('cerulean'),
  
  # Application title
  titlePanel("Canadian 7s Rugby Readiness Index"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Date",
                  label = "Date:",
                  choices = md$Date,
                  selected = "2018-01-27")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot") %>% withSpinner(),
      tableOutput("distTable")
    )
)))
