library(tidyverse)
library(shiny)
library(rsconnect)
library(plotly)
library(viridis)
library(dplyr)


dataset <- fifa1

# Create  UI for the Shiny app
ui <- fluidPage(
  # Create Title and Introduction to the plots
  titlePanel("Comparisons Within FIFA 19 Dataset"),
  helpText("This Shiny app gives the user the fluidity of comparing some important aspects of the FIFA 19 Data but mostly
            on the Club Level (Individual Player Statistics cannot be seen).
           The variables being compared in this plot are the Age group of the players, 
           their Overall and Potential Ratings, their market Value in the current transfer window, 
           their weekly Wages, the League they play in (Europe top 5) and the Position they play on the field "),
  
  # Create a panel to chose different sample sizes of players
  sidebarPanel( 
    helpText("Choose a random Sample size or slide to end for population"),
    sliderInput('sampleSize', 'Number of Players', min=1, max=nrow(dataset),
                value=min(1000, nrow(dataset)), step=100, round=4, format = "####"),
    
    # Select variable for x-axis for the plot
    
    helpText("Select a variable to be plotted on your X-axis (variables contain both continuous and categorical)"),
    
    selectInput('x', 'X', names(dataset[,c("Ages", "Overall","Potential","Value", "Wages", "League", "Position" )]), selected = "League"),
    
    # Chose continuous variable for y-axis
    
    helpText("Choose a variable (limited to continuous variables only)"),
    selectInput('y', 'Y', names(dataset[,c("Overall", "Potential", "Value", "Wages") ]))  ,
    
    # Select type of plot
    
    helpText("Select type of Plot dependent on variables selected above"),
    selectInput("plot.type", "Type of Plot:",
                list(boxplot="boxplot", density = "density",  bar="bar", scatter = "scatter")),
    
    # Add Jitter for boxplots
    
    helpText("Jitter points for Boxplots"),
    checkboxInput("show.points", "show points", FALSE),
    
    # Transform values to log 10 if needed 
    
    helpText("transform y to log10 for Value and Wages"),
    checkboxInput("log", "Tranform Y to log(10)", FALSE),
    helpText("transform x to log10 for Value and Wages"),
    checkboxInput("log1", "Tranform X to log(10)", FALSE),
    
    # facet plots row-wise or columnwise 
    
    selectInput('facet_row', 'Facet Row', c(None='.', names(dataset[,c("Ages","League", "Position")]))),
    selectInput('facet_col', 'Facet Column', c(None='.', names(dataset[,c("Ages","League", "Position")])))
    
  ),
  
  # Main plot output
  
  mainPanel(
    plotlyOutput('plot', height = "700px", width = "900px")
  )
)





