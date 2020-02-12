library(tidyverse)
library(shiny)
library(rsconnect)
library(plotly)
library(viridis)
library(dplyr)

fifa <- read_csv("data.csv")
fifa <- fifa %>% 
  as_tibble() %>% 
  select(-c( ID, Photo, Flag, `Club Logo`, `Real Face`, Joined, `Loaned From`, `Contract Valid Until`
  ))

fifa <- fifa %>% 
  mutate(ValueMultiplier = ifelse(str_detect(Value, "K"), 1000, ifelse(str_detect(Value, "M"), 1000000, 1))) %>%
  mutate(Value = as.numeric(str_extract(Value, "[[:digit:]]+\\.*[[:digit:]]*")) * ValueMultiplier) %>%
  mutate(Position = ifelse(is.na(Position), "Unknown", Position))

fifa <- fifa %>% 
  mutate(WageMultiplier = ifelse(str_detect(Wage, "K"), 1000, ifelse(str_detect(Wage, "M"), 1000000, 1))) %>%
  mutate(Wages = as.numeric(str_extract(Wage, "[[:digit:]]+\\.*[[:digit:]]*")) * WageMultiplier)

fifa <- fifa %>% 
  mutate("Position" = case_when(
    Position %in% c("GK")~ "Goalkeeper",
    Position %in% c("CB","LB","LCB","LWD","RB","RCB","RWB")~"Defender",
    Position %in% c("CDM","CM","LAM","LCM","LDM","LM","RAM","RCM","RDM","RM")~"Midfielder",
    Position %in% c("CF","LF","LS","LW","RF","RS","RW","ST")~ "Forward",
    Position %in% c("NA")~ "Unknown"
  ))


fifa <- fifa %>% 
  mutate(League = case_when(
    Club %in% c("Arsenal","Bournemouth", "Brighton", "Burnley", 
                "Cardiff City", "Chelsea", "Crystal Palace","Everton",
                "Fulham","Huddersfield","Leicester City","Liverpool",
                "Manchester City","Manchester United","Newcastle United",
                "Southampton","Tottenham Hotspur", "Watford","West Ham United","Wolverhampton Wanderers")~ "EPL",
    
    Club %in% c("Amiens SC", "Angers SCO", "AS Monaco","AS Saint-Etienne", 
                "Dijon FCO","En Avant de Guingamp","FC Nantes"
                ,"LOSC Lille","Montpellier HSC","Nimes Olympique",
                "OGC Nice","Olympique Lyonnais","Olympique Marseille","Paris Saint-Germain", "RC Strasbourg Alsace","Stade Malherbe Caen"
                ,"Stade de Reims","Stade Rennais FC","Toulouse FC")~"Ligue 1",
    
    Club %in% c("1. FC Nurnberg","1. FSV Mainz 05","Borussia Dortmund","Dusseldorf",
                "FC Augsburg","FC Bayern","Frankfurt","Hannover 96",
                "Hertha BSC","Leverkusen","M'gladbach","RB Leipzig",
                "SC Freiburg","Schalke","TSG 1899 Hoffenheim","VfB Stuttgart",
                "VfL Wolfsburg","Werder Bremen") ~ "Bundesliga",
    
    Club %in% c("Atalanta","Bologna","Cagliari","Chievo Verona","Empoli","Fiorentina",
                "Frosinone","Genoa","Inter","Juventus","Lazio","Milan","Napoli",
                "Parma","Roma","Sampdoria","Sassuolo","SPAL","Torino","Udinese")~"Serie A",
    
    Club %in% c("Athletic Club","Atletico Madrid","CD Leganes","Deportivo Alaves","FC Barcelona",
                "Getafe CF","Girona FC","Levante UD","Real Valladolid CF","Rayo Vallecano",
                "RC Celta","RCD Espanyol","Real Betis","Real Madrid","Real Sociedad","SD Eibar",
                "SD Huesca","Sevilla FC","Valencia CF","Villarreal CF")~"La Liga"
  ))
fifa <- fifa %>% 
  mutate("Ages" = case_when(
    Age <=20 ~ "20 and Under", 
    Age > 20 & Age <=25 ~ "21 to 25", 
    Age > 25 & Age <=30 ~ "26 to 30", 
    Age > 30 & Age <=35 ~ "31 to 35", 
    Age > 35 & Age <=40 ~ "36 to 40", 
    Age > 40 ~ "Over 40"
  ))

fifa <- fifa %>% 
  group_by(Club) %>% 
  mutate("Total Club Value" = round(sum(Value)/1000000)) %>% 
  mutate("Total Wages Paid by Club" = round(sum(Wages)/100000)) 


fifa <- fifa %>% 
  mutate("Top Countries" = if_else(Nationality %in% c("England","Germany", "Spain", "Argentina", "France", "Brazil", "Italy", "Columbia", "Japan", "Netherlands"), Nationality, 'NA'))



fifa <- fifa %>% 
  group_by(Club) %>% 
  mutate("Top Overall" = if_else( Club %in% c("Inter","Juventus","Milan","Napoli","Roma","Real Madrid","FC Barcelona","Paris Saint-Germain", "Manchester United", "SL Benfica","FC Bayern Munchen", "Chelsea", "Manchester City", "Tottenham Hotspur", "FC Porto", "Sporting CP", "Liverpool", "Bayer 04 Leverkusen", "Lazio", "Borussia Dortmund"), Club, 'NA') )

fifa <- fifa %>% 
  mutate("Top Potential" = if_else( Club %in% c("Inter","Juventus","Milan","Napoli","Roma","Real Madrid","FC Barcelona","Paris Saint-Germain", "Manchester United", "SL Benfica","FC Bayern Munchen", "Chelsea", "Manchester City", "Tottenham Hotspur", "Atletico Madrid", "Olympique Lyonnais", "Liverpool", "Arsenal", "Valencia CF", "Borussia Dortmund"), Club, 'NA' )) 

fifa1<- fifa %>% 
  select(Name, 'Ages', Overall,Potential,  Potential, Value, Wages, League, 'Position' )


dataset <- fifa1


ui <- fluidPage(
  
  titlePanel("Comparisons Within FIFA 19 Dataset"),
  helpText("This Shiny app gives the user the fluidity of comparing some important aspects of the FIFA 19 Data but mostly
            on the Club Level (Individual Player Statistics cannot be seen).
           The variables being compared in this plot are the Age group of the players, 
           their Overall and Potential Ratings, their market Value in the current transfer window, 
           their weekly Wages, the League they play in (Europe top 5) and the Position they play on the field "),
  sidebarPanel(
    helpText("Choose a random Sample size or slide to end for population"),
    sliderInput('sampleSize', 'Number of Players', min=1, max=nrow(dataset),
                value=min(1000, nrow(dataset)), step=100, round=4, format = "####"),
    
    helpText("Select a variable to be plotted on your X-axis (variables contain both continuous and categorical)"),
    
    selectInput('x', 'X', names(dataset[,c("Ages", "Overall","Potential","Value", "Wages", "League", "Position" )]), selected = "League"),
    helpText("Choose a variable (limited to continuous variables only)"),
    selectInput('y', 'Y', names(dataset[,c("Overall", "Potential", "Value", "Wages") ]))  ,
    helpText("Select type of Plot dependent on variables selected above"),
    selectInput("plot.type", "Type of Plot:",
                list(boxplot="boxplot", density = "density",  bar="bar", scatter = "scatter")),
    helpText("Jitter points for Boxplots"),
    checkboxInput("show.points", "show points", FALSE),
    helpText("transform y to log10 for Value and Wages"),
    checkboxInput("log", "Tranform Y to log(10)", FALSE),
    helpText("transform x to log10 for Value and Wages"),
    checkboxInput("log1", "Tranform X to log(10)", FALSE),
    selectInput('facet_row', 'Facet Row', c(None='.', names(dataset[,c("Ages","League", "Position")]))),
    selectInput('facet_col', 'Facet Column', c(None='.', names(dataset[,c("Ages","League", "Position")])))
    
  ),
  
  mainPanel(
    plotlyOutput('plot', height = "700px", width = "900px")
  )
)





server <- function(input, output){
  
  dataset <- reactive({
    fifa1[sample(nrow(fifa1), input$sampleSize),]
  })
  
  output$plot <- renderPlotly({
    
    #plot.obj <- dataset()
    #if(is.null(plot.obj)) return()
    #if(plot.obj$x == "" | plot.obj$y =="") return()
    
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(),
                      "density" 	=	geom_density(alpha=.75),
                      "bar" 		=	geom_bar(position="dodge"),
                      "scatter" = geom_point()
    )
    
    
    if(input$plot.type=="boxplot" | input$plot.type=="scatter"  )	{		#control for 1D or 2D graphs
      p<-ggplot(dataset(),
                aes_string(
                  x 		= input$x,
                  y 		= input$y,
                  fill 	= input$x
                  #,alpha= 0.4
                  # let type determine plotting
                )
      )  + plot.type
      
      if(input$show.points==TRUE)
      {
        p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
      }
      if(input$log == TRUE)
      {
        p<-p+ scale_y_log10() 
      }
      if(input$log1 == TRUE)
      {
        p<-p+ scale_x_log10() 
      }
      
      
    } else {
      
      p<-ggplot(dataset(),
                aes_string(
                  x 		= input$x,
                  fill 	= input$y,
                  group 	= input$y
                  #color 	= as.factor(plot.obj$group)
                )
      ) + plot.type
    }
    
    p<-p+labs(
      
      guide = FALSE
    ) 
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    ggplotly(p)
    
  })
  
  
}


shinyApp(ui, server)



