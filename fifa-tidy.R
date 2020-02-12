library(tidyverse)
library(dplyr)
#Read data into R and remove columns not needed
fifa <- read_csv("data.csv")
fifa <- fifa %>% 
  as_tibble() %>% 
  select(-c( ID, Photo, Flag, `Club Logo`, `Real Face`, Joined, `Loaned From`, `Contract Valid Until`
  ))

# Values were given in different formats, like 5k =5000 so replace with real values
fifa <- fifa %>% 
  mutate(ValueMultiplier = ifelse(str_detect(Value, "K"), 1000, ifelse(str_detect(Value, "M"), 1000000, 1))) %>%
  mutate(Value = as.numeric(str_extract(Value, "[[:digit:]]+\\.*[[:digit:]]*")) * ValueMultiplier) %>%
  mutate(Position = ifelse(is.na(Position), "Unknown", Position))

# Same for wages
fifa <- fifa %>% 
  mutate(WageMultiplier = ifelse(str_detect(Wage, "K"), 1000, ifelse(str_detect(Wage, "M"), 1000000, 1))) %>%
  mutate(Wages = as.numeric(str_extract(Wage, "[[:digit:]]+\\.*[[:digit:]]*")) * WageMultiplier)

# Group specific Player positions into General Playing plositions
fifa <- fifa %>% 
  mutate("Position" = case_when(
    Position %in% c("GK")~ "Goalkeeper",
    Position %in% c("CB","LB","LCB","LWD","RB","RCB","RWB")~"Defender",
    Position %in% c("CDM","CM","LAM","LCM","LDM","LM","RAM","RCM","RDM","RM")~"Midfielder",
    Position %in% c("CF","LF","LS","LW","RF","RS","RW","ST")~ "Forward",
    Position %in% c("NA")~ "Unknown"
  ))

# Group teams from countries into leagues. Just top 5 European, classify rest as other
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

# Create age groups
fifa <- fifa %>% 
  mutate("Ages" = case_when(
    Age <=20 ~ "20 and Under", 
    Age > 20 & Age <=25 ~ "21 to 25", 
    Age > 25 & Age <=30 ~ "26 to 30", 
    Age > 30 & Age <=35 ~ "31 to 35", 
    Age > 35 & Age <=40 ~ "36 to 40", 
    Age > 40 ~ "Over 40"
  ))

#Group clubs by their total club value
fifa <- fifa %>% 
  group_by(Club) %>% 
  mutate("Total Club Value" = round(sum(Value)/1000000)) %>% 
  mutate("Total Wages Paid by Club" = round(sum(Wages)/100000)) 

# Top National teams
fifa <- fifa %>% 
  mutate("Top Countries" = if_else(Nationality %in% c("England","Germany", "Spain", "Argentina", "France", "Brazil", "Italy",
                                                      "Columbia", "Japan", "Netherlands"), Nationality, 'NA'))


# Group clubs by Overall rankings
fifa <- fifa %>% 
  group_by(Club) %>% 
  mutate("Top Overall" = if_else( Club %in% c("Inter","Juventus","Milan","Napoli","Roma","Real Madrid","FC Barcelona","Paris Saint-Germain", 
                                              "Manchester United", "SL Benfica","FC Bayern Munchen", "Chelsea", "Manchester City",
                                              "Tottenham Hotspur", "FC Porto", "Sporting CP", "Liverpool", "Bayer 04 Leverkusen", 
                                              "Lazio", "Borussia Dortmund"), Club, 'NA') )
# Group clubs by Overall Potential
fifa <- fifa %>% 
  mutate("Top Potential" = if_else( Club %in% c("Inter","Juventus","Milan","Napoli","Roma","Real Madrid","FC Barcelona",
                                                "Paris Saint-Germain", "Manchester United", "SL Benfica","FC Bayern Munchen", 
                                                "Chelsea", "Manchester City", "Tottenham Hotspur", "Atletico Madrid", "Olympique Lyonnais",
                                                "Liverpool", "Arsenal", "Valencia CF", "Borussia Dortmund"), Club, 'NA' )) 

# Select variables for shiny app
fifa1<- fifa %>% 
  select(Name, 'Ages', Overall,Potential,  Potential, Value, Wages, League, 'Position' )

