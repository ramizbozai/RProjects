install.packages("taRifx")
install.packages("fuzzyjoin")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("plotly")

library(taRifx)
library(tidyverse)
library(dplyr)
library(fuzzyjoin)
library(plotly)

passing <- read.csv(file = "~/Documents/NFLAnalysis/2018_adv_passing_stats.csv", header = TRUE, sep = ",") %>% 
  rename(DropPct = Drop., BadPct = Bad., Name = Player, Team = Tm)
                                                                                              
madden_ratings <- read.csv(file = "~/Documents/NFLAnalysis/M20PlayerRatingsJul19.csv", header = TRUE, sep = ",") %>% 
  rename(Pos = position) 

#Clean up col names in ratings df
cleancols <- sub(".rating", "", names(madden_ratings), fixed = TRUE)
names(madden_ratings) <- cleancols

#Lets remove all non-alphanumeric characters
madden_ratings$Name <- str_replace_all(madden_ratings$Name, "[^[:alnum:]]", " ")
passing$Name <- str_replace_all(passing$Name, "[^[:alnum:]]", " ")

#Select ratings of interest pertinent to QBs
madden_ratings_select <- madden_ratings %>% 
  select(Name, Team, Pos, ovr, awareness, throw.power, strength, acceleration, injury, 
         throw.accuracy.short, play.recognition, agility)

#Let's exclude records for non QBs
passing_filt <- passing %>% filter(grepl("qb", Pos, ignore.case = TRUE))
ratings_filt <- madden_ratings_select %>% filter(grepl("qb", Pos, ignore.case = TRUE))

#Calculate a few metrics for QBs
passing_filt <- passing_filt %>% mutate(CmpPct = Cmp / Att)



#Left join the ratings data to stats for 2019 QBs. Drop the duplicate col.
joined_data <- stringdist_left_join(passing_filt, ratings_filt, by = "Name") %>% 
  select(-c(Name.y,Team.y,Pos.y)) %>% rename(Name = Name.x, Team = Team.x, Pos = Pos.x)

#There seems to be missing values in the Madden ratings set. Let's take a look.
not_in_madden <- joined_data %>% filter(is.na(ovr))

#Aha! These guys either retired on left the league, this makes sense


#Remove these players from our joined df.
joined_data_current <- joined_data %>% filter(!is.na(ovr))

#Generate correleation matrix
statscorr <- data.frame(cor(joined_data_current[,unlist(lapply(joined_data_current, is.numeric))]))

passing_filt <- japply(passing_filt, which(sapply(passing_filt, class)=="integer"), as.numeric )

#Plot Age by Injury score
ageinj <- joined_data_current %>% 
ggplot(mapping = aes(x=Age, y=injury, group = 1, 
                     text = paste("Name: ", Name, "<br>Age: ", Age, "<br>Injury: ", injury))) +
geom_point() + 
  geom_smooth(method='lm')
ggplotly(ageinj, tooltip = "text")

#Plot Hits by Injury score
ageinj <- joined_data_current %>% 
  ggplot(mapping = aes(x=Hits, y=injury, group = 1, 
                       text = paste("Name: ", Name, "<br>Hits: ", Hits, "<br>Injury: ", injury))) +
  geom_point() + 
  geom_smooth(method='lm')
ggplotly(ageinj, tooltip = "text")

#Plot Hurry by Bad Throws Pct 
ageinj <- joined_data_current %>% 
  ggplot(mapping = aes(x=Hrry, y=BadPct, group = 1, 
                       text = paste("Name: ", Name, "<br>Hurry: ", Hrry, "<br>BadPct: ", BadPct))) +
  geom_point() + 
  geom_smooth(method='lm')
ggplotly(ageinj, tooltip = "text")

#Plot Drops by Throw Power score 
ageinj <- joined_data_current %>% 
  ggplot(mapping = aes(x=Drops, y=throw.power, group = 1, 
                       text = paste("Name: ", Name, "<br>Drops: ", Drops, "<br>Throw Power: ", throw.power))) +
  geom_point() + 
  geom_smooth(method='lm')
ggplotly(ageinj, tooltip = "text")

#Plot Hits by Agility score 
ageinj <- joined_data_current %>% 
  ggplot(mapping = aes(x=Hits, y=agility, group = 1, 
                       text = paste("Name: ", Name, "<br>Hits: ", Hits, "<br>Agility: ", agility))) +
  geom_point() + 
  geom_smooth(method='lm')
ggplotly(ageinj, tooltip = "text")



#Can we compare hits to offensive line ovr?
  #Have to make sure teams are coded correctly (for joins)