install.packages("taRifx")
install.packages("fuzzyjoin")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("plotly")
install.packages("ggcorrplot")
install.packages("ggpmisc")

library(taRifx)
library(tidyverse)
library(dplyr)
library(fuzzyjoin)
library(plotly)
library(ggcorrplot)
library(ggpmisc)

#Create a "not in" operator.  I use this heavily in all of my analyses and begin every script with this.
`%!in%` = Negate(`%in%`)

#Set working directory
setwd("~/Documents/GitHub/RProjects/NFLAnalysis")

#Read in 2018 regular passing data from Pro Football Reference and rename select columns
passing_stats <- read.csv(file = "./2018_reg_passing_stats.csv", header = TRUE, sep = ",") %>% 
  rename(Name = Player, Team = Tm, CmpPct = Cmp.)

#Read in 2018 advanced passing data from Pro Football Reference and rename select columns
adv_passing_stats <- read.csv(file = "./2018_adv_passing_stats.csv", header = TRUE, sep = ",") %>% 
  rename(DropPct = Drop., BadPct = Bad., Name = Player, Team = Tm)
                                                                                              
#Read in initial Madden 20 player ratings CSV
madden_ratings <- read.csv(file = "./M20PlayerRatingsJul19.csv", header = TRUE, sep = ",") %>% 
  rename(Pos = position) 

#Clean up col names in ratings df
cleancols <- sub(".rating", "", names(madden_ratings), fixed = TRUE)
names(madden_ratings) <- cleancols

#Remove all non-alphanumeric characters
madden_ratings$Name <- str_replace_all(madden_ratings$Name, "[^A-Za-z ]", "") 

passing_stats$Name <- str_replace_all(passing_stats$Name, "[^A-Za-z ]", "")

adv_passing_stats$Name <- str_replace_all(adv_passing_stats$Name, "[^A-Za-z ]", "")

#Join regular passing and advanced passing data into one dataframe
all_passing_stats <- inner_join(passing_stats, adv_passing_stats)

#Since there seems to be passing data for non QBs, let's filter for the QB position only
all_passing_stats_filt <- all_passing_stats %>% filter(grepl("qb", Pos, ignore.case = TRUE))

madden_ratings_filt <- madden_ratings %>% filter(grepl("qb", Pos, ignore.case = TRUE))

####

#Select ratings of interest pertinent to QBs

#First take a peek at what we have available in the Madden ratings dataset
glimpse(madden_ratings_filt)

madden_ratings_filt_select <- madden_ratings_filt %>% 
  select(Name, Team, Pos, ovr, awareness, throw.power, strength, play.action, acceleration, injury, 
         throw.accuracy.short, throw.accuracy.deep, break.sack, toughness, agility, elusiveness)


#Select variables of interest from the all passing dataframe

#First take a peek at what we have available in tthe all passing dataframe
glimpse(all_passing_stats_filt)

#Seems like it will be easier if we drop the columns we don't need, and let's also rename some of the columns for ease of use later (dictionary available at Pro Football Reference)
all_passing_stats_filt_select <- all_passing_stats_filt %>% select(-c(Rk, GS, QBrec, TD., Int.)) %>% 
  rename(FirstD=X1D, SkYds = Yds.1, SkPct = Sk., Q4CBK = X4QC)


#Add a few calculations for QBs
all_passing_stats_filt_select <- all_passing_stats_filt_select %>% 
  mutate(TDtoInt = round(TD/Int, 2), #Nothing fancy, just a simple ratio to how often a QB throws an errant pass (or can we blane WRs?)
         SktoBltz = Sk/Bltz) 

#Inner join the ratings data to stats for 2019 QBs. Drop the duplicate col and clean col names
joined_data <- stringdist_left_join(all_passing_stats_filt_select, madden_ratings_filt_select) %>% 
  select(-c(Name.y,Team.y,Pos.y)) %>% rename(Name = Name.x, Team = Team.x, Pos = Pos.x)

#Let's see if we have both types (Madden rating and passing data for all players)

#If there is no ovr rating, must mean the Madden dataset didn't have a value
not_in_madden <- joined_data %>% filter(is.na(ovr))

#Some of these make sense like Sam Bradford and Mark Sanchez (left the league), but what about others like Derek Carr and Nick Foles?

#Lets see if they exist in the raw datasets

#Create a string of names to search for
name_search <- c("carr", "foles", "tannehill")

madden_ratings %>% filter(grepl(paste0(name_search, collapse = "|"), Name, ignore.case = TRUE))

#So it seems like the players exist, but I didn't specify a join by above so R defaulted to joining by Name, Pos, and Team (which is different as players have either been traded/signed elsewhere)

#Redo the join, this time explictly with a "by" argument
joined_data <- stringdist_left_join(all_passing_stats_filt_select, madden_ratings_filt_select, by = "Name") %>% 
  select(-c(Name.y,Team.y,Pos.y)) %>% rename(Name = Name.x, Team = Team.x, Pos = Pos.x)


#Now let's recheck to see if we have any missing rating data
not_in_madden <- joined_data %>% filter(is.na(ovr))

#Looks much better - with only 6 players unmatched. Double check to make sure.

#Create a string of names to search for
name_search <- c("osweiler","sanchez")

madden_ratings %>% filter(grepl(paste0(name_search, collapse = "|"), Name, ignore.case = TRUE))

#Wrong Sanchez, but that's a good thing. Let's keep moving.

#Drop the 6 players where we know we don't have ratings for them (using ovr variable)
joined_data_filt <- joined_data %>% filter(!is.na(ovr))

### Start comparing ratings and looking at the data ###

#Generate correleation matrix
statscorr <- data.frame(cor(joined_data_filt[,unlist(lapply(joined_data_filt, is.numeric))]))

#Visualize matrix with ggcorrplot
ggcorrplot(statscorr) #Yikes, this is alot to process. Pretty much unusable.

ggcorrplot(statscorr, method = "circle") #Not much better

ggcorrplot(statscorr, type = "upper") #Meh.

#Unless I narrow down the attributes a corr matrix on such a large amount of variables is pretty much useless.

#Let's plot now

#Plot Age by Injury score
ageinj <- joined_data_filt %>% 
ggplot(mapping = aes(x=Age, y=injury, group = 1, 
                     text = paste("Name: ", Name, "<br>Age: ", Age, "<br>Injury: ", injury))) +
geom_point() + 
  geom_smooth(method='lm') +
  xlab("Age") +
  ylab("Injury") +
  ggtitle("Age by Injury Rating") +
  theme(plot.title = element_text(hjust = 0.5)) #Center title

ggplotly(ageinj, tooltip = "text")

#This seems pretty obvious - older players like Brady, Rivers, Manning, etc. are much more injury prone.

#Plot Hits by Injury score - Does Madden take into account the amount of times a QB is hit into their injury score
hitsinj <- joined_data_filt %>% 
  ggplot(mapping = aes(x=Hits, y=injury, group = 1, 
                       text = paste("Name: ", Name, "<br>Hits: ", Hits, "<br>Injury: ", injury))) +
  geom_point() + 
  geom_smooth(method='lm') +
  xlab("Hits") +
  ylab("Injury") +
  ggtitle("Number of Hits Taken by Injury Rating") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplotly(hitsinj, tooltip = "text")

#Doesn't appear to be a clear trend in..

#Plot Times Hurred by Bad Throws
hrrybadth <- joined_data_filt %>% 
  ggplot(mapping = aes(x=Hrry, y=BadTh, group = 1, 
                       text = paste("Name: ", Name, "<br>Hurry: ", Hrry, "<br>BadPct: ", BadPct))) +
  geom_point() + 
  geom_smooth(method='lm') +
  xlab("Hurries") +
  ylab("Bad Throws") +
  ggtitle("Number of Hurries by Bad Throws") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplotly(hrrybadth, tooltip = "text")

#Plot Drops by Throw Power score 
powerdrops <- joined_data_filt %>% 
  ggplot(mapping = aes(x=throw.power, y=Drops, group = 1, 
                       text = paste("Name: ", Name, "<br>Drops: ", Drops, "<br>Throw Power: ", throw.power))) +
  geom_point() + 
  geom_smooth(method='lm')+
  xlab("Throw Power Rating") +
  ylab("Drops") +
  ggtitle("Throw Power by Drops") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplotly(powerdrops)


#Plot Hits by Agility score 
hitsagility <- joined_data_filt %>% 
  ggplot(mapping = aes(x=Hits, y=agility, group = 1, 
                       text = paste("Name: ", Name, "<br>Hits: ", Hits, "<br>Agility: ", agility))) +
  geom_point() + 
  geom_smooth(method='lm') +
  xlab("Number of Hits") +
  ylab("Agility") +
  ggtitle("Number of Hits by Agility Rating") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplotly(hitsagility)