library(tidyverse)
library(ggplot2)
library(ggthemes)
library(cowplot)

game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

##Check the data
head(game_goals)
head(top_250)
head(season_goals)

###Filter and arrange the data
goal_plyr <- game_goals %>% 
  group_by(player) %>%
  summarise(tot_goal=sum(goals, na.rm=T), tot_asst=sum(assists, na.rm=T))
head(goal_plyr)

barplot(goal_plyr$tot_goal)
plot(goal_plyr$tot_goal, goal_plyr$tot_asst)

unique(game_goals$season)
goal_yr <- game_goals %>% 
  group_by(season) %>%
  summarise(tot_goal=sum(goals, na.rm=T), tot_sht=sum(shots, na.rm=T))
head(goal_yr)
plot(goal_yr$tot_goal, goal_yr$tot_sht)

###saving the plots
p1<-  ggplot(goal_yr, aes(tot_goal, tot_sht)) +
  geom_point(aes(size=3), colour="yellow") +
    geom_smooth(method = lm, colour= "lightgreen")+
    theme_dark()+theme(legend.position = "none")+
    labs(subtitle= "Numbers of goal scored is highly correlated with total number of shots",
          title =  "Tidy_tuesday 3rd March", x= "Total Number of goals", y= "Total number of shots")

p2 <-ggplot(goal_yr, aes(season, tot_goal)) +
    geom_point(aes(size=1.5), colour="darkgreen") + 
    geom_line(aes(), colour="darkblue", linetype= "solid") + 
    geom_smooth()+
    theme_bw()+theme(legend.position = "none")+
    labs(title= "Numbers of goal over years", x="Years", y= "Total number of goals", 
         subtitle = "The number of goals did not show a steady increase over the years with a major fall around 2000")
  
  ##Drawing the plot with multiple elements
ggdraw() + draw_plot(p2, 0.05, 0, .9, .5)+ draw_plot(p1, .25, .45, .5, .55) 
  