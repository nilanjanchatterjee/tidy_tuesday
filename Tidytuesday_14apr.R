library(tidyverse)
library(cowplot)


polls <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

head(polls)
head(rankings)


p2 <-ggplot(rankings)+
  geom_bar(aes(x=year, y=points), stat= "identity", fill="darkgreen")+
  labs(x="Gender", y= "Rank given by voter", 
       title = "Number of total points",
       subtitle = "Songs from 1990-2000 have higher points compared to other decades",
       caption = "Data :Simon Jockers(Datawrapper)\n Visualization: Nilanjan @footloose_nil")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20))
p2

p3<-  ggplot(rankings, aes(x=n, y=points))+
  geom_point(size=3, aes(col=year))+
  geom_smooth(method = "lm", colour="Black")+
    theme_bw()+
    scale_color_gradientn(colours= rainbow(7))+
  geom_label(x=17, y=132, label= "Juicy", fill= "lightgreen")+
  geom_label(x=11, y=108, label= "Fight The Power", fill= "yellow")+
  geom_label(x=11.1, y=91, label= "Shook Ones (Part II)", fill="lightgreen")+
  geom_label(x=13.5, y=74, label= "Nuthin' But A 'G' Thang", fill="lightgreen")+
  geom_label(x=16, y=86, label= "The Message", fill="orange")+
    labs(x="Total number of Votes", y= "Total points of a song", 
         title = "Number of total points vs Number of Votes",
         subtitle = "Songs from 1990-2000 have higher votes compared to other decades",
         col="Year")+
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 20))

p3          

ggdraw()  +draw_plot(p3, .3, .5, .7, .5) +draw_plot(p2, 0.05, 0, .9, .5) 

ggsave("Tidy_tuesday_14_apr.jpeg", width = 12, height = 10, dpi =   150)
