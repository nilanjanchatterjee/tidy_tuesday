library(tidyverse)
library(ggplot2)

### Get the data
tdf_winners <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

ggplot(tdf_winners, aes(x= age, y= stages_led))+
  geom_point(aes(colour=nationality), size=3)+
  geom_point(aes(colour=nationality),shape=7, size=3)+
  geom_smooth(method="loess", formula= y~x^4, colour="black")+
  labs(title= "Relation between winners age and number of stages led", 
       x= "Age of winner", y= "Number of stages led", colour= "Nationality",
       caption = "Data: tdf Data package\nVisulization: Nilanjan Chatterjee @footloose_nil")+
  theme_dark()+
  theme(axis.title = element_text(size=16, colour = "yellow"),
        title = element_text(size=18, colour = "white"),
        axis.text =  element_text(size=12, color = "yellow"),
        plot.background = element_rect(fill = "black"),
        legend.title = element_text(colour = "black"))

ggsave("tour_de_france.jpeg", dpi=200, width = 12, height = 8, units = "in")
