library(tidyverse)
library(gganimate)

##Get the data
brewing_materials <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

head(brewing_materials)

yrly_prod <-brewing_materials %>%
  group_by(year,month) %>%
  summarise (tot_prod =sum(month_current, na.rm = T))

yrly_barr <-beer_states %>%
  group_by(year) %>%
  summarise (tot_prod =sum(barrels, na.rm = T))

ani <-ggplot(yrly_barr) +
  geom_line(aes(x = year, y = tot_prod), size=2, col="green") +
  theme_dark()+
  theme(plot.title =  element_text(size=18),
        axis.title = element_text(size=15),
        text = element_text(size = 14))+
  labs(title = "Number of total barrel produced across year", y= "Number of total barrels", x="Year",
       caption = "Data:Alcohol and Tobacco Tax and Trade Bureau\nVisualization: Nilanjan Chatterjee(@footloose_nil)",
       subtitle = "Beer production decreased over the years specially after the year 2015")+
  transition_reveal(along = year)
ani
anim <- animate(ani, width = 800, height = 600)
anim_save("Tidytuesday_31mar.gif", anim)
