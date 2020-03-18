library(tidyverse)
library(gridExtra)

office_ratings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

head(office_ratings)

scatterplot <- ggplot(office_ratings, aes(x= imdb_rating, y=total_votes, color = as.factor(season)))+
  geom_point(size=2)+
  geom_smooth(method = lm , se = FALSE)+
  theme_bw()+
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9','#FF3200', '#123456', '#AAFF34',
                              '#45EE21','#654321','#CC00FF'))+
  labs(x= "Imdb Ratings", y= "Total number of votes", color ="Season")


  scale_color_brewer(palette="YlGnBu")
    
  xdensity <- ggplot(office_ratings, aes(x= imdb_rating, fill=as.factor(season))) + 
    geom_density(alpha=.5) + 
    scale_fill_manual(values = c('#999999','#E69F00','#56B4E9','#FF3200', '#123456', '#AAFF34',
                                 '#45EE21','#654321','#CC00FF')) + 
    theme(legend.position = "none")+
    labs(title = "Tidy tuesday 17th March", 
         subtitle = "Relaitonship between Imdb Rating and total number of votes for the episodes across seasons",
         x= "Imdb Ratings")
    
  xdensity
  
  grid.arrange(xdensity, scatterplot, ncol=1, nrow=2, heights=c(2, 4))
  ggsave("Tidu_tuesday_18mar_1.jpg", dpi = 300)
  
  #Create unique column as the air date of episodes were same
  office_ratings$airep <-paste(office_ratings$air_date, office_ratings$episode)
  head(office_ratings)
  
  ##Create circular barplot with the imdb ratings
  p <- ggplot(office_ratings, aes(x=airep, y=imdb_rating))+ 
                geom_bar(stat="identity", aes(fill=as.factor(season))) +
                theme_dark()  +
                coord_polar()+
    annotate("text",x=-1, y=10, label= "10", colour="white",size=8)+
    annotate("text",x=15, y=5, label= "Season 2", colour="black",size=6)+
    annotate("text",x=39, y=3, label= "Season 3", colour="black", size=6)+
    annotate("text",x=-5, y=-5, label= " ", colour="black", size=6)+
    annotate("text",x=-1, y=5, label= "6.7", colour="white", size=8)+
    scale_fill_manual(values = c('#001199','#E69F00','#56B4E9','#FF3200', '#123456', '#AAFF34',
                                 '#45EE21','#654321','#CC00FF'))+
    labs(fill ="Season", color ="Season")+
    theme( axis.text = element_blank(),
      axis.title = element_text(),
      panel.grid = element_line(colour = "black"))+
    labs(title = "Tidy tuesday 17th March", 
         subtitle = "The imdb ratings of the episodes across seasons were almost similar",
         x=" ", y= "ÏMDB ratings",
         caption =  "Data: Schrute package \nVisualization: @footlooose_nil")
p     
ggsave("Tidy_tuesday_18mar.jpg", dpi = 300)

