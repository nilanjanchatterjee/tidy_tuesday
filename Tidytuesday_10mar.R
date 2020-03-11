library(tidyverse)
library(ggplot2)

tuition_cost <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')

tuition_income <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv') 

salary_potential <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')

historical_tuition <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')

diversity_school <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')

div_st <-diversity_school %>%
  group_by(state, category) %>%
  summarise(tot_enrl=sum(enrollment, na.rm=T))

head(div_st)

##Changing the category labels
div_st$category <-recode(div_st$category, "American Indian / Alaska Native"= "American Indian")
div_st$category <-recode(div_st$category, "Native Hawaiian / Pacific Islander" = "Hawaiian & Pacific")


# Get the name and the y position of each label
label_data <- diversity_school %>%
  group_by(state) %>%
  summarise(tot_enrl=sum(enrollment, na.rm=T))
label_data$id <- seq(1, nrow(label_data))
angle <- 90 - 360 * (as.numeric(label_data$id)-0.5) /nrow(label_data)     
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)


###Plotting 
p <- ggplot(div_st, aes(x=as.factor(state), y=log(tot_enrl)))+
  geom_bar(stat="identity", aes(fill=category)) +
  theme_dark() + 
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()) +
  labs(subtitle= "Number of students (log transformed) enrolled across various category in different states",
       title =  "Tidy_tuesday 10rd March")+
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  coord_polar(start = 0)  +
  geom_text(data=label_data, aes(x=id, y=log(tot_enrl)+100, label=state, hjust=hjust), color="black", 
            fontface="bold",alpha=0.6, size=4, angle=label_data$angle, inherit.aes = FALSE ) 

p
   