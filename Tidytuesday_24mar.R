library(tidyverse)
library(ggridges)
library(viridis)

###Loading the data
tbi_age <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

###Numer of cases diagnosed across services
case_srvc <-tbi_military %>%
  group_by(service, severity,year) %>%
  summarise(tot_dgnsd =sum(diagnosed, na.rm = T))

head(case_srvc)

case_srvc1 <-tbi_military %>%
  group_by(service,year) %>%
  summarise(tot_dgnsd =sum(diagnosed, na.rm = T))

case_typ <-tbi_year %>%
  group_by(type, injury_mechanism) %>%
  summarise(tot_num = sum(number_est))

###plotting the number of diagnosed people across services
ggplot(case_srvc1)+
  geom_line(aes(x= year, y= tot_dgnsd, colour=service), size=2)+
  theme_dark()+
  labs(title = "Number of diagnosed people across different service",x="Year",
       y = "Total diagnosed people", 
       caption= "Data: Brain injury association\n Visualization: Nilanjan Chatterjee @footloose_nil",
       subtitle = "The number of diagnosed people in army was significantly higher than other services")

ggsave("Tidy_tuesday_24mar.jpg", dpi=300)

ggplot(case_srvc)+
  geom_line(aes(x= year, y= tot_dgnsd, colour=severity), size=2)+
  facet_wrap(vars(service), nrow=2)+
  theme_dark()+
  labs(title = "Number of diagnosed people across different services and severity",x="Year",
       y = "Total diagnosed people", 
       caption= "Data: Brain injury association\n Visualization: Nilanjan Chatterjee @footloose_nil",
       subtitle = "The number of diagnosed people in army was significantly higher than other services and 
       most of the diagnosed person had mild injury")

ggsave("Tidy_tuesday1_24mar.jpg", dpi=300)

ggplot(tbi_year)+
  geom_line(aes(x= year, y= number_est, colour=injury_mechanism), size=2)+
  facet_wrap(vars(type), nrow=2)+
  theme_dark()+
  labs(title = "Number of diagnosed people across different services and severity",x="Year",
       y = "Total diagnosed people", 
       caption= "Data: Brain injury association\n Visualization: Nilanjan Chatterjee @footloose_nil",
       subtitle = "The number of diagnosed people in army was significantly higher than other services and 
       most of the diagnosed person had mild injury")

ggsave("Tidy_tuesday1_24mar.jpg", dpi=300)
