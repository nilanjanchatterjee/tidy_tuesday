library(ggplot2)
library(mapdata)
library(raster)
library(tidyverse)
library(reshape2)

measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')
head(measles)
summary(measles)

plot(measles$lng, measles$lat)

msls<-(measles[, 15:16])
msls1 <-msls[complete.cases(msls*0),,drop=F]

msl <-SpatialPoints(msls1[, 2:1], proj4string = crs("+init=epsg:4326"))

coastlines <- shapefile("D:/Work/River bird/ne-coastlines-10m/ne_10m_coastline.shp")
plot(coastlines)


map('worldHires',c('usa'), xlim=c(-130,-65),ylim=c(20,50), fill = T,col="grey",resolution = 2)
map('state', fill = T, col="grey", resolution = 2)
plot(msl, pch=19, col="purple")

schl_st <-measles %>% 
  group_by(state) %>%
  summarise(mean_enroll=mean(enroll, na.rm=T), mean_mmr=mean(mmr, na.rm=T), mean_ovrl = mean(overall, na.rm=T))
head(schl_st)

typ_st <-measles %>% 
  group_by(type) %>%
  summarise(mean_enroll=mean(enroll, na.rm=T), mean_mmr=mean(mmr, na.rm=T), mean_ovrl = mean(overall, na.rm=T))
head(typ_st)

dfm <- melt(schl_st[,c('state','mean_mmr','mean_ovrl')],id.vars = 1)
head(dfm)
ggplot(dfm, aes(fill=variable, y=value, x=state)) + 
  geom_bar(position="dodge", stat="identity")

prcnt_exmpt <-measles %>%
  group_by(state) %>%
  summarise(mean_xrel=mean(xrel, na.rm=T), mean_xmed=mean(xmed, na.rm=T), mean_xper = mean(xper, na.rm=T))
head(prcnt_exmpt)

typ_exmpt <-measles %>%
  group_by(type) %>%
  summarise(mean_xrel=mean(xrel, na.rm=T), mean_xmed=mean(xmed, na.rm=T), mean_xper = mean(xper, na.rm=T))
head(typ_exmpt)

prcnt_exmpt <-measles %>%
  group_by(state) %>%
  summarise(mean_xrel=mean(xrel, na.rm=T), mean_xmed=mean(xmed, na.rm=T), mean_xper = mean(xper, na.rm=T))
head(prcnt_exmpt)
dfm1 <- melt(prcnt_exmpt[,c('state','mean_xrel','mean_xmed','mean_xper')],id.vars = 1)
dfm1$value[is.nan(dfm1$value)]<-0
head(dfm1)
ggplot(dfm1, aes(fill=variable, y=value, x=state)) + 
  geom_bar(position="dodge", stat="identity")

###Adding the dataset names with the map layer
choro <- left_join(map_data("state"), prcnt_exmpt %>% 
    add_rownames("region") %>% 
    mutate(region=tolower(state)))

p1 <-ggplot(choro, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = mean_xmed)) + 
  coord_quickmap()+theme_bw()+
  borders("state", colour="white") +
  ggtitle(subtitle= "Percentage of Students exempted from vaccination for medical reasons",
  label = " ")+ 
  labs(fill= "percentage")


###Adding the dataset names with the map layer
vacr <- left_join(map_data("state"), schl_st %>% 
                     add_rownames("region") %>% 
                     mutate(region=tolower(state)))

p2 <-ggplot(vacr, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = mean_mmr)) + 
  coord_quickmap()+theme_bw()+
  borders("state", colour="white") +
  ggtitle(subtitle= "School's Measles, Mumps, and Rubella (MMR) vaccination rate", label = "")+ 
  labs(fill= "Vaccination rate")

p3 <-ggplot(typ_st, aes(x=type, y=mean_mmr))+geom_bar(stat = "identity", fill="purple")+
  theme_bw()+ labs(x= "Type of School", y="Mean Vaccination rate")+
  theme(axis.title.x=element_text(size=rel(1.5)), axis.title.y = element_text(size=rel(1.5)))+
  ggtitle( label = "Tidy tuesday 25th February, 2020")

##Drawing the plot with multiple elements
ggdraw() + draw_plot(p2, .5, .45, .5, .55) +draw_plot(p1, 0.3, 0, .5, .5) +
  draw_plot(p3, 0, 0.5, .48, .45)
