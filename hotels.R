#****************************************************************************************
#Purpose: Tidy Tueday: Data Exploration
#Author: Lucy Njoki
#Date: 12/02/2020
#****************************************************************************************

rm(list = ls())

#loading necessary packages
library("tidyverse")
library("ggthemes")
library("extrafont")

theme_set(theme_tufte())

mytheme <- function(){
  theme(
    text = element_text(family = "Source Sans Pro", size  = 12),
    plot.title =  element_text(family = "Source Sans Pro", size = rel(1.2),
                              hjust = 0.5),
    axis.text.x =  element_text(family = "Source Sans Pro", size = rel(1.0),                           ),
    axis.text.y = element_text(family = "Source Sans Pro", size = rel(1.0)),
    
    axis.line = element_line(size = 0.5, colour = "black"),
    axis.title.y = element_text(hjust = 0.5),
    axis.title.x = element_text(hjust = 0.5),
    title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )
}

#Loading data
hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')
hotels

#exploring data

dim(hotels) #32 variables & 119390 observations

glimpse(hotels)

colnames(hotels)

summary(hotels)


#checking for missing values
sapply(hotels, function(x) sum(is.na(x)))

#variable child has 4 missing values

#*************************************************************************************
#Plots
#*************************************************************************************

#type of meals by repetition visits
hotels$is_repeated_guest <- as.factor(hotels$is_repeated_guest)
meals <- hotels %>% 
  group_by(meal, is_repeated_guest) %>%
  summarise(Meanval = round(mean(adults),2))
meals

ggplot(meals, aes(x = meal, y = Meanval, fill = is_repeated_guest)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.6)+
  geom_text(aes(label = paste(Meanval, sep = "")), size  =4,
            position = position_dodge(0.5),
            vjust = -0.25, hjust = 0.5)+
  labs(x = "Type of Meals", y = "Average", 
       title = "Distribution of type of meals by repetition visit",
       subtitle = "") + 
  mytheme()