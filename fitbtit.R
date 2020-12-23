library(jsonlite)
library(tidyverse)
library(patchwork)
library(ggpubr)
library(gganimate)
library(waffle)
library(viridis)

d1 <- fromJSON("steps-2020-01-20.json", flatten=TRUE)
d2 <- fromJSON("steps-2020-02-19.json", flatten=TRUE)
d3 <- fromJSON("steps-2020-03-20.json", flatten=TRUE)
d4 <- fromJSON("steps-2020-04-19.json", flatten=TRUE)
d5 <- fromJSON("steps-2020-05-19.json", flatten=TRUE)
d6 <- fromJSON("steps-2020-06-18.json", flatten=TRUE)
d7 <- fromJSON("steps-2020-07-18.json", flatten=TRUE)
d8 <- fromJSON("steps-2020-08-17.json", flatten=TRUE)
d9 <- fromJSON("steps-2020-09-16.json", flatten=TRUE)
d10 <- fromJSON("steps-2020-10-16.json", flatten=TRUE)
d11 <- fromJSON("steps-2020-11-15.json", flatten=TRUE)
d12 <- fromJSON("steps-2020-12-15.json", flatten=TRUE)

sleep <- read.csv("sleep_score.csv")

merged <- do.call("rbind", list(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12))
merged$value <- as.numeric(merged$value)

new <- do.call( rbind , strsplit( as.character(merged$dateTime ) , " " ) )
df <- cbind( merged , Date = new[,1] , Time = new[,2] )

df <- df %>%
  select(2:3)

agg <- df %>%
  group_by(Date) %>% 
  summarise(num = n(),
            total = sum(value))


sleep2 <- do.call( rbind , strsplit( as.character(sleep$timestamp ) , "T0" ) )
sleep3 <- cbind(sleep , Date = sleep2[,1])

sleep3 <- sleep3 %>%
  select(3,7,10)

sleep3 <- sleep3 %>%
  group_by(Date) %>% 
  summarise(score = sum(overall_score),
            deep_sleep = sum(deep_sleep_in_minutes)) 

sleep3$Date <- as.character(sleep3$Date)
sleep3$Date <- as.Date(sleep3$Date, "%Y-%m-%d")
#sleep3$Date <- format(as.Date(sleep3$Date, format="%Y-%m-%d"),"%m-%d-%Y")


agg$Date <- as.character(agg$Date)
agg$Date <- as.Date(agg$Date, "%m/%d/%Y")
agg$Date <- sub('0', '2', agg$Date)


test <- full_join(sleep3, agg, by = "Date")
test$Date <- as.Date(test$Date, "%Y-%m-%d")


monthlySteps <- test %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(month = format(Date, '%m')) %>% 
  group_by(month) %>% 
  summarise(avSteps = mean(total))


test <- test %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(month = format(Date, '%m')) %>%
  mutate(day = format(Date, '%d'))


# Plotting ----------------------------------------------------------------
#average daily steps per month, barplot
ggplot(monthlySteps, aes(x = month, y = avSteps, fill = month))+
  geom_bar(stat = "identity", alpha = .75, color = "black")+
  coord_flip()+
  geom_hline(yintercept = mean(monthlySteps$avSteps), color = "yellow", linetype = "dashed")+
  scale_x_discrete(limits = rev(levels(monthlySteps$month)))+
  geom_text(data = NULL, x = 12, y = 550, label = "January", color = "white")+
  geom_text(data = NULL, x = 11, y = 575, label = "February", color = "white")+
  geom_text(data = NULL, x = 10, y = 400, label = "March", color = "white")+
  geom_text(data = NULL, x = 9, y = 310, label = "April", color = "white")+
  geom_text(data = NULL, x = 8, y = 310, label = "May", color = "white")+
  geom_text(data = NULL, x = 7, y = 350, label = "June", color = "white")+
  geom_text(data = NULL, x = 6, y = 310, label = "July", color = "white")+
  geom_text(data = NULL, x = 5, y = 500, label = "August", color = "white")+
  geom_text(data = NULL, x = 4, y = 720, label = "September", color = "white")+
  geom_text(data = NULL, x = 3, y = 550, label = "October", color = "white")+
  geom_text(data = NULL, x = 2, y = 700, label = "November", color = "white")+
  geom_text(data = NULL, x = 1, y = 700, label = "December", color = "white")+
  theme(
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = "#20354d"),
    panel.grid = element_blank(),
    axis.line.x = element_line(color="white"),
    #axis.line.y = element_line(color="white"),
    axis.text = element_text(color = "white", size = 10),
    axis.title = element_text(color = "white"),
    plot.title = element_text(color = "white", hjust = .5, size = 20),
    axis.title.y = element_text(margin = margin(r=8)),
    legend.position = "none",
    axis.text.y = element_blank()
  )+
  labs(
    title = "Average Daily Steps per Month in 2020",
    y = "Average Steps",
    x = ""
  )



#step time series
p1 <- ggplot(test, aes(x = Date, y = total))+
  geom_line(color = "#28c99e") + 
  geom_hline(yintercept = mean(test$total), color = "yellow")+
  xlab("")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  scale_y_continuous(breaks = c(0,5000,10000,15000,20000,25000,30000))+
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = "#20354d"),
    panel.grid = element_blank(),
    axis.line.x = element_line(color="white"),
    axis.line.y = element_line(color="white"),
    axis.text = element_text(color = "white", size = 10),
    axis.title = element_text(color = "white"),
    plot.title = element_text(color = "white", hjust = -.21, size = 20),
    plot.subtitle = element_text(color = "white",  size = 10, hjust = -.21),
    axis.title.y = element_text(margin = margin(r=8))
  )+
  labs(
    title = "Steps per Day in 2020",
    subtitle = "Time series visualization of the number of steps \nI took each day in 2020, as recorded by my Fitbit",
    y = "Total Steps",
    x = "Month"
  )

#animate the plot by date
anim <- p1 +
  geom_point()
  transition_reveal(Date)

anim_save("step.gif", animation = last_animation())



#sleep plot
p2 <- test %>%
  filter(score < 100) %>%
  ggplot(aes(x = Date, y = score))+
    geom_line(color = "palevioletred2")+ 
    geom_hline(yintercept = mean(test$score), color = "yellow")+
    xlab("")+
    scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = "#20354d"),
    panel.grid = element_blank(),
    axis.line.x = element_line(color="white"),
    axis.line.y = element_line(color="white"),
    axis.text = element_text(color = "white", size = 10),
    axis.title = element_text(color = "white"),
    plot.title = element_text(color = "white", hjust = -.11, size = 20),
    plot.subtitle = element_text(color = "white",  size = 10, hjust = -.13),
    axis.title.y = element_text(margin = margin(r=8))
  )+
  labs(
    y = "Fitbit Sleep Score",
    x = "Month",
    title = "2020 Sleep Score",
    subtitle = "Time series visualization of my nightly sleep\n score in 2020, as recorded by my Fitbit"
  )


ggarrange(p1, p2,  ncol = 1)

