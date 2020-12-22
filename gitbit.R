library(jsonlite)
library(tidyverse)
library(patchwork)
library(ggpubr)
library(gganimate)
library(waffle)

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

ggplot(monthlySteps, aes(x = month, y = avSteps))+
  geom_bar(stat = "identity")

monthlySteps %>%
  geom_waffle(avSteps, n_rows = 12, color = "grey97")
 
  
  # geom_richtext(data = subset(data, expedition_role == "climbers"), aes(x = 1, y = Inf, label = paste0("**", n, "**", " climbers<br>")), hjust = 0,  vjust = 0, lineheight = 0.9, size = 3.5, family = f1, color = "grey10", fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  #geom_richtext(data = subset(data, expedition_role == "guides"), aes(x = 51, y = Inf, label = paste0("**", n, "**", " guides<br>")), hjust = 1, vjust = 0, lineheight = 0.9, size = 3.5, family = f1, color = "#4CACEA", fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  scale_fill_manual(
    name = NULL,
    values = c("grey10", "#4CACEA"),
    labels = c("climbers", "guides")
  ) +
  xlim(0, 52) +
  coord_fixed(clip = "off") +
  facet_wrap(vars(month), ncol = 1, labeller = labeller(month = toupper)) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_text(margin = margin(10, 0, 8, 0), family = f1, size = 11),
    plot.background = element_rect(fill = "grey97", color = NA)
  )



#step plot
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

