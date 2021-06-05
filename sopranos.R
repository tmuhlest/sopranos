### Sopranos visuals
setwd("~/NC State")
library(tidyverse)
library(readxl)
library(RColorBrewer)

# read in data
df <- read_excel("sopranos.xlsx", 7)
views <- read_excel("sopranos.xlsx", 9)

# data cleaning
temp <- table(df$Killer1) %>% as.data.frame()
temp1 <- table(df$Killer2) %>% as.data.frame()
tk <- merge(temp, temp1, by = "Var1", all = TRUE)
tk[is.na(tk)] <- 0
tk$total <- tk$Freq.x + tk$Freq.y
# 3 people actually killed big pussy so Paulie needs to get credit for it which is given here
tk$total[tk$Var1 == "Paulie Walnuts"] <- tk$total[tk$Var1 == "Paulie Walnuts"] + 1
names(tk) <- c("Name", "Killer1", "Killer2", "Kills")
rm(temp, temp1)

# Murders by character
tk %>%
  arrange(desc(Kills)) %>%
  filter(Kills > 2) %>% 
  ggplot( aes(x = reorder(Name, -Kills), y = Kills, fill = as.factor(Name))) + 
  geom_bar(stat = "summary", fun = "mean" ) +
  ggtitle("Murders by Soprano's Characters") +
  labs(x="Name", y="Number of Murders", fill = "Name") +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme_minimal() 
  
# Murders by Season
df %>%
  filter(Murdered == "Y") %>% 
  group_by(Season) %>% 
  summarise(Kills = length(Season)) %>% 
  mutate(Season = as.factor(Season)) %>% 
  mutate(Kills = as.numeric(Kills)) %>% 
  ggplot( aes(x = Season, y = Kills, fill = as.factor(Season))) + 
  geom_bar(stat = "summary", fun = "mean" ) +
  ggtitle("Murders by Soprano's Season") +
  labs(x="Season", y="Number of Murders", fill = "Season") +
  theme_minimal()   

# data cleaning
temp <- df %>%
  group_by(Episode) %>% 
  summarize(num_kills = length(Season))

views <- merge(views, temp, by = "Episode", all.x = TRUE)
rm(temp) 
views %>% 
  mutate(Episode_num = as.factor(Episode_num))
views$num_kills[is.na(views$num_kills)] <- 0

# Murders by episode
views %>% 
  ggplot(aes(x = Episode_num, y = num_kills)) +
  geom_line(color = "darkred") +
  ggtitle("Soprano's Murders by Episode") +
  labs(x="Episode Number", y="Number of Murders") +
  theme_minimal() 
  
# Views by episode
views %>% 
  ggplot(aes(x = Episode_num, y = Viewers)) +
  geom_line(color = "steelblue") +
  ggtitle("Soprano's Viewers by Episode") +
  labs(x="Episode Number", y="Number of Viewers (Millions)") +
  theme_minimal() 

# Murders and Views by episode
ggplot(views, aes(x=Episode_num)) + 
  geom_line(aes(y = num_kills), color = "darkred") + 
  geom_line(aes(y = Viewers), color="steelblue", linetype="twodash") + 
  ggtitle("Soprano's Murders by Episode and Viewers (Millions)") +
  labs(x="Episode Number", y="Number of Murders/ Viewers (Millions") +
  theme_minimal() 

# correlation of viewers and murders removing the first season
temp <- views %>% filter(Season != 1)
temp %>% 
  ggplot(aes(x = Viewers, y = num_kills)) +
  geom_point(color = "darkgreen") + 
  ggtitle("Number of Kills vs Number of Viewers (Millions) [Seasons 2 - 6]") +
  labs(x="Viewers (Millions)", y="Number of Murders") +
  theme_minimal() 
cor(temp$Viewers, temp$num_kills)
rm(temp)
  # There is no correlation between murders and viewers