# Packages
library(tidyverse)
library(ggplot2)
library(devtools)
library(worldfootballR)

df <- fb_player_scouting_report("https://fbref.com/en/players/5ea8ef20/Matteo-Guendouzi", pos_versus = "primary")

player_df <- df[c(150,157,151,159,162,163,176,178,192,196,193,194),]

player_df$index <- 1:12

player_df <- player_df %>%
  mutate (type = case_when(
    index %in% 1:6 ~ 'Attacking',
    index %in% 7:12 ~ 'Possesion'
    ))

## ------ Rotate axis text in plot

# angle of axis text
temp <- (360/(length(player_df$index))/2)
#find the difference in angle between 2 labels and div by two
myAng <- seq(-temp, -360+temp, length.out = length(player_df$index))
#get the angle for every label
ang <-ifelse(myAng < -90, myAng+180, myAng)
#rotate label by 180 in some places for readability
ang <-ifelse(ang < -90, ang+180, ang)

color1 <- "#F5F5F5"
color2 <- "#ADD8E6"
  
ggplot(data = player_df, aes(x = reorder(Statistic, index), y = Percentile, label= Percentile, fill = type)) +
  # add the bar/pizza slices that are colored
  geom_bar(data = player_df, width = 1,
           color = "grey",
           stat = "identity") +
  # wrap bar chart as around polar center
  coord_polar() +
  # add the bg behind each bar (alpha at .5 for slight transparency so the bars standout)
  geom_bar(aes(y=100, fill=type), stat="identity", width = 1, alpha=0.5) +
  # add & customize line that borders whole pizza
    geom_hline(yintercept = seq(0, 100, by = 100),
              color = "grey",
              size = 1) +
  # add & customize lines between each pizza slice 
    geom_vline(xintercept = seq(.5, 12, by = 1),
               color = "gray",
               size = .5) +
  # add percentile labels (labels are choice of fill and color)
   geom_label(color = "gray20", fill = "white", size= 2.5, fontface ="bold", family = "AppleGothic", show.legend = FALSE) +
  # manually set the colors of bars (there are two for this plot)
  scale_fill_manual(values = c(color1, color2)) +
  # theme manipulation to customize plot (play around with the values to your liking)
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "white", color = "white"),
        legend.title = element_blank(),
        legend.text = element_text(color = "gray20", family = "AppleGothic", face = "bold"),
        legend.key.size = unit(.5, "cm"),
        legend.box.spacing = unit(0, "mm"),
        plot.title = element_text(hjust = .5, colour = "gray20", face = "bold", size = 16, family = "AppleGothic"),
        plot.subtitle = element_text(hjust = .5, color = "gray20", size = 9, family = "AppleGothic"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold", size = 6.8, colour = "gray20"),
        axis.title = element_blank(),
        axis.text.x = element_text(face = "bold", size = 7, family = "AppleGothic", angle = ang)) +
  # add title & subtitle
  labs(title = "Matteo Guendouzi in Possession",
       subtitle = "vs. Ligue 1 Midfielders | 2021-2022 Season | via: worldfootballR | @kagopooe", x = NULL, y = NULL)
        
# save plot
ggsave("matteoguendotb.png", height = 6, width = 6, dpi = "retina")
        
        