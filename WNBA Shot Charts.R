# Load necessary packages -------------------------------------------------
library(wehoop)
library(tidyverse)
library(hexbin)
library(sportyR)

# Read in the WNBA shots data ---------------------------------------------
wnba_shots <- read_csv('http://www.stat.cmu.edu/cmsac/sure/2021/materials/data/eda_projects/wnba_shots_2021.csv')

#These two tables are necessary for later faceting by team name in the shot charts
home_teams <- wnba_shots %>%
  dplyr::select(home_team_id, home_team_name, home_team_abbrev) %>%
  distinct()

team_table <- home_teams %>%
  rename(team_id = home_team_id, team_name = home_team_name,
         team_abbrev = home_team_abbrev)

#Dig out just the player names from the text column
wnba_shots <- mutate(wnba_shots, shooter = word(text, 1, 2))

#Add distance column
wnba_shots <- mutate(wnba_shots, distance = round(sqrt((coordinate_x - 25)^2 + coordinate_y^2))) 
  

# We join the table above to our original table
wnba_shots <- wnba_shots %>%
  left_join(team_table, by = "team_id") 

#Remove shot data that are free throws becuase the x and y coords are wacky
wnba_shots <- filter(wnba_shots, 
         str_detect(type_text, "Free Throw", negate = TRUE))

#Add column of 1s. This is helpful for hexagonal shot chart
column_of_ones <- replicate(6438, 1)
wnba_shots <- mutate(wnba_shots, "shot_taken" = column_of_ones)

#write_csv(wnba_shots, "/Users/matthewyep/Desktop/Carnegie Mellon/cmsacamp-eda-project/wnba_shots.csv")

wnba_shots <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/cmsacamp-eda-project/wnba_shots.csv")

# Graoh of where teams make and miss shots
geom_basketball(league = "WNBA", full_surf = FALSE, rotate = TRUE) +
  geom_point(data = filter(wnba_shots, coordinate_y <= 40), 
             aes(x = coordinate_x - 25, y = coordinate_y - 47 + 4,
                 color = scoring_play),
             alpha = 0.25, size = 0.5) + 
  scale_color_manual(values = c("darkblue", "darkorange")) +
  facet_wrap(~ team_name, ncol = 4)


#Graph of each team's favorite spots to shoot
geom_basketball(league = "WNBA", full_surf = FALSE, rotate = TRUE) + 
  stat_summary_hex(data = filter(wnba_shots, coordinate_y <= 40), 
                   mapping = aes(x = coordinate_x - 25, y = coordinate_y - 47 + 4, 
                                 z = shot_taken, group = -1), 
                   binwidth = c(4,4),
                   fun = function(x) ifelse (length(x) > 8, sum(x), NA)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange") +
  facet_wrap(~ team_name, ncol = 4) + theme(legend.position = "bottom") +
  coord_fixed() 

top4 <- filter(wnba_shots, team_name %in% c("Las Vegas", "Connecticut", "Seattle", "New York"))

type_of_shot <- c() 
for(d in wnba_shots$distance) {
  if (d > 21) {
    type_of_shot <- c(type_of_shot, "3 Pointer")
  } else if (17 < d <= 21) {
    type_of_shot <- c(type_of_shot, "Long 2")
  } else if (4 < d <= 17) {
    type_of_shot <- c(type_of_shot, "Midrange")
  } else {
    type_of_shot <- c(type_of_shot, "Layup/Near Basket Shot")
  }
}

top4_first_options = filter(top4, shooter %in% c("Breanna Stewart", "Jonquel Jones", "Jackie Young", "Betnijah Laney"))

top4 %>%
  ggplot(aes(x = team_name, 
             fill = type_text)) + 
  geom_bar(position = "dodge") + theme_bw()

top4_first_options %>%
  ggplot(aes(x = shooter, 
             fill = type_text)) + 
  geom_bar(position = "dodge") + theme_bw() +
  labs(y = "Count", 
       x = "Shooter",
       title = "Shot Locations for each of the Top 4 WNBA Team's First Options")







