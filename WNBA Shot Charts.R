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

#Add records to team name
team_record <- c()
for (team in wnba_shots$team_name) {
  if (team == "Atlanta") {
    team_record <- c(team_record, "Atlanta Dream (5-6)")
  } else if (team == "Connecticut") {
    team_record <- c(team_record, "Connecticut Sun (8-3)")
  } else if (team == "Chicago") {
    team_record <- c(team_record, "Chicago Sky (4-7)")
  } else if (team == "Dallas") {
    team_record <- c(team_record, "Dallas Wings (5-6)")
  } else if (team == "Indiana") {
    team_record <- c(team_record, "Indiana Fever (1-11)")
  } else if (team == "Las Vegas") {
    team_record <- c(team_record, "Las Vegas Aces (8-3)")
  } else if (team == "Los Angeles") {
    team_record <- c(team_record, "Los Angeles Sparks(4-5)")
  } else if (team == "Minnesota") {
    team_record <- c(team_record, "Minnesota Lynx (4-5)")
  } else if (team == "New York") {
    team_record <- c(team_record, "New York Liberty (6-4)")
  } else if (team == "Phoenix") {
    team_record <- c(team_record, "Phoenix Mercury (5-6)")
  } else if (team == "Seattle") {
    team_record <- c(team_record, "Seattle Storm (10-2)")
  } else {
    team_record <- c(team_record, "Washington Mystics (4-6)")
  }
}

wnba_shots <- mutate(wnba_shots, "team_record" = team_record)


#Add a column of type of shots
type_of_shot <- c() 
for(d in wnba_shots$distance) {
  if (d > 21) {
    type_of_shot <- c(type_of_shot, "3 Pointer")
  } else if (d >15 & d<= 21) {
    type_of_shot <- c(type_of_shot, "Midrange")
  } else if (d > 3 & d <= 15) {
    type_of_shot <- c(type_of_shot, "Inside the Paint")
  } else {
    type_of_shot <- c(type_of_shot, "Layup/Near Basket Shot")
  }
}
wnba_shots <- mutate(wnba_shots, "type_of_shot" = type_of_shot)

write_csv(wnba_shots, "/Users/matthewyep/Desktop/Carnegie Mellon/cmsacamp-eda-project/wnba_shots.csv")

wnba_shots <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/cmsacamp-eda-project/wnba_shots.csv")

#Graph of each team's favorite spots to shoot
geom_basketball(league = "WNBA", full_surf = FALSE, rotate = TRUE) + 
  stat_summary_hex(data = filter(wnba_shots, coordinate_y <= 40), 
                   mapping = aes(x = coordinate_x - 25, y = coordinate_y - 47 + 4, 
                                 z = shot_taken, group = -1), 
                   binwidth = c(4,4),
                   fun = function(x) ifelse (length(x) > 8, sum(x), NA)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange") +
  facet_wrap(~ team_record, ncol = 4) + theme(legend.position = "bottom")

#Graph just the top 4 teams and Indiana
five_teams <- filter(wnba_shots, team_name %in% c("Las Vegas", "Connecticut", "Seattle", "New York","Indiana"))
geom_basketball(league = "WNBA", full_surf = FALSE, rotate = TRUE) + 
  stat_summary_hex(data = filter(five_teams, coordinate_y <= 40), 
                   mapping = aes(x = coordinate_x - 25, y = coordinate_y - 47 + 4, 
                                 z = shot_taken, group = -1), 
                   binwidth = c(4,4),
                   fun = function(x) ifelse (length(x) > 8, sum(x), NA)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange") +
  facet_wrap(~ team_name, ncol = 5) + theme(legend.position = "bottom") 

#Create tables of just the top scorers for Seattle Storm and Las Vegas Aces
seattle_top_scorers <- wnba_shots %>%
  filter(team_name == "Seattle") %>%
  filter(shooter %in% c("Breanna Stewart", "Jewell Loyd", "Sue Bird"))

vegas_top_scorers <- wnba_shots %>%
  filter(team_name == "Las Vegas") %>%
  filter(shooter %in% c("Jackie Young", "A'ja Wilson", "Liz Cambage"))

#Bar charts of the key players favorite types of shots
seattle_top_scorers %>%
  ggplot(aes(x = shooter, 
             fill = type_of_shot)) + 
  geom_bar(position = "dodge", colour = "black") + theme_bw() +
  labs(y = "Count", 
       x = "Shooter",
       title = "The Storm shoot ALOT of three pointers") + 
  scale_fill_manual(values = c("darkgreen","chartreuse4","goldenrod1","gold1")) +
  theme(panel.background = element_rect(fill = "burlywood"),
        axis.text=element_text(size=12),
        axis.title = element_text(size=14))

vegas_top_scorers %>%
  ggplot(aes(x = shooter, 
             fill = type_of_shot)) + 
  geom_bar(position = "dodge", colour = "black") + theme_bw() +
  labs(y = "Count", 
       x = "Shooter",
       title = "The Aces attack the paint and take high percentage shots close to the basket") +
  scale_fill_manual(values = c("gray14","grey74","red","gold3")) +
  theme(panel.background = element_rect(fill = "burlywood"),
        axis.text=element_text(size=12),
        axis.title = element_text(size=14))


# Graoh of where teams make and miss shots
geom_basketball(league = "WNBA", full_surf = FALSE, rotate = TRUE) +
  geom_point(data = filter(wnba_shots, coordinate_y <= 40), 
             aes(x = coordinate_x - 25, y = coordinate_y - 47 + 4,
                 color = scoring_play),
             alpha = 0.25, size = 0.5) + 
  scale_color_manual(values = c("darkblue", "darkorange")) +
  facet_wrap(~ team_name, ncol = 4)












