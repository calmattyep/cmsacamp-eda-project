# Load necessary packages -------------------------------------------------
library(wehoop)
library(tidyverse)
library(hexbin)
library(sportyR)

# Read in the WNBA shots data ---------------------------------------------
wnba_shots <- read_csv('http://www.stat.cmu.edu/cmsac/sure/2021/materials/data/eda_projects/wnba_shots_2021.csv')

home_teams <- wnba_shots %>%
  dplyr::select(home_team_id, home_team_name, home_team_abbrev) %>%
  distinct()

# I'll make a new table modifying the home_teams table (could do this for the 
# away_teams) and simply rename the columns to just drop the home_ portion
team_table <- home_teams %>%
  rename(team_id = home_team_id, team_name = home_team_name,
         team_abbrev = home_team_abbrev)

# Now I can join this info over back to the original shot data using left_join
wnba_shots <- wnba_shots %>%
  left_join(team_table, by = "team_id") 

#Remove shot data that are free throws becuase the x and y coords are wacky
wnba_shots <- filter(wnba_shots, 
         str_detect(type_text, "Free Throw", negate = TRUE))

#add column of 1s, then apply sum as the function 
column_of_ones = replicate(6438, 1)
wnba_shots <- mutate(wnba_shots, "shot_taken" = column_of_ones)

# Graoh of where teams make and miss shots
geom_basketball(league = "WNBA", full_surf = FALSE, rotate = TRUE) +
  geom_point(data = wnba_shots, 
             aes(x = coordinate_x - 25, y = coordinate_y - 47 + 4,
                 color = scoring_play),
             alpha = 0.25, size = 0.5) + # modifying the point size
  scale_color_manual(values = c("darkblue", "darkorange")) +
  # Facet by team name and make 4 columns of plots
  facet_wrap(~ team_name, ncol = 4)

geom_basketball(league = "WNBA", full_surf = FALSE, rotate = TRUE) + 
  stat_summary_hex(data = filter(wnba_shots, coordinate_y <= 40), 
                   mapping = aes(x = coordinate_x - 25, y = coordinate_y - 47 + 4, 
                                 z = shot_taken, group = -1), 
                   binwidth = c(3,3),
                   fun = function(x) ifelse (length(x) > 8, sum(x), NA)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange") +
  facet_wrap(~ team_name, ncol = 4) + theme(legend.position = "bottom") +
  coord_fixed() 


