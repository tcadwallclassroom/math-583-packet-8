## Chi sq independence

library(gssr)

num_vars <- c("age")
cat_vars <- c("sex", "degree", "partyid", "race", "grass", "owngun", "gunlaw")
my_vars <- c(num_vars, cat_vars)

gss18 <- gss_get_yr(2018)
data <- gss18
data <- data %>% 
  select(all_of(my_vars)) %>% 
  mutate(
    # Convert all missing to NA
    across(everything(), haven::zap_missing),
    # Make all categorical variables factors and relabel nicely
    across(all_of(cat_vars), forcats::as_factor)
  )

# Now, recode "degree" to a binary variable, "college"

data <- data %>% 
  mutate(
    college = recode(degree,
                     "lt high school" = "no degree",
                     "high school" = "no degree",
                     "junior college" = "degree",
                     "bachelor" = "degree",
                     "graduate" = "degree"
    )
  )

# and recode "partyid" to a simpler "party":

data <- data %>% 
  mutate(
    party = recode(partyid,
                   "strong democrat" = "DEM",
                   "not str democrat" = "DEM",
                   "ind,near dem" = "IND",
                   "independent" = "IND",
                   "ind,near rep" = "IND",
                   "not str republican" = "REP",
                   "strong republican" = "REP",
                   "other party" = "OTH"
    )
  )


table <- table(data$race, data$gunlaw)
table %>% addmargins() 
table %>% prop.table(1)
chisq.test(table)
chisq.test(table)$resid


vac_data <- c(41462,753863,1191827,6826558)
demo_data <- c(0.005,0.169,0.053,0.773)

vac_data_names <- c(rep("AIAN", 41462),
                    rep("Black", 753863),
                    rep("Other", 1191827),
                    rep("White", 6826558))

vac_data_frame <- data.frame(vaccines = vac_data_names)

vac_prop_vec = c("AIAN" = 0.005,
                 "Black" = 0.169,
                 "Other" = 0.053,
                 "White" = 0.773)

observed_gof_statistic <- vac_data_frame %>%
  specify(response = vaccines) %>%
  hypothesize(null = "point",
              p = vac_prop_vec) %>%
  calculate(stat = "Chisq")

null_dist_gof <- vac_data_frame %>%
  specify(response = vaccines) %>%
  hypothesize(null = "point",
              p = vac_prop_vec) %>%
  generate(reps = 10, type = "draw") %>%
  calculate(stat = "Chisq")

null_dist_gof %>%
  visualize() + 
  shade_p_value(observed_gof_statistic,
                direction = "greater")

p_value_gof <- null_dist_gof %>%
  get_p_value(observed_gof_statistic,
              direction = "greater")

p_value_gof

chisq.test(vac_data,p = demo_data, correct = F)

vaccines <- c(rep("AIAN",41462),
              rep("Black",753863),
              rep("Other",1191827),
              rep("White",6826558))
vacdata <- data.frame(vaccines)

### Police Shooting Data from Washington Post

#|Race/Ethnicity| US Population (in millions)|
#  |---|---|
#  |White (non-Hispanic) | 197 |
#  |Black (non-Hispanic)	|42|
#  |Hispanic (of any race)	|39|
#  |Other  (non-Hispanic)	|49|
#  |Total	|327|
  
library(readr)
fatal_police_shootings_data <- 
  read_csv("data-police-shootings-master/fatal-police-shootings-data.csv")

fps_data <- fatal_police_shootings_data %>% 
  mutate(
    race_full = recode(race,
                     "A" = "Asian",
                     "B" = "Black",
                     "H" = "Hispanic",
                     "N" = "Native American",
                     "O" = "Other",
                     "W" = "White"
    )
  )


demographics_2020 <- c("Asian" = 0.059,
                       "Black" = 0.119,
                       "Hispanic" = 0.195,
                       "Native American" = 0.009,
                       "Other" = 0.045,
                       "White" = 0.573)

table_race <- table(fps_data$race_full)
chisq.test(table_race, p = demographics_2020)
chisq.test(table_race, p = demographics_2020)$residuals

table(fps_data$race_full)
fps_data %>% na.omit(race_full) %>% ggplot(aes(x = race_full)) +
  geom_bar(color = "black",
           fill = "forestgreen")

fps_data %>% na.omit(gender) %>% ggplot(aes(x = gender)) +
  geom_bar(color = "black",
           fill = "forestgreen")

observed_gof_statistic <- fps_data %>%
  specify(response = race_full) %>%
  hypothesize(null = "point",
              p = demographics_2020) %>%
  calculate(stat = "Chisq")


null_dist_gof <- fps_data %>%
  specify(response = race_full) %>%
  hypothesize(null = "point",
              p = demographics_2020) %>%
  generate(reps = 1000, type = "draw") %>% 
  calculate(stat = "Chisq")

null_dist_gof %>%
  visualize() + 
  shade_p_value(observed_gof_statistic,
                direction = "greater")

p_value_gof <- null_dist_gof %>%
  get_p_value(observed_gof_statistic,
              direction = "greater")
