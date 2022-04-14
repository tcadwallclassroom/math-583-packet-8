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

