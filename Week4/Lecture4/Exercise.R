install.packages("foreign")
library(foreign)
library(dplyr)
library(jtools)

#################################################
# import physical activity and demographic data #
#################################################

# import individual activity data for 1999, 2001, 2003
ACTIVITY_1999 <- read.xport("C:/Users/Tony/Documents/GitHub/MGMT-298D/Week4/Lecture4/PAQIAF.XPT")
ACTIVITY_2001 <- read.xport("C:/Users/Tony/Documents/GitHub/MGMT-298D/Week4/Lecture4/PAQIAF_B.XPT")
ACTIVITY_2003 <- read.xport("C:/Users/Tony/Documents/GitHub/MGMT-298D/Week4/Lecture4/PAQIAF_C.XPT")

# import demographic data for 1999, 2001, 2003
DEMO_1999 <- read.xport("C:/Users/Tony/Documents/GitHub/MGMT-298D/Week4/Lecture4/DEMO.XPT")
DEMO_2001 <- read.xport("C:/Users/Tony/Documents/GitHub/MGMT-298D/Week4/Lecture4/DEMO_B.XPT")
DEMO_2003 <- read.xport("C:/Users/Tony/Documents/GitHub/MGMT-298D/Week4/Lecture4/DEMO_C.XPT")

################
# combine rows #
################

ACTIVITY <- bind_rows(ACTIVITY_1999, ACTIVITY_2001, ACTIVITY_2003)
DEMO <- bind_rows(DEMO_1999, DEMO_2001, DEMO_2003)
rm(ACTIVITY_1999, ACTIVITY_2001, ACTIVITY_2003, DEMO_1999, DEMO_2001, DEMO_2003)

################################
# exercise hours by individual #
################################

# attaching dataset loads it
attach(ACTIVITY)
View(ACTIVITY)

# hours per activity
ACTIVITY$HOURS_ACTIVITY <- PADTIMES * (PADDURAT / 60)

# intensity per activity
ACTIVITY$INTENSITY_ACTIVITY <- PADTIMES * (PADDURAT / 60) * PADMETS

# total exercise hours per individual
HOURS <- ACTIVITY %>% 
  group_by(SEQN) %>% 
  summarise(HOURS_TOTAL = sum(HOURS_ACTIVITY))

# average intensity individual
INTENSITY <- ACTIVITY %>% 
  group_by(SEQN) %>% 
  summarise(INTENSITY_AVG = sum(INTENSITY_ACTIVITY)/sum(HOURS_ACTIVITY))

# name of activity from NHANES
ACTIVITY$DESCRIPTION <- factor(ACTIVITY$PADACTIV, 
                               labels=c("AEROBICS","BASEBALL","BASKETBALL","BICYCLING","BOWLING","DANCE","FISHING","FOOTBALL",
                                        "GARDENING", "GOLF", "HIKING", "HOCKEY", "HUNTING", "JOGGING", "KAYAKING", "PUSHUPS", 
                                        "RACQUETBALL", "ROLLERBLADING", "ROWING", "RUNNING", "SITUPS", "SKATING", "SKIING_CROSS", "SKIING_DOWNHILL", 
                                        "SOCCER", "SOFTBALL", "STAIRS", "STRETCHING", "SWIMMING", "TENNIS", "TREADMILL", "VOLLEYBALL", "WALKING", 
                                        "WEIGHTS", "YARDWORK", "BOXING", "FRISBEE", "HORSEBACK_RIDING", "MARTIAL_ARTS", "WRESTLING", "YOGA", 
                                        "CHEERLEADING", "CHILDREN_GAMES", "ROPE_JUMPING", "SKATEBOARDING", "SURFING", "TRAMPOLINE", "OTHER"))

# must remember to also detach dataset 
detach(ACTIVITY)

#################
# merge columns #
#################

# merge activity hours and average intensity
EXERCISE <- merge(HOURS, INTENSITY, by = "SEQN")

# create dummy variable for any exercise if HOURS_TOTAL > 0
EXERCISE$ANY <- EXERCISE$HOURS_TOTAL > 0

# create logged variables
EXERCISE$LN_HOURS <- log(EXERCISE$HOURS_TOTAL)
EXERCISE$LN_INTENSITY <- log(EXERCISE$INTENSITY_AVG)

# merge with demographic data
MERGED <- full_join(EXERCISE, DEMO, by="SEQN")

#####################
# clean up datasets #
#####################

# select the variables we wish to keep
vars <- c("SEQN", "HOURS_TOTAL", "INTENSITY_AVG", "LN_HOURS", "LN_INTENSITY", "ANY", "INDHHINC", "RIDRETH1", "RIDAGEYR", "DMDMARTL", "RIAGENDR")
CLEAN <- MERGED[vars]
rm(vars)

# replace NA values with 0 for any exercise
CLEAN$ANY[is.na(CLEAN$ANY)] <- 0

# remove anyone under age 25 (based on Meltzer and Jena paper)
CLEAN <- CLEAN[CLEAN$RIDAGEYR >= 25,]

# cut continuous variable into buckets
CLEAN$AGE <- cut(CLEAN$RIDAGEYR, 
                 breaks = c(0, 25, 35, 45, 55, 65, 75, Inf), 
                 labels = c("Below_25", "25_35", "35_45", "45_55", "55_65", "65_75", "Above_75"))

# create factor variables
CLEAN$GENDER <- factor(CLEAN$RIAGENDR, 
                       labels = c("Male", "Female"))

CLEAN$MARITAL <- factor(CLEAN$DMDMARTL, 
                        labels = c("Married", "Widowed", "Divorced", "Divorced", "Never_Married", "Living_Partner", "Refused", "Unsure"))

CLEAN$RACE <- factor(CLEAN$RIDRETH1, 
                     labels=c("Hispanic", "Hispanic", "White", "Black", "Other/Multi"))

CLEAN$INCOME <- cut(CLEAN$INDHHINC,
                    breaks = c(0, 4.5, 7.5, 10.5, 11.5, 76, 98, Inf), 
                    labels = c("Below_20k", "20k_45k", "45k_75k", "Above_75k", "Below_20k", "Refused", "Unsure"))



# keep only new variable definitions
vars <- c("SEQN", "HOURS_TOTAL", "INTENSITY_AVG", "LN_HOURS", "LN_INTENSITY", "ANY", "INCOME", "RACE", "AGE", "MARITAL", "GENDER")
CLEAN <- CLEAN[vars]
rm(vars)

# save datasets
write.csv(CLEAN, "C:/Users/elong/Dropbox/UCLA/HealthcareAnalytics/Lectures/Class4/NHANES.csv", row.names = FALSE)
write.csv(ACTIVITY, "C:/Users/elong/Dropbox/UCLA/HealthcareAnalytics/Lectures/Class4/ACTIVITY.csv", row.names = FALSE)

# remove unused datasets
rm(DEMO, EXERCISE, HOURS, INTENSITY, MERGED)