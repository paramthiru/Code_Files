# Author: Darshana Anandan
# Guided Project: NYC Schools
# Description: 
##  1) To understand how NYC high schools' demographics (race, sex, income, 
##    etc.) affect student performance.
##  2) To explore if student, teacher, and parent perceptions of NYC school 
##    quality is related to demographic and academic success metrics.
##  3) Do students, teachers, and parents have similar perceptions of NYC school 
##    quality?

# ==============================================================================
# Clean the environment

rm(list = ls())
setwd("C:/Users/Darshana/Documents/Data_Samples/NYC_School_Data")

# ==============================================================================
# Import libraries

#install.packages("readr")
#install.packages("tidyverse")
library(readr)
library(tidyverse)
library(purrr)
library(dplyr)
library(ggplot2)

# ==============================================================================
# PART 1: NYC high schools' demographics & student performance.
# ==============================================================================

# Importing the Data

sat_results <- read_csv("sat_results.csv")
ap_2010 <- read_csv("ap_2010.csv")
class_size <- read_csv("class_size.csv")
demographics <- read_csv("demographics.csv")
graduation <- read_csv("graduation.csv")
hs_directory <- read_csv("hs_directory.csv")

# ==============================================================================
# Cleaning the Data

## SAT results data: Changing data types and creating new variables

map(sat_results, class)
sat_results <- sat_results %>%
  mutate(across(contains("SAT"), as.numeric))%>%
  mutate(avg_sat_score = `SAT Writing Avg. Score` + 
           `SAT Critical Reading Avg. Score` + `SAT Math Avg. Score`)
map(sat_results, class)

## AP exam data: Changing data types and creating new variables

map(ap_2010, class)
ap_2010 <- ap_2010 %>%
  mutate(across(3:5, as.numeric)) %>%
  mutate(exams_per_student = `Total Exams Taken`/`AP Test Takers`) %>%
  mutate(high_score_percent = 
           (`Number of Exams with scores 3 4 or 5`/`Total Exams Taken`)*100)

## Class size data

## Simplifying the dataframe and creating new variables
class_size <- class_size %>%
  filter(`GRADE` == "09-12", `PROGRAM TYPE` == "GEN ED")%>%
  group_by(`CSD`, `SCHOOL CODE`, `SCHOOL NAME`)%>%
  summarize(
    avg_class_size = mean(`AVERAGE CLASS SIZE`),
    avg_largest_class = mean(`SIZE OF LARGEST CLASS`),
    avg_smallest_class = mean(`SIZE OF SMALLEST CLASS`))
  
## Creating a Key using string manipulation
class_size <- class_size %>%
  mutate(DBN = str_c(CSD, `SCHOOL CODE`, sep = "")) %>%
  mutate(DBN = str_pad(DBN, width=6, side='left', pad="0"))

## Graduation data

### Simplifying the dataframe
graduation <- graduation %>%
  filter(`Demographic` == "Total Cohort", `Cohort` == "2006") %>%
  select(`DBN`, `School Name`, `Total Grads - % of cohort`, 
         `Dropped Out - % of cohort`)

### Parsing numbers from strings
graduation <- graduation %>%
  mutate(`Total Grads - % of cohort` = parse_number(`Total Grads - % of cohort`),
         `Dropped Out - % of cohort` = parse_number(`Dropped Out - % of cohort`))

## Demographics data: Simplifying the dataframe

demographics <- demographics %>%
  filter(`schoolyear` == "20112012", `grade9` != "NA") %>%
  select( `DBN`, contains("percent"), contains("per"), 
          `total_enrollment`)

## High school directory

## Simplifying the dataframe
hs_directory <- hs_directory %>%
  select(dbn, school_name, `Location 1`, boro)%>%
  rename(DBN = dbn)

## Splitting Strings to extract data on latitudes and longitudes
hs_directory <- hs_directory %>%
  separate(col=`Location 1`, 
           into = c("string_1", "string_2", "string_3"), 
           sep="\n") %>%
  select(- string_1, -string_2) %>%
  rename(lat_long=string_3) %>%
  separate(col=lat_long,
         into = c("lat", "long"),
         sep=",") %>%
  mutate(lat=str_sub(lat,2,-1), long=str_sub(long,1,-2)) %>%
  mutate(across(c(lat, long), as.numeric))

# ==============================================================================
# Joining the Data

## Checking for duplicates across dataframes
sat_results_duplicated <- sum(duplicated(sat_results$DBN))
ap_2010_duplicated <- sum(duplicated(ap_2010$DBN))
class_size_duplicated <- sum(duplicated(class_size$DBN))
demographics_duplicated <- sum(duplicated(demographics$DBN))
graduation_duplicated <- sum(duplicated(graduation$DBN))
hs_directory_duplicated <- sum(duplicated(hs_directory$DBN))

duplicate_DBN <- list( "sat_results" = sat_results_duplicated, 
                       "ap_2010" = ap_2010_duplicated, 
                       "class_size" = class_size_duplicated,
                       "demographics" = demographics_duplicated, 
                       "graduation" = graduation_duplicated, 
                       "hs_directory" = hs_directory_duplicated)
print(duplicate_DBN)

## Removing the duplicates
ap_2010 <- ap_2010 %>%
  distinct(DBN, .keep_all = TRUE)

## Joining the dataframes
combined <- sat_results %>%
  full_join(ap_2010, by="DBN")%>%
  left_join(class_size, by="DBN")%>%
  left_join(demographics, by="DBN")%>%
  left_join(graduation, by="DBN")%>%
  left_join(hs_directory, by="DBN")

## Cleaning the combined data
combined <- combined %>%
  select(-SchoolName, -`SCHOOL NAME.y`, -`School Name`, -school_name)%>%
  rename(school_name = `SCHOOL NAME.x`)

write.csv(combined, "combined.csv")

# ==============================================================================
# Basic data analysis

## Visualizing relationships between variables using scatter plots
ggplot(data=combined,
       aes(x=frl_percent, y=avg_sat_score)) +
  geom_point()
ggplot(data=combined,
       aes(x=ell_percent, y=avg_sat_score)) +
  geom_point()
ggplot(data=combined,
       aes(x=sped_percent, y=avg_sat_score)) +
  geom_point()

## Pivoting a dataframe into longer one
### According to Socio-economic indicators
combined_socio_longer <- combined %>%
  pivot_longer(cols = c(frl_percent, ell_percent, sped_percent),
               names_to = "socio_indicator",
               values_to = "percent")
ggplot(data = combined_socio_longer,
       aes(x = percent, y = avg_sat_score, color = socio_indicator)) +
  geom_point()
### According to Race-related indicators
combined_race_longer <- combined %>%
  pivot_longer(
    cols = c(asian_per, black_per, hispanic_per, white_per),
    names_to = "race",
    values_to = "percent")
ggplot(data = combined_race_longer, 
       aes(x=percent, y=avg_sat_score, color=race)) +
  geom_point()

## Correlation analysis
cor_mat <- combined %>%
  select(where(is.numeric)) %>%
  cor(use="pairwise.complete.obs")
cor_tib <- cor_mat %>%
  as_tibble(rownames = "variable")
apscore_cors <- cor_tib %>%
  select(variable, high_score_percent)%>%
  filter(high_score_percent >0.25| high_score_percent< -0.25) 
print(cor_tib)

## Summary stats based on borough
summary_1 <- combined %>%
  drop_na(boro)%>%
  group_by(boro) %>%
  summarize(sat_avg=mean(avg_sat_score, na.rm=TRUE))

# ==============================================================================
# PART 2: Student, Teacher, and Parent perceptions of NYC schools
# ==============================================================================

# Importing the Data

combined <- read_csv("combined.csv") 
survery_community <- read_tsv("masterfile11_gened_final.txt")
survey_d75 <- read_tsv("masterfile11_d75_final.txt")
