library(tidyverse)
library(janitor)
library(lubridate)

#the csv for motor vehicle accidents is 2 million rows, so I used an api called and filter out only 2023 crashes to save space

api_url <- URLencode("https://data.cityofnewyork.us/resource/h9gi-nx95.csv?$where=crash_date >= '2023-01-01T00:00:01.000' AND crash_date <= '2023-12-31T00:00:01.000' LIMIT 100000")

twenty23crashes <- read_csv(api_url)

write_csv(twenty23crashes, "twenty23crashes.csv")

#used write_csv to export 2023 crashes as csv, then to open up the total amount in QGIS.
#the reason why is because the CSV has all five boroughs but there are a lot of accidents with no borough code, so filtering in R might
#leave out some accidents that are in manhattan, so what I did was put the complete 2023 file in QGIS, and edit it spatially
#to leave only the manhattan points, saved it as a csv in QGIS and opened it here again in R

twenty23crashes_manhattan <- read_csv("twenty23crashes_manhattan.csv")


# i am deleting columns here i don't need
twenty23crashes_manhattan_cleaned <- twenty23crashes_manhattan %>% 
  select(-collision_, -vehicle_ty, -vehicle__1, -vehicle__2, -vehicle__3, -vehicle__4, -crash_time, -borough, -zip_code, -location, -on_street_, -off_street, -cross_stre)

#when I did the API pull, it did not pull the full name of some of the columns, so I'm renaming them according to the API Docs

twenty23crashes_manhattan_cleaned <- twenty23crashes_manhattan_cleaned %>% 
  rename(num_of_persons_injured = number_of_,
         num_of_persons_killed = number_o_1,
         num_of_ped_injured = number_o_2,
         num_of_ped_killed = number_o_3,
         num_of_cyclist_injured = number_o_4,
         num_of_cyclist_killed = number_o_5,
         num_of_motorist_injured = number_o_6,
         num_of_motorist_killed = number_o_7,
         contributing_factor = contributi)
       
#in this next section i'm isolating each "type" of crash by anything greater than 1
  
num_of_persons_injured <- twenty23crashes_manhattan_cleaned %>% 
  filter(num_of_persons_injured >= 1) %>% 
  select(crash_date, latitude, longitude, num_of_persons_injured)

num_of_persons_killed <- twenty23crashes_manhattan_cleaned %>% 
  filter(num_of_persons_killed >= 1) %>% 
  select(crash_date, latitude, longitude, num_of_persons_killed)

num_of_ped_injured <- twenty23crashes_manhattan_cleaned %>% 
  filter(num_of_ped_injured >= 1) %>% 
  select(crash_date, latitude, longitude, num_of_ped_injured)     

num_of_ped_killed <- twenty23crashes_manhattan_cleaned %>% 
  filter(num_of_ped_killed >= 1) %>% 
  select(crash_date, latitude, longitude, num_of_ped_killed)

num_of_cyclist_injured <- twenty23crashes_manhattan_cleaned %>% 
  filter(num_of_cyclist_injured >= 1) %>% 
  select(crash_date, latitude, longitude, num_of_cyclist_injured)

num_of_cyclist_killed <- twenty23crashes_manhattan_cleaned %>% 
  filter(num_of_cyclist_killed >= 1) %>% 
  select(crash_date, latitude, longitude, num_of_cyclist_killed)

num_of_motorist_injured <- twenty23crashes_manhattan_cleaned %>% 
  filter(num_of_motorist_injured >= 1) %>% 
  select(crash_date, latitude, longitude, num_of_motorist_injured)

num_of_motorist_killed <- twenty23crashes_manhattan_cleaned %>% 
  filter(num_of_motorist_killed >= 1) %>% 
  select(crash_date, latitude, longitude, num_of_motorist_killed)

#exporting these as CSVs so I can inport each one individually into QGIS
#and create different symbology for each one #### note: looking back as I worked on the project, I did not need to extract each accident
# because it seems that num_of_persons included ped/motorist/cyclists in the point data, so it was redundant
# and I'm only using persons_injured vs persons_killed

write_csv(num_of_cyclist_injured, "num_of_cyclist_injured.csv")
write_csv(num_of_cyclist_killed, "num_of_cyclist_killed.csv")
write_csv(num_of_motorist_injured, "num_of_motorist_injured.csv")
write_csv(num_of_motorist_killed, "num_of_motorist_killed.csv")
write_csv(num_of_ped_injured, "num_of_ped_injured.csv")
write_csv(num_of_ped_killed, "num_of_ped_killed.csv")
write_csv(num_of_persons_injured, "num_of_persons_injured.csv")
write_csv(num_of_persons_killed, "num_of_persons_killed.csv")

#still interesting to see all the data broken down

################################################################################

# this next bit is me working my first chart of total injuries above/below 60th and total deaths above/below 60th

#i'm totaling numbers here just to have, not separated by 60th st division

total_num_of_persons_injured = sum(num_of_persons_injured$num_of_persons_injured)

total_num_of_persons_killed = sum(num_of_persons_killed$num_of_persons_killed)

#spatially edited the csv's above to make sure i separated above/below 60th st, saved as csv through QGIS, and reading it in to R

total_persons_killed_above_60th <- read_csv("personskilledabove60th.csv")

total_persons_killed_below_60th <- read_csv("personskilledbelow60th.csv")

total_persons_injured_above_60th <- read_csv("personsinjuredabove60th.csv")

total_persons_injured_below_60th <- read_csv("personsinjuredbelow60th.csv")

#totaling numbers for first chart

total_killed_above_60th = sum(total_persons_killed_above_60th$num_of_persons_killed)

total_killed_below_60th = sum(total_persons_killed_below_60th$num_of_persons_killed)

total_injured_above_60th = sum(total_persons_injured_above_60th$num_of_persons_injured)

total_injured_below_60th = sum(total_persons_injured_below_60th$num_of_persons_injured)

#saving sums + labels separately to create a new dataframe, renaming columns, and organizing it to use for Datawrapper/Flourish

total_sums <- c(total_killed_above_60th, total_killed_below_60th, total_injured_above_60th, total_injured_below_60th)

total_labels <- c("Above 60th", "Below 60th", "Above 60th", "Below 60th")

total_sums_and_labels <- data.frame(Sum = total_sums, Label = total_labels)

injury_vs_killed <- c("Killed", "Killed", "Injury", "Injury")

total_sums_and_labels$result_of_accident <- injury_vs_killed

total_sums_and_labels <- rename(total_sums_and_labels, total_num_of_persons = Sum)

total_sums_and_labels <- rename(total_sums_and_labels, above_or_below_60th_st = Label)

########################################################################################

#this is the start of my code for creating my quarter by quarter breakdown chart

#since crash_date was already a column, having installed lubridate, it automatically recognized each quarter accurately when
#i used the code below

total_persons_injured_above_60th$quarter <- quarter(total_persons_injured_above_60th$crash_date)

total_persons_injured_below_60th$quarter <- quarter(total_persons_injured_below_60th$crash_date)

total_persons_killed_above_60th$quarter <- quarter(total_persons_killed_above_60th$crash_date)

total_persons_killed_below_60th$quarter <- quarter(total_persons_killed_below_60th$crash_date)

#totaling by quarter for above/below 60th and injury/death

injured_above_60th_quarterly <- total_persons_injured_above_60th %>%
  group_by(quarter) %>%
  summarise(sum_injured = sum(num_of_persons_injured, na.rm = TRUE))

injured_below_60th_quarterly <- total_persons_injured_below_60th %>%
  group_by(quarter) %>%
  summarise(sum_injured = sum(num_of_persons_injured, na.rm = TRUE))

killed_above_60th_quarterly <- total_persons_killed_above_60th %>%
  group_by(quarter) %>%
  summarise(sum_injured = sum(num_of_persons_killed, na.rm = TRUE))

killed_below_60th_quarterly <- total_persons_killed_below_60th %>%
  group_by(quarter) %>%
  summarise(sum_injured = sum(num_of_persons_killed, na.rm = TRUE))

#to combine all into one dataframe, adding new column for above and below in each dataframe and then binding them

injured_above_60th_quarterly <- injured_above_60th_quarterly %>% 
  mutate(above_or_below = "above")

injured_below_60th_quarterly <- injured_below_60th_quarterly %>% 
  mutate(above_or_below = "below")

injured_quarterly_sum <- rbind(injured_below_60th_quarterly, injured_above_60th_quarterly)

killed_above_60th_quarterly <- killed_above_60th_quarterly %>% 
  mutate(above_or_below = "above")

killed_below_60th_quarterly <- killed_below_60th_quarterly %>% 
  mutate(above_or_below = "below")

killed_quarterly_sum <- rbind(killed_below_60th_quarterly, killed_above_60th_quarterly)

########################################################################################

#this is the start of my code for creating my neighborhood by neighborhood breakdown chart

#i got the NTA for 2020 and spatially joined my point data with polygon data for neighborhoods

#now I am inporting the neighborhood data + joined point data csv I made in QGIS

neighborhood_breakdown_stats <- read_csv("neighborhood_injury_death_statistics.csv")

#deleting columns I don't need

neighborhood_stats_cleaned <- neighborhood_breakdown_stats %>% 
  select(-CountyFIPS, -BoroCode, -NTA2020, -NTAAbbrev, -BoroName, -NTAType, -CDTA2020, -CDTAName, -Shape_Leng, -Shape_Area)

#renaming columns

neighborhood_stats_cleaned_squeaky_clean <- neighborhood_stats_cleaned %>% 
  rename(neighborhood_name = NTAName,
         total_injuries = num_of_persons_injured_sum,
         total_deaths = num_of_persons_killed_sum)

#i forgot to label which neighborhoods were above 60th and below 60th, which I could have probably done more easily in QGIS
#so this is a longer more complicated way of doing it in R

above_or_below <- c("Below", "Below", "Below", "Below", "Below", "Below", "Below", "Below", "Below", "Below", "Below", "Below", "Below", "Below", "Below", "Below", "Below", "Below", "Above", "Above", "Above", "Above", "Above", "Above", "Above", "Above", "Above", "Above", "Above", "Above", "Above", "Above", "Above", "Above", "Above", "Above", "Above")

neighborhood_stats_cleaned_squeaky_clean$above_or_below_60th <- above_or_below

#now I can use this data for flourish for my final chart


         
