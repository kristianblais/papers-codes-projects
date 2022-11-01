###-------------------------------------------------
#- Load Packages from Library and Register API Keys
###-------------------------------------------------

library(tidycensus)
library(tidyverse)
library(sp)
library(sf)
library(rgdal)
library(ggmap)
library(ggthemes)
library(formattable)
library(stats)
library(lmtest)
library(stargazer)
library(plm)
library(data.table)
library(fixest)
library(ggiplot)
library(gridExtra)
census_key <- read.csv("api_keys")[1]
map_key <- read.csv("api_keys")[2]
census_api_key(census_key)
register_google(map_key)

###-------------------------------------------------
#- Define Custom Functions
###-------------------------------------------------

# Crosswalk converts 2000 PUMAs to their corresponding 2010 PUMA
crosswalk = function(data,cross,date){
  if(date < 2012) {
    cross_temp = cross %>% 
      inner_join(data,by=c("GEOID00" = "GEOID")) %>% 
      filter(!is.na(estimate)) %>% 
      select(GEOID10,State10_Name,PUMA10_Name,pPUMA00_Pop10,variable,estimate,moe,year) %>%
      group_by(GEOID10,variable,year) %>% 
      summarise(state = first(State10_Name),
                PUMA_name = first(PUMA10_Name),
                estimate = sum(estimate*pPUMA00_Pop10/100),
                moe = sum(moe*pPUMA00_Pop10/100)) %>% 
      rename(GEOID = GEOID10)
    return(cross_temp)
  } 
  else if(date >= 2012) {
    cross_temp = cross %>% 
      inner_join(data,by=c("GEOID10" = "GEOID")) %>% 
      filter(!is.na(estimate)) %>% 
      group_by(GEOID10,variable,year) %>% 
      summarise(state = first(State10_Name),
                PUMA_name = first(PUMA10_Name),
                estimate = sum(estimate),
                moe = sum(moe)) %>% 
      rename(GEOID = GEOID10)
    return(cross_temp)
  }
  else{
    return("ERROR: I don't know how you did it, but something is incredibly wrong")
  }
}

# This function is to match asynchromous age defintions across datasets
check_age = function(data,ages){
  age_bin = rep("",length(data))
  for(j in 1:length(data)){
    counter = 0
    for(i in 1:length(ages)){
      if(grepl(ages[i],data[j])){
        age_bin[j] = ages[i]
        counter = counter + 1
      }
    }
  }
  if(counter>1){
    return(errorCondition("Multiple Matches, fix input"))
  }
  else if(counter == 1){
    return(age_bin)
  }
  else{
    return(NA)
  }
}

# Custom function to pull data from the Census
acs1_puma_pull = function(codes, year_start, year_end, state, crosswalk_file, na_check = FALSE){
  acs_data = c()
  nacount = rep(0,year_end-year_start)
  for(i in year_start:year_end){
  j = i-year_start+1
  temp_acs <- get_acs(
      geography = "public use microdata area",
      survey = "acs1",
      variables = codes,
      st = state,
      year = i
    ) %>% 
    mutate(year = i)
  nacount[j] = sum(is.na(temp_acs$estimate))
  temp_acs_crossed = crosswalk(temp_acs,crosswalk_file,i) 
  
  if(j==1){
    acs1_data = temp_acs_crossed
  }
  else if(j>1){
    acs1_data = bind_rows(acs1_data,temp_acs_crossed)
  }
  }
  if(na_check==TRUE){
    print(nacount)
  }
  return(acs1_data)
}

# Function to convert to Robust Standard Errors
se_robust <- function(x){
  coeftest(x, vcov = vcovHC, type = "HC1")[, "Std. Error"]
}

## (EDIT) Removed some custom functions for brevity ##

###-------------------------------------------------
#- Define static lists and variables
###-------------------------------------------------

## (EDIT) Removed for brevity ##

###-------------------------------------------------
#- Import and Transform Spatial-Coded Data
###-------------------------------------------------

# Load crosswalk file to convert old PUMA definitions to 2020 versions
puma_crosswalk <- read_csv("PUMA2000_PUMA2010_crosswalk.csv")

# Load map of all PUMAS as a shapefile
puma_map <- st_read("GIS/ipums_puma_2010.shp")

# Load population data of all CSAs and clean it up
csa_population <- read_csv("GIS/csa_population.csv") %>% 
  rename("CSA_POP" = Census)
csa_population$Geography <- sub(".","",csa_population$Geography)

# Load CSA infromation from shapefile with dropped geometry
csa_info <- st_read("GIS/cb_2021_us_csa_5m.shp") %>% st_drop_geometry() %>% 
  select(-c(AFFGEOID,GEOID,ALAND,AWATER))

# Load boundaries of all PUMAS within CSAs
csa_puma_map <- st_read("GIS/csa_pumas.shp") %>% 
  select(!c(NAMELSAD,LSAD)) %>% 
  inner_join(csa_info,by="CSAFP") %>% # Join the CSA infromation
  inner_join(csa_population,by=c("NAMELSAD"="Geography")) %>% # Join the CSA population data
  select(-c(NAME)) %>% 
  st_transform(st_crs(puma_map)) # Re-Project to match PUMA map

# Load CSAs to and create a 40km buffer to ensure PUMAs will be contained
csa <- st_read("GIS/cb_2018_us_csa_500k.shp") %>% 
  st_transform(st_crs(puma_map))  # Re-Project to match PUMA map
csa_buffer <- csa %>%   
  st_buffer(40000)

# Load spatial locations of Link Light Rail Lines and Stations
lightrail_line <- st_read("GIS/LINKLine.shp") %>% 
  st_transform(st_crs(puma_map))  
lightrail_stations <- st_read("GIS/LINKStations.shp") %>% 
  st_transform(st_crs(puma_map)) %>% 
  mutate(phase_1 = STATION %in% station_list_phase1,
         phase_2 = STATION %in% station_list_phase2)

# Load spatial MSA data
msa <- st_read("GIS/msa_puma_with_station.shp") %>% 
  st_transform(st_crs(puma_map)) %>% # Re-Project to match PUMA map
  select(-c(Join_Count, TARGET_FID))

# Obtain Seattle CSA boundary specifically
seatac_csa = csa_puma_map %>% filter(grepl("Seattle",NAMELSAD))

# Create a 2.5km buffer around light rail stations
station_buffers = lightrail_stations %>% 
  filter(phase_1 == TRUE | phase_2 == TRUE) %>% 
  st_buffer(2500)

# Append stations to PUMA map, will be used for map making
puma_map_seatac = seatac_csa %>%
  st_join(station_buffers) %>% 
  group_by(GEOID,STATEFIP,State,PUMA,Name) %>% 
  summarise(within_csa = (sum(as.integer(CSAFP), na.rm = TRUE)>0),
            has_station_p1 = (sum(phase_1,na.rm=TRUE)>0),
            has_station_p2 = (sum(phase_2,na.rm=TRUE)>0))

###-------------------------------------------------
#- Begin Loading and Cleaning Data from 1-year ACS
###-------------------------------------------------

# Load labor force indicators by race, and age, and sex
raw_emp_sex_age = acs1_puma_pull(lf_vars_byagesex,2005,2019,c("WA"),puma_crosswalk)
emp_sex_age = raw_emp_sex_age %>% 
  mutate(inorout = if_else(grepl("ni",variable),"nilf","lf"), # Number of people in and out of LF in a given PUMA
         sex = ifelse(grepl("m",variable),"m","f"), # Number of people of each sex in a PUMA
         age = check_age(variable,age_bin_list)) %>% # Number of people within each age bin in a PUMA
  pivot_wider(names_from = inorout,values_from = c(estimate,moe)) %>% 
  group_by(GEOID,year,age,sex) %>% 
  summarise(                                       # Summarize function removes empty values from our pivot 
    estimate_lf = sum(estimate_lf,na.rm = TRUE),
    moe_lf = sum(moe_lf,na.rm = TRUE),
    estimate_nilf = sum(estimate_nilf,na.rm = TRUE),
    moe_nilf = sum(moe_nilf,na.rm = TRUE)
  ) %>% 
  mutate( 
    age_lowerbound = as.numeric(substr(age,1,2)), # Turns age range to a single number (lowest age of said range)
    estimate_pop = estimate_lf + estimate_nilf,
    moe_pop = moe_lf + moe_nilf,
    lfpr = estimate_lf/(estimate_lf+estimate_nilf)
  )

# Load male unemployment data by age
raw_unemp_men = acs1_puma_pull(unemp_vars_men,2005,2019,c("WA"),puma_crosswalk)
unemp_men = raw_unemp_men %>% 
  mutate(inorout = if_else(grepl("unemp",variable),"unemp","lf"),
         age = check_age(variable,age_bin_list)) %>% 
  pivot_wider(names_from = inorout,values_from = c(estimate,moe)) %>% 
  group_by(GEOID,year,age) %>% 
  summarise(
    estimate_lf = sum(estimate_lf,na.rm = TRUE),
    moe_lf = sum(moe_lf,na.rm = TRUE),
    estimate_unemp = sum(estimate_unemp,na.rm = TRUE),
    moe_unemp = sum(moe_unemp,na.rm = TRUE)
  ) %>% 
  mutate(
    age_lowerbound = as.numeric(substr(age,1,2)),
    unemp_rate = estimate_unemp/estimate_lf
  )

# Load in average hours worked a week by sex
raw_hours_sex = acs1_puma_pull(hours_vars_bysex,2005,2019,c("WA"),puma_crosswalk)
hours_sex = raw_hours_sex %>%
  ungroup() %>% 
  mutate(sex = ifelse(grepl("m",variable),"m","f")) %>% 
  select(GEOID, year, sex, estimate, moe) %>% 
  rename(estimate_hoursworked = estimate,
         moe_hoursworked = moe)

# Load in amount of people married in a PUMA
raw_marriage_sex = acs1_puma_pull(marital_vars_bysex,2005,2019,c("WA"),puma_crosswalk)
marriage_sex = raw_marriage_sex %>%
  filter(grepl("present",variable)|grepl("never",variable)|grepl("total",variable)) %>% 
  mutate(marriage_status = if_else(grepl("total",variable),"total",if_else(grepl("never",variable),"never_married","married")),
         sex = ifelse(grepl("f_",variable),"f","m")) %>% 
  pivot_wider(names_from = marriage_status,values_from = c(estimate,moe)) %>%
  ungroup() %>% 
  group_by(GEOID,year,sex) %>%
  summarise(
    estimate_married = sum(estimate_married,na.rm = TRUE),
    moe_married = sum(moe_married,na.rm = TRUE),
    estimate_nevermarried = sum(estimate_never_married,na.rm = TRUE),
    moe_nevermarried = sum(moe_never_married,na.rm = TRUE),
    estimate_total = sum(estimate_total,na.rm = TRUE),
    moe_total = sum(moe_total,na.rm = TRUE)
  ) %>% 
  mutate(perc_married = estimate_married/estimate_total,
         perc_nevermarried = estimate_nevermarried/estimate_total)

# Load highest level of education obtained by sex
raw_education_sex = acs1_puma_pull(edu_vars_bysex, 2005, 2019, c("WA"), puma_crosswalk)
education_sex = raw_education_sex %>% 
  mutate(sex = ifelse(grepl("f_",variable),"f","m"),
         total = ifelse(grepl("total_pop",variable),"total_pop","college_educated")) %>% 
  group_by(GEOID,year, sex, total) %>% 
  summarise(
    estimate = sum(estimate,na.rm = TRUE),
    moe = sum(moe,na.rm = TRUE)
  ) %>% 
  pivot_wider(names_from = total, values_from = c(estimate,moe)) %>% 
  mutate(perc_college = estimate_college_educated/estimate_total_pop)

# Load amount of households with children in a given PUMA
raw_childcount = acs1_puma_pull(children_vars, 2005, 2019, c("WA"), puma_crosswalk)
childcount = raw_childcount %>%
  select(-c(state,PUMA_name)) %>% 
  pivot_wider(names_from = variable, values_from = c(estimate,moe)) %>% 
  mutate(perc_with_children = (estimate_children_under18)/estimate_total_families)

### LOAD IN SAME LABOR OUTCOMES WITH RACE UN-AGGREGATED ###

## (EDIT) Removed repeating structures for brevity ##

#Refine it to be just female employment in the Seattle metropolitan area
emp_seattle = emp_sex_age %>%
  group_by(GEOID,year,sex) %>% 
  summarise(
    lf = sum(estimate_lf),
    nilf = sum(estimate_lf),
    pop = sum(estimate_pop),
    moe_lf = sum(moe_lf),
    moe_nilf = sum(moe_nilf),
    moe_pop = sum(moe_pop)
  ) %>% 
  mutate(lfpr = lf/pop) %>% 
  right_join(puma_map_seatac,by=c("GEOID")) %>% 
  select(!c(State,STATEFIP,PUMA)) %>% 
  st_sf(sf_column_name = "geometry")

emp_seattle_seniors = emp_sex_age %>%
  filter(age_lowerbound > 65) %>% 
  group_by(GEOID,year,sex) %>% 
  summarise(
    lf = sum(estimate_lf),
    nilf = sum(estimate_lf),
    pop = sum(estimate_pop),
    moe_lf = sum(moe_lf),
    moe_nilf = sum(moe_nilf),
    moe_pop = sum(moe_pop)
  ) %>% 
  mutate(lfpr = lf/pop) %>% 
  right_join(puma_map_seatac,by=c("GEOID")) %>% 
  select(!c(State,STATEFIP,PUMA)) %>% 
  st_sf(sf_column_name = "geometry")

unemp_seattle_men = unemp_men %>% 
  group_by(GEOID,year) %>% 
  summarise(
    lf_men = sum(estimate_lf),
    unemp_men = sum(estimate_unemp)
  ) %>% 
  mutate(unemp_rate_men = unemp_men/lf_men) %>% 
  filter(GEOID %in% puma_map_seatac$GEOID)

### Combine data to be ready for regression analysis ###

# Combine marriage, education, male unemployment, and childcount to the female LFPR and Hours Worked
data_reg1 = emp_seattle %>% 
  st_drop_geometry() %>%
  filter(sex == "f") %>% 
  mutate(after_open = ifelse(year>2009,TRUE,FALSE),
         open_X_station = ifelse(year>2009 & has_station_p1 == TRUE,TRUE,FALSE)) %>%
  left_join(filter(marriage_sex,sex=="f"), by = c("GEOID", "year")) %>% 
  left_join(filter(education_sex,sex=="f"), by = c("GEOID", "year")) %>% 
  left_join(childcount, by = c("GEOID","year")) %>% 
  left_join(filter(hours_sex, sex =="f"), by = c("GEOID", "year")) %>% 
  mutate(hours_per_capita =estimate_hoursworked/estimate_total) %>% 
  left_join(unemp_seattle_men, by = c("GEOID","year")) %>% 
  select(-c(Name,sex.x,sex.y)) %>% 
  filter(!is.na(lfpr))

# Same as above but specifically for Seniors

## (EDIT) Removed for brevity ##

# Again same but for non-white population

## (EDIT) Removed for brevity

###-------------------------------------------------
#- Prepare Data to Plot Diff-in-Diff Time Trends
###-------------------------------------------------

dind_plot_data = data_reg1 %>% 
  ungroup() %>%
  group_by(year, has_station_p1) %>% 
  summarise(lfpr = mean(lfpr),
          hours = mean(hours_per_capita))

# Plot trend of LFPR for PUMAS inside vs outside station buffer
plot_lfpr_seattle = ggplot(data = data_reg1, aes(year,lfpr,color = has_station_p1)) + 
  stat_summary(fun = "mean", geom="line") + 
  labs(x = "Year", y = "Labor Force\nParticipation Rate\n", color = "Served by Rail Station\n") +
  scale_color_manual(labels = c("Outside Service Area","In Service Area"), values = c("red","blue")) +
  geom_vline(aes(xintercept = 2009),color = "black", 
             size = 1) +
  annotate(geom = "text",
           label = "Light Rail Opens",
           x = 2010,
           y = 0.69,
           angle = 0,
           hjust = -0.1) +
  theme_minimal()
  
# Plot trend of hours worked for PUMAS inside vs outside station buffer
plot_hours_seattle = ggplot(data = data_reg1, aes(year,hours_per_capita,color = has_station_p1)) + 
  stat_summary(fun = "mean", geom="line") + 
  labs(x = "Year", y = "Female Median\nHours Worked A Week\n", color = "Served by Rail Station\n") +
  scale_color_manual(labels = c("Outside Service Area","In Service Area"), values = c("red","blue")) +
  geom_vline(aes(xintercept = 2009),color = "black", 
             size = 1) +
  annotate(geom = "text",
           label = "Light Rail Opens",
           x = 2010,
           y = 25.5,
           angle = 0,
           hjust = -0.1) +
  theme_minimal()
# Combine both plots into 1 image
grid.arrange(plot_lfpr_seattle,plot_hours_seattle)

###-------------------------------------------------
#- Perform Dynamic Diff-in-Diff Regressions
###-------------------------------------------------

data_dynamic_reg = data_reg1 %>%
  mutate(after_open = ifelse(year>=2010,TRUE,FALSE),
         treatment_year = ifelse(has_station_p1 == TRUE,2010,0)) %>% 
  mutate(log_hours = log(hours_per_capita)) %>%
  filter(!is.na(lfpr)) %>% 
  as.data.table() 

# Add reference year
data_dynamic_reg[, time_to_treat := ifelse(has_station_p1==TRUE, year - `treatment_year`, 0)]

# First perform regression for all women
# Regressing on Labor Force Participation Rate
dynamic_dind_lfpr = feols(lfpr ~ i(time_to_treat, has_station_p1, ref = -2) + ## Our key interaction: time × treatment status
		  perc_college + perc_married + perc_with_children + unemp_rate_men|                    ## Other controls
		  GEOID + year,                             ## FEs
		 cluster = ~GEOID,                          ## Clustered SEs
		 data = data_dynamic_reg)

# Regressing on Avg Weekly Hours Worked
dynamic_dind_hours = feols(log_hours ~ i(time_to_treat, has_station_p1, ref = -2) + ## Our key interaction: time × treatment status
		  perc_college + perc_married + perc_with_children + unemp_rate_men|                    ## Other controls
		  GEOID + year,                             ## FEs
		 cluster = ~GEOID,                          ## Clustered SEs
		 data = data_dynamic_reg)

# Same as above but for seniors
data_dynamic_seniors = data_reg_seniors %>%
  mutate(after_open = ifelse(year>=2010,TRUE,FALSE),
         treatment_year = ifelse(has_station_p1 == TRUE,2010,0)) %>% 
  filter(!is.na(lfpr)) %>%
  mutate(log_hours = log(hours_per_capita)) %>% 
  as.data.table() 

data_dynamic_seniors[, time_to_treat := ifelse(has_station_p1==TRUE, year - `treatment_year`, 0)]

dynamic_dind_lfpr_seniors = feols(lfpr ~ i(time_to_treat, has_station_p1, ref = -2) + ## Our key interaction: time × treatment status
		  perc_college + perc_married + perc_with_children + unemp_rate_men|                    ## Other controls
		  GEOID + year,                             ## FEs
		 cluster = ~GEOID,                          ## Clustered SEs
		 data = data_dynamic_seniors)

# Same for above except for non-white population

## (EDIT) Removed for brevity

### Plot results and combine in single plot grid

x1  = ggiplot(dynamic_dind_lfpr,geom_style = 'errorbar') + 
  labs(title = 'Female Labor Force \nParticipation Rate - All')  + 
  xlab("Time to Treatment") +
  ylab("Estimate and 95% C.I.") + 
  theme_minimal()

x2 = ggiplot(dynamic_dind_hours, geom_style = 'errorbar') + 
  labs(title = 'Log of Female Average \nWeekly Hours Worked - All')  + 
  xlab("Time to Treatment") +
  ylab("Estimate and 95% C.I.") + 
  theme_minimal()

## (EDIT) Removed some repeating stuctures for brevity ## 

figurex = grid.arrange(x1,x2,x3,x5)

# Display dynamic regression results in table
summary(dynamic_dind_hours)
stargazer(dynamic_dind)
mdls1 = list(simple_reg1,simple_plm1,controled_reg1,controled_plm1)
stargazer(
  mdls1, single.row = FALSE, 
  se = lapply(mdls, se_robust))

etable(list(dynamic_dind_lfpr, dynamic_dind_hours,dynamic_dind_lfpr_seniors,dynamic_dind_lfpr_minorities), tex=TRUE)

###-------------------------------------------------
#- Repeat Process for Inter-MSA Analysis
#-
#- Begin Loading 1-Y ACS Data and Spatial MSA Data
###-------------------------------------------------

# Read popluation data for MSAs
msa_pop = read_csv("msa_population.csv",col_names = TRUE) %>% 
  rename("MSA_POPULATION" = Census)
msa_pop$Geography <- sub(".","",msa_pop$Geography)

# Filter MSAs so that we only have ones with rapid-transit systems
our_msas = msa %>% 
  inner_join(msa_pop, by = c("NAMELSAD"="Geography")) %>% 
  filter(MSA_POPULATION > 0,
         Gen >= 2) %>% 
  select(-c(CSAFP_1,GEOID_1,Shape_Leng,Shape_Area,SOURCE,SOURCEDATE,CSAFP_1,SYS_AGENCY,STATIONURL,STATIONSYS))

# Load Female LFPR by sex and age for all MSA states that have transit, by PUMA
lfs_acs_raw = acs1_puma_pull(lf_vars_byagesex,2005,2019,c(unique(our_msas$STATEFIP)),puma_crosswalk)
lfs_acs = lfs_acs_raw %>% 
  mutate(inorout = if_else(grepl("ni",variable),"nilf","lf"),
         sex = ifelse(grepl("m",variable),"m","f"),
         age = check_age(variable,age_bin_list)) %>% 
  pivot_wider(names_from = inorout,values_from = c(estimate,moe)) %>% 
  group_by(GEOID,year,age,sex) %>% 
  summarise(
    estimate_lf = sum(estimate_lf,na.rm = TRUE),
    moe_lf = sum(moe_lf,na.rm = TRUE),
    estimate_nilf = sum(estimate_nilf,na.rm = TRUE),
    moe_nilf = sum(moe_nilf,na.rm = TRUE)
  ) %>% 
  mutate(
    estimate_pop = estimate_lf + estimate_nilf,
    moe_pop = moe_lf + moe_nilf,
    lfpr = estimate_lf/(estimate_lf+estimate_nilf)
  )

# Load male UE for all MSA states, by PUMA
raw_unemp_men_msa = acs1_puma_pull(unemp_vars_men,2005,2019,c(unique(our_msas$STATEFIP)),puma_crosswalk)
unemp_men_msa = raw_unemp_men_msa %>% 
  mutate(inorout = if_else(grepl("unemp",variable),"unemp","lf"),
         age = check_age(variable,age_bin_list)) %>% 
  pivot_wider(names_from = inorout,values_from = c(estimate,moe)) %>% 
  group_by(GEOID,year) %>% 
  summarise(
    estimate_lf = sum(estimate_lf,na.rm = TRUE),
    moe_lf = sum(moe_lf,na.rm = TRUE),
    estimate_unemp = sum(estimate_unemp,na.rm = TRUE),
    moe_unemp = sum(moe_unemp,na.rm = TRUE)
  ) %>% 
  mutate(unemp_rate = estimate_unemp/estimate_lf)

# Load Female Weekly Hours for all MSA states
hours_acs_raw = acs1_puma_pull(hours_vars_bysex,2005,2019,c(unique(our_msas$STATEFIP)),puma_crosswalk)
hours_acs = hours_acs_raw %>%
  ungroup() %>% 
  mutate(sex = ifelse(grepl("m",variable),"m","f")) %>% 
  select(GEOID, year, sex, estimate, moe) %>% 
  rename(estimate_hoursworked = estimate,
         moe_hoursworked = moe)

## (EDIT) Removed some repeating processes for brevity ##

###-------------------------------------------------
#- Clean and Filter Data for MSA Analysis
###-------------------------------------------------

# Consolidate ACS LF data and bind it to our MSAs
lfs_msa = lfs_acs %>%
  group_by(GEOID,year,sex) %>% 
  summarise(
    lf = sum(estimate_lf),
    nilf = sum(estimate_nilf),
    pop = sum(estimate_pop),
    moe_lf = sum(moe_lf),
    moe_nilf = sum(moe_nilf),
    moe_pop = sum(moe_pop)
  ) %>% 
  mutate(lfpr = lf/pop) %>% 
  right_join(our_msas,by=c("GEOID")) %>% 
  select(!c(PUMA)) %>% 
  st_sf(sf_column_name = "geometry")

# Add indicator variables, control variables, and clean data
data_msa = lfs_msa %>% 
  st_drop_geometry() %>%
  filter(sex == "f") %>% 
  mutate(is_seattle = grepl("Seattle",NAME_1), #Add Seattle indicator variable
         after_open = ifelse(year>2009,TRUE,FALSE), #indicator var that =1 if Link L.R. already built 
         open_X_seattle = ifelse(year>2009 & is_seattle == TRUE,TRUE,FALSE)) %>%
  left_join(filter(marriage_acs,sex=="f"), by = c("GEOID", "year")) %>% 
  left_join(filter(education_acs,sex=="f"), by = c("GEOID", "year")) %>% 
  left_join(children_acs, by = c("GEOID","year")) %>% 
  left_join(filter(hours_acs, sex =="f"), by = c("GEOID", "year")) %>% 
  mutate(log_hours = log(estimate_hoursworked/estimate_total)) %>% 
  left_join(select(rename(filter(st_drop_geometry(lfs_msa), sex == "m"),male_lfpr = lfpr),c("GEOID","year","male_lfpr"))) %>%
  select(-c(Name,sex.x,sex.y)) %>% 
  mutate(include =ifelse(Opening<2005 | is_seattle==TRUE, TRUE,FALSE)) %>% 
  filter(!is.na(lfpr),
         include) %>% 
  left_join(unemp_men_msa, by = c("GEOID","year"))

###---------------------------------------------------------
#- Graph LF Trends overtime between Seattle and other MSAs
###---------------------------------------------------------

msa_graph_data = data_msa %>% 
  ungroup() %>% 
  filter(MSA_POPULATION > 3200000) %>% 
  group_by(year, is_seattle) %>% 
  summarise(lfpr = sum(lf)/(sum(lf)+sum(nilf)))

ggplot(data = msa_graph_data) +
  geom_line(aes(year,lfpr,color = is_seattle)) +
  theme_minimal()

ggplot(data = msa_graph_data, aes(year,lfpr,color = is_seattle)) + 
  stat_summary(fun = "mean", geom="line") + 
  labs(x = "Year", y = "", color = "City\n") +
  scale_color_manual(labels = c("Other Cities","Seattle"), values = c("red","blue")) +
  geom_vline(aes(xintercept = 2010),color = "black", 
             size = 1) +
  annotate(geom = "text",
           label = "Light Rail Opens",
           x = 2010,
           y = 0.69,
           angle = 0,
           hjust = -0.1) +
  theme_minimal()

###-------------------------------------------------
#- Perform Dynamic D-in-D for MSAs for 
###-------------------------------------------------

  data_dynamic_msa = data_msa %>%
  mutate(after_open = ifelse(year>=2010,TRUE,FALSE),
         treatment_year = ifelse(is_seattle == TRUE,2010,0)) %>%
  filter(!is.na(lfpr)) %>%
  mutate(gen2 = ifelse(Gen == 2|is_seattle == TRUE,TRUE,FALSE),
         gen3 = (Gen == 3)) %>% 
  filter(gen3==TRUE|gen2==TRUE) %>% 
  filter(MSA_POPULATION > 2000000) %>% 
  as.data.table()

data_dynamic_msa[, time_to_treat := ifelse(is_seattle==TRUE, year - `treatment_year`, 0)]

data_dynamic_msa_2 = data_dynamic_msa %>% 
  filter(gen2 == TRUE)

data_dynamic_msa_3 = data_dynamic_msa %>% 
  filter(gen3 == TRUE)

# Comparing Female LFPR between all relevant MSAs
dynamic_dind_msa_lfpr = feols(lfpr ~ i(time_to_treat, is_seattle, ref = -2) + ## Our key interaction: time × treatment status
		  perc_college + perc_married + perc_with_children + unemp_rate |                    ## Other controls
		  CBSAFP + year,                             ## FEs
		 cluster = ~CSAFP,                          ## Clustered SEs
		 data = data_dynamic_msa)

# Compating Female Avg Hr Worked between all relevant MSAs
dynamic_dind_msa_hours = feols(log_hours ~ i(time_to_treat, is_seattle, ref = -2) + ## Our key interaction: time × treatment status
		  perc_college + perc_married + perc_with_children + unemp_rate|                    ## Other controls
		  CBSAFP + year,                             ## FEs
		 cluster = ~CSAFP,                          ## Clustered SEs
		 data = data_dynamic_msa)

# Comparing LFPR between Seattle and only MSAs with 2nd Gen Rapid Transit
dynamic_dind_msa_lfpr_2 = feols(lfpr ~ i(time_to_treat, is_seattle, ref = -2) + ## Our key interaction: time × treatment status
		  perc_college + perc_married + perc_with_children + unemp_rate |                    ## Other controls
		  CBSAFP + year,                             ## FEs
		 cluster = ~CSAFP,                          ## Clustered SEs
		 data = data_dynamic_msa_2)

# Comparing Avg Hr Worked for 2nd Gen MSA's
dynamic_dind_msa_hours_2 = feols(log_hours ~ i(time_to_treat, is_seattle, ref = -2) + ## Our key interaction: time × treatment status
		  perc_college + perc_married + perc_with_children + unemp_rate|                    ## Other controls
		  CBSAFP + year,                             ## FEs
		 cluster = ~CSAFP,                          ## Clustered SEs
		 data = data_dynamic_msa_2)

# Comparing LFPR to only 3rd gen rapid transit MSAs 
dynamic_dind_msa_lfpr_3 = feols(lfpr ~ i(time_to_treat, is_seattle, ref = -2) + ## Our key interaction: time × treatment status
		  perc_college + perc_married + perc_with_children + unemp_rate |                    ## Other controls
		  CBSAFP + year,                             ## FEs
		 cluster = ~CSAFP,                          ## Clustered SEs
		 data = data_dynamic_msa_3)

# Avg Hrs Worked between Gen 3 MSAs
dynamic_dind_msa_hours_3 = feols(log_hours ~ i(time_to_treat, is_seattle, ref = -2) + ## Our key interaction: time × treatment status
		  perc_college + perc_married + perc_with_children + unemp_rate|                    ## Other controls
		  CBSAFP + year,                             ## FEs
		 cluster = ~CSAFP,                          ## Clustered SEs
		 data = data_dynamic_msa_3)

### Plot all results seperately

xall1  = ggiplot(dynamic_dind_msa_lfpr,geom_style = 'errorbar') + 
  labs(title = 'Female Labor Force \nParticipation Rate - Gen 2+3')  + 
  xlab("Time to Treatment") +
  ylab("Estimate and 95% C.I.") + 
  theme_minimal()

xall2 = ggiplot(dynamic_dind_msa_hours, geom_style = 'errorbar') + 
  labs(title = 'Log of Female Average \nWeekly Hours Worked - Gen 2+3')  + 
  xlab("Time to Treatment") +
  ylab("Estimate and 95% C.I.") + 
  theme_minimal()

## (EDIT) Removed repeating struvtures for brevity ##

# Combine all seperate results on two grids
figurexall = grid.arrange(xall1,xall2) #Plots with all MSAs
figurexgen = grid.arrange(x21,x31,x22,x32) #Plots dividing MSAs by rapid transit generation

# Generate regresion table for all lf/gen combos
etable(list(dynamic_dind_msa_lfpr,
            dynamic_dind_msa_lfpr_2,
            dynamic_dind_msa_lfpr_3,
            dynamic_dind_msa_hours,
            dynamic_dind_msa_hours_2,
            dynamic_dind_msa_hours_2), tex=TRUE)