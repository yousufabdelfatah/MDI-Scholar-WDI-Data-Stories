library(tidyverse)
library(WDI)

# Get COVID data ----------------------------------------------------------
api_root_url <- "http://cvapi.zognet.net/"

# get world covid-19 data
country_url = paste0(api_root_url, "manifest.json")
world_url = paste0(api_root_url, "world.json")


# get world covid-19 data
ctry_data <- jsonlite::fromJSON(country_url)
wld_data <- jsonlite::fromJSON(world_url)$data
wld_data$iso <- "WLD"
wld_data$name <- "World"
wld_data <- wld_data[, c("iso", "name", "date", "confirmed", "deaths", "recovered")]
wld_data$`COVID-19 cases: Active` <- wld_data$confirmed - wld_data$deaths - wld_data$recovered

# get country list
country_list = names(ctry_data)

# country level covid-19 data
full_country_data <- vector(mode = "list", length = length(country_list))

for (i in seq_along(country_list)) { # loop through countries
  daturl = paste(api_root_url, country_list[i], ".json", sep = "")
  country_info = RJSONIO::fromJSON(daturl, nullValue = NA)[[1]]
  iso = country_info[["iso"]]
  name = country_info[["name"]]
  country_raw = RJSONIO::fromJSON(daturl, nullValue = NA)[[2]]
  country_data = lapply(country_raw, function(j) cbind(j$date, j$confirmed, j$deaths, j$recovered))
  country_data = data.frame(do.call('rbind', country_data), stringsAsFactors = FALSE)
  colnames(country_data) = c( "date", "confirmed", "deaths", "recovered")
  country_data <- cbind(iso, name, country_data) # add country info to the data set
  country_data$iso <- as.character(country_data$iso)
  country_data$name <- as.character(country_data$name)
  full_country_data[[i]] <- country_data # attach the country info to the full list
}

full_country_data <- dplyr::bind_rows(full_country_data)

full_country_data <- full_country_data %>%
  select("iso", "name", "date", "confirmed", "deaths")

#get world bank data
death_comp <- WDI(indicator = c('Maternal mortality ratio (modeled estimate, per 100,000 live births)'  = 'SH.STA.MMRT', 
                               'mortality rate (under-5) (per 1,000 live births)' = 'SH.DYN.MORT', 
                               'Mortality rate attributed to household and ambient air pollution, age-standardized (per 100,000 population)' = 'SH.STA.AIRP.P5',
                               'Mortality rate attributed to unintentional poisoning (per 100,000 population)' = 'SH.STA.POIS.P5',
                               'Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene (per 100,000 population)' = 'SH.STA.WASH.P5',
                               'Cause of death, by injury (% of total)' = 'SH.DTH.INJR.ZS',
                               'Cause of death, by non-communicable diseases (% of total)' = 'SH.DTH.NCOM.ZS',
                               'Refugee population by country or territory of asylum' = 'SM.POP.REFG',
                               'Refugee population by country or territory of origin' = 'SM.POP.REFG.OR'), latest = 1) 

case_comp <- WDI(indicator = c('Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)' = 'SI.POV.DDAY',
                                'Prevalence of stunting, height for age (% of children under 5)' = 'SH.STA.STNT.ZS',
                                'Prevalence of underweight, weight for age (% of children under 5)' = 'SH.STA.MALN.ZS',
                                'Adolescents out of school (% of lower secondary school age)' = 'SE.SEC.UNER.LO.ZS',
                                'Children out of school (% of primary school age)' = 'SE.PRM.UNER.ZS',
                                'Literacy rate (or illiteracy)' = 'SE.ADT.LITR.ZS',
                                'PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)' = 'EN.ATM.PM25.MC.ZS',
                                'People using safely managed drinking water services (% of population) (compare to opposite)' = 'SH.H2O.SMDW.ZS',
                                'People with basic handwashing facilities including soap and water (% of population)' = 'SH.STA.HYGN.ZS',
                                'Low-birthweight babies (% of births)' = 'SH.STA.BRTW.ZS'), latest = 1)

# there are several countries with years before 2015 in death_comp so let's get rid of them
death_comp <- death_comp %>% 
  filter(year >= 2015)
  
#sum cases and deaths
summed_data <- full_country_data %>% 
  group_by(name) %>% 
  summarise(total_covid_deaths = sum(as.numeric(deaths)),
            total_covid_cases=sum(as.numeric(confirmed)))
  

# join the data frames
death_comp_data <- summed_data %>% 
  select(-total_covid_cases) %>% 
  left_join(death_comp, by = c("name" = "country"))


case_comp_data <- summed_data %>% 
  select(-total_covid_deaths) %>% 
  left_join(case_comp, by = c("name" = "country"))

glimpse(case_comp_data)

# this doens't make sense you can't sum percentages- we either need to make covid cases a percent or per 1000 etc
case_comp_data %>%
  select(-iso2c) %>% 
  replace(is.na(.), 0) %>% 
  summarise_at(c("total_covid_cases",
                 "Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)",
                 "Prevalence of stunting, height for age (% of children under 5)",
                 "Prevalence of underweight, weight for age (% of children under 5)",
                 "Adolescents out of school (% of lower secondary school age)",
                 "Children out of school (% of primary school age)",
                 "Literacy rate (or illiteracy)",
                 "PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)",
                 "People using safely managed drinking water services (% of population) (compare to opposite)",
                 "People with basic handwashing facilities including soap and water (% of population)",
                 "Low-birthweight babies (% of births)"), sum) %>% 
  pivot_longer(cols = c("total_covid_cases",
                        "Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)",
                        "Prevalence of stunting, height for age (% of children under 5)",
                        "Prevalence of underweight, weight for age (% of children under 5)",
                        "Adolescents out of school (% of lower secondary school age)",
                        "Children out of school (% of primary school age)",
                        "Literacy rate (or illiteracy)",
                        "PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)",
                        "People using safely managed drinking water services (% of population) (compare to opposite)",
                        "People with basic handwashing facilities including soap and water (% of population)",
                        "Low-birthweight babies (% of births)")) %>% 
  ggplot(aes(name, value)) +
  geom_col() +
  facet_wrap(~name,scales="free")
  






