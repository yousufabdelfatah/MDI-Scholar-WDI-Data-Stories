library(tidyverse)
library(WDI)
library(corrr)

# # Get COVID data ----------------------------------------------------------
# api_root_url <- "http://cvapi.zognet.net/"
# 
# # get world covid-19 data
# country_url = paste0(api_root_url, "manifest.json")
# world_url = paste0(api_root_url, "world.json")
# 
# 
# # get world covid-19 data
# ctry_data <- jsonlite::fromJSON(country_url)
# wld_data <- jsonlite::fromJSON(world_url)$data
# wld_data$iso <- "WLD"
# wld_data$name <- "World"
# wld_data <- wld_data[, c("iso", "name", "date", "confirmed", "deaths", "recovered")]
# wld_data$`COVID-19 cases: Active` <- wld_data$confirmed - wld_data$deaths - wld_data$recovered
# 
# # get country list
# country_list = names(ctry_data)
# 
# # country level covid-19 data
# full_country_data <- vector(mode = "list", length = length(country_list))
# 
# for (i in seq_along(country_list)) { # loop through countries
#   daturl = paste(api_root_url, country_list[i], ".json", sep = "")
#   country_info = RJSONIO::fromJSON(daturl, nullValue = NA)[[1]]
#   iso = country_info[["iso"]]
#   name = country_info[["name"]]
#   country_raw = RJSONIO::fromJSON(daturl, nullValue = NA)[[2]]
#   country_data = lapply(country_raw, function(j) cbind(j$date, j$confirmed, j$deaths, j$recovered))
#   country_data = data.frame(do.call('rbind', country_data), stringsAsFactors = FALSE)
#   colnames(country_data) = c( "date", "confirmed", "deaths", "recovered")
#   country_data <- cbind(iso, name, country_data) # add country info to the data set
#   country_data$iso <- as.character(country_data$iso)
#   country_data$name <- as.character(country_data$name)
#   full_country_data[[i]] <- country_data # attach the country info to the full list
# }
# 
# full_country_data <- dplyr::bind_rows(full_country_data)
# 
# full_country_data <- full_country_data %>%
#   select("iso", "name", "date", "confirmed", "deaths")
# 
# #Get world bank data -----------------------------------------------------
# death_comp <- WDI(indicator = c('Maternal mortality ratio (modeled estimate, per 100,000 live births)'  = 'SH.STA.MMRT', 
#                                'mortality rate (under-5) (per 1,000 live births)' = 'SH.DYN.MORT', 
#                                'Mortality rate attributed to household and ambient air pollution, age-standardized (per 100,000 population)' = 'SH.STA.AIRP.P5',
#                                'Mortality rate attributed to unintentional poisoning (per 100,000 population)' = 'SH.STA.POIS.P5',
#                                'Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene (per 100,000 population)' = 'SH.STA.WASH.P5',
#                                'Cause of death, by injury (% of total)' = 'SH.DTH.INJR.ZS',
#                                'Cause of death, by non-communicable diseases (% of total)' = 'SH.DTH.NCOM.ZS',
#                                'Refugee population by country or territory of asylum' = 'SM.POP.REFG',
#                                'Refugee population by country or territory of origin' = 'SM.POP.REFG.OR',
#                                'Population' = 'SP.POP.TOTL'), latest = 1) 
# 
# case_comp <- WDI(indicator = c('Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)' = 'SI.POV.DDAY',
#                                 'Prevalence of stunting, height for age (% of children under 5)' = 'SH.STA.STNT.ZS',
#                                 'Prevalence of underweight, weight for age (% of children under 5)' = 'SH.STA.MALN.ZS',
#                                 'Adolescents out of school (% of lower secondary school age)' = 'SE.SEC.UNER.LO.ZS',
#                                 'Children out of school (% of primary school age)' = 'SE.PRM.UNER.ZS',
#                                 'Literacy rate (or illiteracy)' = 'SE.ADT.LITR.ZS',
#                                 'PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)' = 'EN.ATM.PM25.MC.ZS',
#                                 'People using safely managed drinking water services (% of population) (compare to opposite)' = 'SH.H2O.SMDW.ZS',
#                                 'People with basic handwashing facilities including soap and water (% of population)' = 'SH.STA.HYGN.ZS',
#                                 'Low-birthweight babies (% of births)' = 'SH.STA.BRTW.ZS',
#                                 'Population' = 'SP.POP.TOTL'), latest = 1)
# 
# # Manipulate Data ----------------------------------------------------------
# # there are several countries with years before 2015 so let's get rid of them
# death_comp <- death_comp %>% 
#   filter(year >= 2015)
# 
# case_comp <- case_comp %>% 
#   filter(year >= 2015)
#   
# #sum cases and deaths
# summed_data <- full_country_data %>% 
#   group_by(name) %>% 
#   summarise(total_covid_deaths = sum(as.numeric(deaths)),
#             total_covid_cases=sum(as.numeric(confirmed)))
#   
# 
# # join the data frames
# death_comp_data <- summed_data %>% 
#   select(-total_covid_cases) %>% 
#   left_join(death_comp, by = c("name" = "country"))
# 
# 
# case_comp_data <- summed_data %>% 
#   select(-total_covid_deaths) %>% 
#   left_join(case_comp, by = c("name" = "country"))
# 
# 
# # calculate percentages 
# case_comp_data$covid_case_pct_pop <- case_comp_data$total_covid_cases /  case_comp_data$Population
# 
# case_comp_data$covid_case_per_1000 <- case_comp_data$total_covid_cases /  1000
# 
# case_comp_data$covid_case_per_100000 <- case_comp_data$total_covid_cases /  100000
# 
# death_comp_data$covid_death_pct_pop <- death_comp_data$total_covid_deaths /  death_comp_data$Population
# 
# death_comp_data$covid_death_per_1000 <- death_comp_data$total_covid_deaths /  1000
# 
# death_comp_data$covid_death_per_100000 <- death_comp_data$total_covid_deaths /  100000
case_comp_data <- read.csv("Data/case_comp_data.csv")

death_comp_data <- read.csv("Data/deatb_comp_data.csv")
# Summary Stats and Correlation Matrix ---------------------------------------
no_year <- case_comp_data %>% 
  select(-c(year, iso2c)) %>% 
  group_by(name) %>% 
  replace(is.na(.), 0) %>% 
  summarise_all(mean) %>% 
  na_if(0)

min_max <- no_year %>% 
  select(-name) %>% 
  summarise_all(list(min, max))

summary_stats <- no_year %>% 
  select(-name) %>% 
  drop_na() %>% 
  summarise_all(list(mean = mean,max = max,min = min))

matrix <- (no_year) %>% 
  select(-name) %>% 
  drop_na() %>% 
  correlate()

# plot correlations
rplot(matrix)

matrix2 <- matrix

matrix2[, -1] <- matrix[, -1] + rnorm(cumprod(dim(matrix[, -1]))[-1], sd = 0.001)

matrix2 %>% 
  network_plot(min_cor = .2)

# Visualizations ------------------------------------------------------------


case_comp_data %>%
  select(-iso2c) %>% 
  replace(is.na(.), 0) %>% 
  summarise_at(c("covid_case_pct_pop",
                 "PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)"
                  ), sum) %>% 
  pivot_longer(cols = c("covid_case_pct_pop",
                        "PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)"
                        )) %>% 
  ggplot(aes(name, value)) +
  geom_col() +
  facet_wrap(~name,scales="free")



# lay things out spatially
world <- 
  map_data("world") %>% 
  select(long,lat,group,country=region) %>% 
  # Again standardize the country names
  mutate(country = 
           countrycode::countrycode(country,"country.name","country.name")) %>% 
  mutate(country = ifelse(country == "South Sudan","Sudan",country))

# subset the relevant African countries in the data.
world_comp <- 
  case_comp_data %>% 
  group_by(name) %>% 
  mutate(country = 
           countrycode::countrycode(name,"country.name","country.name")) %>% 
  inner_join(world,by="country")

# map 
library("ggthemes")

world_comp %>% 
  ggplot(aes(x=long,y=lat,group=group,fill=total_covid_cases)) +
  geom_polygon(color="white",size=.25) +
  scale_fill_gradient2_tableau() +
  theme_map() +
  labs(fill="total covid cases",
       title = "Average Life Expectancy in Africa"
       ) +
  theme(text=element_text(family = "serif",face="bold",size=14))

world_comp %>% 
  ggplot(aes(x=long,y=lat,group=group,fill=covid_case_per_100000)) +
  geom_polygon(color="white",size=.25) +
  scale_fill_gradient2_tableau() +
  theme_map() +
  labs(fill="Covid Cases Per 100000",
       title = "Cases per 100000"
  ) +
  theme(text=element_text(family = "serif",face="bold",size=14))

# write.csv(death_comp_data, "Data/death_comp_data.csv", row.names = FALSE)

# write.csv(case_comp_data, "Data/case_comp_data.csv", row.names = FALSE)
