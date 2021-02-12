library(WDI)
library(wbstats)
library(tidyverse)

setwd('/Users/yousufabdelfatah/Desktop/WDI')

# Homicides ----------------------------------------------------------------
intentional_homicide = WDI(indicator='VC.IHR.PSRC.P5')
intentional_homicide_fem = WDI(indicator = "VC.IHR.PSRC.FE.P5")
intentional_homicide_male = WDI(indicator = "VC.IHR.PSRC.MA.P5")

intentional_homicide["Female"] <- intentional_homicide_fem["VC.IHR.PSRC.FE.P5"]
intentional_homicide["Male"] <- intentional_homicide_male["VC.IHR.PSRC.MA.P5"]

intentional_homicide %>% 
  ggplot(aes(x= year, y = VC.IHR.PSRC.P5)) +
  geom_point()
 
intentional_homicide %>% 
  group_by(year) %>% 
  drop_na() %>% 
  summarise(VC.IHR.PSRC.P5 = sum(VC.IHR.PSRC.P5)) %>% 
  ggplot(aes(x= year, y = VC.IHR.PSRC.P5)) +
  geom_point()

intentional_homicide %>% 
  group_by(year) %>% 
  drop_na() %>% 
  summarise(Female = sum(Female)) %>% 
  ggplot(aes(x= year, y = Female)) +
  geom_point() 

intentional_homicide %>% 
  group_by(year) %>% 
  drop_na() %>% 
  summarise(Male = sum(Male)) %>% 
  ggplot(aes(x= year, y = Male)) +
  geom_point()

# go into country level and regional level 
  
# Conflict -------------------------------------------------------------
# WDI Battle Deaths
battle_deaths <- WDI(indicator = 'VC.BTL.DETH')

battle_deaths %>% 
  ggplot(aes(x= year, y = VC.BTL.DETH)) +
  geom_point() +
  geom_text(aes(label=ifelse(VC.BTL.DETH>75000,as.character(country),'')),hjust=0,vjust=0, size = 3)

battle_deaths %>% 
  group_by(year) %>% 
  drop_na() %>% 
  summarise(VC.BTL.DETH = sum(VC.BTL.DETH)) %>% 
  ggplot(aes(x= year, y = VC.BTL.DETH)) +
  geom_point() 

# Battle Deaths go up crazy
# What happens if you take Syria out
# Steven pinker type has violence gone down

#ACLED
ACLED <- read_csv("ACLED.csv")

ACLED %>% 
  group_by(year) %>% 
  count(event_type)

# ACLED could 
# we could potentially break it out by event type?


#UCDP State Based Violence, Non-State Violence, One sided violence

# Governance --------------------------------------------------------------

Economic_management <- WDI(indicator = 'IQ.CPA.ECON.XQ')
Management_and_Institutions <- WDI(indicator = 'IQ.CPA.PUBS.XQ')
Social_inclusion <- WDI(indicator = 'IQ.CPA.SOCI.XQ')
structural_policies <- WDI(indicator = 'IQ.CPA.STRC.XQ')
resource_allocation <- WDI(indicator = 'IQ.CPA.IRAI.XQ')
rules_based_governance <-  WDI(indicator = 'IQ.CPA.PROP.XQ')

wdi_gov <- inner_join(Economic_management, Management_and_Institutions) %>% 
  inner_join(., Social_inclusion) %>% 
  inner_join(., structural_policies) %>% 
  inner_join(., resource_allocation) %>% 
  inner_join(., rules_based_governance)

wdi_gov %>% 
  drop_na() %>% 
  group_by(country) %>% 
  summarise(IQ.CPA.ECON.XQ = mean(IQ.CPA.ECON.XQ)) %>% 
  ggplot(aes(country, IQ.CPA.ECON.XQ)) +
  geom_col()


potential_gov <- wbsearch("governance") 
WGI_inidic <- wbsearch("wgi") 

# PEFA - Public Expenditure and Financial Accountability (PEFA)
# provides a framework for assessing and reporting on the strengths and weaknesses of public financial management (PFM)

PEFA <- read_csv("PEFA_assessments.csv")

#QoG (Quality of Government) (what level of detail should we go in)

QoG_Basic <- read_csv("QoG_Basic.csv")
QoG_Standard <- read_csv("QoG_Standard.csv")

# Aid Indicators -------------------------------------------------------------

# for next week make some charts and write bullet points for each (thursady morning)




