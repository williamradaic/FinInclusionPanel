library(haven)
library(tidyverse)
library(dplyr)
library(readxl)

# Main dataframe

FAS <- read_dta("FAS.dta")


# Mobile subscriptions

mobile_sub <- read_excel("Raw_Data/API_IT.CEL.SETS.P2_DS2_en_excel_v2_2163817.xls")

mobile_sub <- mobile_sub %>%
  pivot_longer("2004":"2019", names_to = "year", values_to = "mobile_sub")


# ICT import 

ict_import <- read_excel("Raw_Data/API_TM.VAL.ICTG.ZS.UN_DS2_en_excel_v2_2178789.xls", 
                         skip = 3)

ict_import <- ict_import %>%
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "ict_import") %>%
  select(-c(`1960`:`2003`,`2020`))


# Internet users

internet_users <- read_excel("Raw_Data/API_IT.NET.USER.ZS_DS2_en_excel_v2_2163445.xls", 
skip = 3)

internet_users <- internet_users %>%
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "internet_users") %>%
  select(-c(`1960`:`2003`,`2020`))


# Fixed tel subscriptions

fix_tel <- read_excel("Raw_Data/API_IT.MLT.MAIN_DS2_en_excel_v2_2164245.xls", 
                      skip = 3)

fix_tel <- fix_tel %>%
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "fix_tel") %>%
  select(-c(`1960`:`2003`,`2020`))


# GDP per capita growth

gdp_capita_g <- read_csv("Raw_Data/GDPCapitaGrowth.csv", 
                         skip = 3)

gdp_capita_g <- gdp_capita_g %>%
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "gdp_capita_g") %>%
  select(-c(`1960`:`2003`,`2020`, `X66`))


# Female labor force share

female_labor <- read_csv("Raw_Data/a0f8708e-c025-40a1-bc37-80be5d565b34_Data.csv")

female_labor <- female_labor %>% 
  mutate(across(.cols = -c(`Series Name`, `Series Code`, `Country Name`, `Country Code`), .fns = as.numeric)) %>%
  select(-c(`1996 [YR1996]`:`2003 [YR2003]`,`2020 [YR2020]`)) %>%
  slice_head(n = (nrow(female_labor) - 5))

colnames(female_labor) = c(colnames(female_labor[1:4]), "2004":"2019")

female_labor_part_rate = female_labor %>% slice_head(n = 263) %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "female_labor_part_rate")

female_labor_pct_total = female_labor %>%
  filter(`Series Code` == "SL.TLF.TOTL.FE.ZS")    %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "female_labor_pct_total")

female_male_ratio = female_labor %>%
  filter(`Series Code` == "SL.TLF.CACT.FM.ZS")    %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "female_male_ratio")


# WGI

WGI <- read_excel("Raw_Data/WGI_wb.xlsx")

WGI <- WGI %>% mutate(across(.cols = -c(`Series Name`, `Series Code`, `Country Name`, `Country Code`), .fns = as.numeric))

wgi_corruption <- WGI %>%
  filter(`Series Name` == "Control of Corruption: Estimate") %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "corruption")

wgi_effectiveness <- WGI %>%
  filter(`Series Name` == "Government Effectiveness: Estimate") %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "effectiveness")

wgi_regulatory <- WGI %>%
  filter(`Series Name` == "Regulatory Quality: Estimate") %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "regulatory")

wgi_ruleoflaw <- WGI %>%
  filter(`Series Name` == "Rule of Law: Estimate") %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "ruleoflaw")

wgi_accountability <- WGI %>%
  filter(`Series Name` == "Voice and Accountability: Estimate") %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "accountability")













