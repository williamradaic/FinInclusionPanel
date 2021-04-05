library(haven)
library(tidyverse)
library(dplyr)
library(readxl)

# Main dataframe

FAS <- read_dta("Raw_Data/FAS_cl.dta")


# Mobile subscriptions

mobile_sub <- read_excel("Raw_Data/API_IT.CEL.SETS.P2_DS2_en_excel_v2_2163817.xls")

mobile_sub <- mobile_sub %>%
  pivot_longer("2004":"2019", names_to = "year", values_to = "mobile_sub", names_transform = list(year = as.numeric))


# ICT import 

ict_import <- read_excel("Raw_Data/API_TM.VAL.ICTG.ZS.UN_DS2_en_excel_v2_2178789.xls", 
                         skip = 3)

ict_import <- ict_import %>%
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "ict_import", names_transform = list(year = as.numeric)) %>%
  select(-c(`1960`:`2003`,`2020`))


# Internet users

internet_users <- read_excel("Raw_Data/API_IT.NET.USER.ZS_DS2_en_excel_v2_2163445.xls", 
skip = 3)

internet_users <- internet_users %>%
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "internet_users", names_transform = list(year = as.numeric)) %>%
  select(-c(`1960`:`2003`,`2020`))


# Fixed tel subscriptions

fix_tel <- read_excel("Raw_Data/API_IT.MLT.MAIN_DS2_en_excel_v2_2164245.xls", 
                      skip = 3)

fix_tel <- fix_tel %>%
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "fix_tel", names_transform = list(year = as.numeric)) %>%
  select(-c(`1960`:`2003`,`2020`))


# GDP per capita growth

gdp_capita_g <- read_csv("Raw_Data/GDPCapitaGrowth.csv", 
                         skip = 3)

gdp_capita_g <- gdp_capita_g %>%
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "gdp_capita_g", names_transform = list(year = as.numeric)) %>%
  select(-c(`1960`:`2003`,`2020`, `X66`))


# Female labor force share

female_labor <- read_csv("Raw_Data/a0f8708e-c025-40a1-bc37-80be5d565b34_Data.csv")

female_labor <- female_labor %>% 
  mutate(across(.cols = -c(`Series Name`, `Series Code`, `Country Name`, `Country Code`), .fns = as.numeric)) %>%
  select(-c(`1996 [YR1996]`:`2003 [YR2003]`,`2020 [YR2020]`)) %>%
  slice_head(n = (nrow(female_labor) - 5))

colnames(female_labor) = c(colnames(female_labor[1:4]), "2004":"2019")

female_labor_part_rate = female_labor %>% slice_head(n = 263) %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "female_labor_part_rate", names_transform = list(year = as.numeric))

female_labor_pct_total = female_labor %>%
  filter(`Series Code` == "SL.TLF.TOTL.FE.ZS")    %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "female_labor_pct_total", names_transform = list(year = as.numeric))

female_male_ratio = female_labor %>%
  filter(`Series Code` == "SL.TLF.CACT.FM.ZS")    %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "female_male_ratio", names_transform = list(year = as.numeric))


# WGI

WGI <- read_excel("Raw_Data/WGI_wb.xlsx")

WGI <- WGI %>% mutate(across(.cols = -c(`Series Name`, `Series Code`, `Country Name`, `Country Code`), .fns = as.numeric))

wgi_corruption <- WGI %>%
  filter(`Series Name` == "Control of Corruption: Estimate") %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "corruption", names_transform = list(year = as.numeric))

wgi_effectiveness <- WGI %>%
  filter(`Series Name` == "Government Effectiveness: Estimate") %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "effectiveness", names_transform = list(year = as.numeric))

wgi_regulatory <- WGI %>%
  filter(`Series Name` == "Regulatory Quality: Estimate") %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "regulatory", names_transform = list(year = as.numeric))

wgi_ruleoflaw <- WGI %>%
  filter(`Series Name` == "Rule of Law: Estimate") %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "ruleoflaw", names_transform = list(year = as.numeric))

wgi_accountability <- WGI %>%
  filter(`Series Name` == "Voice and Accountability: Estimate") %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "accountability", names_transform = list(year = as.numeric))


# Inflation (consumer prices)

inflation <- read_csv("Raw_Data/InflationRate.csv", 
                      skip = 3)

inflation <- inflation %>%
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "inflation", names_transform = list(year = as.numeric)) %>%
  select(-c(`1960`:`2003`,`2020`,`X66`))

# Doing Business

dbusiness <- read_excel("Raw_Data/DB_WB.xlsx", 
                        sheet = "Sheet1")

dbusiness <- dbusiness %>%
  filter((`DB Year` != 2020)) %>%
  rename("year" = `DB Year`) %>%
  mutate(across(.cols = -c(Region, Economy, `Income group`, `Country code`), .fns = as.numeric))


# Heritage (fixed)

heritage <- read_excel("Raw_Data/Heritage_fixed.xlsx", 
                       na = "N/A")

heritage <- heritage %>%
  rename("year" = "Index Year") %>%
  filter(year > 2003) %>%
  filter(year < 2020) %>%
  mutate(across(.cols = -c(`Name`), .fns = as.numeric))







# Merge (beta)

rm(df)

df <- left_join(FAS, inflation, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, dbusiness, by = c("iso3" = "Country code", "year" = "year"))

df <- left_join(df, female_labor_part_rate, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, female_labor_pct_total, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, female_male_ratio, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, fix_tel, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, gdp_capita_g, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, ict_import, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, internet_users, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, mobile_sub, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, wgi_accountability, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, wgi_corruption, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, wgi_effectiveness, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, wgi_regulatory, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, wgi_ruleoflaw, by = c("iso3" = "Country Code", "year" = "year"))

df <- inner_join(df, heritage, by = c("Country Name.x" = "Name", "year" = "year"))



# Toy models

library(plm)
library(labelled)
library(stargazer)

df = pdata.frame(remove_labels(df), index = c("iso3","year"))

model1 <- plm(i_borrowers_A1_pop ~ Property.Rights + Cost....of.claim. + female_labor_pct_total + fix_tel + gdp_capita_g + ict_import + internet_users + mobile_sub, model = "within", data = df, na.action = na.omit)
summary(model1)

stargazer(model1, type = 'text')



gmm_model <- plm(i_borrowers_A1_pop ~ Property.Rights + Cost....of.claim. + female_labor_pct_total + fix_tel + gdp_capita_g + ict_import + internet_users + mobile_sub, model = "within", data = df, na.action = na.omit)
summary(gmm_model)
stargazer(gmm_model, type = 'text')

df$Score.Cost....of.claim.




