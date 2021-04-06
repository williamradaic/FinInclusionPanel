library(haven)
library(tidyverse)
library(dplyr)
library(readxl)

# FAS #############

FAS <- read_dta("Raw_Data/FAS_cl.dta")


# Mobile subscriptions #############

mobile_sub <- read_excel("Raw_Data/API_IT.CEL.SETS.P2_DS2_en_excel_v2_2163817.xls")

mobile_sub <- mobile_sub %>%
  pivot_longer("2004":"2019", names_to = "year", values_to = "mobile_sub", names_transform = list(year = as.numeric))


# ICT import #############

ict_import <- read_excel("Raw_Data/API_TM.VAL.ICTG.ZS.UN_DS2_en_excel_v2_2178789.xls", 
                         skip = 3)

ict_import <- ict_import %>%
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "ict_import", names_transform = list(year = as.numeric)) %>%
  select(-c(`1960`:`2003`,`2020`))


# Internet users #############

internet_users <- read_excel("Raw_Data/API_IT.NET.USER.ZS_DS2_en_excel_v2_2163445.xls", 
skip = 3)

internet_users <- internet_users %>%
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "internet_users", names_transform = list(year = as.numeric)) %>%
  select(-c(`1960`:`2003`,`2020`))


# Fixed tel subscriptions #############

fix_tel <- read_excel("Raw_Data/API_IT.MLT.MAIN_DS2_en_excel_v2_2164245.xls", 
                      skip = 3)

fix_tel <- fix_tel %>%
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "fix_tel", names_transform = list(year = as.numeric)) %>%
  select(-c(`1960`:`2003`,`2020`))


# GDP per capita growth #############

gdp_capita_g <- read_csv("Raw_Data/GDPCapitaGrowth.csv", 
                         skip = 3)

gdp_capita_g <- gdp_capita_g %>%
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "gdp_capita_g", names_transform = list(year = as.numeric)) %>%
  select(-c(`1960`:`2003`,`2020`, `X66`))


# Female labor force share #############

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


# WGI #############

WGI <- read_excel("Raw_Data/WGI_wb.xlsx")

WGI <- WGI %>% mutate(across(.cols = -c(`Series Name`, `Series Code`, `Country Name`, `Country Code`), .fns = as.numeric))

wgi_corruption <- WGI %>%
  filter(`Series Name` == "Control of Corruption: Estimate") %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "wgi_corruption", names_transform = list(year = as.numeric))

wgi_effectiveness <- WGI %>%
  filter(`Series Name` == "Government Effectiveness: Estimate") %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "wgi_effectiveness", names_transform = list(year = as.numeric))

wgi_regulatory <- WGI %>%
  filter(`Series Name` == "Regulatory Quality: Estimate") %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "wgi_regulatory", names_transform = list(year = as.numeric))

wgi_ruleoflaw <- WGI %>%
  filter(`Series Name` == "Rule of Law: Estimate") %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "wgi_ruleoflaw", names_transform = list(year = as.numeric))

wgi_accountability <- WGI %>%
  filter(`Series Name` == "Voice and Accountability: Estimate") %>% 
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "wgi_accountability", names_transform = list(year = as.numeric))


# Inflation (consumer prices) #############

inflation <- read_csv("Raw_Data/InflationRate.csv", 
                      skip = 3)

inflation <- inflation %>%
  pivot_longer(`2004`:`2019`, names_to = "year", values_to = "inflation", names_transform = list(year = as.numeric)) %>%
  select(-c(`1960`:`2003`,`2020`,`X66`))

# Doing Business #############

dbusiness <- read_excel("Raw_Data/DB_WB.xlsx", 
                        sheet = "Sheet1")

dbusiness <- dbusiness %>%
  filter((`DB Year` != 2020)) %>%
  rename("year" = `DB Year`) %>%
  mutate(across(.cols = -c(Region, Economy, `Income group`, `Country code`), .fns = as.numeric))


# Heritage (fixed) #############

heritage <- read_excel("Raw_Data/Heritage_fixed.xlsx", 
                       na = "N/A")

heritage <- heritage %>%
  rename("year" = "Index Year") %>%
  filter(year > 2003) %>%
  filter(year < 2020) %>%
  mutate(across(.cols = -c(`Name`), .fns = as.numeric))


# Polity V #############

polity_v <- read_excel("Raw_Data/polity_v.xls")



# Merge (beta) #############

# rm(df)

df <- left_join(FAS, inflation, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, female_labor_part_rate, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, female_labor_pct_total, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, female_male_ratio, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, fix_tel, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, gdp_capita_g, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, ict_import, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, internet_users, by = c("iso3" = "Country Code", "year" = "year"))

## inner join para excluir divisão de regiões como rows da df
df <- inner_join(df, mobile_sub, by = c("iso3" = "Country Code", "year" = "year"))
##

df <- left_join(df, wgi_accountability, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, wgi_corruption, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, wgi_effectiveness, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, wgi_regulatory, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, wgi_ruleoflaw, by = c("iso3" = "Country Code", "year" = "year"))

df <- left_join(df, dbusiness, by = c("iso3" = "Country code", "year" = "year"))

df <- left_join(df, heritage, by = c("Country Name.x" = "Name", "year" = "year"))

df <- left_join(df, polity_v, by = c("iso3" = "scode", "year" = "year"))


## Clean df variables #############

df_clean <- df %>% 
  select(-c(i_mob_agent_active_pop, i_mob_agent_active_km2,  i_mob_agent_registered_pop, i_mob_agent_registered_km2, i_depositors_A1_sme_perNFC, i_depositors_A1_hhs_pop, contains("hhs"), contains("sme"), contains("_M"), contains("_F"), contains(".x"), contains(".y"))) %>%
  select(-c(Economy, `country`, `p5`))

df_clean2 <- df_clean %>% 
  mutate(.before = i_branches_A1_km2, i_branches_sum_km2 = (i_branches_A1_km2 %>% replace_na(0) + i_branches_A2_km2 %>% replace_na(0) + i_branches_A3B1a_km2 %>% replace_na(0) + i_branches_A4_km2 %>% replace_na(0) + i_nonbranch_A1_km2 %>% replace_na(0)) %>% na_if(0)) %>% # Adicionei nonbranch (correspondentes bancários). Acho que pode ser relevante para países específicos, e tá razoavelmente análogo à essa variável aqui.
  mutate(.before = i_branches_A1_pop, i_branches_sum_pop = (i_branches_A1_pop %>% replace_na(0) + i_branches_A2_pop %>% replace_na(0) + i_branches_A3B1a_pop %>% replace_na(0) + i_branches_A4_pop %>% replace_na(0)  + i_nonbranch_A1_pop %>% replace_na(0)) %>% na_if(0)) %>%
  mutate(.before = i_depositors_A1_pop, i_depositors_sum_pop = (i_depositors_A1_pop %>% replace_na(0) + i_depositors_A2_pop %>% replace_na(0)) %>% na_if(0)) %>%
  mutate(.before = i_deposit_acc_A1_pop, i_deposit_acc_sum_pop = (i_deposit_acc_A1_pop %>% replace_na(0) + i_deposit_acc_A2_pop %>% replace_na(0)) %>% na_if(0)) %>%
  mutate(.before = i_borrowers_A1_pop, i_borrowers_sum_pop = (i_borrowers_A1_pop %>% replace_na(0) + i_borrowers_A2_pop %>% replace_na(0) + i_borrowers_A3B1a_pop %>% replace_na(0)) %>% na_if(0)) %>%
  mutate(.before = i_loan_acc_A1_pop, i_loan_acc_sum_pop = (i_loan_acc_A1_pop %>% replace_na(0) + i_loan_acc_A2_pop %>% replace_na(0) + i_loan_acc_A3B1a_pop %>% replace_na(0)) %>% na_if(0)) %>%
  mutate(.before = i_deposits_A1_GDP, i_deposits_sum_GDP = (i_deposits_A1_GDP %>% replace_na(0) + i_deposits_A2_GDP %>% replace_na(0)) %>% na_if(0)) %>%
  mutate(.before = i_loans_A1_GDP, i_loans_sum_GDP = (i_loans_A1_GDP %>% replace_na(0) + i_loans_A2_GDP %>% replace_na(0) + i_loans_A3B1a_GDP %>% replace_na(0)) %>% na_if(0)) %>%
  mutate(.before = i_cards_credit_pop, i_cards_sum_pop = (i_cards_credit_pop %>% replace_na(0) + i_cards_debit_pop %>% replace_na(0)) %>% na_if(0)) %>%
  mutate(.before = i_policies_B2_life_pop, i_policies_B2_sum_pop = (i_policies_B2_life_pop %>% replace_na(0) + i_policies_B2_nonlife_pop %>% replace_na(0)) %>% na_if(0)) 
  
  
df_clean3 <- df_clean2 %>%
  select(-c(contains("A2"), contains("A3"), contains("A3B1a"), contains("A4"), i_nonbranch_A1_km2, i_nonbranch_A1_pop)) %>%
  relocate(c(Region, `Income group`), .before = i_branches_sum_km2) %>%
  relocate(c(iso3, year)) %>%
  select(-c(DB_score_1720, DB_score_15, DB_score_1014, getting_credit_1520, getting_credit_0514, score_enforcing_1720, score_enforcing_0415))



# PCA for financial inclusion #############

pca_1 <- princomp(df_clean3 %>% select(c(i_depositors_sum_pop, i_borrowers_sum_pop)) %>% drop_na(), cor = T)

pca_1 <- prcomp(df_clean3 %>% select(c(i_depositors_sum_pop, i_borrowers_sum_pop)) %>% drop_na(), scale = T)

plot(pca_1)

library(factoextra)
fviz_eig(pca_1)

d_acct = ((na.omit(df_clean3$i_deposit_acc_sum_pop)) - min(df_clean3$i_deposit_acc_sum_pop, na.rm = T) / max(df_clean3$i_deposit_acc_sum_pop, na.rm = T) - min(df_clean3$i_deposit_acc_sum_pop, na.rm = T))


df_clean3 <- df_clean3 %>%
  mutate(d_acc = (i_deposit_acc_sum_pop - min(i_deposit_acc_sum_pop) / max(df_clean3$i_deposit_acc_sum_pop) - min(df_clean3$i_deposit_acc_sum_pop)))

for (i in (1:nrow(df_clean3))) {
  df_clean3$d_acc[i] = ((df_clean3$i_deposit_acc_sum_pop[i] - min(df_clean3$i_deposit_acc_sum_pop, na.rm = T)) / (max(df_clean3$i_deposit_acc_sum_pop, na.rm = T) - min(df_clean3$i_deposit_acc_sum_pop, na.rm = T)))
}

summary(df_clean3$d_acc)

d_branches = matrix(NA, nrow = nrow(df_clean3))

df_clean3 = cbind(df_clean3, d_branches)

for (i in (1:nrow(df_clean3))) {
  df_clean3$d_branches[i] = ((df_clean3$i_branches_sum_pop[i] - min(df_clean3$i_branches_sum_pop, na.rm = T)) / (max(df_clean3$i_branches_sum_pop, na.rm = T) - min(df_clean3$i_branches_sum_pop, na.rm = T)))
}

summary(df_clean3$d_branches)


d_atm = matrix(NA, nrow = nrow(df_clean3))

df_clean3 = cbind(df_clean3, d_atm)

for (i in (1:nrow(df_clean3))) {
  df_clean3$d_atm[i] = ((df_clean3$i_ATMs_pop[i] - min(df_clean3$i_ATMs_pop, na.rm = T)) / (max(df_clean3$i_ATMs_pop, na.rm = T) - min(df_clean3$i_ATMs_pop, na.rm = T)))
}

summary(df_clean3$d_atm)











#library(foreign)

#write.dta(df, file = "df_1.dta")

#library(haven)

write_dta(df_clean3 %>% janitor::clean_names(), "df_clean3.dta", version = 14)


# Toy models #############

library(plm)
library(labelled)
library(stargazer)

df = pdata.frame(remove_labels(df_clean3), index = c("iso3","year"))

dfteste <- df_clean3 %>%
  select(c(i_borrowers_A1_pop,iso3, year, wgi_ruleoflaw, female_labor_pct_total, fix_tel, gdp_capita_g, ict_import, internet_users, mobile_sub))

dfteste = pdata.frame(remove_labels(dfteste), index = c("iso3","year"))

make.pbalanced(dfteste, "shared.individuals")
is.pconsecutive(df)

model1 <- plm(i_borrowers_A1_pop ~ wgi_ruleoflaw + female_labor_pct_total + fix_tel + gdp_capita_g + ict_import + internet_users + mobile_sub, model = "within", data = dfteste, na.action = na.omit)
summary(model1)

stargazer(model1, type = 'text')



gmm_model <- plm(i_borrowers_A1_pop ~ Property.Rights + Cost....of.claim. + female_labor_pct_total + fix_tel + gdp_capita_g + ict_import + internet_users + mobile_sub, model = "within", data = df, na.action = na.omit)
summary(gmm_model)
stargazer(gmm_model, type = 'text')





