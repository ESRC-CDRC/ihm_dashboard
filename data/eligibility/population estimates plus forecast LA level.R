library(tidyverse)



################################################################################
######### Based on population forcast data                            ##########
################################################################################

read_csv('data/eligibility/local_authority_pop_estimates.csv') %>%
  filter(AGE_GROUP != 'All ages') %>%
  mutate(age = as.numeric(ifelse(AGE_GROUP == "90 and over", 90, AGE_GROUP))) %>%
  filter(age >= 60) %>%
  write_csv("data/eligibility/local_authority_pop_estimates.csv")


data <- read_csv('data/eligibility/local_authority_pop_estimates.csv') %>% 
  filter(AREA_CODE %in% c('E08000030','E08000031','E08000026','E08000028','E08000027','E08000029','E08000025'))



# 63
pop_2016 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2016`) %>%
  filter(AGE_GROUP >= 63) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2016' = sum(`2016`)) %>%
  gather(year, count, -AREA_CODE)

#63.75
pop_2017 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2017`) %>%
  spread(AGE_GROUP, `2017`) %>%
  mutate(`63` = `63`*0.25) %>%
  gather(AGE_GROUP , count, -AREA_CODE) %>%
  filter(AGE_GROUP >= 63) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2017' = sum(count)) %>%
  gather(year, count, -AREA_CODE)


#64.5
#63.75
pop_2018 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2018`) %>%
  spread(AGE_GROUP, `2018`) %>%
  mutate(`64` = `64`*0.5) %>%
  gather(AGE_GROUP , count, -AREA_CODE) %>%
  filter(AGE_GROUP >= 64) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2018' = sum(count)) %>%
  gather(year, count, -AREA_CODE)

#65.25
pop_2019 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2019`) %>%
  spread(AGE_GROUP, `2019`) %>%
  mutate(`65` = `65`*0.75) %>%
  gather(AGE_GROUP , count, -AREA_CODE) %>%
  filter(AGE_GROUP >= 65) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2019' = sum(count)) %>%
  gather(year, count, -AREA_CODE)

#66
pop_2020 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2020`) %>%
  filter(AGE_GROUP >= 66) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2020' = sum(`2020`)) %>%
  gather(year, count, -AREA_CODE)

#66
pop_2021 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2021`) %>%
  filter(AGE_GROUP >= 66) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2021' = sum(`2021`)) %>%
  gather(year, count, -AREA_CODE)

#66
pop_2022 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2022`) %>%
  filter(AGE_GROUP >= 66) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2022' = sum(`2022`)) %>%
  gather(year, count, -AREA_CODE)

#66
pop_2023 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2023`) %>%
  filter(AGE_GROUP >= 66) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2023' = sum(`2023`)) %>%
  gather(year, count, -AREA_CODE)

#66
pop_2024 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2024`) %>%
  filter(AGE_GROUP >= 66) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2024' = sum(`2024`)) %>%
  gather(year, count, -AREA_CODE)

#66
pop_2025 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2025`) %>%
  filter(AGE_GROUP >= 66) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2025' = sum(`2025`)) %>%
  gather(year, count, -AREA_CODE)

#66
pop_2026 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2026`) %>%
  filter(AGE_GROUP >= 66) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2026' = sum(`2026`)) %>%
  gather(year, count, -AREA_CODE)

#2027 age = 66.5
pop_2027 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2027`) %>%
  spread(AGE_GROUP, `2027`) %>%
  mutate(`66` = `66`/2) %>%
  gather(AGE_GROUP , count, -AREA_CODE) %>%
  filter(AGE_GROUP >= 66) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2027' = sum(count)) %>%
  gather(year, count, -AREA_CODE)

#67
pop_2028 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2028`) %>%
  filter(AGE_GROUP >= 67) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2028' = sum(`2028`)) %>%
  gather(year, count, -AREA_CODE)

#67
pop_2029 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2029`) %>%
  filter(AGE_GROUP >= 67) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2029' = sum(`2029`)) %>%
  gather(year, count, -AREA_CODE)

#67
pop_2030 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2030`) %>%
  filter(AGE_GROUP >= 67) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2030' = sum(`2030`)) %>%
  gather(year, count, -AREA_CODE)

#67
pop_2031 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2031`) %>%
  filter(AGE_GROUP >= 67) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2031' = sum(`2031`)) %>%
  gather(year, count, -AREA_CODE)

#67
pop_2032 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2032`) %>%
  filter(AGE_GROUP >= 67) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2032' = sum(`2032`)) %>%
  gather(year, count, -AREA_CODE)

#67
pop_2033 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2033`) %>%
  filter(AGE_GROUP >= 67) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2033' = sum(`2033`)) %>%
  gather(year, count, -AREA_CODE)

#67
pop_2034 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2034`) %>%
  filter(AGE_GROUP >= 67) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2034' = sum(`2034`)) %>%
  gather(year, count, -AREA_CODE)

#67
pop_2035 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2035`) %>%
  filter(AGE_GROUP >= 67) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2035' = sum(`2035`)) %>%
  gather(year, count, -AREA_CODE)

#67
pop_2036 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2036`) %>%
  filter(AGE_GROUP >= 67) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2036' = sum(`2036`)) %>%
  gather(year, count, -AREA_CODE)

#67
pop_2037 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2037`) %>%
  filter(AGE_GROUP >= 67) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2037' = sum(`2037`)) %>%
  gather(year, count, -AREA_CODE)


# 2038 age = 67.5
pop_2038 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2038`) %>%
  spread(AGE_GROUP, `2038`) %>%
  mutate(`67` = `67`/2) %>%
  gather(AGE_GROUP , count, -AREA_CODE) %>%
  filter(AGE_GROUP >= 67) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2038' = sum(count)) %>%
  gather(year, count, -AREA_CODE)

# 2039 age = 68
pop_2039 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2039`) %>%
  filter(AGE_GROUP >= 68) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2039' = sum(`2039`)) %>%
  gather(year, count, -AREA_CODE)

pop_2040 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2040`) %>%
  filter(AGE_GROUP >= 68) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2040' = sum(`2040`)) %>%
  gather(year, count, -AREA_CODE)

pop_2041 <- 
  data %>% 
  select(AREA_CODE, AGE_GROUP, `2041`) %>%
  filter(AGE_GROUP >= 68) %>% 
  group_by(AREA_CODE) %>% 
  summarise('2041' = sum(`2041`)) %>%
  gather(year, count, -AREA_CODE)


eleg_data <- bind_rows(pop_2016, pop_2017, pop_2018, pop_2019, pop_2020, 
          pop_2021, pop_2022, pop_2023, pop_2024, pop_2025, pop_2026, pop_2027, pop_2028, pop_2029, pop_2030,
          pop_2031, pop_2032, pop_2033, pop_2034, pop_2035, pop_2036, pop_2037, pop_2038, pop_2039, pop_2040,
          pop_2041)


eleg_data %>% write_csv("data/eligibility/elegible_pop_wm_forecast.csv")



ggplot(eleg_data, aes(x = year, y = count, group = AREA_CODE)) +
  geom_line(colour = "red") +
  geom_point() +
  labs(x = "Year", y = "Eligible Population (n)")
