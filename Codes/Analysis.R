data <- read.csv("Clean_data.csv")
head(data)

str(data)
library(dplyr)
library(ggplot2)

rownames(data) <- data$ZCTA

data <- data %>% subset(c(-1))

# cols = c("ZCTA","Total.Population","insurance_access","Less.than.high.school.graduate","High.school.graduate","Some.college.or.associate.s.degree",
#          "Bachelor.s.degree.or.higher","All_death","Total.Male.","Total.Female.","")

# White %
p1 <- ggplot(data = data, 
             aes(x = White.alone, y = Income)) + 
  geom_point() + xlab('White % Composition') + ylab('Median Income for ZCTA')
p1 + geom_smooth(method="lm")

# Black %
p1 <- ggplot(data = data, 
             aes(x = Black.or.African.American.alone, y = Income)) + 
  geom_point() + xlab('Black % Composition') + ylab('Median Income for ZCTA')
p1 + geom_smooth(method="lm")

# Asian %
p1 <- ggplot(data = data, 
             aes(x = Asian.alone, y = Income)) + 
  geom_point() + xlab('Asian % Composition') + ylab('Median Income for ZCTA')
p1 + geom_smooth(method="lm")

plot(data$Income ~ data$Total.Male.)
plot(data$Income ~ log(data$zip_level_facilities*100/data$Total.Population))

plot(data$Income ~ data$Bachelor.s.degree.or.higher)
plot(data$Income ~ data$Some.college.or.associate.s.degree)

model <- lm(data$Income ~ log(data$zip_level_facilities*100/data$Total.Population))
summary(model)


model_white_educ <- lm(Income ~ White.alone + Bachelor.s.degree.or.higher + Bachelor.s.degree.or.higher*White.alone, data=data)
summary(model_white_educ)

model_black_educ <- lm(Income ~ Black.or.African.American.alone + Bachelor.s.degree.or.higher + Bachelor.s.degree.or.higher*Black.or.African.American.alone, 
                       data=data)
summary(model_black_educ)

quantile(data$Black.or.African.American.alone)

# data %>% rowwise() %>% mutate(max_race = max(c(White.alone, Black.or.African.American.alone, Asian.alone, American.Indian.and.Alaska.Native.alone, Native.Hawaiian.and.Other.Pacific.Islander.alone, Some.Other.Race.alone)))
data$max_race <- rowMaxs(as.matrix(data[,c("White.alone", "Black.or.African.American.alone", "Asian.alone", "American.Indian.and.Alaska.Native.alone", "Native.Hawaiian.and.Other.Pacific.Islander.alone", "Some.Other.Race.alone")]))

quantile(data$Black.or.African.American.alone, 0.9)
quantile(data$Asian.alone)

# data$White_Majority <- ifelse(data$White.alone == data$max_race, 1, 0)
# data$Black_Majority <- ifelse(data$Black.or.African.American.alone == data$max_race, 1, 0)
# data$Asian_majority <- ifelse(data$Asian.alone == data$max_race, 1, 0)
# data$Other_majority <- ifelse(data$American.Indian.and.Alaska.Native.alone == data$max_race | data$Native.Hawaiian.and.Other.Pacific.Islander.alone == data$max_race | data$Some.Other.Race.alone == data$max_race, 1, 0)
white_prop <- data$White.alone
african_american_prop <- data$Black.or.African.American.alone
asian_prop <- data$Asian.alone
bachelors <- data$Bachelor.s.degree.or.higher
less_than_high_school <- data$Less.than.high.school.graduate + data$High.school.graduate
income <- data$Income
employed <- data$Employed_16_above
heart_death <- data$Heart_death/data$Total.Population
facilites <- data$per_capita_facility
facilites_filtered <- data$per_capita_facility_filtered
county <- data$county_name

# model_educ_interaction <- lm(Income ~ Bachelor.s.degree.or.higher + Black_Majority + Asian_majority + Other_majority + 
#                                       Bachelor.s.degree.or.higher*Black_Majority + Bachelor.s.degree.or.higher*Asian_majority + 
#                                       Bachelor.s.degree.or.higher*Other_majority, data=data)
# summary(model_educ_interaction)
# 
# model_educ_black_not_black <- lm(Income ~ Bachelor.s.degree.or.higher + Black_Majority + Bachelor.s.degree.or.higher*Black_Majority, data=data)
# summary(model_educ_black_not_black)
# 
# model_educ_black_not_black_no_interaction <- lm(Income ~ Bachelor.s.degree.or.higher + Black_Majority, data=data)
# summary(model_educ_black_not_black_no_interaction)
# 
# # Checking if white vs not white has a significant impact on the income. 
# # Result: The individual impact of the variables are statistically significant but the interaction fo being white 
# # and education seems to play no role on income
# 
# model_educ_white_not_white_no_interaction <- lm(Income ~ White_Majority + Bachelor.s.degree.or.higher, data=data)
# summary(model_educ_white_not_white_no_interaction)
# 
# model_no_educ_white_no_interaction <- lm(Income ~ Less.than.high.school.graduate + White_Majority + Black_Majority, data=data)
# summary(model_no_educ_white_no_interaction)
# 
# model_no_educ_white_no_interaction <- lm(Income ~ Less.than.high.school.graduate + White_Majority + Black_Majority + Asian_majority, data=data)
# summary(model_no_educ_white_no_interaction)

summary(less_than_high_school)
# On average, 30% of population in California zip codes have less than or equal to high school level education

summary(bachelors)

# High Education

## Whites
education_whites <- lm(income ~ white_prop + bachelors)
summary(education_whites)

car::vif(education_whites)

education_whites <- lm(income ~ white_prop + bachelors + white_prop*bachelors)
summary(education_whites)

# education_whites <- lm(income ~ white_prop + bachelors + employed)
# summary(education_whites)
# 
# car::vif(education_whites)
# 
# education_whites <- lm(income ~ white_prop + bachelors + employed + white_prop*bachelors)
# summary(education_whites)
# 
# education_whites <- lm(income ~ white_prop + bachelors + employed + white_prop*bachelors + white_prop*employed)
# summary(education_whites)

## Blacks
education_blacks <- lm(income ~ black_prop + bachelors)
summary(education_blacks)

car::vif(education_blacks)

education_blacks <- lm(income ~ black_prop + bachelors + black_prop*bachelors)
summary(education_blacks)

education_blacks <- lm(income ~ black_prop + bachelors + employed)
summary(education_blacks)

car::vif(education_blacks)

education_blacks <- lm(income ~ black_prop + bachelors + employed + black_prop*bachelors)
summary(education_blacks)

education_blacks <- lm(income ~ black_prop + bachelors + employed + black_prop*bachelors +black_prop*employed)
summary(education_blacks)

## Asians
education_asian <- lm(income ~ asian_prop + bachelors + asian_prop*bachelors)
summary(education_asian)




education_mix <- lm(income ~ white_prop + black_prop + bachelors + 
                        white_prop*bachelors + black_prop*bachelors + white_prop*black_prop)
summary(education_mix)


# Low Education
no_education_whites <- lm(income ~ less_than_high_school + white_prop + white_prop*less_than_high_school)
summary(no_education_whites)

no_education_blacks <- lm(income ~ less_than_high_school + black_prop + black_prop*less_than_high_school)
summary(no_education_blacks)


model <- lm(heart_death ~ income + facilites)
summary(model)

model1 <- lm(heart_death ~ income + facilites_filtered)
summary(model1)

facilites_model <- lm(facilites ~ income + bachelors + white_prop)
summary(facilites_model)

facilites_model <- lm(facilites ~ white_prop)
summary(facilites_model)

facilites_model <- lm(facilites ~ black_prop)
summary(facilites_model)

facilites_model <- lm(facilites_filtered ~ white_prop)
summary(facilites_model)

facilites_model <- lm(facilites_filtered ~ black_prop)
summary(facilites_model)

facilites_model <- lm(facilites_filtered ~ white_prop + as.factor(data$county_name))
summary(facilites_model)


data$per_capita_facility <- data$zip_level_facilities/data$Total.Population
data$per_capita_facility_heart <- data$zip_level_facilities_filtered/data$Total.Population

summary(lm(facilites ~ african_american_prop + county))
summary(lm(facilites ~ white_prop + county))
summary(lm(facilites ~ asian_prop + county))



facilites_model <- lm(facilites ~ income + bachelors + black_prop)
summary(facilites_model)

facilities_filtered_model <- lm(facilites_filtered ~ income + bachelors + white_prop)
summary(facilities_filtered_model)

facilities_filtered_model <- lm(facilites_filtered ~ income + bachelors + black_prop)
summary(facilities_filtered_model)
