library(dplyr)
library(ggplot2)
library(stats)

df <- read.csv('Clean_data.csv')
str(df)

zip_level_fac <- df$zip_level_facilities*100/df$Total.Population
zip_level_fac_heart <- df$zip_level_facilities_filtered*100/df$Total.Population
county_level_fac <- df$county_level_facilities_filtered
nearby_fac <- df$OSM_healthcare_poi/df$Total.Population
deaths <- df$All_death/df$Total.Population
heart_deaths <- df$Heart_death*100/df$Total.Population
comorbidity_heart <- df$CHD_CrudePrev + df$OBESITY_CrudePrev + df$CSMOKING_CrudePrev + df$DIABETES_CrudePrev
comorbidity_all <- df$CHD_CrudePrev + df$OBESITY_CrudePrev + df$CANCER_CrudePrev + df$DIABETES_CrudePrev
white <- df$White.alone
black <- df$Black.or.African.American.alone
asian <- df$Asian.alone
other <- df$Some.Other.Race.alone
male <- df$Total.Male.
babies <- df$Male..Under.5. + df$Female..Under.5.
kids <- df$Male..5.to.17. + df$Female..5.to.17.
young_adults <- df$Male..18.to.39. + df$Female..18.to.39.
middle_aged <- df$Male..40.to.59. + df$Female..40.to.59.
senior_citizens <- df$Male..60.and.Over. + df$Female..60.and.Over.
bachelors <- df$Bachelor.s.degree.or.higher
college <- df$Some.college.or.associate.s.degree
high_school <- df$High.school.graduate
less_than_high_school <- df$Less.than.high.school.graduate
income <- df$Income
total_pop <- df$Total.Population
insurance <- df$insurance_access
employed <- df$Employed_16_above
county <- df$county_name

p1 <- ggplot(data = df, aes(x = black, y = zip_level_fac)) + geom_point() + xlab('Black % Composition') + ylab('Health Facilities per 100 People')
p1 + geom_smooth(method="lm")
plot(black, beyond_high_school)
plot(asian, nearby_fac)

white_dummy <- ifelse(white > black & white > asian & white > other, 1, 0)
black_dummy <- ifelse(black > white & black > asian & black > other, 1, 0)
asian_dummy <- ifelse(asian > white & asian > black & asian > other, 1, 0)
bachelors_dummy <- ifelse(bachelors > college & bachelors > high_school & bachelors > less_than_high_school, 1, 0)
college_dummy <- ifelse(college > bachelors & college > high_school & college > less_than_high_school, 1, 0)
hs_dummy <- ifelse(high_school > bachelors & high_school > college & high_school > less_than_high_school, 1, 0)
sc_dummy <- ifelse(senior_citizens > babies & senior_citizens > kids & senior_citizens > young_adults & senior_citizens > middle_aged, 1, 0)
ya_dummy <- ifelse(young_adults > babies & young_adults > kids & young_adults > middle_aged & young_adults > senior_citizens, 1, 0)
ma_dummy <- ifelse(middle_aged > babies & middle_aged > kids & middle_aged > young_adults & middle_aged > senior_citizens, 1, 0)
kids_dummy <- ifelse(kids > babies & kids > young_adults & kids > middle_aged & kids > senior_citizens, 1, 0)

# Level 1a: All Mortality

lm <- lm(heart_deaths ~ comorbidity_all + white + asian + black + senior_citizens + middle_aged + male + income)
summary(lm)

# More facilities in a neighborhood, more per capita deaths. Could be reporting bias.
# Higher comorbidity, higher per capita deaths.
# Higher % of whites, higher per capita deaths.

# Level 1b: Heart Mortality

lm <- lm(heart_deaths ~ comorbidity_heart + black + other + white + asian + income)
summary(lm)

# Level 2a: Co-morbidities

lm <- lm(comorbidity_heart ~ income + white + black + asian + senior_citizens)
summary(lm)

# Greater % of whites, greater comorbidity index
# Higher income neighborhoods, lower comorbidity index
# More educated neighborhoods, lower comorbidity index

# Level 2b: Access to Healthcare

lm <- lm(zip_level_fac ~ county + black)
summary(lm)

lm <- lm(zip_level_fac ~ income + black + white)
summary(lm)

lm <- lm(income ~ male + bachelors + white + asian + black + young_adults + middle_aged)
summary(lm)
