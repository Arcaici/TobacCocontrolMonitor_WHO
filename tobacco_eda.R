#---------------------------cleaning---------------------------#

#loading dataset
tobacco <- read.csv("data.csv", header = TRUE, sep = ",")

# dataset format
tobacco
names(tobacco)

# selecting a subset of features
features_selected <- c("ParentLocation", "Location", "Period", "Dim1", "Value")
selected_data <- tobacco[,features_selected]

# check on geographic regions and Location
unique(selected_data$ParentLocation)
unique(selected_data$Location)
unique(selected_data$Value)

# renaming columns
names(selected_data) <- c("geo_region", "state", "year", "sex", "value")
head(selected_data)

# renaiming geo regions

rename_areas <- function(item){
  changeTo = item
  if (item == "Eastern Mediterranean") {
      changeTo = "Eastern Med."
  }
  
  if (item == "Western Pacific") {
    changeTo = "West. Pacific"
  }
  if (item == "South-East Asia") {
    changeTo = "S-E Asia"    
  }  
  
  return(as.character(changeTo))
}

selected_data$geo_region <- sapply(selected_data$geo_region,rename_areas)


# removing confidence intervals from value
typeof(selected_data$value)

remove_interval <- function(item){
  space_index <- unlist(gregexpr("\\s", item))[1]
  clean_value <- substring(item, 1, space_index-1)
  return(as.integer(clean_value))
}

selected_data$value <- sapply(selected_data$value,remove_interval)

# removing years 2023 and 2025 (last update of data is 17/01/2022)
selected_data<- selected_data[selected_data$year <= 2022, ]

# check if there are Nan values
has_nan <- apply(selected_data, 2, function(x) any(is.nan(x)))
has_nan

# summaries
summary(selected_data)

# sex groups and dataset divisions
unique(selected_data$sex)

female_tobacco <- selected_data[selected_data$sex =="Female",]
male_tobacco <- selected_data[selected_data$sex =="Male",]
bothsex_tobacco <- selected_data[selected_data$sex =="Both sexes",]

# gender summary
summary(female_tobacco)
summary(male_tobacco)
summary(bothsex_tobacco)

#---------------------------Exploration BothSex---------------------------#

occurrency_each_year = 3444/7
occurrency_each_year
number_of_state = unique(selected_data$state)
number_of_state

# this function check:
#   1) if the number of state is equal to 164
#   2) if each state as the same number of sample
#   3) if the number of samples of each state is equal for each year
#   
# and return:
#   I) if all above conditions are TRUE return the number of samples of state per year
#  II) else return -1 

check_same_number_of_sample_for_state_and_year <- function(df){
  result = -1
  all_state_as_same_number_of_sample= FALSE
  
  if (length(unique(df$state)) == 164){
    states_occurrency_count <- table(df$state, df$year)
    states_counts_df <- as.data.frame(states_occurrency_count)
    if (length(unique(states_counts_df$Freq)) == 1) {
      all_state_as_same_number_of_sample = TRUE
    }
  }
  if(all_state_as_same_number_of_sample) {
    result =  (nrow(df) / length(unique(df$year))) / length(unique(df$state))
  }
  return(result)
}

bothsex_sample_per_state <- check_same_number_of_sample_for_state_and_year(bothsex_tobacco)
bothsex_sample_per_state

# The number of sample for each state in each year is 3 for each year in any state,
# so i will use mean for have a single sample for each year of each state
library(dplyr)

both_cleaned <- bothsex_tobacco %>%
  group_by(year,geo_region, state) %>%
  summarize(value = mean(value))

both_cleaned

both_lastyear <- both_cleaned[both_cleaned$year == 2020, ]
summary(both_lastyear)

library(ggplot2)

# frequency plot of value of both sex
ggplot(both_lastyear, aes(x = value)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(x = "Value", y = "Frequency", title = "Frequency Plot of usage of Tobacco in percentage of both  (2020)") +
  theme(text = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5))

# Distribution plot of value of both sex
ggplot(both_lastyear, aes(x = value)) +
  geom_density(fill = "blue", color = "black", alpha = 0.5) +
  labs(x = "Value", y = "Density", title = "Distribution Plot of usage of Tobacco in percentage of both sex (2020)")

# Boxplot of tobacco use in Geographic areas

ggplot(both_lastyear, aes(x = factor(both_lastyear$geo_region), y = both_lastyear$value, fill= factor(both_lastyear$geo_region))) +
  geom_boxplot(color = "black", width = 0.5) +  
  labs(x = "Geographic Area",              
       y = "Tobacco use",           
       title = "Boxplot of Tobacco usage by Geographic Area in 2020")+
  theme(legend.position = "none")

# Violinplot of tobacco use in Geographic areas

ggplot(both_lastyear, aes(x = factor(both_lastyear$geo_region), y = both_lastyear$value, fill= factor(both_lastyear$geo_region))) +
  geom_violin(color = "black", width = 0.5) +
  geom_point()+
  labs(x = "Geographic Area",              
       y = "Tobacco use",           
       title = "Violinplot of Tobacco usage by Geographic Area in 2020") +
theme(text = element_text(size = 14), 
      plot.title = element_text(size = 14, hjust = 0.5), legend.position = "none")

# Calculate the mean tobacco use for each Area


mean_by_area_2020 <- both_lastyear %>%
  group_by(geo_region) %>%
  summarise(MeanTobaccoUse = mean(value, na.rm = TRUE))

mean_by_area_2020

# Bar plot of tobacco use in states respect to all country areas
ggplot(mean_by_area_2020, aes(x = factor(mean_by_area_2020$geo_region ), y = mean_by_area_2020$MeanTobaccoUse, fill= factor(mean_by_area_2020$geo_region))) +
  geom_bar(stat = "identity") +
  labs(x = "Geographic Area",              
       y = "Tobacco use",           
       title = "Mean of Tobacco usage by Geographic Area in 2020") +
  theme(text = element_text(size = 14), 
        plot.title = element_text(size = 14, hjust = 0.5), legend.position = "none")



# Table of 
mean_by_year_area <- both_cleaned %>%
  group_by(year, geo_region) %>%
  summarise(MeanTobaccoUse = mean(value, na.rm = TRUE))

mean_by_year_area
# Create a time series plot
ggplot(mean_by_year_area, aes(x = year, y = MeanTobaccoUse, group = geo_region, color = as.factor(geo_region))) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Mean Tobacco Use Percentage", title = "Time Series of Mean Tobacco Use Percentage by Geographic Area") +
  scale_color_discrete(name = "Geographic Area") +
  theme_minimal() +
  theme(text = element_text(size = 14), 
        plot.title = element_text(size = 14, hjust = 0.5))


#---------------------------Exploration Over sex---------------------------#

male_sample_per_state <- check_same_number_of_sample_for_state_and_year(male_tobacco)
male_sample_per_state

# The number of sample for each state in each year is 3 for each year in any state,
# so i will use mean for have a single sample for each year of each state

male_cleaned <- male_tobacco %>%
  group_by(year,geo_region, state, sex) %>%
  summarize(value = mean(value))

male_cleaned

female_sample_per_state <- check_same_number_of_sample_for_state_and_year(female_tobacco)
female_sample_per_state

# The number of sample for each state in each year is 3 for each year in any state,
# so i will use mean for have a single sample for each year of each state

female_cleaned <- female_tobacco %>%
  group_by(year,geo_region, state, sex) %>%
  summarize(value = mean(value))

female_cleaned

sex_combined <- rbind(male_cleaned, female_cleaned)
sex_combined_lastyear <- sex_combined[sex_combined$year==2020, ]

# frequency plot of value by sex
ggplot(sex_combined_lastyear, aes(x = value, fill=sex)) +
  geom_histogram(binwidth = 0.5, color = "black") +
  labs(x = "Value", y = "Frequency", title = "Frequency Plot of usage of Tobacco in percentage of both  (2020)") +
  theme(legend.position = "none") +
  facet_grid(. ~ sex) 

# Distribution plot of value by sex
ggplot(sex_combined_lastyear, aes(x = value, fill=sex)) +
  geom_density(color = "black", alpha = 0.5) +
  labs(x = "Value", y = "Density", title = "Distribution Plot of usage of Tobacco in percentage of both sex (2020)") +
  theme(text = element_text(size = 14), 
        plot.title = element_text(size = 14, hjust = 0.5), legend.position = "none") +
  facet_grid(. ~ sex)

# Boxplot of Tobacco usage by sex from 2000 to 2020
ggplot(sex_combined, aes(x = factor(sex_combined$sex), y = sex_combined$value, fill= factor(sex_combined$sex))) +
  geom_boxplot(color = "black", width = 0.5) +  
  labs(x = "sex",              
       y = "Tobacco use",           
       title = "Boxplot of Tobacco usage by sex from 2000 to 2020")+
  theme(legend.position = "none") +
  facet_grid(. ~ year) 

# Boxplot of Tobacco usage by sex in 2020
ggplot(sex_combined_lastyear, aes(y = value, x = sex, color = sex)) +
  geom_point(alpha = 0.5, position = position_jitter(width = 0.2)) +  
  labs(x = "Sex",              
       y = "Tobacco use",           
       title = "Scatterplot of Tobacco usage by sex in 2020") +
  theme(legend.position = "none")


# Boxplot of tobacco use in Geographic areas divided by sex

ggplot(sex_combined_lastyear, aes(x = factor(sex_combined_lastyear$geo_region), y = sex_combined_lastyear$value, fill= factor(sex_combined_lastyear$geo_region))) +
  geom_boxplot(color = "black", width = 0.5) +  
  labs(x = "Geographic Area",              
       y = "Tobacco use",           
       title = "Boxplot of Tobacco usage by Geographic Area in 2020 devide by Sex")+
  theme(text = element_text(size = 14), 
        plot.title = element_text(size = 14, hjust = 0.5), legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(. ~ sex) 

# Violinplot of tobacco use in Geographic areas divided by sex

ggplot(sex_combined_lastyear, aes(x = factor(sex_combined_lastyear$geo_region), y = sex_combined_lastyear$value, fill= factor(sex_combined_lastyear$geo_region))) +
  geom_violin(color = "black", width = 0.5) +
  labs(x = "Geographic Area",              
       y = "Tobacco use",           
       title = "Violinplot of Tobacco usage by Geographic Area in 2020 devide by Sex") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(. ~ sex)

# Calculate the mean tobacco use for each Area with sex

mean_by_area_sex_2020 <- sex_combined_lastyear %>%
  group_by(geo_region, sex) %>%
  summarise(MeanTobaccoUse = mean(value, na.rm = TRUE))

mean_by_area_sex_2020

# Bar plot of tobacco use in states respect to all country areas divided by sex
ggplot(mean_by_area_sex_2020, aes(x = factor(mean_by_area_sex_2020$geo_region ), y = mean_by_area_sex_2020$MeanTobaccoUse, fill= factor(mean_by_area_sex_2020$geo_region))) +
  geom_bar(stat = "identity") +
  labs(x = "Geographic Area",              
       y = "Tobacco use",           
       title = "Mean of Tobacco usage by Geographic Area in 2020 devide by Sex") +
  theme(text = element_text(size = 14), 
        plot.title = element_text(size = 14, hjust = 0.5), legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(. ~ sex)


# Calculate the mean tobacco use for each Area and Sex
mean_by_year_area_sex <- sex_combined %>%
  group_by(year, geo_region, sex) %>%
  summarise(MeanTobaccoUse = mean(value, na.rm = TRUE))

mean_by_year_area_sex

# Time Series of Mean Tobacco Use Percentage by Geographic Area divide by Sex
ggplot(mean_by_year_area_sex, aes(x = year, y = MeanTobaccoUse, group = geo_region, color = as.factor(geo_region))) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Mean Tobacco Use Percentage", title = "Time Series of Mean Tobacco Use Percentage by Geographic Area divide by Sex") +
  scale_color_discrete(name = "Geographic Area") +
  theme_minimal() +
  theme(text = element_text(size = 14), 
        plot.title = element_text(size = 14, hjust = 0.5), axis.text.x = element_text(angle = 35, hjust = 1)) +
  facet_grid(. ~ sex)


# Hypothesis: i want to discover if the mean of women/men using tobacco is changed over time
# in the last five recorded years

#---------------------------Normality Test Male---------------------------#

unique(male_cleaned$year)

male_tobacco_test <- male_cleaned[male_cleaned$year >= 2010, ]

# Male qqplot from years 2010 to 2020
qqnorm(male_tobacco_test$value[male_tobacco_test$year == "2010"], main = "Normal Q-Q Plot of 2010 Data over male")
qqline(male_tobacco_test$value[male_tobacco_test$year == "2010"], col = "red")
qqnorm(male_tobacco_test$value[male_tobacco_test$year == "2015"], main = "Normal Q-Q Plot of 2015 Data over male")
qqline(male_tobacco_test$value[male_tobacco_test$year == "2015"], col = "red")
qqnorm(male_tobacco_test$value[male_tobacco_test$year == "2018"], main = "Normal Q-Q Plot of 2018 Data over male")
qqline(male_tobacco_test$value[male_tobacco_test$year == "2018"], col = "red")
qqnorm(male_tobacco_test$value[male_tobacco_test$year == "2019"], main = "Normal Q-Q Plot of 2019 Data over male")
qqline(male_tobacco_test$value[male_tobacco_test$year == "2019"], col = "red")
qqnorm(male_tobacco_test$value[male_tobacco_test$year == "2020"], main = "Normal Q-Q Plot of 2020 Data over male")
qqline(male_tobacco_test$value[male_tobacco_test$year == "2020"], col = "red")


shapiro_test_male_2010 <- shapiro.test(male_tobacco_test$value[male_tobacco_test$year == "2010"])
shapiro_test_male_2015 <- shapiro.test(male_tobacco_test$value[male_tobacco_test$year == "2015"])
shapiro_test_male_2018 <- shapiro.test(male_tobacco_test$value[male_tobacco_test$year == "2018"])
shapiro_test_male_2019 <- shapiro.test(male_tobacco_test$value[male_tobacco_test$year == "2019"])
shapiro_test_male_2020 <- shapiro.test(male_tobacco_test$value[male_tobacco_test$year == "2020"])

print("verifying normality- Male 2010:")
print(shapiro_test_male_2010)
print("verifying normality- Male 2015:")
print(shapiro_test_male_2015)
print("verifying normality- Male 2018:")
print(shapiro_test_male_2018)
print("verifying normality- Male 2019:")
print(shapiro_test_male_2019)
print("verifying normality- Male 2020:")
print(shapiro_test_male_2020)

#---------------------------Homoscedasticity Test Male---------------------------#

male_homo_test <- bartlett.test(value ~ year, data = male_tobacco_test)
male_homo_test

#---------------------------Sphericity Test Male---------------------------#

male_tobacco_test$state <- as.factor(male_tobacco_test$state)
male_tobacco_test$year <- as.factor(male_tobacco_test$year)
summary(male_tobacco_test)
print(male_tobacco_test[male_tobacco_test$state == "Albania",])

tab_male_matrix <- matrix(male_tobacco_test$value, ncol=5, byrow=T,
                          dimnames=list(state=1:164, cond=c("2010","2015","2018","2019", "2020")))

S <- var(tab_male_matrix) # matrice di covarianza
J <- 5
numeratore <- J^2*mean(diag(S)-mean(S))^2
denominatore <- (J-1)*(sum(S^2)-2*J*sum(apply(S,1,mean)^2)+
                         +J^2*mean(S)^2)
epsilon <- numeratore/denominatore
epsilon
#------------Conclusion Male(Normality, Homoscedasticity and Sphericity)------------#
# Normality is not satisfied in Males groups(years)
# Homoscedasticity is satisfied
# Sphericity doesn't affect the results because as a low deviation

#---------------------------Hypothesis Test Male---------------------------#

# the normality is rejected so i'm using a non parametric test
friedman.test(male_tobacco_test$value, male_tobacco_test$year, male_tobacco_test$state)

# because i assume that the Q-Q plots seems to  be not so distant from normality
# and i assume that there are a large number of samples and the other conditions are satisfied
# i use a repited anova technique for a double check on results

male_analysis <- aov(male_tobacco_test$value ~ male_tobacco_test$year + Error(male_tobacco_test$state/male_tobacco_test$year))
summary(male_analysis)

# are there differences between states? 
#(TOASK ! rifiutando l'ipotesi nulla dell'anova e quindi sapendo che la distribuzione dei gruppi non è secondo la distribuzione F posso comunque calcolare la F_sub?)
MQ_sub <- 825.3
MQ_res <- 1.9
F_sub <- MQ_sub/MQ_res
F_sub
p_value <- 1 - pf(F_sub, 163, 656)
p_value
# why return 0 TODO !!

# null hypothesis is rejected so i do a post-hoc analysis 
# for check the differencies respect groups
pairwise.wilcox.test(male_tobacco_test$value, male_tobacco_test$year, paired=T,p.adjust.method = "bonferroni")
pairwise.wilcox.test(male_tobacco_test$value, male_tobacco_test$year,paired=T,p.adjust.method = "BH")

#---------------------------Normality Test Female---------------------------#

unique(female_cleaned$year)
female_tobacco_test <- female_cleaned[female_cleaned$year >= 2010, ]

# Female qqplot from years 2010 to 2020
qqnorm(female_tobacco_test$value[female_tobacco_test$year == "2010"], main = "Normal Q-Q Plot of 2010 Data over female")
qqline(female_tobacco_test$value[female_tobacco_test$year == "2010"], col = "green")
qqnorm(female_tobacco_test$value[female_tobacco_test$year == "2015"], main = "Normal Q-Q Plot of 2015 Data over female")
qqline(female_tobacco_test$value[female_tobacco_test$year == "2015"], col = "green")
qqnorm(female_tobacco_test$value[female_tobacco_test$year == "2018"], main = "Normal Q-Q Plot of 2018 Data over female")
qqline(female_tobacco_test$value[female_tobacco_test$year == "2018"], col = "green")
qqnorm(female_tobacco_test$value[female_tobacco_test$year == "2019"], main = "Normal Q-Q Plot of 2019 Data over female")
qqline(female_tobacco_test$value[female_tobacco_test$year == "2019"], col = "green")
qqnorm(female_tobacco_test$value[female_tobacco_test$year == "2020"], main = "Normal Q-Q Plot of 2020 Data over female")
qqline(female_tobacco_test$value[female_tobacco_test$year == "2020"], col = "green")

shapiro_test_female_2010 <- shapiro.test(female_tobacco_test$value[female_tobacco_test$year == "2010"])
shapiro_test_female_2015 <- shapiro.test(female_tobacco_test$value[female_tobacco_test$year == "2015"])
shapiro_test_female_2018 <- shapiro.test(female_tobacco_test$value[female_tobacco_test$year == "2018"])
shapiro_test_female_2019 <- shapiro.test(female_tobacco_test$value[female_tobacco_test$year == "2019"])
shapiro_test_female_2020 <- shapiro.test(female_tobacco_test$value[female_tobacco_test$year == "2020"])

print("verifying normality- Female 2010:")
print(shapiro_test_female_2010)
print("verifying normality- Female 2015:")
print(shapiro_test_female_2015)
print("verifying normality- Female 2018:")
print(shapiro_test_female_2018)
print("verifying normality- Female 2019:")
print(shapiro_test_female_2019)
print("verifying normality- Female 2020:")
print(shapiro_test_female_2020)


#---------------------------Homoscedasticity Test Female---------------------------#

female_homo_test <- bartlett.test(value ~ year, data = female_tobacco_test)
female_homo_test

#---------------------------Sphericity Test Female---------------------------#

female_tobacco_test$state <- as.factor(female_tobacco_test$state)
female_tobacco_test$year <- as.factor(female_tobacco_test$year)
summary(female_tobacco_test)
print(female_tobacco_test[female_tobacco_test$state == "Albania",])

tab_female_matrix <- matrix(female_tobacco_test$value, ncol=5, byrow=T,
                          dimnames=list(state=1:164, cond=c("2010","2015","2018","2019", "2020")))

S <- var(tab_female_matrix) # matrice di covarianza
J <- 5
numeratore <- J^2*mean(diag(S)-mean(S))^2
denominatore <- (J-1)*(sum(S^2)-2*J*sum(apply(S,1,mean)^2)+
                         +J^2*mean(S)^2)
epsilon <- numeratore/denominatore
epsilon

#------------Conclusion Female (Normality, Homoscedasticity and Sphericity)------------#
# Normality is not satisfied in Females groups(years)
# Homoscedasticity is satisfied
# Sphericity doesn't affect the results because as a low deviation


#---------------------------Hypothesis Test Female---------------------------#

# the normality is rejected so i'm using a non parametric test
friedman.test(female_tobacco_test$value, female_tobacco_test$year, female_tobacco_test$state)

# null hypothesis is rejected so i do a post-hoc analysis 
# for check the differencies respect groups
pairwise.wilcox.test(female_tobacco_test$value, female_tobacco_test$year, paired=T,p.adjust.method = "bonferroni")
pairwise.wilcox.test(female_tobacco_test$value, female_tobacco_test$year,paired=T,p.adjust.method = "BH")
