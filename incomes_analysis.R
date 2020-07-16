library(ggplot)

###############################################################################################################################################################################################################################################################################################################################################################
# Author: Isuru Abeysekara
# Role: Data Analyst Intern
# R script that reads and analyzes the incomes of clients from the Cases dataset.
#
###############################################################################################################################################################################################################################################################################################################################################################


# Reading in cases dataset 
file_one= read.csv(file=filename1, header=TRUE, stringsAsFactors = FALSE, skip=19);

#Performing basic analysis to understand the context behind client's incomes
mean(file_one$Case.Personal.income)

#Basic histogram to inspect income distributions visually

file_one%>%
  ggplot(aes(x=Case.Personal.income))+ geom_histogram()+ scale_x_continuous(breaks=c(0,4999, 9999,14999,19999, 29999, 39999, 49999, 59999, 79999), limits=c(0, 39999))+
  geom_density()

#Grouping incomes together
file_one$income_cat <- cut(file_one$Case.Personal.income, c(-1, 4999, 9999, 14999, 19999, 29999, 39999, 49999, 59999, 69999, Inf), c("0-4999", "5000-9999", "10000- 14999", "15000-19999", "20000-29999", "30000-39999", "40000-49999", "50000-59999", "60000-69999", "70000 to Inf" ))



#Adding age and age category variables 

today <- as.Date("2020-07-09");
age_days <- today - as.Date(file_one$Date.of.Birth);
file_one$age <- age_days/365;

file_one$age_cat <- cut(as.numeric(file_one$age), c(0,19,29,39,49,59,69,79,89), c("0-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"));

# Checking median incomes per age group -- Interesting results here for age groups of 20-29 and 40-49 
file_one%>%
  group_by(age_cat)%>%
  summarize(med_inc= median(Case.Personal.income, na.rm= TRUE))

file_one%>%
  group_by(as.factor(County))%>%
  summarize(med_inc= median(Case.Personal.income, na.rm= TRUE))

# Median incomes are zero across all counties. Check what percentage of people have 0 in these counties-- will hint at the level of unemployment

file_one%>%
  group_by(as.factor(County))%>%
  summarize(mean_inc= mean(Case.Personal.income, na.rm= TRUE))%>%
  arrange(-mean_inc)


file_n$income_cat <- cut(file_n$Case.Personal.income, c(-1, 4999, 9999, 14999, 19999, 29999, 39999, 49999, 59999, 69999, Inf), c("0-4999", "5000-9999", "10000- 14999", "15000-19999", "20000-29999", "30000-39999", "40000-49999", "50000-59999", "60000-69999", "70000 to Inf" ))


#Best performing services by income
file_n%>%
  group_by(income_cat)%>%
  count(Assistance.Category)%>%
  arrange(-n)

#Worst performing services by income
file_n%>%
  group_by(income_cat)%>%
  count(Assistance.Category)%>%
  arrange(n)


# Research questions for the future
# How is age correlated to income?
# How good is housing/lodging services? Who uses it the most? Look more into thrift store vouches- how can we check its value?



