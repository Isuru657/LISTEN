install.packages("ggplot2")
library(ggplot2)

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

str(file_one)

file_one%>%
  group_by(income_cat)%>%
  count(income_cat)%>%
  ggplot(aes(x=income_cat, y=n))+ geom_col()

file_one%>%
  group_by(County)%>%
  ggplot(aes(x=County, y=Case.Personal.income, fill=County))+
  geom_boxplot()+ coord_cartesian(ylim=c(0, 20000))

file_one%>%
  ggplot(aes(x=Case.Personal.income, y=))

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

#Analysis goals
#Basic income distribution
file_one%>%
  group_by(income_cat)%>%
  count(income_cat)%>%
  ggplot(aes(x=income_cat, y=n))+ geom_col()

#Income distribution per county
file_one%>%
  group_by(County)%>%
  ggplot(aes(x=County, y=Case.Personal.income, fill=County))+
  geom_boxplot()+ coord_cartesian(ylim=c(0, 20000))

file_one%>%
  group_by(County)%>%
  summarize(med_income= median(Case.Personal.income, na.rm=TRUE))

# Median income is zero for all counties
file_one%>%
  filter(Case.Personal.income==0)%>%
  group_by(County)%>%
  count(County)
  

file_one%>%
  group_by(County)%>%
  summarize(mean_income= mean(Case.Personal.income, na.rm=TRUE))

#Age distribution
file_one%>%
  group_by(County)%>%
  ggplot(aes(x=County, y=age, fill=County))+
  geom_boxplot()

file_one%>%
  group_by(County)%>%
  summarize(med_age= median(age, na.rm=TRUE))
file_one%>%
  group_by(County)%>%
  summarize(mean_age= mean(age, na.rm=TRUE))

file_one%>%
  group_by(County)%>%
  count(age_cat)

#No of families served comes from the assistance data file

z <- strsplit(file_one$Case.Personal.Expense.Sources, " - ")
lz <- length(z)
source_case <- vector(mode="character", length=lz)
amount_case <- vector(mode="character", length=lz)
freq_case <- vector(mode="character", length=lz)



for (i in 1:lz){
  source_case[i]=z[[i]][1]
  amount_case[i]=z[[i]][2]
  freq_case[i]=z[[i]][3]
}

freq_case <- str_replace(freq_case, ";", "")
file_one$expense_source <- source_case
file_one$expense_amount <- as.numeric(amount_case)
file_one$expense_freq <- freq_case

str(file_one)

amt <- vector(mode="numeric", length=lz)
for (i in 1:lz){
  if (str_contains(file_one$expense_freq[i], "Weekly")){
    amt[i]=(file_one$expense_amount[i])*4
  }
  else if (str_contains(file_one$expense_freq[i], "Monthly")){
    amt[i]=(file_one$expense_amount[i])*1
  }
  else if (str_contains(file_one$expense_freq[i], "Biweekly")){
    amt[i]=(file_one$expense_amount[i])*2
  }
}

# Rent
file_one$expense_amount_monthly <- amt
file_one%>%
  filter(str_contains(file_one$expense_source, "Rent"))%>%
  ggplot(aes(x=County, y=expense_amount_monthly, fill=County))+
  geom_boxplot()

pays_rent <- vector(mode="character", length=lz)
for (i in 1:lz){
  if (str_contains(file_one$expense_source[i], "Rent"))
    {
    pays_rent[i]=TRUE;
  }
  else
    {
    pays_rent[i]=FALSE;
  }
}
pays_rent <- as.factor(pays_rent)
file_one$pays_rent <- pays_rent

#How many pay rent
file_one%>%
  group_by(pays_rent)%>%
  count(pays_rent)
file_one%>%
  filter(pays_rent=="TRUE")%>%
  ggplot(aes(x=County, y=expense_amount_monthly, fill=County))+
  geom_boxplot()

#Geographic distribution of rent
file_one%>%
  filter(pays_rent=="TRUE")%>%
  group_by(County)%>%
  summarize(med_rent= median(expense_amount_monthly, na.rm= TRUE))

file_one%>%
  filter(pays_rent=="TRUE")%>%
  group_by(County)%>%
  summarize(mean_rent= mean(expense_amount_monthly, na.rm= TRUE))

#Comparing monthly income vs rent
#How much of income is rent?

#How much rent does a citizen in the Upper Valley pay?

## Mean rent
file_one%>%
  filter(pays_rent=="TRUE")%>%
  summarize(mean_rent= mean(expense_amount_monthly, na.rm= TRUE))

## Median rent
file_one%>%
  filter(pays_rent=="TRUE")%>%
  summarize(median_rent= median(expense_amount_monthly, na.rm= TRUE))

## Rent histogram
file_one%>%
  filter(pays_rent=="TRUE")%>%
  ggplot(aes(x=expense_amount_monthly))+
  geom_histogram()


## After treating extreme values

## Mean income
file_one%>%
  filter(pays_rent=="TRUE")%>%
  filter(expense_amount_monthly<2000)%>%
  summarize(mean_rent= mean(expense_amount_monthly, na.rm= TRUE))

## Median income
file_one%>%
  filter(pays_rent=="TRUE")%>%
  filter(expense_amount_monthly<2000)%>%
  summarize(median_rent= median(expense_amount_monthly, na.rm= TRUE))

## Getting bins of people that fall into different rent categories

rent_table <- file_one%>%
  filter(pays_rent=="TRUE")%>%
  select(Case.., expense_amount_monthly, County)


rent_table$rent_cat <- cut(rent_table$expense_amount_monthly, c(0, 499, 999, 1499, 1999, 2499, 2999, 3499, Inf), c("0-499", "500-999", "1000- 1499", "1500-1999", "20000-2499", "2500-2999", "3000-3499", "3500 to Inf"))
#Rent counts table
rent_table%>%
  group_by(rent_cat)%>%
  count(rent_cat)

#Rent counts by county
rent_table%>%
  group_by(County, rent_cat)%>%
  count(rent_cat)%>%
  ggplot(aes(x=rent_cat, y=n, fill=County))+
  geom_col()






