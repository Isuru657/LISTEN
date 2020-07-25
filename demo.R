###############################################################################################################################################################################################################################################################################################################################################################
# Author: Isuru Abeysekara
# Role: Data Analyst Intern
# R script that reads and analyzes the demographics of clients from the Cases dataset.
#
###############################################################################################################################################################################################################################################################################################################################################################

demo <- read.csv(file.choose(), skip=17)
str(demo)

gender <- demo[1:3, 1:14]

race <- demo[4:11, 1:14]

health_insurance <- demo[22:23, 1:14]

car <- demo[23:24, 1:14]

vet <- demo[27:28, 1:14]



gender <- gender%>%
  gather(key="Gender", value="Values")

race <- race%>%
  gather(key="Race", value="Values")

vet <- vet%>%
  gather(key="Veteran status", value="Values")

health_insurance <- health_insurance%>%
  gather(key="Insurance status", value="Values")

senior <- file_one%>%
            filter(age>65)

senior%>%
  group_by(County)%>%
  count(County)

senior_tot <- dim(senior)[1]

senior_household <- file_one%>%
  filter(age>65)%>%
  distinct(Household..)
  
household <- file_one%>%
  distinct(Household..)

senior_household_tot <- dim(senior_household)[1]

file_one%>%
  filter(Household.Size==1)%>%
  filter(age>65)%>%
  count(Case..)%>%
  summarize(tot=sum(n, na.rm= TRUE))

household_tot <- dim(household)[1]



child_household <- file_one%>%
  filter(age<18)%>%
  distinct(Household..)

child_household_tot <- dim(child_household)[1]





  
