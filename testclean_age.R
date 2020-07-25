###############################################################################################################################################################################################################################################################################################################################################################
# Author: Isuru Abeysekara
# Role: Data Analyst Intern
# R script that cleaned datasets. The code has been packaged into a function to ensure reproducability. The function includes variables that I had to pick through visual inspection because there was no way to write a program to pick out the variables in the script.
#
###############################################################################################################################################################################################################################################################################################################################################################
library(dplyr)
library(tidyverse)
library(sjmisc)

#Filenames.

filename1= 'data/cases.csv';
filename2= 'data/assistance.csv';

# Function takes in two parameters: The file names above. It returns the structure of the cleaned dataset for the user to ensure that the dataset has been cleaned successfully.
# It also saves the dataset in the current working directory.

readingFile <- function(filename1, filename2){
  
  file_one= read.csv(file=filename1, header=TRUE, stringsAsFactors = FALSE, skip=19);
  
  # Variables ommitted here- mainly because some are unused and some would be repeated once the datasets merge.
  
  file_one= subset(file_one, select= -c(Entry.Agent, Entry.Agency, First.Name, Middle.Name, Last.Name, Suffix, Date.of.Birth, SS.Number, Street.Address, Street.Apt.Number, City, State, Zip.code, County, Case.Personal.income, Case.Personal.Income.Sources, Household.Income, Case.Personal.expenses, Case.Personal.Expense.Sources, Household.Expenses))
  file_two= read.csv(file=filename2, header=TRUE, stringsAsFactors = FALSE, skip=3);
  file_two= subset(file_two, select= -c(Agent.Name, Agency.Name));
  
  # Datasets combined by mapping the two unique case id variables.
  
  file_n= left_join(file_one, file_two, by= c("Case.."="Case.."));
  
  
  # Renaming variables.
  
  colnames(file_n[1]) <- "Case.Number";
  
  # Removing non-meaningful rows in the dataset.
  
  file_n <- file_n %>%
    filter(!file_n$First.Name=="test");
  file_n <-  file_n %>%
    filter(!file_n$Last.Name=="");
  file_n <- file_n %>%
    filter(!file_n$First.Name=="");
  
  # Changing datatypes of key variables.
  
  file_n$Case.. <- as.factor(file_n$Case..);
  file_n$State <- as.factor(file_n$State);
  file_n$Household.. <- as.factor(file_n$Household..);
  file_n$Entry.Date.x <- as.Date(file_n$Entry.Date.x);
  file_n$Last.Assistance.Date<-  as.Date(file_n$Last.Assistance.Date);
  file_n$Date.of.Birth <- as.Date(file_n$Date.of.Birth);
  file_n$City <- as.factor(file_n$City);
  file_n$Zip.code <- as.factor(file_n$Zip.code);
  file_n$Assistance.Category <- as.factor(file_n$Assistance.Category);
  
  file_n$State <- fct_collapse(file_n$State, 
                                     VT= c("vt","vT","Vt","VT" ),
                                     NH= c("nh","NH")); #Streamlining factor levels entered incorrectly.
  
  
  # Splitting income and expense sources to allow for more in-depth data analysis.
  
  file_n$Personal.Income.Split <- strsplit(file_n$Case.Personal.Income.Sources, "; |  - ");
  file_n$Case.Personal.Expense.Sources.Split <- strsplit(file_n$Case.Personal.Expense.Sources, ";");
  
  
  file_n$County <- strsplit(file_n$County, ", ")
  file_n$County <- file_n$County[1]
  
  #Calculating Date of Birth of clients
  
  today <- as.Date("2020-07-09");
  age_days <- today - file_n$Date.of.Birth;
  file_n$age <- age_days/365;
  
  # Grouping ages into a categoric variable for data analysis
  
  file_n$age_cat <- cut(as.numeric(file_n$age), c(0,19,29,39,49,59,69,79,89), c("0-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"));
  
  #Omitting split income and expenses sources variables
  
  omit_cols <- -c(55, 54);
  
  #Writing file in working directory
  
  write.csv(file_n[omit_cols], "listen_combined.csv", row.names= FALSE);
  
  # Returns structure of dataset. Allows for debugging.
  
  str(file_n);
}

regroupAge <- function (num, data, new_num){
  if (num<20) {data$agecat}
}

#Splitting the expense variable
y <- strsplit(file_n$Case.Personal.Expense.Sources, " - ")



ly <- length(y)
lx <- length(y[[1]])

source <- vector(mode="character", length=ly)
amount <- vector(mode="character", length=ly)
freq <- vector(mode="character", length=ly)

for (i in 1:ly){
    source[i]=y[[i]][1]
    amount[i]=y[[i]][2]
    freq[i]=y[[i]][3]
  }

freq <- str_replace(freq, ";", "")

file_n$expense_source <- source
file_n$expense_amount <- amount
file_n$expense_freq <- freq

#Splitting the income variable
z <- strsplit(file_n$Case.Personal.Income.Sources, " - ")
source <- vector(mode="character", length=ly)
amount <- vector(mode="character", length=ly)
freq <- vector(mode="character", length=ly)

for (i in 1:ly){
  source[i]=z[[i]][1]
  amount[i]=z[[i]][2]
  freq[i]=z[[i]][3]
}

freq <- str_replace(freq, ";", "")

file_n$income_source <- source
file_n$income_amount <- amount
file_n$income_freq <- freq
file_n$expense_amount <- as.numeric(file_n$expense_amount)

amt <- vector(mode="numeric", length=ly)
for (i in 1:ly){
  if (str_contains(file_n$expense_freq[i], "Weekly")){
    amt[i]=as.numeric(file_n$expense_amount[i])*4
  }
  else if (str_contains(file_n$expense_freq[i], "Monthly")){
    amt[i]=as.numeric(file_n$expense_amount[i])*1
  }
  else if (str_contains(file_n$expense_freq[i], "Biweekly")){
    amt[i]=as.numeric(file_n$expense_amount[i])*2
  }
}




file_n$expense_amount_monthly <- amt
head(file_n)

# Calling function below.
file_new <- readingFile(filename1 = filename1, filename2 = filename2)
