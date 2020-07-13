###############################################################################################################################################################################################################################################################################################################################################################
# Author: Isuru Abeysekara
# Role: Data Analyst Intern
# R script that cleaned datasets. The code has been packaged into a function to ensure reproducability. The function includes variables that I had to pick through visual inspection because there was no way to write a program to pick out the variables in the script.
#
###############################################################################################################################################################################################################################################################################################################################################################

#Filenames.

filename1= 'data/cases.csv';
filename2= 'data/assistance.csv';

#Function takes in two parameters: The file names above. It returns the structure of the cleaned dataset for the user to ensure that the dataset has been cleaned successfully.

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
  file_n$State <- fct_collapse(file_n$State, 
                                     VT= c("vt","vT","Vt","VT" ),
                                     NH= c("nh","NH")); #Streamlining factor levels entered incorrectly.
  
  
  # Splitting income and expense sources to allow for more in-depth data analysis.
  file_n$Personal.Income.Split <- strsplit(file_n$Case.Personal.Income.Sources, "; |  - ");
  file_n$Case.Personal.Expense.Sources.Split <- strsplit(file_n$Case.Personal.Expense.Sources, ";");
  
  # Returns structure of dataset. Allows for debugging.
  str(file_n);
}

# Calling function below.
file_new <- readingFile(filename1 = filename1, filename2 = filename2)
