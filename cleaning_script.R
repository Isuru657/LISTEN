  library(dplyr)
  library(readr)
  library(tidyverse)

  setwd("~/Desktop/LISTEN/")
  filename1= 'data/cases.csv';
  filename2= 'data/assistance.csv';
  readingFile <- function(filename1, filename2){
    print("Reading file one");
    file= read.csv(file=filename1, header=TRUE, stringsAsFactors = FALSE, skip=19);
    print("Read complete");
    file= subset(file, select= -c(Entry.Agent, Entry.Agency, First.Name, Middle.Name, Last.Name, Suffix, Date.of.Birth, SS.Number, Street.Address, Street.Apt.Number, City, State, Zip.code, County, Case.Personal.income, Case.Personal.Income.Sources, Household.Income, Case.Personal.expenses, Case.Personal.Expense.Sources, Household.Expenses))
    print("Reading file two");
    file2= read.csv(file=filename2, header=TRUE, stringsAsFactors = FALSE, skip=3);
    print("Read complete");
    file2= subset(file2, select= -c(Agent.Name, Agency.Name));
    print("Merging files together");
    file_ultra= left_join(file, file2, by= c("Case.."="Case.."))
  }
  file_ultra= left_join(file, file2, by= c("Case.."="Case..")) 
  file_ultra <- readingFile(filename1 = filename1, filename2 = filename2)
  colnames(file_ultra) [1] <- "Case.Number"
  tail(file_ultra)
  "Removing test cases"
  file_ultra <- file_ultra %>%
    filter(!file_ultra$First.Name=="test") 
  file_ultra <-  file_ultra %>%
    filter(!file_ultra$Last.Name=="")
  file_ultra <- file_ultra %>%
    filter(!file_ultra$First.Name=="")
  file_ultra$Case.Number <- as.factor(file_ultra$Case.Number)
  file_ultra$State <- as.factor(file_ultra$State)
  file_ultra$State_2 <- fct_collapse(file_ultra$State, 
                                     VT= c("vt","vT","Vt","VT" ),
                                     NH= c("nh","NH"))
  
  file_ultra$y <- strsplit(file_ultra$Case.Personal.Income.Sources, "; |  - ")
  file_ultra$Case.Personal.Expense.Sources <- strsplit(file_ultra$Case.Personal.Expense.Sources, ";")
  str(file_ultra)
  file_ultra$Household.. <- as.factor(file_ultra$Household..)
  file_ultra$Entry.Date.x <- as.Date(file_ultra$Entry.Date.x)
  file_ultra$Last.Assistance.Date<-  as.Date(file_ultra$Last.Assistance.Date)
  file_ultra$Date.of.Birth <- as.Date(file_ultra$Date.of.Birth)
  file_ultra$County <- strsplit(file_ultra$County, ", ")
  file_ultra$County <- file_ultra$County[1]
  today <- as.Date("2020-07-09")
  age_days <- today - file_ultra$Date.of.Birth
  file_ultra$age <- age_days/365
  file_ultra$City <- as.factor(file_ultra$City)
  file_ultra$Zip.code <- as.factor(file_ultra$Zip.code)
  
  cols <- -c(22,24,27, 55);
  str(file_ultra[, cols])
  write.csv(file_ultra[, cols], "C:\\Users\\isuruabeysekara\\Desktop\\listen_combined.csv", row.names= FALSE)

  
  file_new <- readingFile(filename1 = filename1, filename2 = filename2)
  