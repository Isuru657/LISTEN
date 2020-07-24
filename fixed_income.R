library(stringr)

###############################################################################################################################################################################################################################################################################################################################################################
# Author: Isuru Abeysekara
# Role: Data Analyst Intern
# R script that reads and analyzes the fixed incomes of clients from the Cases dataset.
#
###############################################################################################################################################################################################################################################################################################################################################################

source <- vector(mode="character", length=ly)
amount <- vector(mode="character", length=ly)
freq <- vector(mode="character", length=ly)

z <- strsplit(file_one$Case.Personal.Income.Sources, " - ")
lz <- length(z)
source_case <- vector(mode="character", length=lz)
amount_case <- vector(mode="character", length=lz)
freq_case <- vector(mode="character", length=lz)

for (i in 1:lz){
  source_case[i]=z[[i]][1]
  amount_case[i]=z[[i]][2]
  freq_case[i]=z[[i]][3]
}

file_one$income_source <- source_case
file_one$income_amt <- amount_case
file_one$income_freq <- freq_case
file_one$income_amt <- as.numeric(file_one$income_amt)

food_stamps <- c("food stamps", "Food stamps", "Food Stamps", "food stamps ", "Food stamps ", "Food stamps/ general assistance")
levels(as.factor(source_case))
levels(as.factor(freq_case))
levels(as.factor(amount_case))

social_security <- c("Social Secuity", "social security", "Social Security", "social security ", "Social security ", "Social Security ", "Social Security (Barb 800 + Husband1200)", "Social Security (Pending)", "Social Security & part-time (CCREL 4-8 hrs/wk)", "Social Security & VA",  "Social security husband ", "Social Security x2", "Son's SSI","ss","SS","SS (husband) and DHMC","SS & Petion","SS and Pension","SS Survivor Benefits","SS-Children","SS-Joyce/Perry","SS-Kids Receive","SS/DHMC","SS/Pension"
                     ,"SSI","SSI ","SSI (653)/child support ($500)","SSI (daughter's from father)","SSI (for disabled daughter)","SSI (mom and kids)","SSI (Tonya)","SSI & Early Retirement","SSI & Reach Up","SSI + TANF","SSI 3 payments","ssi edward","SSI kids ","SSI Retirement Funds","SSI-Denise, SS-John","SSI?","SSI/ from grandchilder","SSI/ Payee source ","SSI/Ron and Jeremy","SSI/VA" )

disability <- c("SSD","ssdi","SSDI","ssdi ","SSDI ","SSDI (& daughter)","SSDI (husband)","SSDI (Kids & Ben)","SSDI (Medicare) -Husband","SSDI (recently lost due to part time job cleaning)"
                ,"SSDI (Son-Dakota)","SSDI (Starting Feb. 3rd)","SSDI & Construction (Partner)","SSDI and SSI","SSDI with Chloe","SSDI-Clyde","SSDI-Daniel","SSDI-Karin and Richard","SSDI-Voc Rehab","SSDI/AD","SSDI/Part-time work","SSDI/PT at T.J. Maxx"                              
                , "Kohl's/SSD", "Short Term Disability", "Short term disability", "Social security disability ", "Social Security Disability ",  "son and self disability")

ss_ssd <- c("SSI/SSDI","SSI/SSDI (fiance)", "SSDI/SSI", "Social Security Disability + food stamps", "SSI and SSDI")
fixed_income <- vector(mode="character", length=lz)
fixed_income_type <- vector(mode="character", length=lz)

len_ss <- length(social_security)

for (i in 1:lz){
  for (j in 1:len_ss){
    if (str_contains(file_one$income_source[i], social_security[j])){
      fixed_income[i]= "TRUE"
      fixed_income_type[i]="SSI"
    }
  }
}

len_d <- length(disability)

for (i in 1:lz){
  for (j in 1:len_d){
    if (str_contains(file_one$income_source[i], disability[j])){
      fixed_income[i]= "TRUE"
      fixed_income_type[i]="SSDI"
    }
  }
}

len_ssd <- length(ss_ssd)

for (i in 1:lz){
  for (j in 1:len_ssd){
    if (str_contains(file_one$income_source[i], ss_ssd[j])){
      fixed_income[i]= "TRUE"
      fixed_income_type[i]="SS/SSDI"
    }
  }
}

file_one$fixed_income <- fixed_income

file_one$fixed_income_type <- fixed_income_type


file_one$fixed_income <- as.factor(file_one$fixed_income)
levels(file_one$fixed_income)[1]="FALSE"
file_one$fixed_income_type <- as.factor(file_one$fixed_income_type)
levels(file_one$fixed_income_type)[1]="NA"

#How many people are on a fixed income?

file_one%>%
  group_by(fixed_income)%>%
  count(fixed_income)

file_one%>%
  group_by(fixed_income_type)%>%
  count(fixed_income_type)

#Geographic distribution of fixed income
file_one%>%
  group_by(County, fixed_income)%>%
  count(fixed_income)

file_one%>%
  group_by(County, fixed_income)%>%
  filter(fixed_income!="FALSE")%>%
  count(fixed_income)%>%
  ggplot(aes(x=County, y=n, fill=County))+
  geom_col()

file_one%>%
  group_by(County, fixed_income_type)%>%
  count(fixed_income)%>%
  

file_one%>%
  group_by(County, fixed_income_type)%>%
  filter(fixed_income_type!="NA")%>%
  count(fixed_income)%>%
  ggplot(aes(x=County, y=n, fill=fixed_income_type))+
  geom_col()



