library(lubridate)

#Most popular services (top 5)
service_dat <- file_n%>%
  group_by(Assistance.Category)%>%
  count(Assistance.Category)%>%
  arrange(-n)

service_levels <- service_dat[1:10, 1]
service_levels<- as.character(service_levels)
class(service_levels)

# Most popular services based on age
file_n%>%
  group_by(age_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(age_cat=="0-19")%>%
  arrange(-n)

file_n%>%
  group_by(age_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(age_cat=="20-29")%>%
  arrange(-n)

file_n%>%
  group_by(age_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(age_cat=="30-39")%>%
  arrange(-n)

file_n%>%
  group_by(age_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(age_cat=="40-49")%>%
  arrange(-n)

file_n%>%
  group_by(age_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(age_cat=="50-59")%>%
  arrange(-n)

file_n%>%
  group_by(age_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(age_cat=="60-69")%>%
  arrange(-n)

file_n%>%
  group_by(age_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(age_cat=="70-79")%>%
  arrange(-n)

file_n%>%
  group_by(age_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(age_cat=="80-89")%>%
  arrange(-n)

#Most popular services based on income category

file_n%>%
  group_by(income_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(income_cat=="0-4999")%>%
  arrange(-n)

file_n%>%
  group_by(income_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(income_cat=="5000-9999")%>%
  arrange(-n)

file_n%>%
  group_by(income_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(income_cat=="10000-14999")%>%
  arrange(-n)

file_n%>%
  group_by(income_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(income_cat=="20000-29999")%>%
  arrange(-n)

file_n%>%
  group_by(income_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(income_cat=="30000-39999")%>%
  arrange(-n)

file_n%>%
  group_by(income_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(income_cat=="40000-49999")%>%
  arrange(-n)

file_n%>%
  group_by(income_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(income_cat=="50000-59999")%>%
  arrange(-n)

file_n%>%
  group_by(income_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(income_cat=="60000-69999")%>%
  arrange(-n)

file_n%>%
  group_by(income_cat, Assistance.Category)%>%
  count(Assistance.Category)%>%
  filter(income_cat=="70000 to Inf")%>%
  arrange(-n)

#Counts per month
file_n%>%
  group_by(month(Assistance.Date))%>%
  count(Assistance.Date)%>%
  summarize(sum_month= sum(n, na.rm= TRUE))

file_n$Assistance_month <- month(file_n$Assistance.Date)

#Seasonal fluctuations
file_n%>%
  group_by(Assistance_month)%>%
  count(Assistance_month)%>%
  ggplot(aes(x=Assistance_month, y=n))+
  geom_col()

file_n%>%
  filter(Case.Personal.income==0)%>%
  group_by(Assistance_month)%>%
  count(Assistance_month)

#Rearrange for early months of June etc
file_n%>%
  filter(Case.Personal.income==0)%>%
  group_by(Assistance_month)%>%
  count(Assistance_month)%>%
  ggplot(aes(x=Assistance_month, y=n))+
  geom_col()
 
#What services go together? Not informative 
file_n%>%
  group_by(Assistance.Date)%>%
  count(Assistance.Category)%>%
  filter(n>1)%>%
  arrange(-n)


