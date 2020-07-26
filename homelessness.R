library(openxlsx)
homelessness_1 <- read.xlsx(file.choose(), sheet = 2, startRow = 1, colNames=TRUE)
?read.xlsx
homelessness_1
homelessness_2 <- read.xlsx(file.choose(), sheet= 3, startRow = 1)
str(homelessness_1)
str(homelessness_2)
homeless <- rbind(homelessness_1, homelessness_2)
homeless$Date <- convertToDate(homeless$Date)

homeless%>%
  group_by(HUD.Definition.of.Homelessness)%>%
  count(HUD.Definition.of.Homelessness)%>%
  arrange(-n)