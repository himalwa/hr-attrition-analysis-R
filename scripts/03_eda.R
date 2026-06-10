library(tidyverse)

hr_clean<-read.csv("data/processed/hr_clean.csv")

#rateoverallattrition
attrition_overall<- hr_clean %>%
  count(Attrition) %>%
  mutate(
    Percentage = round(n / sum(n) *100,2)
  )
attrition_overall

#rateovertimeattrition
attrition_overtime<- hr_clean %>%
  group_by(OverTime) %>%
  summarise(
    Employees = n(),
    AttritionCount = sum(Attrition == "Yes"),
    AttritionRate = round(AttritionCount / Employees *100,2)
  ) %>%
  arrange(desc(AttritionRate))

attrition_overtime

#bydepartment
attrition_department<- hr_clean %>%
  group_by(Department) %>%
  summarise(
    Employees = n(),
    AttritionCount = sum(Attrition == "Yes"),
    AttritionRate = round(AttritionCount / Employees *100,2)
  ) %>%
  arrange(desc(AttritionRate))

attrition_department

#byjobrole
attrition_jobrole<- hr_clean %>%
  group_by(JobRole) %>%
  summarise(
    Employees = n(),
    AttritionCount = sum(Attrition == "Yes"),
    AttritionRate = round(AttritionCount / Employees *100,2)
  ) %>%
  arrange(desc(AttritionRate))

attrition_jobrole

#byagegroup
attrition_agegroup<- hr_clean %>%
  group_by(AgeGroup) %>%
  summarise(
    Employees = n(),
    AttritionCount = sum(Attrition == "Yes"),
    AttritionRate = round(AttritionCount / Employees *100,2)
  ) %>%
  arrange(desc(AttritionRate))

attrition_agegroup

#bymaritalstatus
attrition_marital<- hr_clean %>%
  group_by(MaritalStatus) %>%
  summarise(
    Employees = n(),
    AttritionCount = sum(Attrition == "Yes"),
    AttritionRate = round(AttritionCount / Employees *100,2)
  ) %>%
  arrange(desc(AttritionRate))

attrition_marital

#numericsummaryattrition
numeric_summary<- hr_clean %>%
  group_by(Attrition) %>%
  summarise(
    AvgAge = round(mean(Age), 2),
    AvgMonthlyIncome = round(mean(MonthlyIncome), 2),
    AvgYearsAtCompany = round(mean(YearsAtCompany), 2),
    AvgTotalWorkingYears = round(mean(TotalWorkingYears), 2),
    AvgDistanceFromHome = round(mean(DistanceFromHome), 2)
  )
numeric_summary
