library(tidyverse)

hr_clean<- read.csv("data/processed/hr_clean.csv")

#chisq test
overtime_table <- table(
  hr_clean$OverTime,
  hr_clean$Attrition
)

overtime_table

chisq.test(overtime_table)
#simpan hasil chi
overtime_chi<- chisq.test(overtime_table)

overtime_chi

department_table <- table(
  hr_clean$Department,
  hr_clean$Attrition
)

department_chi<- chisq.test(department_table)

department_chi

jobrole_table <- table(
  hr_clean$JobRole,
  hr_clean$Attrition
)

jobrole_chi<- chisq.test(jobrole_table)

jobrole_chi

marital_table <- table(
  hr_clean$MaritalStatus,
  hr_clean$Attrition
)

marital_chi<- chisq.test(marital_table)

marital_chi

#Ttest numerical
age_test<- t.test(
  Age ~ Attrition,
  data = hr_clean
)

age_test

income_test<- t.test(
  MonthlyIncome ~ Attrition,
  data = hr_clean
)

income_test

tenure_test<- t.test(
  YearsAtCompany ~ Attrition,
  data = hr_clean
)

tenure_test

workingyears_test<- t.test(
  TotalWorkingYears ~ Attrition,
  data = hr_clean
)

workingyears_test

distance_test<- t.test(
  DistanceFromHome ~ Attrition,
  data = hr_clean
)

distance_test
