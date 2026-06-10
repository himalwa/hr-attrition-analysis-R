library(tidyverse)

hr_clean<- read.csv("data/processed/hr_clean.csv")

#v1
attrition_overall<- hr_clean %>%
  count(Attrition)

plot_attrition<- ggplot(
  attrition_overall,
  aes(
    x = Attrition,
    y = n,
    fill = Attrition
  )
)+
  geom_col()+
  geom_text(
    aes(label = n),
    vjust = -0.3
  )+
  labs(
    title = "Employee Attrition Distribution",
    x = "Attrition Status",
    y = "Number of Employees"
  )+
  theme_minimal()

plot_attrition

#v2
attrition_overtime<- hr_clean %>%
  group_by(OverTime) %>%
  summarise(
    Employees = n(),
    AttritionCount = sum(Attrition == "Yes"),
    AttritionRate = round(
      AttritionCount / Employees *100,
      1
    )
  )

plot_overtime<- ggplot(
  attrition_overtime,
  aes(
    x = OverTime,
    y = AttritionRate,
    fill = OverTime
  )
)+
  geom_col()+
  geom_text(
    aes(label = paste0(AttritionRate, "%")),
    vjust = -0.3
  )+
  labs(
    title = "Attrition Rate by Overtime Status",
    x = "Overtime",
    y = "Attrition Rate (%)"
  )+
  theme_minimal()

plot_overtime

#v3
attrition_jobrole<- hr_clean %>%
  group_by(JobRole) %>%
  summarise(
    Employees = n(),
    AttritionCount = sum(Attrition == "Yes"),
    AttritionRate = round(
      AttritionCount / Employees *100,
      1
    )
  ) %>%
  arrange(desc(AttritionRate))

plot_jobrole<- ggplot(
  attrition_jobrole,
  aes(
    x = reorder(JobRole, AttritionRate),
    y = AttritionRate
  )
)+
  geom_col()+
  coord_flip()+
  geom_text(
    aes(label = paste0(AttritionRate, "%")),
    hjust = -0.1
  )+
  labs(
    title = "Attrition Rate by Job Role",
    x = "Job Role",
    y = "Attrition Rate (%)"
  )+
  theme_minimal()

plot_jobrole

#v4
attrition_agegroup<- hr_clean %>%
  group_by(AgeGroup) %>%
  summarise(
    Employees = n(),
    AttritionCount = sum(Attrition == "Yes"),
    AttritionRate = round(
      AttritionCount / Employees *100,
      1
    )
  )

plot_agegroup<- ggplot(
  attrition_agegroup,
  aes(
    x = AgeGroup,
    y = AttritionRate,
    fill = AgeGroup
  )
)+
  geom_col()+
  geom_text(
    aes(label = paste0(AttritionRate, "%")),
    vjust = -0.3
  )+
  labs(
    title = "Attrition Rate by Age Group",
    x = "Age Group",
    y = "Attrition Rate (%)"
  )+
  theme_minimal()

plot_agegroup

#v5
plot_income<- ggplot(
  hr_clean,
  aes(
    x = Attrition,
    y = MonthlyIncome,
    fill = Attrition
  )
)+
  geom_boxplot()+
  labs(
    title = "Monthly Income by Attrition Status",
    x = "Attrition",
    y = "Monthly Income"
  )+
  theme_minimal()

plot_income

#savevs
ggsave(
  "outputs/charts/01_attrition_distribution.png",
  plot = plot_attrition,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "outputs/charts/02_attrition_overtime.png",
  plot = plot_overtime,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "outputs/charts/03_attrition_jobrole.png",
  plot = plot_jobrole,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "outputs/charts/04_attrition_agegroup.png",
  plot = plot_agegroup,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "outputs/charts/05_income_boxplot.png",
  plot = plot_income,
  width = 8,
  height = 5,
  dpi = 300
)

