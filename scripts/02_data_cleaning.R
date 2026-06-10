library(tidyverse)

hr<- read.csv("data/raw/train.csv")

#hapuskolom
hr_clean<- hr %>%
  select(
    -EmployeeCount,
    -Over18,
    -StandardHours,
    -EmployeeNumber
  )

#cekjumlahkolom
ncol(hr)
ncol(hr_clean)

#addfeature
hr_clean<- hr %>%
  mutate(
    AgeGroup = case_when(
      Age < 30 ~ "<30",
      Age < 40 ~ "30-39",
      Age < 50 ~ "40-49",
      TRUE ~ "50+"
    )
  )

#cek hr_clean
table(hr_clean$AgeGroup)

#cekartition
table(hr_clean$Attrition)

#tambahfileclean
write.csv(
  hr_clean,
  "data/processed/hr_clean.csv",
  row.names = FALSE
)
