#dla wszystkich kolumn (może chwilę potrwać) oraz nie zabardzo jest cokolwiek poza dla tych z brakami
#macierz_kor_brakW <- cor_mat(
#  braki_danych,
#  method = "pearson",
#  alternative = "two.sided",
#  conf.level = 0.95
#)
# KORELACJA JEST NIEWIELKA

#do dalszej modyfikacji
braki_danych_aam <- braki_danych %>%
  select(Age_1 = Age, Att_1 = Attrition, Moi_1 = MonthlyIncome)
HR_test <- cbind(HR, braki_danych_aam)

macierz_kor_test_age <- cor_mat(
  HR_test$TotalWorkingYears,
  vars = HR[braki_danych_aam],
  method = "pearson",
  alternative = "two.sided",
  conf.level = 0.95
)

braki_danych_age <- braki_danych %>% select(Age)
braki_danych_att <- braki_danych %>% select(Attrition)
braki_danych_moi <- braki_danych %>% select(MonthlyIncome)


braki_danych_aam_names <- c("Age", "Attrition", "MonthlyIncome")


#macierz korelacji dla przepracowanych lat
macierz_kor_brak_age <- cor_mat(
  HR$TotalWorkingYears,
  vars = braki_danych_age,
  method = "pearson",
  alternative = "two.sided",
  conf.level = 0.95
)
macierz_kor_brak_att <- cor_mat(
  HR_test$TotalWorkingYears,
  vars = braki_danych_att,
  method = "pearson",
  alternative = "two.sided",
  conf.level = 0.95
)
macierz_kor_brak_aam <- cor_mat(
  HR$TotalWorkingYears,
  vars = braki_danych_aam[Age],
  method = "pearson",
  alternative = "two.sided",
  conf.level = 0.95
)
