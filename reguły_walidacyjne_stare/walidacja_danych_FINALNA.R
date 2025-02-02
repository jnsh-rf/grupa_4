# reguły walidacyjne korzystające z tidyverse
walidacja_danych <- function(HR) {
  HR %>% mutate(
    AgeRule = if_else(
      Age >= 18 & Age <= 67 & Age >= (18 + TotalWorkingYears) & is.numeric(Age),# 67 wiek emerytalny
      TRUE, 
      FALSE, 
      missing = NA
    ),
    AttritionRule = if_else(
      Attrition %in% c("No", "Yes"),
      TRUE,
      FALSE,
      missing = NA
    ),
    DistanceFromHomeRule = if_else(
      DistanceFromHome >= 0 & is.numeric(DistanceFromHome), 
      TRUE, 
      FALSE, 
      missing = NA
    ),
    DailyRateRule = if_else(
      DailyRate > 0 & is.numeric(DailyRate), 
      TRUE, 
      FALSE, 
      missing = NA
    ),
    HourlyRateRule = if_else(
      is.numeric(HourlyRate) & HourlyRate > 0, 
      TRUE, 
      FALSE, 
      missing = NA
    ),
    MonthlyIncomeRule = if_else(
      is.numeric(MonthlyIncome) & MonthlyIncome> 0, 
      TRUE, 
      FALSE, 
      missing = NA
    ),
    MonthlyRateRule = if_else(
      is.numeric(MonthlyRate) & MonthlyRate > 0, 
      TRUE, 
      FALSE, 
      missing = NA
    ),
    NumCompaniesWorkedRule = if_else(
      is.numeric(NumCompaniesWorked) & 
      (
        NumCompaniesWorked > 0 | (NumCompaniesWorked == 0 & YearsAtCompany == TotalWorkingYears)
      ), 
      TRUE, 
      FALSE, 
      missing = NA
    ),
    TotalWorkingYearsRule = if_else(
      is.numeric(TotalWorkingYears) & TotalWorkingYears >= 0, 
      TRUE, 
      FALSE, 
      missing = NA
    ),
    TrainingTimesLastYearRule = if_else(
      is.numeric(TrainingTimesLastYear) & TrainingTimesLastYear >= 0 & TrainingTimesLastYear <= 6, 
      TRUE, 
      FALSE, 
      missing = NA
    ),
    YearsAtCompanyRule = if_else( 
      is.numeric(YearsAtCompany) &
      YearsAtCompany >= 0 & YearsAtCompany <= 49 & # 67 - wiek emerytalny w USA odjąć 18
      (
        (YearsAtCompany >= YearsInCurrentRole) | 
        (YearsAtCompany >= YearsSinceLastPromotion) | 
        (YearsAtCompany >= YearsWithCurrManager)
      ), 
      TRUE, 
      FALSE, 
      missing = NA
    ),
    YearsInCurrentRoleRule = if_else(
      is.numeric(YearsInCurrentRole) & YearsInCurrentRole >= 0 & YearsInCurrentRole < 49, 
      TRUE, 
      FALSE, 
      missing = NA
    ),
    YearsSinceLastPromotionRule = if_else(
      is.numeric(YearsSinceLastPromotion) & YearsSinceLastPromotion >= 0, 
      TRUE, 
      FALSE, 
      missing = NA
    ),
    YearsWithCurrManagerRule = if_else(
      is.numeric(YearsWithCurrManager) & YearsWithCurrManager >= 0, 
      TRUE, 
      FALSE, 
      missing = NA
    ),
    PercentSalaryHikeRule = if_else(
      (PerformanceRating == 1 & PercentSalaryHike == 0) |
      (PerformanceRating == 2 & PercentSalaryHike >= 1 & PercentSalaryHike <= 10) |
      (PerformanceRating == 3 & PercentSalaryHike >= 11 & PercentSalaryHike <= 19) |
      (PerformanceRating == 4 & PercentSalaryHike >= 20), 
      TRUE, 
      FALSE, 
      missing = NA
    ),
    JobRoleRule = if_else(
      (Department == "Sales" & JobRole %in% c("Sales Executive", "Sales Representative", "Manager")) |
      (Department == "Research & Development" & JobRole %in% c("Research Scientist", "Laboratory Technician", 
                                                                 "Research Director", "Manufacturing Director", 
                                                                 "Healthcare Representative", "Manager")) |
      (Department == "Human Resources" & JobRole %in% c("Human Resources", "Manager")), 
      TRUE, 
      FALSE, 
      missing = NA
    )
  )
}

#################################################################################################################
#                                       WYKRES i TABELA                                                         #
#################################################################################################################
podsumowanie_walidacji <- walidacja_danych(HR) %>%
  select(ends_with("Rule")) %>%
  mutate(across(everything(), ~ case_when(
    is.na(.) ~ "NA",
    . == TRUE ~ "Pass",
    . == FALSE ~ "Fail"
  ), .names = "Status_{.col}")) %>%
  pivot_longer(
    cols = starts_with("Status_"),
    names_to = "Rule",
    values_to = "Status",
    names_prefix = "Status_"
  )

wykres_walidacja <- podsumowanie_walidacji %>%
  group_by(Rule, Status) %>%
  summarize(Count = n(), .groups = "drop") %>%
  mutate(Rule = gsub("Rule$", "", Rule)) %>%
  pivot_wider(names_from = Status, values_from = Count, values_fill = list(Count = 0))

wykres_walidacja_fin <- wykres_walidacja %>%
  pivot_longer(cols = -Rule, names_to = "Status", values_to = "Count")

wykres_walidacja_legenda <- wykres_walidacja_fin %>%
  mutate(total_count = sum(Count)) %>%
  group_by(Status) %>%
  mutate(Percentage = sum(Count) / total_count * 100) %>%
  summarize(Percentage = unique(Percentage)) %>%
  ungroup() %>%
  mutate(Label = paste0(Status, sprintf(" (%.1f%%)", Percentage))) %>%
  pull(Label)

# wizualizacja reguł walidacyjnych
ggplot(wykres_walidacja_fin, aes(x = Rule, y = Count, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = colors, labels = wykres_walidacja_legenda) +
  theme_minimal() +
  coord_flip() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.justification = "right",
    legend.title = element_blank(),
    plot.title = element_blank()
  )


#  tabela z podsumowaniem
walidacja_rezultat <- wykres_walidacja %>%
  mutate(
    Total = `Fail` + `Pass` + `NA`,
    `Fail %` = sprintf("%.1f%%", (`Fail` / Total) * 100),
    `Pass %` = sprintf("%.1f%%", (`Pass` / Total) * 100),
    `NA %` = sprintf("%.1f%%", (`NA` / Total) * 100)
  ) %>%
  select(Rule, `N Passed` = Pass, `N Failed` = Fail, `N NA` = `NA`, `Fail %`, `Pass %`, `NA %`) %>%
  kable("html", escape = FALSE, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

walidacja_rezultat