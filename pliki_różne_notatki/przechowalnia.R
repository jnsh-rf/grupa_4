# kod przeniesiony z chunka str_danych (wykorzystany jako inline code)
invisible(n_miss(HR))
invisible(prop_miss(HR))

janitor::clean_names(numerals="right")

```{r viz_do_rozdzielenia, echo=TRUE}
vis_miss(HR)
vis_dat(HR)

gg_miss_fct(HR, fct = Attrition)
#wizualizacja współwystępowania braków, brakuje w 4 kolumnach, stąd nsets = 4
gg_miss_upset(HR, nsets = 4)
# pakiet finalfit
explain <- names(HR)
missplot_complete <- HR %>% missing_pattern("Age", explain)
# na tep odstawie usunieto wszystki zmienne za wyjątkiem YearsWithCurrManager, JobRole i Gender oraz zmiennych z brakami
explain_u <- c("Age", "Attrition", "MonthlyIncome", "Gender", "JobRole", "YearsWithCurrManager")
missplot_finalfit <- HR %>% missing_pattern("Age", explain_u)
```

```{r kor_brak_old, eval=FALSE, warning=FALSE, include=FALSE}
ggplot(data = HR, aes(x = Age, y = MonthlyIncome)) +
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("darkorange", "cyan4"),
                     aesthetics = c("colour", "fill")) +
  theme_minimal()
```

Zmienne tekstowe Department, EducationField, Gender, JobRole oraz MaritalStatus powinny być zakodowane jako zmienne kategorialne, podczas gdy zmienne numeryczne Attrition, BusinessTravel, Education, EnvironmentSatisfaction, JobInvolvement, JobLevel, JobSatisfaction, PerformanceRating, RelationshipSatisfaction, StockOptionLevel, WorkLifeBalance, , OverTime  powinny być uporządkowanymi zmiennymi kategorialnymi.

#sprawdzamy unikatowe wartości zmiennych "tekstowych" - przenieść do osobnego bloku (tymczasowo potrzebny tutaj)
all_char_values <- list(
  val_1 = table(HR$Attrition),
  val_2 = table(HR$BusinessTravel),
  val_3 = table(HR$Department),
  val_4 = table(HR$EducationField),
  val_5 = table(HR$Gender),
  val_6 = table(HR$JobRole),
  val_7 = table(HR$MaritalStatus),
  val_8 = table(HR$Over18),
  val_9 = table(HR$OverTime)
)

# to też trzeba zinputować
#HR_0 <- walidacja_danych(HR) %>%
#  mutate(NumCompaniesWorked = if_else(NumCompaniesWorkedRule == FALSE, NA, NumCompaniesWorked)) %>%
#  select(names(HR))

#uproszczenie listy all_char_values
kategorie <- lapply(all_char_values, names)

HR_factor <- HR %>%
  mutate(
    Attrition = factor(Attrition, levels = kategorie$val_1), 
    BusinessTravel = factor(BusinessTravel, levels = kategorie$val_2),
    Department = factor(Department, levels = kategorie$val_3),
    EducationField = factor(EducationField, levels = kategorie$val_4),
    Gender = factor(Gender, levels = kategorie$val_5),
    JobRole = factor(JobRole, levels = kategorie$val_6),
    MaritalStatus = factor(MaritalStatus, levels = kategorie$val_7),
    Over18 = factor(Over18, levels = kategorie$val_8),
    OverTime = factor(OverTime, levels = kategorie$val_9)
  )
## pózniej naprawić i uproscic kod


Podczas tworzenia macierzy korelacji dla zmiennych numerycznych, uwzględniono dodatkowo zmienną Attrition, która została uprzednio po jej uprzednim przekodowaniu na typ factor, a następnie na numeric, celem zbadania korelacji tej zmiennej z pozostałymi zmiennymi numerycznymi.

W wyniku obliczeń, w przypadku niektórych zmiennych, nie została obliczona wartość współczynnika korelacji. Wynika to z zastosowanej metody obliczeń, tj. współczynnika korelacji Pearsona, którego wzór jest następujący:
  
  $$
  r = \frac{{}\sum_{i=1}^{n} (x_i - \overline{x})(y_i - \overline{y})}{\sqrt{\sum_{i=1}^{n} (x_i - \overline{x})^2  \sum_{i=1}^{n}(y_i - \overline{y})^2}}
$$
  
  Brak wartości korelacji w takich przypadkach związany jest z sytuacją, w której mianownik równania przyjmuje wartość zerową. Tego typu sytuacja ma miejsce, gdy którakolwiek ze zmiennych przyjmuje jedną stałą wartość (brak zmienności), co skutkuje zerową wariancją. W takich przypadkach obliczenie współczynnika korelacji staje się niemożliwe, a tym samym zmienne te należy usunąć, gdyż nie wnoszą one żadnej dodatkowej informacji do dalszej analizy danych.


```{r rpart, echo=TRUE}
im_age_rpart <- round_half_up(imputate_na(numeric_HR, Age, yvar = c("TotalWorkingYears", "JobLevel", "MonthlyIncome"), method = "rpart"), 0)

nazw_kol_hr <- colnames(numeric_HR) %>% 
  setdiff("Attrition")

#znales sposób na zagniezdzenie nazw_kol_hr w im_att_rpart

writeClipboard(nazw_kol_hr)

im_att_rpart <- round_half_up(imputate_na(numeric_HR, Attrition, 
                                          yvar = c("DailyRate", "DistanceFromHome", "Education", "EmployeeCount",
                                                   "EmployeeNumber", "EnvironmentSatisfaction", "HourlyRate",
                                                   "JobInvolvement", "JobLevel", "JobSatisfaction", "MonthlyIncome",
                                                   "NumCompaniesWorked", "PercentSalaryHike", "PerformanceRating",
                                                   "RelationshipSatisfaction", "StandardHours", "StockOptionLevel",
                                                   "TotalWorkingYears", "TrainingTimesLastYear","WorkLifeBalance",
                                                   "YearsAtCompany", "YearsInCurrentRole", "YearsSinceLastPromotion",
                                                   "YearsWithCurrManager"), 
                                          method = "rpart"), 0)

im_moi_rpart <- round_half_up(imputate_na(numeric_HR, MonthlyIncome, yvar = c("JobLevel", "TotalWorkingYears", "YearsAtCompany", "Age"), method = "rpart"), 0)

im_ncw_rpart <- round_half_up(imputate_na(numeric_HR, NumCompaniesWorked, yvar = c("JobLevel", "TotalWorkingYears", "YearsAtCompany", "Age"), method = "rpart"), 0)

plot(im_age_rpart)
plot(im_att_rpart)
plot(im_moi_rpart)
```



cbind(im_age_mice, im_att_mice, im_moi_knn) %>%
  mutate(
    Age = im_age_mice,
    Attrition = im_att_mice,
    MonthlyIncome = im_moi_knn) %>%
  select(-im_age_mice, im_att_mice, im_moi_knn) %>%
  mutate(Attrition = ifelse(Attrition == 1, "No", 
                            ifelse(Attrition == 2, "Yes", Attrition)))

view(HR_imputowane)

# nie chce sie renderować
