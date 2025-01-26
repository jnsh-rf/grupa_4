rules <- validator(
  Age >= 18 & Age <= 80 & Age >= (18 + TotalWorkingYears) & is.numeric(Age), #Wiek nie może być niższy niż lata pracy osoby pełnoletniej
  DistanceFromHome >= 0 & is.numeric(DistanceFromHome), 
  DailyRate > 0 & is.numeric(DailyRate), 
  # EmployeeCount == 1 & is.numeric(EmployeeCount), # usuń przenosząc do raportu  
  HourlyRate > 0 & is.numeric(HourlyRate), 
  MonthlyIncome > 0 & is.numeric(MonthlyIncome), 
  MonthlyRate > 0 & is.numeric(MonthlyRate), 
  NumCompaniesWorked >= 0 & is.numeric(NumCompaniesWorked), 
  # PercentSalaryHike >= 0 & is.numeric(PercentSalaryHike), # zastąpione regułami warunkowymi - usuń przenosząc do raportu 
  # StandardHours == 80 & is.numeric(StandardHours), # usuń przenosząc do raportu 
  TotalWorkingYears >= 0 & is.numeric(TotalWorkingYears), 
  TrainingTimesLastYear >= 0 & TrainingTimesLastYear <= 6 & is.numeric(TrainingTimesLastYear), # poprawa błędnej nazwy zmiennej, zmiana na szkolenie <= 6 (raz na 2 m-ce) 
  YearsAtCompany >= 0 & YearsAtCompany < 60 & 
    (YearsAtCompany >= YearsInCurrentRole | 
       YearsAtCompany >= YearsSinceLastPromotion | 
       YearsAtCompany >= YearsWithCurrManager) & is.numeric(YearsAtCompany), 
  # Lata w firmie nie mogą być krótsze niż czas od ostatniego awansu, lata pracy z tym samym menedżerem lub lata na obecnym stanowisku.
  TotalWorkingYears >= 0 & is.numeric(TotalWorkingYears), 
  YearsInCurrentRole >= 0 & YearsInCurrentRole < 60 & is.numeric(YearsInCurrentRole), 
  YearsSinceLastPromotion >= 0 & is.numeric(YearsSinceLastPromotion), 
  YearsWithCurrManager >= 0 & is.numeric(YearsWithCurrManager), 
  
  # reguły warunkowe
  if (PerformanceRating == 1) PercentSalaryHike == 0,
  if (PerformanceRating == 2) PercentSalaryHike >= 1 & PercentSalaryHike <= 10,
  if (PerformanceRating == 3) PercentSalaryHike >= 11 & PercentSalaryHike <= 19,
  if (PerformanceRating == 4) PercentSalaryHike >= 20,
  if (Department == "Sales") JobRole %in% c("Sales Executive", "Sales Representative", "Manager"),
  if (Department == "Research & Development") JobRole %in% c("Research Scientist", "Laboratory Technician", "Research Director", "Manufacturing Director", "Healthcare Representative", "Manager"),
  if (Department == "Human Resources") JobRole %in% c("Human Resources", "Manager")
  
  # Zmienne kategoryczne
  # Attrition %in% c("Yes", "No"), 
  # BusinessTravel %in% c("Travel_Rarely", "Travel_Frequently", "Non-Travel"), 
  # Department %in% c("Sales", "Research & Development", "Human Resources"), 
  # Education %in% 1:5 & is.numeric(Education), 
  # EducationField %in% c("Life Sciences", "Medical", "Marketing", "Technical Degree", "Human Resources", "Other"), 
  # EnvironmentSatisfaction %in% 1:4 & is.numeric(EnvironmentSatisfaction), 
  # Gender %in% c("Male", "Female"), 
  # JobInvolvement %in% 1:4 & is.numeric(JobInvolvement), 
  # JobLevel %in% 1:5 & is.numeric(JobLevel), 
  # JobRole %in% c("Sales Executive", "Research Scientist", "Laboratory Technician", 
  #                "Manufacturing Director", "Healthcare Representative", 
  #                "Manager", "Sales Representative", "Research Director", 
  #                "Human Resources"), 
  # JobSatisfaction %in% 1:4 & is.numeric(JobSatisfaction), 
  # MaritalStatus %in% c("Single", "Married", "Divorced"), 
  # Over18 == "Y", ############## usuń przenosząc do raportu 
  # OverTime %in% c("Yes", "No"), 
  # PerformanceRating %in% 1:4 & is.numeric(PerformanceRating), 
  # RelationshipSatisfaction %in% 1:4 & is.numeric(RelationshipSatisfaction), 
  # WorkLifeBalance %in% 1:4 & is.numeric(WorkLifeBalance), 
  # StockOptionLevel %in% 0:3 & is.numeric(StockOptionLevel)
)

out <- confront(HR, rules)


summary(out)
plot(out, main="HR")