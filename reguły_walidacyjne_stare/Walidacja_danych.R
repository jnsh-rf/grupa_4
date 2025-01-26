library(validate)

rules <- validator(
  Age >= 18 & Age <= 80 & is.numeric(Age), 
  DistanceFromHome >= 0 & is.numeric(DistanceFromHome), 
  DailyRate > 0 & is.numeric(DailyRate), 
  EmployeeCount == 1 & is.numeric(EmployeeCount),
  HourlyRate > 0 & is.numeric(HourlyRate), 
  MonthlyIncome > 0 & is.numeric(MonthlyIncome), 
  MonthlyRate > 0 & is.numeric(MonthlyRate), 
  NumCompaniesWorked >= 0 & is.numeric(NumCompaniesWorked), 
  PercentSalaryHike >= 0 & is.numeric(PercentSalaryHike), 
  StandardHours == 80 & is.numeric(StandardHours), 
  TotalWorkingYears >= 0 & is.numeric(TotalWorkingYears), 
  TrainingTimesLastYear >= 0 & TotalWorkingYears < 100 & is.numeric(TrainingTimesLastYear), 
  YearsAtCompany >= 0 & YearsAtCompany < 60 & is.numeric(YearsAtCompany), 
  YearsInCurrentRole >= 0 & YearsInCurrentRole< 60 & is.numeric(YearsInCurrentRole), 
  YearsSinceLastPromotion >= 0 & is.numeric(YearsSinceLastPromotion), 
  YearsWithCurrManager >= 0 & is.numeric(YearsWithCurrManager), 
  
  

  # Zmienne kategoryczne
  Attrition %in% c("Yes", "No"), 
  BusinessTravel %in% c("Travel_Rarely", "Travel_Frequently", "Non-Travel"), 
  Department %in% c("Sales", "Research & Development", "Human Resources"), 
  Education %in% 1:5 & is.numeric(Education), 
  EducationField %in% c("Life Sciences", "Medical", "Marketing", "Technical Degree", "Human Resources", "Other"), 
  EnvironmentSatisfaction %in% 1:4 & is.numeric(EnvironmentSatisfaction), 
  Gender %in% c("Male", "Female"), 
  JobInvolvement %in% 1:4 & is.numeric(JobInvolvement), 
  JobLevel %in% 1:5 & is.numeric(JobLevel), 
  JobRole %in% c("Sales Executive", "Research Scientist", "Laboratory Technician", 
                 "Manufacturing Director", "Healthcare Representative", 
                 "Manager", "Sales Representative", "Research Director", 
                 "Human Resources"), 
  JobSatisfaction %in% 1:4 & is.numeric(JobSatisfaction), 
  MaritalStatus %in% c("Single", "Married", "Divorced"), 
  Over18 == "Y",
  OverTime %in% c("Yes", "No"), 
  PerformanceRating %in% 1:4 & is.numeric(PerformanceRating), 
  RelationshipSatisfaction %in% 1:4 & is.numeric(RelationshipSatisfaction), 
  WorkLifeBalance %in% 1:4 & is.numeric(WorkLifeBalance), 
  StockOptionLevel %in% 0:3 & is.numeric(StockOptionLevel), 
  
)

out   <- confront(HR, rules)
