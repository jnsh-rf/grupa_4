library(tidyverse)
library(dlookr)
library(patchwork)

# zmienne na podstawie (npdst_*), których imputowane będą brakujące wartości
napdst_Age <- c("TotalWorkingYears", "JobLevel", "MonthlyIncome")
napdst_Attrition <- c("DailyRate", "DistanceFromHome", "Education", "EmployeeCount",
                          "EmployeeNumber", "EnvironmentSatisfaction", "HourlyRate",
                          "JobInvolvement", "JobLevel", "JobSatisfaction", "MonthlyIncome",
                          "PercentSalaryHike", "PerformanceRating",
                          "RelationshipSatisfaction", "StandardHours", "StockOptionLevel",
                          "TotalWorkingYears", "TrainingTimesLastYear","WorkLifeBalance",
                          "YearsAtCompany", "YearsInCurrentRole", "YearsSinceLastPromotion",
                          "YearsWithCurrManager")
napdst_MonthlyIncome <- c("JobLevel", "TotalWorkingYears", "YearsAtCompany", "Age")
napdst_NumCompaniesWorked <- c("DailyRate", "DistanceFromHome", "Education", "EmployeeCount",
                                   "EmployeeNumber", "EnvironmentSatisfaction", "HourlyRate",
                                   "JobInvolvement", "JobLevel", "JobSatisfaction", "MonthlyIncome",
                                   "PercentSalaryHike", "PerformanceRating", "RelationshipSatisfaction", 
                                   "StandardHours", "StockOptionLevel", "TotalWorkingYears", 
                                   "TrainingTimesLastYear","WorkLifeBalance", "YearsAtCompany", 
                                   "YearsInCurrentRole", "YearsSinceLastPromotion", "YearsWithCurrManager")

imputed_data <- numeric_HR

# knn z VIM radzi sobie lepiej ze wszystkimi typami zmiennych
# plot oraz rpart() wymaga zmiennych numerycznych 

imputed_data <- imputed_data %>%  
  mutate(
    Age_knn = round_half_up(kNN(numeric_HR, variable = "Age")$Age),    
    Age_mice = round_half_up(imputate_na(numeric_HR, xvar = "Age", yvar = napdst_Age, method = "mice")),    
    Age_rpart = round_half_up(imputate_na(numeric_HR, xvar = "Age", yvar = napdst_Age, method = "rpart")),
      
    Attrition_knn = round_half_up(kNN(numeric_HR, variable = "Attrition")$Attrition),    
    Attrition_mice = round_half_up(imputate_na(numeric_HR, xvar = "Attrition", yvar = napdst_Attrition, method = "mice")),    
    Attrition_rpart = round_half_up(imputate_na(numeric_HR, xvar = "Attrition", yvar = napdst_Attrition, method = "rpart")),
    
    MonthlyIncome_knn = round_half_up(kNN(numeric_HR, variable = "MonthlyIncome")$MonthlyIncome),    
    MonthlyIncome_mice = round_half_up(imputate_na(numeric_HR, xvar = "MonthlyIncome", yvar = napdst_MonthlyIncome, method = "mice")),    
    MonthlyIncome_rpart = round_half_up(imputate_na(numeric_HR, xvar = "MonthlyIncome", yvar = napdst_MonthlyIncome, method = "rpart")),
      
    NumCompaniesWorked_knn = round_half_up(kNN(numeric_HR, variable = "NumCompaniesWorked")$NumCompaniesWorked),    
    NumCompaniesWorked_mice = round_half_up(imputate_na(numeric_HR, xvar = "NumCompaniesWorked", yvar = napdst_NumCompaniesWorked, method = "mice")),    
    NumCompaniesWorked_rpart = round_half_up(imputate_na(numeric_HR, xvar = "NumCompaniesWorked", yvar = napdst_NumCompaniesWorked, method = "rpart"))
  )

# print(imputed_data)

create_plot <- function(data, original, imputed, title) {
  ggplot2::ggplot(data) +
    ggplot2::geom_density(ggplot2::aes(x = {{ original }}), color = "#96d170", alpha = 0.4) + 
    ggplot2::geom_density(ggplot2::aes(x = {{ imputed }}), color = "#cb8873", alpha = 0.4) + 
    ggplot2::labs(title = title, x = "Wartość", y = "Gęstość rozkładu") +
    ggplot2::theme_minimal()
}

plot_age_knn <- create_plot(imputed_data, Age, Age_knn, "Age: k-Nearest Neighbors")
plot_age_mice <- create_plot(imputed_data, Age, Age_mice, "Age: MICE")
plot_age_rpart <- create_plot(imputed_data, Age, Age_rpart, "Age: Recursive Partitioning")

plot_attrition_knn <- create_plot(imputed_data, Attrition, Attrition_knn, "Attrition: k-Nearest Neighbors")
plot_attrition_mice <- create_plot(imputed_data, Attrition, Attrition_mice, "Attrition: MICE")
plot_attrition_rpart <- create_plot(imputed_data, Attrition, Attrition_rpart, "Attrition: Recursive Partitioning")

plot_income_knn <- create_plot(imputed_data, MonthlyIncome, MonthlyIncome_knn, "MonthlyIncome: k-Nearest Neighbors")
plot_income_mice <- create_plot(imputed_data, MonthlyIncome, MonthlyIncome_mice, "MonthlyIncome: MICE")
plot_income_rpart <- create_plot(imputed_data, MonthlyIncome, MonthlyIncome_rpart, "MonthlyIncome: Recursive Partitioning")

plot_ncw_knn <- create_plot(imputed_data, NumCompaniesWorked, NumCompaniesWorked_knn, "NumCompaniesWorked: k-Nearest Neighbors")
plot_ncw_mice <- create_plot(imputed_data, NumCompaniesWorked, NumCompaniesWorked_mice, "NumCompaniesWorked: MICE")
plot_ncw_rpart <- create_plot(imputed_data, NumCompaniesWorked, NumCompaniesWorked_rpart, "NumCompaniesWorked: Recursive Partitioning")

combined_age_plots <- patchwork::wrap_plots(plot_age_knn, plot_age_mice, plot_age_rpart, nrow = 1)
combined_attrition_plots <- patchwork::wrap_plots(plot_attrition_knn, plot_attrition_mice, plot_attrition_rpart, nrow = 1)
combined_income_plots <- patchwork::wrap_plots(plot_income_knn, plot_income_mice, plot_income_rpart, nrow = 1)
combined_ncw_plots <- patchwork::wrap_plots(plot_ncw_knn, plot_ncw_mice, plot_ncw_rpart, nrow = 1)

print(combined_age_plots)
print(combined_attrition_plots)
print(combined_income_plots)
print(combined_ncw_plots)