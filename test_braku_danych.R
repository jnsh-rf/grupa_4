library(readr)
library(tidyverse)
HR <- read_csv("HR.csv")
# View(HR)

install.packages("naniar")
install.packages("visdat")
install.packages("Amelia")
library(Amelia)
library(naniar)
library(visdat)

?amelia

#wszystkie brakujące obserwacje - brakuje 400
n_miss(HR)
#udział brakujących w całości - 0.007774538 (0,7774%)
prop_miss(HR)
#tabela podsumowująca braki dla wszystkich zmiennych
brak_wart <- tibble(miss_var_summary(HR))
brak_wart
#mamy 331 rzedów z jednym brakiem, 33 rzedy z dwoma brakami oraz 1 z trzema brakami
miss_case_table(HR)


#wizualizacja braków
vis_miss(HR)
vis_dat(HR)
#wstępnie - wyglądają na przypadkowe braki (brak "skupisk" braków)
#zgrupowane po wierszach z brakami i uporządkowanie kolumn w kolejności brakujących
vis_miss(HR, cluster = TRUE, sort_miss = TRUE)
#mapa ciepła (heatmap) - przykładowa
gg_miss_fct(HR, fct = JobLevel)
#wizualizacja współwystępowania braków, brakuje w 3 kolumnach, stąd nsets = 3
gg_miss_upset(HR, nsets = 3)
#Braki w:
# - Age i Attrition wystepują razem 12 razy
# - Attrition i MonthlyIncome razem 12 razy
# - Age i MonthlyIncome razem 9 razy
# - wszystkie razem tylko 1 raz

ggplot(data = HR, aes(x = Age, y = MonthlyIncome)) +
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("darkorange", "cyan4"))
  theme_minimal()
  
# macierz korelacji brakujących danych
library(rstatix)

braki_danych <- as.data.frame(ifelse(is.na(HR),1,0))
braki_danych_n <- braki_danych %>%
  select(Age, Attrition, MonthlyIncome)
#dla kolumn z brakami
macierz_kor_brak <- cor_mat(
  braki_danych_n,
  method = "pearson",
  alternative = "two.sided",
  conf.level = 0.95
)

#imputacja danych
HR_imp <- amelia(HR, m = 5)
#Amelia Error Code:  38 
#The following variable(s) are characters: 
#  Attrition (N), BusinessTravel (N), Department (N), EducationField (N), Gender (N), JobRole (N), MaritalStatus (N), Over18 (N), OverTime (N)
#You may have wanted to set this as a ID variable to remove it
#from the imputation model or as an ordinal or nominal
#variable to be imputed.  Please set it as either and try again.
HR_imp <- amelia(HR, m = 5, noms = c("Attrition", "BusinessTravel", "Department", "EducationField", "Gender", "JobRole", "MaritalStatus", "Over18", "OverTime"))
#Amelia Error Code:  43 
#You have a variable in your dataset that does not vary.  Please remove this variable. Variables that do not vary:  EmployeeCount, Over18, StandardHours 
table(HR$EmployeeCount)
table(HR$Over18)
table(HR$StandardHours)
#trzeba je usunąć
HR_imp <- amelia(HR, m = 5, noms = c("Attrition", "BusinessTravel", "Department", "EducationField", "Gender", "JobRole", "MaritalStatus", "OverTime"), idvars = c("EmployeeCount", "Over18", "StandardHours"))

#plot(HR_imp, which.vars = c("Age", "Attrition", "MonthlyIncome"))

impu <- cbind(HR_imp$imputations[[1]], HR_imp$missMatrix)

por_imp1 <- data.frame(age_orig = HR$Age, att_orig = HR$Attrition, moi_orig = HR$MonthlyIncome,
                       age_impu = impu$Age, att_impu = impu$Attrition, moi_impu = impu$MonthlyIncome) %>%
  mutate(nr = row_number())

# wykres pokazujący jak kształtują się wartości imputowane względem wartości rzeczywistych
ggplot(por_imp1, aes(x = nr)) +
  geom_line(aes(y = age_orig, color = "lightgreen")) +
  geom_line(aes(y = age_impu, color = "tomato")) +
  labs(title = "Porównanie oryginalnych i imputowanych wartości",
       x = "Oryginalne wartości",
       y = "Imputowane wartości") +
  theme_minimal()
# scale_color_manual(values = c("lightgreen", "tomato"), labels = c("Oryginalne", "Imputowane")) +



ggplot(por_imp1, aes(x = age_orig, y = age_impu, color = missing)) +
  geom_point(alpha = 0.7) +
  labs(title = "Porównanie oryginalnych i imputowanych wartości",
       x = "Oryginalne wartości",
       y = "Imputowane wartości") +
  scale_color_manual(values = c("black", "red"), labels = c("Oryginalne", "Imputowane")) +
  theme_minimal()

# Wizualizacja różnic
ggplot(comparison, aes(x = population_original, y = population_imputed, color = missing)) +
  geom_point(alpha = 0.7) +
  labs(title = "Porównanie oryginalnych i imputowanych wartości",
       x = "Oryginalne wartości",
       y = "Imputowane wartości") +
  scale_color_manual(values = c("black", "red"), labels = c("Oryginalne", "Imputowane")) +
  theme_minimal()
is.na