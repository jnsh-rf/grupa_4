library(readr)
library(tidyverse)
HR <- read_csv("HR.csv")
# View(HR)

install.packages("naniar")
install.packages("visdat")
install.packages("Amelia")
install.packages("dlookr")
install.packages("rstatix")
library(Amelia)
library(naniar)
library(visdat)
library(dlookr)
library(rstatix)

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

table(HR$Attrition)
hr_factors <- HR %>%
  factor(HR$Attrition, ifelse(HR$Attrition %in% ("No"), 0, 1),
         levels = c(0, 1), labels = c("No", "Yes"))

# przeobi na  0, 1 zrobic labels dla no yes  

m_kor_hr <- cor_mat(
  HR,
  method = "pearson",
  alternative = "two.sided",
  conf.level = 0.95
)


age_i <- round(imputate_na(HR, ))

### przerobic chr na num (factor) 

#sprawdzamy unikatowe wartości zmiennych "tekstowych"
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

#weryfikacja - w razie czego odkomentować
#str(HR_factor_V2)

### stworzyć macierz korelacji

# dla wartości numerycznych
numeric_HR <- HR %>% select(where(is.numeric))

macierz_kor_numeric <- cor_mat(
  numeric_HR,
  method = "pearson",
  alternative = "two.sided",
  conf.level = 0.95
)

# uwzględnienie factor po ich przekodowaniu
factor_HR <- HR_factor %>%
  mutate(across(where(is.factor), ~ as.numeric(as.factor(.))))

macierz_kor_factor <- cor_mat(
  factor_HR,
  method = "pearson",
  alternative = "two.sided",
  conf.level = 0.95
)

#wizualizacja korelacji
#install.packages("ggcorrplot")
#library(ggcorrplot)

ggcorrplot(macierz_kor_numeric)
ggcorrplot(macierz_kor_factor)

View(macierz_kor_factor)
# Extract the rows corresponding to Age, Attrition, and MonthlyIncome
istotne_kor_factor <- macierz_kor_factor[c("Age", "Attrition", "MonthlyIncome"), ]


# 1470, 35
# Step 1: Add row names as a column and select the desired columns
istotne_kor_factor1 <- macierz_kor_factor %>%
  rownames_to_column(var = "kor_wzgledem") %>%
  select(kor_wzgledem, Age, Attrition, MonthlyIncome)

# Step 2: Pivot the table to make columns into rows and rows into columns
istotne_kor_factor1_long <- istotne_kor_factor1 %>%
  pivot_longer(cols = -kor_wzgledem, names_to = "variable", values_to = "value")

# Step 3: Extract rows into separate vectors
age_vector <- istotne_kor_factor%>% filter( Age) 
att_vector <- istotne_kor_factor1_long %>% filter(Attrition)
monthly_income_vector <- istotne_kor_factor1_long %>% filter(MonthlyIncome)





# Find the strongest correlation for each variable (excluding self-correlation)
silne_kor_factor <- apply(istotne_kor_factor, 1, function(x) {
  max(abs(x[-which(names(x) %in% c("Age", "Attrition", "MonthlyIncome"))]))
})

# Get the names of the variables with the strongest correlations
silne_kor_factor_names <- names(silne_kor_factor)

# Print the result
print(silne_kor_factor_names)

mx_kor_czyste <- macierz_kor_factor[rowSums(!is.na(macierz_kor_factor)) > 1, colSums(!is.na(macierz_kor_factor)) > 0] %>%
  column_to_rownames("rowname")
# identyfikacja niepotrzebnych kolumn na podstawie korelacji Pearsona (zarówno char zmienionych na factor, jak i numerycznych), gdyż zmienna przyjmuje tylko jedną wartość
zm_zbedne_V1 <- setdiff(colnames(macierz_kor_factor), colnames(df_korelacja_czyste))
# w razporcie w teksie pod ramką zamiscic tylko paste
print(paste("Usunięto zmienne", paste(zm_zbedne_V1, collapse = ", "), "ze względu na wynikające ze wzoru na korelacje Pearsona właściwosci"))

wiersze_braki <- c("Age", "Attrition", "MonthlyIncome")

# Save as separate vectors
age_vector <- kor_missing_var %>% filter("Age") %>% pull(1)
att_vector <- kor_missing_var %>% filter("Attrition") %>% pull(1)
moi_vector <- kor_missing_var %>% filter("MonthlyIncome") %>% pull(1)
braki_danych_age <- braki_danych %>% select(Age)

# na podstawoie wartosci najbardziej skorelowanych do zminnych age, attrition imputowac (yvar)
# + monthly income jako godziny * stawka
# zrobic wykres czy sie pokrywaj/sa zblizone
# jesli tak podstawic jako na przy pomocy replace_na, jezeli nie to inna metoda




### przewidziec attrition na pdst regresji logistycznej