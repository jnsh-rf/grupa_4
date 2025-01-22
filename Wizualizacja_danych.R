install.packages("writexl")
install.packages("esquisse")
install.packages("ggcorrplot")

library(esquisse)
library(tidyverse)
library(writexl)
library(ggplot2)
library(reshape2)
library(ggcorrplot)

# zapisanie danych po imputacji do Excela
write_xlsx(HR_imputowane, path = "HR_imputowane.xlsx")

library(readxl)
HR_imputowane <- read_excel("HR_imputowane.xlsx")
View(HR_imputowane)

#SKOPIOWANE Z RAPORTU, aby macierz korelacji działała na danych HR_imputowane
#sprawdzamy unikatowe wartości zmiennych "tekstowych" - przenieść do osobnego bloku (tymczasowo potrzebny tutaj)
all_char_values <- list(
  val_1 = table(HR_imputowane$Attrition),
  val_2 = table(HR_imputowane$BusinessTravel),
  val_3 = table(HR_imputowane$Department),
  val_4 = table(HR_imputowane$EducationField),
  val_5 = table(HR_imputowane$Gender),
  val_6 = table(HR_imputowane$JobRole),
  val_7 = table(HR_imputowane$MaritalStatus),
  val_8 = table(HR_imputowane$Over18),
  val_9 = table(HR_imputowane$OverTime)
)

#uproszczenie listy all_char_values
kategorie <- lapply(all_char_values, names)

HR_factor <- HR_imputowane %>%
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

numeric_HR <- HR_imputowane %>%
  mutate(Attrition = factor(Attrition, levels = kategorie$val_1)) %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  select(where(is.numeric))


macierz_korelacji <- cor_mat(
  numeric_HR,
  method = "pearson",
  alternative = "two.sided",
  conf.level = 0.95
)

ggcorrplot(macierz_korelacji,
           insig = "blank",
           lab_size = 3, 
           method="square", 
           colors = c("#4477AA", "white", "#BB4444"), 
           ggtheme=theme_minimal(),
           legend.title = "Korelacja")


#Heatmapa korelacji zmiennych

macierz_korelacji <- cor(HR_imputowane[, sapply(HR_imputowane, is.numeric)], method = "pearson")
stopiona_macierz_kor <- melt(macierz_korelacji)

ggplot(stopiona_macierz_kor, aes(x=rowname, y=variable, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="red", high="green", mid="white", midpoint=0, limit=c(-1, 1), name="Korelacja") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


#wykres liniowy dochód od wieku
ggplot(HR_imputowane, aes(x = Age, y = MonthlyIncome)) +
geom_point()


#boxplot
ggplot(HR_imputowane, aes(x =Attrition , y = Age)) +
  geom_boxplot()




