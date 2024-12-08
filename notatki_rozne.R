#przygotowanie danych do macierzy korelacji
library(tidyverse)
library(dlookr)
library(editrules)
library(VIM)
library(deducorrect)
library(ISLR) 

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

HR_factor_V2 <- HR %>%
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



#wiczenie 1. Użyj imputacji [kNN] (pakiet „VIM”), aby imputować wszystkie brakujące wartośc
mydata_knn <- kNN(mydata)
petal_width_kNN <- mydata_knn$Petal.Width

#Ćwiczenie 2. Użyj imputacji [sequential hotdeck] do imputacji Petal.Width poprzez sortowanie zbioru danych po Species. Porównaj imputowaną wartość Petal.Width z sekwencyjną metodą imputacji hotdeck. Zwróć uwagę na kolejność danych!
seqImpute <- function(x, last=max(x, na.rm=TRUE)){
  n <- length(x)
  x <- c(x, last)
  i <- is.na(x)
  while (any(i)){
    x[i] <- x[which(i)+1]
    i <- is.na(x)
  }
  x[1:n]
}

o <- order(mydata$Species)
petal_width <- mydata$Petal.Width[o]
petal_width_hd <- seqImpute(petal_width)

summary(petal_width_kNN[o] - petal_width_hd)

Pakiet „dlookr” oferuje szereg metod do ich obsługi:
  
  find_na() - znajdź zmienną, która zawiera brakującą wartość w obiekcie dziedziczącym po data.frame lub data.frame.
imputate_na() - brakujące wartości są imputowane z pewnymi reprezentatywnymi wartościami i metodami statystycznymi.
summary.imputation() i plot.imputation() - metody drukowania i podsumowania dla klasy „imputation”.
find_skewness() - może znaleźć zmienną numeryczną, która jest skośna, która dziedziczy po data.frame lub data.frame.
find_outliers() - może znaleźć zmienną numeryczną, która zawiera wartości odstające w obiekcie dziedziczącym po data.frame lub data.frame.
transform() - wykonuje transformację zmiennej w celu standaryzacji i usunięcia skośności zmiennych numerycznych.
summary.transform() i plot.transform() - metody drukowania i podsumowania dla klasy „transform”.
binning() i binning_by() konwertują zmienną numeryczną na zmienną kategoryzacyjną.
transformation_report() - raportuje informacje o transformacji zmiennych numerycznych dla obiektów dziedziczących z data.frame.

Przyjrzyjmy się jednemu przykładowi ze zbiorem danych „carseats”, w którym spróbujemy obsłużyć brudne dane za pomocą tych funkcji.

#Zaimputujmy NA dla zmiennej „Dochód” z medianą.
Income1<- imputate_na(carseats, Income, Urban, method = "median")
summary(Income1)
plot(Income1)