################ Modyfikacja macierzy korelacji
paleta_korelacja <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

ggcorrplot(macierz_kor_numeric,
           # lab = TRUE, 
           # type = "upper"
           # hc.order = TRUE,
           # hc.method = "",
           insig = "blank",
           lab_size = 3, 
           method="square", 
           colors = c("#BB4444","white", "#4477AA"), 
           ggtheme=theme_minimal(),
           legend.title = "Korelacja")

ggcorrplot(macierz_kor_numeric,
           lab = TRUE, 
           # type = "upper"
           hc.order = TRUE,
           hc.method = "",
           insig = "blank",
           lab_size = 3, 
           method="square", 
           colors = c("#007ab3", "white", "tomato2"), 
           title="Correlogram Employee Attritions", 
           ggtheme=theme_minimal(),
           legend.title = "Korelacja")

library(corrplot)
corrplot(cor(macierz_kor_numeric), method = "square", order = "hclust",
         addrect = 3, tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("tomato2", "white", "#01A9DB"))(200))


corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45)
# Generate a lighter palette
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(200), addCoef.col = "black", cl.pos = "n", order = "AOE")

# Create a correlogram with larger dimensions
ggcorrplot(macierz_kor_numeric, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 2.5, 
           method = "square", 
           colors = c("tomato2", "white", "#01A9DB"), 
           title = "Korelogram Atrycji Pracowników", 
           ggtheme = theme_minimal() + 
             theme(axis.text.x = element_text(angle = 45, hjust = 1))) +
  theme(plot.title = element_text(size = 14, face = "bold"))

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

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





#przygotowanie danych do macierzy korelacji
#library(tidyverse)
#library(dlookr)
#library(editrules)
#library(VIM)
#library(deducorrect)
#library(ISLR) 

#sprawdzamy unikatowe wartości zmiennych "tekstowych"
#all_char_values <- list(
#  val_1 = table(HR$Attrition),
#  val_2 = table(HR$BusinessTravel),
#  val_3 = table(HR$Department),
#  val_4 = table(HR$EducationField),
#  val_5 = table(HR$Gender),
#  val_6 = table(HR$JobRole),
#  val_7 = table(HR$MaritalStatus),
#  val_8 = table(HR$Over18),
#  val_9 = table(HR$OverTime)
#)

#uproszczenie listy all_char_values
#kategorie <- lapply(all_char_values, names)

#HR_factor_V2 <- HR %>%
#  mutate(
#   Attrition = factor(Attrition, levels = kategorie$val_1), 
#    BusinessTravel = factor(BusinessTravel, levels = kategorie$val_2),
#    Department = factor(Department, levels = kategorie$val_3),
#    EducationField = factor(EducationField, levels = kategorie$val_4),
#    Gender = factor(Gender, levels = kategorie$val_5),
#    JobRole = factor(JobRole, levels = kategorie$val_6),
#   MaritalStatus = factor(MaritalStatus, levels = kategorie$val_7),
#   Over18 = factor(Over18, levels = kategorie$val_8),
#    OverTime = factor(OverTime, levels = kategorie$val_9)
#    )

#weryfikacja - w razie czego odkomentować
#str(HR_factor_V2)



#wiczenie 1. Użyj imputacji [kNN] (pakiet „VIM”), aby imputować wszystkie brakujące wartośc
#mydata_knn <- kNN(mydata)
#petal_width_kNN <- mydata_knn$Petal.Width

#Ćwiczenie 2. Użyj imputacji [sequential hotdeck] do imputacji Petal.Width poprzez sortowanie zbioru danych po Species. Porównaj imputowaną wartość Petal.Width z sekwencyjną metodą imputacji hotdeck. Zwróć uwagę na kolejność danych!
#seqImpute <- function(x, last=max(x, na.rm=TRUE)){
#  n <- length(x)
#  x <- c(x, last)
#  i <- is.na(x)
#  while (any(i)){
#   x[i] <- x[which(i)+1]
#   i <- is.na(x)
#  }
#  x[1:n]
#}

#o <- order(mydata$Species)
#petal_width <- mydata$Petal.Width[o]
#petal_width_hd <- seqImpute(petal_width)

#summary(petal_width_kNN[o] - petal_width_hd)



#Przyjrzyjmy się jednemu przykładowi ze zbiorem danych „carseats”, w którym spróbujemy obsłużyć brudne dane za pomocą tych funkcji.

#Zaimputujmy NA dla zmiennej „Dochód” z medianą.
#Income1<- imputate_na(carseats, Income, Urban, method = "median")
#summary(Income1)
#plot(Income1)










