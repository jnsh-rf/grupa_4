library(readr)
library(tidyverse)
HR <- read_csv("HR.csv")
# View(HR)

install.packages("naniar")
install.packages("visdat")
library(naniar)
library(visdat)

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
