#statystyka opisowa

install.packages("summarytools")
install.packages("ggstatsplot")
install.packages("ggExtra")
library(ggExtra)
library(ggstatsplot)
library(summarytools)
library(ggplot2)

#tabela odchodzenie i nadgodziny
ctable(x = HR_imputowane$Attrition, 
       y = HR_imputowane$OverTime, 
       prop = "r")

#tabela statystyki opisowe zmiennych numerycznych
HR_filtered<- HR_imputowane[,!names(HR_imputowane)%in% c("AgeGroup", "im_moi_knn",
                            "im_att_mice") ]
stat_op_zm_num <- descr(HR_filtered)
view(dfSummary(HR_filtered))

#-------------------------------------------------------------------------------

# Test chi-kwadrat: Zależność Attrition od JobRole
# Czy odejścia pracowników zależą od stanowiska, na którym pracują?

ggbarstats(data = HR_filtered, x = JobRole, y = Attrition, title = "odejścia w zależności od stanowiska")



#wykres wiolinowy, Test t-Studenta: Różnice w zarobkach między osobami, które odeszły i zostały
#Czy pracownicy, którzy odchodzą, zarabiali mniej?

ggbetweenstats(data = HR_filtered, x = Attrition, y = MonthlyIncome,  
               title = "odejścia z firmy a miesięczny dochód") +
  scale_color_manual(values = c("#219ebc", "#fb8500"))



# ANOVA: Czy długość pracy w firmie różni się między działami?
# Czy lata pracy zależą od działu?
ggbetweenstats(data = HR_filtered, x = Department, y = YearsAtCompany, 
               title = "staż w firmie a dział pracy")



# Sprawdzenie korelacji między YearsAtCompany a MonthlyIncome
# Czy osoby z dłuższym stażem zarabiają więcej?
ggscatterstats(data = HR_filtered, x = YearsAtCompany, y = MonthlyIncome)
 


#Struktura odejść w różnych działach
#Jaki procent pracowników odchodzi z każdego działu?
ggpiestats(data = HR_filtered, x = Department, y = Attrition,
           title =  "odejścia a dział firmy") +
  scale_fill_manual(values = c("#669bbc", "#c1121f", "#fdf0d5")) +
  theme_minimal()

