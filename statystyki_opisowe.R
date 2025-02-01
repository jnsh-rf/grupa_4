#statystyka opisowa

mean(HR_imputowane$im_att_mice)
median(HR_imputowane$im_att_mice)
sd(HR_imputowane$im_att_mice) #standard deviation
var(HR_imputowane$im_att_mice) #variance
coeff_var<-sd(price_PLN)/mean(price_PLN) #coefficient of variability %
coeff_var
IQR(price_PLN)# difference between quartiles =Q3-Q1 
sx<-IQR(price_PLN)/2  #interquartile deviation
coeff_varx<-sx/median(price_PLN) #IQR coefficient of variability %
coeff_varx
min(price_PLN)
max(price_PLN)
quantile(price_PLN,probs=c(0,0.1,0.25,0.5,0.75,0.95,1),na.rm=TRUE)


install.packages("summarytools")
install.packages("ggstatsplot")
library(ggstatsplot)
library(summarytools)

#tabela odchodzenie i nadgodziny
ctable(x = HR_imputowane$Attrition, 
       y = HR_imputowane$OverTime, 
       prop = "r")



#tabela statystyki opisowe zmiennych numerycznych
HR_filtered<- HR_imputowane[,!names(HR_imputowane)%in% c("AgeGroup", "im_moi_knn",
                            "im_att_mice") ]
stat_op_zm_num <- descr(HR_filtered)
view(dfSummary(HR_filtered))

#wykres wiolinowy ze statystykami
ggbetweenstats(y = MonthlyIncome, 
               x = Attrition, 
               data = HR_imputowane)
#-------------------------------------------------------------------------------
install.packages("ggstatsplot")
library(ggstatsplot)
library(ggplot2)

# Test chi-kwadrat: Zależność Attrition od Department
# Czy odejścia pracowników zależą od działu, w którym pracują?

ggbarstats(data = HR_filtered, x = Department, y = Attrition, title = "odejścia w zależności od działu firmy")



#Test t-Studenta: Różnice w zarobkach między osobami, które odeszły i zostały
#Czy pracownicy, którzy odchodzą, zarabiali mniej?

ggbetweenstats(data = HR_filtered, x = Attrition, y = MonthlyIncome,  
               title = "miesięczny dochód a odejścia z firmy")



# ANOVA: Czy długość pracy w firmie różni się między działami?
# Czy lata pracy zależą od działu?
ggbetweenstats(data = HR_filtered, x = Department, y = YearsAtCompany)



# Sprawdzenie korelacji między YearsAtCompany a MonthlyIncome
# Czy osoby z dłuższym stażem zarabiają więcej?
ggscatterstats(data = HR_filtered, x = YearsAtCompany, y = MonthlyIncome) +
  scale_color_manual(values = c("#1B4F72", "#A3E4D7")) +  # Głębokie odcienie niebieskiego i zielonego
  theme_minimal()



#Struktura odejść w różnych działach
#Jaki procent pracowników odchodzi z każdego działu?
ggpiestats(data = HR_filtered, x = Department, y = Attrition) +
  scale_fill_manual(values = c("#154360", "#1D8348", "#D4AC0D", "#935116")) +
  theme_minimal()



#Regresja logistyczna dla Attrition
#Jakie czynniki przewidują odejście z firmy?

ggcoefstats(glm(im_att_mice ~ Age + MonthlyIncome + YearsAtCompany + DistanceFromHome, 
                data = HR_imputowane, family = binomial)) +
  scale_fill_manual(values = c("#1D8348", "#935116", "#154360", "#D4AC0D")) + 
  theme_minimal()

#tylko tutaj trzeba chyba przekodować imputowane attrition na 0 i 1, bo jest 1 i 2


