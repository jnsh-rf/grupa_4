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

