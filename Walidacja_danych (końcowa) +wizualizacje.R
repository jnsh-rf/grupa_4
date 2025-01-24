install.packages("arules")
library(arules)
rules <- validator(
  Age >= 18 & Age <= 80 & Age >= (18 + TotalWorkingYears) & is.numeric(Age), #Wiek nie może być niższy niż lata pracy osoby pełnoletniej
  DistanceFromHome >= 0 & is.numeric(DistanceFromHome), 
  DailyRate > 0 & is.numeric(DailyRate), 
  # EmployeeCount == 1 & is.numeric(EmployeeCount), # usuń przenosząc do raportu  
  HourlyRate > 0 & is.numeric(HourlyRate), 
  MonthlyIncome > 0 & is.numeric(MonthlyIncome), 
  MonthlyRate > 0 & is.numeric(MonthlyRate), 
  NumCompaniesWorked >= 0 & is.numeric(NumCompaniesWorked), 
  # PercentSalaryHike >= 0 & is.numeric(PercentSalaryHike), # zastąpione regułami warunkowymi - usuń przenosząc do raportu 
  # StandardHours == 80 & is.numeric(StandardHours), # usuń przenosząc do raportu 
  TotalWorkingYears >= 0 & is.numeric(TotalWorkingYears), 
  TrainingTimesLastYear >= 0 & TrainingTimesLastYear <= 6 & is.numeric(TrainingTimesLastYear), # poprawa błędnej nazwy zmiennej, zmiana na szkolenie <= 6 (raz na 2 m-ce) 
  YearsAtCompany >= 0 & YearsAtCompany < 60 & 
    (YearsAtCompany >= YearsInCurrentRole | 
       YearsAtCompany >= YearsSinceLastPromotion | 
       YearsAtCompany >= YearsWithCurrManager) & is.numeric(YearsAtCompany), 
  # Lata w firmie nie mogą być krótsze niż czas od ostatniego awansu, lata pracy z tym samym menedżerem lub lata na obecnym stanowisku.
  TotalWorkingYears >= 0 & is.numeric(TotalWorkingYears), 
  YearsInCurrentRole >= 0 & YearsInCurrentRole < 60 & is.numeric(YearsInCurrentRole), 
  YearsSinceLastPromotion >= 0 & is.numeric(YearsSinceLastPromotion), 
  YearsWithCurrManager >= 0 & is.numeric(YearsWithCurrManager), 
  
  # reguły warunkowe
  if (PerformanceRating == 1) PercentSalaryHike == 0,
  if (PerformanceRating == 2) PercentSalaryHike >= 1 & PercentSalaryHike <= 10,
  if (PerformanceRating == 3) PercentSalaryHike >= 11 & PercentSalaryHike <= 19,
  if (PerformanceRating == 4) PercentSalaryHike >= 20,
  if (Department == "Sales") JobRole %in% c("Sales Executive", "Sales Representative", "Manager"),
  if (Department == "Research & Development") JobRole %in% c("Research Scientist", "Laboratory Technician", "Research Director", "Manufacturing Director", "Healthcare Representative", "Manager"),
  if (Department == "Human Resources") JobRole %in% c("Human Resources", "Manager"),
  if (TotalWorkingYears > YearsAtCompany) NumCompaniesWorked > 0
  # Zmienne kategoryczne
  # Attrition %in% c("Yes", "No"), 
  # BusinessTravel %in% c("Travel_Rarely", "Travel_Frequently", "Non-Travel"), 
  # Department %in% c("Sales", "Research & Development", "Human Resources"), 
  # Education %in% 1:5 & is.numeric(Education), 
  # EducationField %in% c("Life Sciences", "Medical", "Marketing", "Technical Degree", "Human Resources", "Other"), 
  # EnvironmentSatisfaction %in% 1:4 & is.numeric(EnvironmentSatisfaction), 
  # Gender %in% c("Male", "Female"), 
  # JobInvolvement %in% 1:4 & is.numeric(JobInvolvement), 
  # JobLevel %in% 1:5 & is.numeric(JobLevel), 
  # JobRole %in% c("Sales Executive", "Research Scientist", "Laboratory Technician", 
  #                "Manufacturing Director", "Healthcare Representative", 
  #                "Manager", "Sales Representative", "Research Director", 
  #                "Human Resources"), 
  # JobSatisfaction %in% 1:4 & is.numeric(JobSatisfaction), 
  # MaritalStatus %in% c("Single", "Married", "Divorced"), 
  # Over18 == "Y", ############## usuń przenosząc do raportu 
  # OverTime %in% c("Yes", "No"), 
  # PerformanceRating %in% 1:4 & is.numeric(PerformanceRating), 
  # RelationshipSatisfaction %in% 1:4 & is.numeric(RelationshipSatisfaction), 
  # WorkLifeBalance %in% 1:4 & is.numeric(WorkLifeBalance), 
  # StockOptionLevel %in% 0:3 & is.numeric(StockOptionLevel)
)
out <- confront(HR_imputowane, rules)


summary(out)
plot(out, main="HR_imputowane")

inspect(i)


### uproszczenie zasad
rules <- simplify_rules(rules)

### lokalizacja błędów
error_locations <- locate_errors(HR_imputowane, rules)
values(error_locations)[1:1470, 1:35] # zmniejsz zakres df do pokazania
summary(error_locations)

### zamiana błędów na NA
fixable_data <- replace_errors(HR_imputowane, rules)
# suma NA w oryginalnym zbiorze vs stworzone z błędów
sum(is.na(HR_imputowane))
sum(is.na(fixable_data))

### wykres do wizualizacji braków
VIM::aggr(fixable_data)
# alternatywa dla
# missplot_finalfit <- HR %>% missing_pattern("Age", explain_u)



#wszystkie brakujące obserwacje - brakuje 197
n_miss(fixable_data)
#udział brakujących w całości - 0.0116035
prop_miss(fixable_data)
#tabela podsumowująca braki dla wszystkich zmiennych
brak_wart2 <- tibble(miss_var_summary(fixable_data))
brak_wart2
#mamy 441 rzedów z jednym brakiem, 72 rzedy z dwoma brakami oraz 4 z trzema brakami
miss_case_table(fixable_data)


#wizualizacja braków
vis_miss(fixable_data)
vis_dat(fixable_data)


vis_miss(fixable_data, cluster = TRUE, sort_miss = TRUE)

hr_knn2 <- VIM::kNN(HR_imputowane)

im_NumCompaniesWorked_knn <- hr_knn$NumCompaniesWorked
im_TotalWorkingYears_knn <- hr_knn$TotalWorkingYears
im_YearsAtCompany_knn <- hr_knn$YearsAtCompany

HR_final <- HR_imputowane %>%
  cbind(im_NumCompaniesWorked_knn, im_TotalWorkingYears_knn, im_YearsAtCompany_knn) %>%
  mutate(
    NumCompaniesWorked = im_NumCompaniesWorked_knn,
    TotalWorkingYears = im_TotalWorkingYears_knn,
    YearsAtCompany = im_YearsAtCompany_knn)



#Wykresy
install.packages("viridis")
install.packages("RColorBrewer")
library(viridis)
library(RColorBrewer)

#Odejścia w zależności od grup wiekowych

ggplot(HR_final, aes(x=Age, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set2") +
  ggtitle("Odejścia w zależności od grup wiekowych") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "Age") 

#Wykres wskazuję, iż większość pracowników odchodzący z pracy jest w wieku 28-36 lat. 
#Osoby w młodym wieku, częściej zmieniają pracę.

#Stan cywilny a rotacja pracowników

HR_final %>% group_by(Attrition, MaritalStatus) %>% summarize(N = n()) %>% mutate(countT = sum(N)) %>%
  group_by(Attrition, MaritalStatus, add=TRUE) %>% mutate(per=paste0(round(100*N/countT,1),'%')) %>%
  ggplot(aes(x=Attrition, y=N, fill=MaritalStatus)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw() + 
  scale_fill_brewer(palette="Purples") +
  geom_text(aes(label = per), size = 4, vjust = 1.2, color = "black", position = position_dodge(0.9)) + 
  ggtitle("Stan cywilny a rotacja pracowników") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Attrition", y = "Count")


library(ggplot2)
library(dplyr)
#Średni miesięczny dochód według stanowiska
HR_final %>%
select(JobRole, MonthlyIncome) %>%
  group_by(JobRole) %>%
  summarize(mean = round(mean(MonthlyIncome), 1)) %>%
  ggplot(aes(x = reorder(JobRole, -mean), y = mean, fill = mean)) + 
  geom_bar(stat = "identity", width = 0.9, color = "black") +
  geom_text(aes(label = mean), size = 4, vjust = 1.5, color = "white") +
  scale_fill_gradient(low = "#F2C1D1", high = "#D85F60") +
  theme_bw() +
  ggtitle("Średni miesięczny dochód według stanowiska") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  labs(x = "Job Role", y = "Monthly Income") +
  scale_y_continuous(labels = scales::comma)



#Rozkład płci w poszczególnych działach

Employees <- HR_final %>%
  mutate(
    AttritionDummy = ifelse(Attrition == "Yes", 1, 0),
    Over18Dummy = ifelse(Over18 == "Y", 1, 0),
    OverTimeDummy = ifelse(OverTime == "Yes", 1, 0),
    GenderDummy = ifelse(Gender == "Male", 1, 0)
  )
GenderDistribution <- Employees %>%
  group_by(Department) %>%
  summarize(MaleCount = sum(GenderDummy), MalePercentage= mean(GenderDummy) * 100, FemaleCount = n() - sum(GenderDummy), FemalePercentage = 100 - mean(GenderDummy) * 100)

View(GenderDistribution)


GenderDistributionLong <- GenderDistribution %>%
  select(Department,MalePercentage, FemalePercentage) %>%
  pivot_longer(
    cols = c(MalePercentage, FemalePercentage),
    names_to = "Gender",
    values_to = "Percentage"
  )


ggplot(data = GenderDistributionLong, aes(x = Department, y = Percentage, fill = Gender)) + 
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = round(Percentage, 1)), position = position_stack(vjust = 0.5), color = "white", size = 4) +
  labs(x = "Dział", y = "Procent (%)", title = "Rozkład płci w poszczególnych działach") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14)
  )
