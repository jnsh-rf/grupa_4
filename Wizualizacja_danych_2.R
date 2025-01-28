#sum(is.na(HR_imputowane))
#sum(is.na(fixable_data))

### wykres do wizualizacji braków
VIM::aggr(HR)
# alternatywa dla
# missplot_finalfit <- HR %>% missing_pattern("Age", explain_u)



#wszystkie brakujące obserwacje - brakuje 197
n_miss(HR)
#udział brakujących w całości - 0.0116035
prop_miss(HR)
#tabela podsumowująca braki dla wszystkich zmiennych
#brak_wart2 <- tibble(miss_var_summary(fixable_data))
#brak_wart2
#mamy 441 rzedów z jednym brakiem, 72 rzedy z dwoma brakami oraz 4 z trzema brakami
miss_case_table(HR)


#wizualizacja braków
vis_miss(HR)
vis_dat(HR) # zrobić to po danych z wprowadzonymi brakami


#vis_miss(HR, cluster = TRUE, sort_miss = TRUE)

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
#install.packages("viridis")
#install.packages("RColorBrewer")
#library(viridis)
#library(RColorBrewer)

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


#library(ggplot2)
#library(dplyr)
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
