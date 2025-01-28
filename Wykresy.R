#Wykresy
install.packages("viridisLite") # dodano do głównej funkcji
install.packages("RColorBrewer") # dodano do głównej funkcji
install.packages("circlize")
install.packages("plotly")
install.packages("ggplot2") # dodano do głównej funkcji w formie tidyverse
#install.packages("circlize")
library(viridis)
library(RColorBrewer)
library(ggplot)
library(plotly)
library(circlize)
library(ggplot2)
library(dplyr)
library(circlize)


#Odejścia w zależności od grup wiekowych

ggplot(HR_imputowane, aes(x=Age, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set2") +
  ggtitle("Odejścia w zależności od grup wiekowych") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "Age") 


#Wykres wskazuję, iż większość pracowników odchodzący z pracy jest w wieku 28-36 lat. 
#Osoby w młodym wieku, częściej zmieniają pracę.


summary(HR_imputowane$Age)  # Średnia i zakres wieku
table(HR_imputowane$Age, HR_imputowane$Attrition)  # Liczba odejść w każdej grupie wiekowej

# Kom: Najwięcej osób odchodzi w wieku 28-33, nie odchodzi 27-42

table_age_attrition <- table(HR_imputowane$AgeGroup, HR_imputowane$Attrition)
chisq.test(table_age_attrition)


HR_imputowane$AgeGroup <- cut(HR_imputowane$Age, breaks = c(20, 30, 40, 50, 60, 70), 
                              labels = c("20-29", "30-39", "40-49", "50-59", "60+"), right = FALSE)


#Stan cywilny a rotacja pracowników

HR_imputowane %>% group_by(Attrition, MaritalStatus) %>% summarize(N = n()) %>% mutate(countT = sum(N)) %>%
  group_by(Attrition, MaritalStatus, add=TRUE) %>% mutate(per=paste0(round(100*N/countT,1),'%')) %>%
  ggplot(aes(x=Attrition, y=N, fill=MaritalStatus)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw() + 
  scale_fill_brewer(palette="Purples") +
  geom_text(aes(label = per), size = 4, vjust = 1.2, color = "black", position = position_dodge(0.9)) + 
  ggtitle("Stan cywilny a rotacja pracowników") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Attrition", y = "Count")



HR_imputowane %>% 
  group_by(Attrition, MaritalStatus) %>% 
  summarize(N = n()) %>% 
  mutate(countT = sum(N)) %>%
  group_by(Attrition, MaritalStatus, add = TRUE) %>%
  mutate(per = paste0(round(100 * N / countT, 1), '%')) %>%
  ggplot(aes(x = Attrition, y = N, fill = MaritalStatus)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  theme_bw() + 
  scale_fill_brewer(palette = "Purples") +
  geom_text(aes(label = per), size = 4, vjust = 1.2, color = "black", position = position_dodge(0.9)) + 
  ggtitle("Stan cywilny a odejścia pracowników") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Rotacja", y = "Liczba pracowników")

table(HR_imputowane$MaritalStatus, HR_imputowane$Attrition)




#Średni miesięczny dochód według stanowiska
HR_imputowane %>%
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


summary(HR_imputowane$MonthlyIncome)


#Rozkład płci w poszczególnych działach

Employees <- HR_imputowane %>%
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


prop.table(table(HR_imputowane$Gender, HR_imputowane$Department), margin = 2)

"Histogram wieku z podziałem na płeć"

ggplot(HR_imputowane, aes(x = Age, fill = Gender)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black", alpha = 0.7) +
  facet_wrap(~Gender) +
  labs(title = "Histogram wieku z podziałem na płeć", x = "Wiek", y = "Liczba pracowników") +
  theme_minimal()

#  theme_bw()



"Rozkład dochodów w zależności od przedziału wiekowego"


HR_imputowane$AgeGroup <- cut(HR_imputowane$Age, breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf), 
                              labels = c("0-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-100", "100+"), 
                              right = FALSE)

#Pudełkowy
ggplot(HR_imputowane, aes(x = AgeGroup, y = MonthlyIncome)) + 

  geom_boxplot(fill = "#4C79A1", color = "black") + 
  labs(x = "Przedział wiekowy", y = "Dochód miesięczny", title = "Rozkład dochodów w zależności od przedziału wiekowego") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14)
  )

  geom_boxplot(fill = "pink", color = "black") + 
  labs(x = "Przedział wiekowy", y = "Dochód miesięczny", title = "Rozkład dochodów w zależności od przedziału wiekowego") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14))


#Słupkowy

HR_imputowane %>%
  group_by(AgeGroup) %>%
  summarize(mean_income = mean(MonthlyIncome, na.rm = TRUE)) %>%
  ggplot(aes(x = AgeGroup, y = mean_income, fill = AgeGroup)) + 
  geom_bar(stat = "identity", show.legend = FALSE, color = "gray") +
  scale_fill_brewer(palette = "Blues") + 

  labs(x = "Przedział wiekowy", y = "Średni dochód (USD)", title = "Średni dochód w zależności od przedziału wiekowego") +

#  labs(x = "Przedział wiekowy", y = "Średni dochód", title = "Średni dochód w zależności od przedziału wiekowego") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14)
  )


income_summary <- HR_imputowane %>%
  group_by(AgeGroup) %>%
  summarize(
    Mean = round(mean(MonthlyIncome, na.rm = TRUE), 2),
    Median = round(median(MonthlyIncome, na.rm = TRUE), 2),
    Min = min(MonthlyIncome, na.rm = TRUE),
    Max = max(MonthlyIncome, na.rm = TRUE)
  )

# Wyświetlanie wyników
print(income_summary)


#Liczba pracowników w podziale na role

ggplot(HR_imputowane, aes(x = reorder(JobRole, -table(JobRole)[JobRole]), fill = JobRole)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Liczba pracowników w podziale na role", x = "Rola", y = "Liczba pracowników") +
  scale_fill_brewer(palette = "RdPu")+
  theme_minimal() +
  theme(legend.position = "none")


install.packages("plotly")
library(plotly)


"Średni miesięczny dochód w podziale na dział i rolę"
heatmap_data <- HR_imputowane %>%
  group_by(Department, JobRole) %>%
  summarise(AvgIncome = mean(MonthlyIncome, na.rm = TRUE))

ggplot(heatmap_data, aes(x = Department, y = JobRole, fill = AvgIncome)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Średni miesięczny dochód w podziale na dział i rolę", 
       x = "Dział", y = "Rola", fill = "Średni dochód") +
  theme_minimal()





"Rozkład lat pracy w firmie w podziale na dział"
ggplot(HR_imputowane, aes(x = Department, y = YearsAtCompany, fill = Department)) +
  geom_violin(trim = FALSE, alpha = 0.8, color = "black") +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black", alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE, option = "F") +
  labs(title = "Rozkład lat pracy w firmie w podziale na dział", 
       x = "Dział", y = "Lata pracy", fill = "Dział") +
  theme_minimal() +
  theme(legend.position = "none")




# Odejścia a zarobki

ggplot(HR_imputowane, aes(x = MonthlyIncome, fill = Attrition, color = Attrition)) +
  geom_density(alpha = 0.5) +  # Usunięto `position="identity"` jako zbędne w density
  theme_minimal() +  # Zmieniono styl na bardziej nowoczesny
  scale_fill_brewer(palette = "Set1", name = "Attrition") +  # Dodano nazwę legendy
  scale_color_brewer(palette = "Set1", guide = "none") +  # Dopasowano kolor linii do wypełnienia, ukryto dodatkową legendę
  ggtitle("Rozkład odejść w zależności od zarobków") + 
  labs(x = "Dochód miesięczny", y = "Gęstość", fill = "Attrition") +  # Poprawiono etykiety osi
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "bottom"
  )


