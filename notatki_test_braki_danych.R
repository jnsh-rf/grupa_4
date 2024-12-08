#z pliku z mikro jesteś ifelse + factor,
#funkcja dla dużej korelacji ; >= 0,6 | <= -0,6 i wybierając nazwy kolumn spełniające warunki i wrzucając je do wektora a ten do tych na pdst których imputujemy

#HR_costam <- ifelse(HR$ZMIENNA == "YOGA", 1,
#                   ifelse(HR$ZMIENNA == "YOGA", 2,
#                         ifelse(HR$ZMIENNA == "YOGA", 3,4)

# age1 <- round(imputate_na(nazwazbioru, Age, Gender, method = "rpart")) lub też "mice"

# plot(age1) # sprawdza czy się pokrywają
# 
#
# ramka <- replace_na(nazwazbioru, age1)
# age_test <- ramka %>%
# mutate(Age = age1) %>%
# select(-age1)
#
# mcar_test() - rodział test statystyczny
#
# pokazać różnymi metodami jak to by się kształtował - hotdeck, inne i coś jeszcze, mice, rpart
# 
#dla macierzy korelacji zrobić rownames? i wykres przedstawiający korelację - im silniejsza tym ciemniejszy a dla braków danych czerwony
#wykres za pomocą ggcorrplot
#ggcorrplot(cor(macierz_kor_HR))
#
#jezeli przy obliczaniu korelacji bedzie wierzsz/kolumna z samymii NA to oznacza, zę wartość zmiennej była identyczna dla wszystkich obserwacji,
# co wynika ze wzoru na korelacje Pearsona, napisaćm żę uzyskane wyniki wynikjaą ze wzrou na wsp kor P, i może go wstawić, a że nie dzili się przez zeor to R radzi sobie z tym wstawiajac NA
#
#
#
#
#
