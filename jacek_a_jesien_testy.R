#jesli na jacka nie pada sucha jesien zapowiada
#"na Jacka" - wspomnienie sw. Jacka, czyli 17 sierpnia

library(httr)
library(jsonlite)
library(tseries)
library(segmented)

# Ustawienie lokalizacji na angielsk??
Sys.setlocale("LC_ALL","English")

# Pobieranie danych pogodowych/klimatycznych dla Krakowa z API open-meteo
r <- GET("https://archive-api.open-meteo.com/v1/archive?latitude=50.06&longitude=19.56&start_date=1950-01-01&end_date=2022-12-31&daily=temperature_2m_max,temperature_2m_min,temperature_2m_mean,precipitation_sum,precipitation_hours,winddirection_10m_dominant&timezone=Europe%2FWarsaw",
         Accept = "application/json")
jsonRespText <- content(r, as = "text")
aux <- fromJSON(jsonRespText)

# Tworzenie ramki danych dla danych pogodowych Krakowa
krakow <- data.frame(time = aux$daily$time,
                     t_2m_max = aux$daily$temperature_2m_max,
                     t_2m_min = aux$daily$temperature_2m_min,
                     t_2m_mean = aux$daily$temperature_2m_mean,
                     p_sum = aux$daily$precipitation_sum,
                     p_h = aux$daily$precipitation_hours,
                     w_d = aux$daily$winddirection_10m_dominant)

krakow$time <- as.Date(krakow$time)

# Wyodr??bnienie dnia, miesi??ca i roku z daty
day <- format(krakow$time, format = "%d")
month <- format(krakow$time, format = "%m")
year <- format(krakow$time, format = "%Y")

# Konwersja dnia, miesi??ca i roku na liczb??
krakow$day <- as.numeric(day)
krakow$month <- as.numeric(month)
krakow$year <- as.numeric(year)

# Wyodrebnienie 17 sierpnia
logic <- krakow$month == 8 & krakow$day == 17
jacek <- krakow$p_sum[logic]

# Wyodrebnienie jesieni 23 wrzesnia - 21 grudnia
logic <- ( krakow$month == 9 & krakow$day >= 23 ) | krakow$month == 10 | krakow$month == 11 | (krakow$month == 12 & krakow$day <= 21)
jesien_opady <- krakow$p_sum[logic]

#Jesien trwa 90 dni
#srednie opady dla kazdej jesieni
js=c()
for(i in 0:(length(jesien_opady)/90-1)){
  js<-append(js,sum(jesien_opady[(i*90+1):(i*90+90)]))
}
# "sucha jesien" przyjmujemy, ze oznacza to, ze suma opadow jest mniejsza od 1 kwartyla
sucho_granica<-quantile(js,0.25)
j<-js<sucho_granica

# "na jacka nie pada" -> opady sa rowne zero
jacek <- jacek == 0

#tworzenie macierzy ykontyngencji
ct<-table(jacek,j)

#Hipoteza zerowa: dane sa niezalenze
X <- chisq.test(ct)
X #p-value = 0.5369, prawdopodobienstwo popelnienia bledu odrzucajac H0 jest rowne 0,53
  #czyli bardzo duzo, zatem nie mozemy odrzucic hipotezy zerowej

#Wnioski: testy pokazaly, ze powiedzenie "Jesli na Jacka nie pada, sucha jesien sie zapowiada" mija sie z prawda (przynajmniej dla Krakowa).
