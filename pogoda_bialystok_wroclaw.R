library(httr)
library(jsonlite)
install.packages("tseries")
install.packages('segmented')
library(tseries)
library(segmented)

# Ustawienie lokalizacji na angielską
Sys.setlocale("LC_ALL","English")

# Pobieranie danych pogodowych/klimatycznych dla Wroclawia z API open-meteo
r <- GET("https://archive-api.open-meteo.com/v1/archive?latitude=51.06&longitude=17.02&start_date=1950-01-01&end_date=2022-12-31&daily=temperature_2m_max,temperature_2m_min,temperature_2m_mean,precipitation_sum,precipitation_hours,winddirection_10m_dominant&timezone=Europe%2FWarsaw",
         Accept = "application/json")
jsonRespText <- content(r, as = "text")
aux <- fromJSON(jsonRespText)

# Tworzenie ramki danych dla danych pogodowych Wroclawia (51.06 N, 17.02 E)
wroclaw <- data.frame(time = aux$daily$time,
                     t_2m_max = aux$daily$temperature_2m_max,
                     t_2m_min = aux$daily$temperature_2m_min,
                     t_2m_mean = aux$daily$temperature_2m_mean,
                     p_sum = aux$daily$precipitation_sum,
                     p_h = aux$daily$precipitation_hours,
                     w_d = aux$daily$winddirection_10m_dominant)

wroclaw$time <- as.Date(wroclaw$time)

# Pobieranie danych pogodowych/klimatycznych dla Bialegostoku z API open-meteo
r <- GET("https://archive-api.open-meteo.com/v1/archive?latitude=53.08&longitude=23.10&start_date=1950-01-01&end_date=2022-12-31&daily=temperature_2m_max,temperature_2m_min,temperature_2m_mean,precipitation_sum,precipitation_hours,winddirection_10m_dominant&timezone=Europe%2FWarsaw",
         Accept = "application/json")
jsonRespText <- content(r, as = "text")
aux <- fromJSON(jsonRespText)
 
# Tworzenie ramki danych dla danych pogodowych Bialegostoku (53.08 N, 23.10 E)
bialystok <- data.frame(time = aux$daily$time,
                       t_2m_max = aux$daily$temperature_2m_max,
                       t_2m_min = aux$daily$temperature_2m_min,
                       t_2m_mean = aux$daily$temperature_2m_mean,
                       p_sum = aux$daily$precipitation_sum,
                       p_h = aux$daily$precipitation_hours,
                       w_d = aux$daily$winddirection_10m_dominant)

bialystok$time <- as.Date(bialystok$time)

# Średnie dzienne temperatury
wb_mean_temp <- ks.test(wroclaw$t_2m_mean, bialystok$t_2m_mean, alternative = "t")

print(wb_mean_temp) #p-value < 2.2e-16, zatem sr. temp. powietrza we Wroclawiu i 
                    #Bialymstoku roznia sie zaczaco

# Suma opadow
wb_p_sum <- ks.test(wroclaw$p_sum, bialystok$p_sum, alternative = "t")
print(wb_p_sum) #p-value = 0.416, zatem opady sa podobne
# Godziny opadow
wb_p_h <- ks.test(wroclaw$p_h, bialystok$p_h, alternative = "t")
print(wb_p_h) #p-value = 0.7231, zatem liczba godzin opadow jest podobna

# Kierunek wiatru
wb_w_d <- ks.test(wroclaw$w_d, bialystok$w_d, alternative = "t")
print(wb_w_d) #p-value < 2.2e-16, zatem kierunek wiatru we Wroclawiu i Bialymstoku rozni sie znaczaco

