library(XML)

# Функция для загрузки и преобразования данных с веб-страницы
load_and_convert_weather_data <- function(url) {
  tables <- readHTMLTable(url)  # Читаем таблицы с веб-страницы
  
  # Первая таблица содержит годы, а вторая - месячные данные и среднегодовые данные
  years <- tables[[1]][, 1]  # Годы из первой таблицы
  
  # Данные за год из второй таблицы (столбцы 1-9)
  monthly_data <- tables[[2]][, 1:9]
  
  # Преобразование только с1 по 9 столбцы второй таблицы
  monthly_data <- suppressWarnings(as.data.frame(lapply(monthly_data, as.numeric)))
  monthly_data[monthly_data > 999] <- NA
  
  return(data.frame(Year = years, Monthly_Data = monthly_data))
}

# Загрузка и преобразование данных только для города Дрезден 
Dresden_url <- "http://www.pogodaiklimat.ru/history/10488.htm"
Dresden_data <- load_and_convert_weather_data(Dresden_url)

# Отфильтровать данные с 2015 по 2024 год
Dresden_data <- Dresden_data[Dresden_data$Year >= 2015 & Dresden_data$Year <= 2024, ]

# Объединение данных в одну таблицу 
weather_report <- Dresden_data$Year
for (i in 1:9) {
  weather_report <- cbind(weather_report, Dresden_data$Monthly_Data[, i])
}

# Переименование столбцов
colnames(weather_report) <- c("Year", "January", "February", "March", "April", "May", "June", "July", "August", "September")

# Вывод отчета
print(weather_report)

