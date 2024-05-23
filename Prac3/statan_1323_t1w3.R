library(XML)

# Функция для загрузки и преобразования данных с веб-страницы
load_and_convert_weather_data <- function(url) {
  tables <- readHTMLTable(url)  # Читаем таблицы с веб-страницы
  
  # Первая таблица содержит годы, а вторая - месячные данные и среднегодовые данные
  years <- tables[[1]][, 1]  # Годы из первой таблицы
  
  # Данные за год из второй таблицы (13-й столбец)
  annual_data <- tables[[2]][, 13]
  
  # Преобразование только к 13-му столбцу второй таблицы
  annual_data <- suppressWarnings(as.numeric(as.character(annual_data)))
  annual_data[annual_data > 999] <- NA
  
  return(data.frame(Year = years, Annual_Data = annual_data))
}

# Загрузка и преобразование данных для каждого города
Dresden_url <- "http://www.pogodaiklimat.ru/history/10488.htm"
Murmansk_url <- "http://www.pogodaiklimat.ru/history/22113.htm"
Yulara_url <- "http://www.pogodaiklimat.ru/history/94462.htm"

Dresden_data <- load_and_convert_weather_data(Dresden_url)
Murmansk_data <- load_and_convert_weather_data(Murmansk_url)
Yulara_data <- load_and_convert_weather_data(Yulara_url)
# Объединение данных в одну таблицу
weather_report <- merge(Dresden_data, Murmansk_data, by = "Year", all = TRUE)
weather_report <- merge(weather_report, Yulara_data, by = "Year", all = TRUE)

# Переименование столбцов
colnames(weather_report)[-1] <- c("Dresden", "Murmansk", "Yulara")

# Вывод отчета
print(weather_report)


# Анализ описательной статистики
summary(weather_report)

# Установка графических параметров
par(mfrow=c(2, 3))  # 2 строки, 3 столбца графиков

# Гистограмма для Dresden
hist(weather_report$Dresden, main="Гистограмма: Дрезден", xlab="Температура")

# Диаграмма Q-Q для Dresden
qqnorm(weather_report$Dresden, main="Диаграмма Q-Q: Дрезден")
qqline(weather_report$Dresden)

# Гистограмма для Murmansk
hist(weather_report$Murmansk, main="Гистограмма: Мурманск", xlab="Температура")

# Диаграмма Q-Q для Murmansk
qqnorm(weather_report$Murmansk, main="Диаграмма Q-Q: Мурманск")
qqline(weather_report$Murmansk)

# Гистограмма для Yulara
hist(weather_report$Yulara, main="Гистограмма: Юлара", xlab="Температура")

# Диаграмма Q-Q для Yulara
qqnorm(weather_report$Yulara, main="Диаграмма Q-Q: Юлара")
qqline(weather_report$Yulara)

# Установим графические параметры
par(mfrow=c(3,1))

# График среднегодовой температуры для каждого города
plot(weather_report$Year, weather_report$Yulara, type="l", col="green", xlab="Год", ylab="Температура", main="Юлара")
plot(weather_report$Year, weather_report$Dresden, type="l", col="blue", xlab="Год", ylab="Температура", main="Дрезден")
plot(weather_report$Year, weather_report$Murmansk, type="l", col="red", xlab="Год", ylab="Температура", main="Мурманск")

# Логарифмическое преобразование данных Юлара
weather_report$Yulara_transformed <- log(weather_report$Yulara)

# Проверка нормальности преобразованных данных
shapiro_test_Yulara_transformed <- shapiro.test(weather_report$Yulara_transformed)

# Вывод результатов теста Шапиро-Уилка
print(shapiro_test_Yulara_transformed)
# Установка графических параметров
par(mfrow=c(2, 2))

# Построение Q-Q диаграммы
qqnorm(weather_report$Yulara_transformed, main = "Q-Q Plot для данных Yulara")
qqline(weather_report$Yulara_transformed)
# Гистограмма для Yulara
hist(weather_report$Yulara, main="Гистограмма: Юлара", xlab="Температура")

# Проведение теста на равенство дисперсий на основе F-критерия однофакторного анализа
test_result <- oneway.test(c(weather_report$Dresden, weather_report$Murmansk, weather_report$Yulara) ~ rep(c("Dresden", "Murmansk", "Yulara"), c(length(weather_report$Dresden), length(weather_report$Murmansk), length(weather_report$Yulara))))

# Вывод результатов теста
print(test_result)

kruskal_test_result <- kruskal.test(list(weather_report$Dresden, weather_report$Murmansk, weather_report$Yulara))
print(kruskal_test_result)

# Удаление строк с отсутствующими значениями
weather_report <- na.omit(weather_report)

# Выполнение однофакторного ANOVA для каждого города
anova_dresden <- aov(Dresden ~ Year, data = weather_report)
anova_murmansk <- aov(Murmansk ~ Year, data = weather_report)
anova_yulara <- aov(Yulara ~ Year, data = weather_report)

# Выполнение процедуры Тьюки-Крамера для попарного сравнения средних значений между городами
tukey_dresden <- TukeyHSD(anova_dresden)
tukey_murmansk <- TukeyHSD(anova_murmansk)
tukey_yulara <- TukeyHSD(anova_yulara)

# Вывод результатов для города Dresden
print("Тьюки-Крамер для Dresden:")
print(tukey_dresden)

# Вывод результатов для города Murmansk
print("Тьюки-Крамер для Murmansk:")
print(tukey_murmansk)

# Вывод результатов для города Yulara
print("Тьюки-Крамер для Yulara:")
print(tukey_yulara)

# Построение графика для города Dresden
library(ggplot2)

# Преобразование результатов в датафрейм
tukey_df <- as.data.frame(tukey_dresden$Year)

# Построение графика средних значений температуры и доверительных интервалов
ggplot(tukey_df, aes(x = rownames(tukey_df), y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  labs(title = "Средние значения температуры и доверительные интервалы для Dresden",
       x = "Год",
       y = "Разница в средней температуре") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Для Мурманска
# Преобразование результатов в датафрейм
mur_df <- as.data.frame(tukey_murmansk$Year)

ggplot(mur_df, aes(x = rownames(mur_df), y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  labs(title = "Средние значения температуры и доверительные интервалы для Мурманска",
       x = "Год",
       y = "Разница в средней температуре") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

