library(XML)

# Загрузка данных с веб-страницы
url <- "http://www.pogodaiklimat.ru/history/10488.htm"
table <- readHTMLTable(url, which = 2)

# Преобразование данных новая версия!
table <- sapply(table, function(x) {
  # Преобразование в числовой формат игнорим непреобразуемые значений
  numeric_values <- suppressWarnings(as.numeric(as.character(x)))
  # Замена всех значений, больших 999 на NA
  numeric_values[numeric_values > 999] <- NA
  return(numeric_values)
})
library(ggplot2)

# Фильтр для проверки данных на корректность
correct_data <- complete.cases(table[, 13])  # Индексы строк с корректными данными 

# Выбираем только корректные данные
temperatures <- table[correct_data, 13]

# Рассчитываем доверительный интервал
confidence_interval <- t.test(temperatures, conf.level = 0.94)$conf.int

# Вывод
print(confidence_interval)

# Строим график
ggplot() +
  geom_point(aes(x = 1, y = mean(temperatures)), color = "blue") +
  geom_errorbar(aes(x = 1, ymin = confidence_interval[1], ymax = confidence_interval[2]), width = 0.1, color = "red") +
  annotate("text", x = 1, y = mean(temperatures), label = round(mean(temperatures), 2), vjust = -1, hjust = -0.5, color = "blue") +
  annotate("text", x = 1, y = confidence_interval[1], label = round(confidence_interval[1], 2), vjust = -1, hjust = -0.5, color = "red") +
  annotate("text", x = 1, y = confidence_interval[2], label = round(confidence_interval[2], 2), vjust = -1, hjust = -0.5, color = "red") +
  labs(x = "", y = "Среднегодовая температура", title = "Доверительный интервал для среднегодовой температуры") +
  theme_minimal()
# Значения для проверки
values_to_check <- c(0, 10, 50)

# Проверяем каждое значение
for (value in values_to_check) {
  if (value >= confidence_interval[1] && value <= confidence_interval[2]) {
    cat("Среднегодовая температура", value, "градусов по Цельсию содержится в доверительном интервале.\n")
  } else {
    cat("Среднегодовая температура", value, "градусов по Цельсию НЕ содержится в доверительном интервале.\n")
  }
}
# Вычисляем выборочное стандартное отклонение среднегодовой температуры
sample_sd <- sd(temperatures)

# Значения для проверки
values_to_check <- c(1, 2, 3)

# Проводим тест хи-квадрат для каждого значения
for (value in values_to_check) {
  # Вычисляем статистику хи-квадрат
  chi_sq_stat <- ((length(temperatures) - 1) * sample_sd^2) / value^2
  
  # Вычисляем p-value
  p_value <- 1 - pchisq(chi_sq_stat, df = length(temperatures) - 1)
  
  # Выводим результаты
  cat("Значение стандартного отклонения", sample_sd, "не превышает", value, "градусов по Цельсию:\n")
  cat("Статистика хи-квадрат:", chi_sq_stat, "\n")
  cat("p-value:", p_value, "\n")
  cat("\n")
}
# Задаем вектор количества дней в каждом месяце сезона
days_in_month <- c(30, 31, 30)  # В моём случае в каждом месяце 30 или 31 день

# Создаем пустой вектор для хранения среднесезонных температур по годам
ast_yearly <- numeric(nrow(table))

# Рассчитываем среднесезонную температуру для каждого года
for (i in 1:nrow(table)) {
  # Берем данные из 9, 10, 11 столбцов (сен, окт, ноя) для текущего года
  season_temperatures <- table[i, c(9, 10, 11)]
  # Фильтруем значения NA
  season_temperatures <- season_temperatures[!is.na(season_temperatures)]
  # Если в году остались значения
  if (length(season_temperatures) > 0) {
    # Рассчитываем средневзвешенную температуру для текущего года
    ast_yearly[i] <- sum(season_temperatures * days_in_month) / sum(days_in_month)
  } else {
    # Если все значения NA, присваиваем NA (текущий год)
    ast_yearly[i] <- NA
  }
}

# Рассчитываем среднюю сезонную температуру за весь период наблюдений игноря NA
ast_season_total <- mean(ast_yearly, na.rm = TRUE)

# Выводим результат округлив до 2-х знаков
cat("Полученное значение среднесезонной температуры за весь период наблюдений:", round(ast_season_total, 2))
# Извлекаем данные о температуре за сентябрь
september_temperatures <- table[, 9]

# Фильтруем значения NA
september_temperatures <- september_temperatures[!is.na(september_temperatures)]

# Считаем среднюю температуру за сентябрь
mean_september_temperature <- mean(september_temperatures)

# Выводим результат
cat("Средняя температура за сентябрь:", mean_september_temperature, "\n")

# Аналогично для остальных месяцев

# Извлекаем данные о температуре за октябрь
october_temperatures <- table[, 10]
october_temperatures <- october_temperatures[!is.na(october_temperatures)]
mean_october_temperature <- mean(october_temperatures)
cat("Средняя температура за октябрь:", mean_october_temperature, "\n")

# Извлекаем данные о температуре за ноябрь
november_temperatures <- table[, 11]
november_temperatures <- november_temperatures[!is.na(november_temperatures)]
mean_november_temperature <- mean(november_temperatures)
cat("Средняя температура за ноябрь:", mean_november_temperature, "\n")
t_test_september <- t.test(september_temperatures, mu = 9.4, alternative = "less")
print(t_test_september)
t_test_october <- t.test(october_temperatures, mu = 9.4, alternative = "two.sided")
print(t_test_october)
t_test_november <- t.test(november_temperatures, mu = 9.4, alternative = "less")
print(t_test_november)
# Загружаем библиотеку для работы с ANOVA
library(stats)

# Создаем датафрейм с данными о температуре за каждый месяц
temperatures <- data.frame(
  Month = rep(c("September", "October", "November"), each = length(september_temperatures)),
  Temperature = c(september_temperatures, october_temperatures, november_temperatures)
)

# Выполняем дисперсионный анализ
anova_result <- aov(Temperature ~ Month, data = temperatures)

# Выводим результаты анализа
print(summary(anova_result))

# Применяем поправку Бонферрони к уровню значимости
alpha <- 0.06
alpha_bonferroni <- alpha / 3  # 3 - количество сравнений (месяцев)
print(alpha_bonferroni)