library(XML)
url <- "http://www.pogodaiklimat.ru/history/10488.htm"
table <- readHTMLTable(url, which = 2)
# Преобразование данных в таблице в числовой формат и замена 999.9 на NA
table[table == 999.9] <- NA
table[, -13] <- sapply(table[, -13], function(x) as.numeric(as.character(x)))
# Загрузка библиотеки
library(psych)
# Модифицированная функция describe
describe_with_quantiles <- function(x) {
  # Фильтрация данных: исключаем NA и нечисловые значения
  x <- as.numeric(x[!is.na(x) & grepl("^-?\\d+\\.?\\d*$", x)])
  desc <- describe(x)
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  cv <- sd(x) / mean(x)  # Коэффициент вариации
  result <- c(
    desc$n,
    desc$min,
    desc$max,
    q1,
    desc$median,
    q3,
    iqr,
    desc$mean,
    desc$sd,
    desc$se,
    cv,
    desc$skew
  )
  names(result) <- c(
    "n", "min", "max", "Q1", "median", "Q3", "IQR","mean", "sd", "se", "cv", "skew"
  )
  return(result)
}
# Применение модифицированной функции describe к каждому столбцу таблицы
month_descriptions_with_quantiles <- lapply(table[, 1:13], describe_with_quantiles)
# Преобразование списка в датафрейм
month_descriptions_with_quantiles_df <- do.call(rbind, month_descriptions_with_quantiles)
# Вывод таблицы
month_descriptions_with_quantiles_df
par(mfrow = c(3, 4))  # Устанавливаем макет графиков 3x4
for (i in 1:12) {  # Начинаем с 1-го столбца
  boxplot(table[, i], main = names(table)[i], outline = TRUE, na.rm = TRUE)
}
outliers_table <- data.frame(month = character(), Value = numeric(), Year = integer())

for (i in 1:12) {  # Начинаем с 1-го столбца и заканчиваем на 12-м
  # Фильтрация значений, исключая NA
  month_data <- as.numeric(table[!is.na(table[[i]]), i])
  
  # Проверка, что данные являются числами
  if(length(month_data) > 0) {
    q1 <- quantile(month_data, 0.25)
    q3 <- quantile(month_data, 0.75)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    
    outliers_indices <- which(month_data < lower_bound | month_data > upper_bound)
    
    # Проверяем, есть ли выбросы в данном столбце, чтобы создать сопоставление строк
    if (length(outliers_indices) > 0) {
      row_numbers <- outliers_indices + 1827
      
      outliers <- data.frame(month = rep(names(table)[i], length(outliers_indices)),
                             Value = table[[i]][outliers_indices],
                             Year = row_numbers)
      outliers_table <- rbind(outliers_table, outliers)
    }
  }
}

# Выводим таблицу выбросов
print(outliers_table)
# Загружаем пакет moments
library(moments)

# Устанавливаем макет графиков 3x4 для отображения гистограмм для каждого месяца
par(mfrow = c(3, 4))

# Создаем цикл для построения гистограммы для каждого месяца
for (i in 1:12) {
  # Получаем данные для текущего месяца, исключая NA
  month_data <- as.numeric(table[[i]][!is.na(table[[i]])])
  
  # Рисуем гистограмму
  hist(month_data, main = names(table)[i], xlab = "Value", ylab = "Frequency", col = "lightblue", border = "white")
  
  # Вычисляем коэффициент асимметрии
  skewness <- skewness(month_data)
  
  # Выводим значение коэффициента асимметрии
  text <- paste("Skewness:", round(skewness, 2))
  mtext(text, side = 1, line = -2, cex = 0.7)
}

# Сбросим макет графиков на значение по умолчанию
par(mfrow = c(1, 1))

# Устанавливаем макет графиков 3x4 для отображения гистограмм и QQ-графиков для каждого месяца
par(mfrow = c(3, 4))

# Создаем вектор для хранения результатов теста Шапиро-Уилка
shapiro_p_values <- numeric(length = 12)

# Создаем цикл для анализа каждого месяца
for (i in 1:12) {
  # Получаем данные для текущего месяца, исключая NA
  month_data <- as.numeric(table[[i]][!is.na(table[[i]])])
  
  # Рисуем гистограмму
  hist(month_data, main = paste("Histogram for", names(table)[i]), xlab = "Value", ylab = "Frequency", col = "lightblue", border = "white")
  
  # Рисуем QQ-график с использованием базовой функции qqnorm
  qqnorm(month_data, main = paste("QQ-plot for", names(table)[i]))
  qqline(month_data)
  
  # Применяем тест Шапиро-Уилка
  shapiro_test <- shapiro.test(month_data)
  shapiro_p_values[i] <- shapiro_test$p.value
  
  # Выводим результат теста
  cat("Month:", names(table)[i], "\n")
  cat("Shapiro-Wilk test p-value:", shapiro_test$p.value, "\n")
  
  # Вычисляем моду, медиану и среднее
  mode_value <- names(sort(table(month_data), decreasing = TRUE))[1]
  median_value <- median(month_data)
  mean_value <- mean(month_data)
  
  # Вычисляем межквартильный размах и стандартное отклонение
  iqr_value <- IQR(month_data)
  sd_value <- sd(month_data)
  
  # Выводим численные характеристики
  cat("Mode:", mode_value, "\n")
  cat("Median:", median_value, "\n")
  cat("Mean:", mean_value, "\n")
  cat("Interquartile Range:", iqr_value, "\n")
  cat("Standard Deviation:", sd_value, "\n")
  
  # Проверяем, попадают ли все значения в диапазон "шести сигм"
  within_six_sigma <- sum(month_data > mean_value - 3 * sd_value & month_data < mean_value + 3 * sd_value) == length(month_data)
  cat("All values within six sigma range:", within_six_sigma, "\n\n")
}

# Сбросим макет графиков на значение по умолчанию
par(mfrow = c(1, 1))

