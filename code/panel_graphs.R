setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Пакет с пакетами
{
library('lubridate')
library('dplyr')
# install.packages('panelView')
library('panelView')
library('ggplot2')
library('stringi')
}

# Подготовка данных
{
data <- read.csv("../data/Таблица_самоизоляция_по_дням_2022_11_30_02_37.csv", sep=";")
city <- read.csv("../data/city.csv")

data <- data[,-1]
city <- city[,c(6,10)]
colnames(data) <- c('city', 'date', 'index')
data <- merge(data, city, by = 'city')

data$date <- as.Date(data$date, format ="%d.%m.%Y")

data$week <- week(data$date)
data$wday <- wday(data$date, label = TRUE, locale = "ru_RU.UTF-8")
data$nday <- wday(data$date-1)
data$week1 <- data$date - (data$nday-1)
data$week2 <- wday(data$week1, label = TRUE, locale = "ru_RU.UTF-8")


data_week <- data %>% group_by(region, week1) %>% summarise(index = mean(index), .groups = 'drop_last')
# https://yandex.ru/company/researches/2020/podomam
# 0 на улице очень много людей
# 1 на улице много людей
# 2 на улице есть люди
# 3 большинство людей дома
# 4 на улице почти никого
data_week <- data_week %>% mutate(yandex = case_when(index < 2.5 ~ 0,
                                                index >= 2.5 & index < 3 ~ 1,
                                                index >= 3 & index < 3.6 ~ 2,
                                                index >= 3.6 & index < 4 ~ 3,
                                                index >= 4 & index <= 5 ~ 4))

pass1 <- read.csv("../data/pass1.csv", sep=";")
pass1$week <- as.Date(pass1$week, format = "%Y-%m-%d")
colnames(pass1)[4] <- 'stopcovid'

data1 <- data_week[,c(1,2,4)]
colnames(data1)[2] <- 'week'
data2 <- merge(data1, pass1, by = c('week', 'region'))
}

# Графики на полных данных

setwd('../figures')

## Яндекс индекс самоизоляции

panel1 <- panelview(1 ~ yandex, 
          data = data2, 
          index = c("region","week"), 
          xlab = "Неделя", 
          ylab = "Регион") 

cairo_pdf(file = 'yandex_v1903.pdf', width = 16, height = 10)
panel1 + theme_minimal() +
  scale_fill_discrete(name = "Индекс самоизоляции",
                      labels = c("Очень много людей","Много людей", "Есть люди", "Большинство дома", "Почти никого")) +
  theme(legend.position = "bottom") +
  ggtitle('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

## Стопкоронавирус этапы ограничений

panel2 <- panelview(1 ~ stopcovid, 
                    data = data2, 
                    index = c("region","week"), 
                    xlab = "Неделя", 
                    ylab = "Регион") 

cairo_pdf(file = 'stopcovid_v1903.pdf', width = 16, height = 10)
panel2 + theme_minimal() + 
  scale_fill_discrete(name = "Стопкороновирус",
                      labels = c('0 этап', '1 этап', '2 этап', '3 этап')) +
  theme(legend.position = "bottom") + 
  ggtitle('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

## Пропуска на передвижения из НПА

panel3 <- panelview(1 ~ pass, 
                    data = data2, 
                    index = c("region","week"), 
                    xlab = "Неделя", 
                    ylab = "Регион") 

cairo_pdf(file = 'pass_v1903.pdf', width = 16, height = 10)
panel3 + theme_minimal() + 
  scale_fill_discrete(name = "Система пропусков", 
                      labels = c("Пропуск в данных","Действуют ограничения", "Нет ограничений")) +
  theme(legend.position = "bottom") + 
  ggtitle('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
dev.off()

## Динамика итогового показателя

data3 <- data2 %>% group_by(week) %>%  summarise(unemp = sum(unemployed))
data3$week <- as.Date(data3$week,  format = "%Y-%m-%d")
data3$week <- as.character(data3$week)

cairo_pdf(file = 'unemployed_v1903.pdf', width = 16, height = 10)
ggplot(data3, aes(x=week, y=unemp, group=1)) + 
  geom_line() +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Неделя") + ylab('Число зарегистрированных безработных')
dev.off()

cairo_pdf(file = 'unemployed_en_v1903.pdf', width = 16, height = 10)
ggplot(data3, aes(x=week, y=unemp, group=1)) + 
  geom_line() +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Week") + ylab('Number of registered unemployed')
dev.off()

## Все графики на английском

data2$region_en <- stri_trans_general(data2$region, "russian-latin/bgn")

panel1_en <- panelview(1 ~ yandex, 
                       data = data2, 
                       index = c("region_en","week"), 
                       xlab = "Week", 
                       ylab = "Region") 

cairo_pdf(file = 'yandex_en_v1903.pdf', width = 16, height = 10)
panel1_en + theme_minimal() +
  scale_fill_discrete(name = "Yandex Self Isolation Index",
                      labels = c("A lot of people","Many people", "There are people", "Most at home", "Almost no one")) +
  theme(legend.position = "bottom") +
  ggtitle('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

panel2_en <- panelview(1 ~ stopcovid, 
                       data = data2, 
                       index = c("region_en","week"), 
                       xlab = "Week", 
                       ylab = "Region") 

cairo_pdf(file = 'stopcovid_en_v1903.pdf', width = 16, height = 10)
panel2_en + theme_minimal() + 
  scale_fill_discrete(name = "Stopcoronavirus",
                      labels = c('0 stage', '1 stage', '2 stage', '3 stage')) +
  theme(legend.position = "bottom") + 
  ggtitle('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

panel3_en <- panelview(1 ~ pass, 
                       data = data2, 
                       index = c("region_en","week"), 
                       xlab = "Week", 
                       ylab = "Region") 

cairo_pdf(file = 'pass_en_v1903.pdf', width = 16, height = 10)
panel3_en + theme_minimal() + 
  scale_fill_discrete(name = "Pass system", 
                      labels = c("Gap in the data","Restrictions apply", "No restrictions")) +
  theme(legend.position = "bottom") + 
  ggtitle('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
dev.off()

# Обрезаем данные одинаково

data3 <- data2 %>% filter(week>='2020-06-08')

# Графички на ограниченных одинаково данных

## Яндекс индекс самоизоляции

panel4 <- panelview(1 ~ yandex, 
                    data = data3, 
                    index = c("region","week"), 
                    xlab = "Неделя", 
                    ylab = "Регион") 

cairo_pdf(file = 'yandex1_v1903.pdf', width = 10, height = 12)
panel4 + theme_minimal() +
  scale_fill_discrete(name = "Индекс самоизоляции",
                      labels = c("Очень много людей","Много людей", "Есть люди", "Большинство дома", "Почти никого")) +
  theme(legend.position = "bottom") +
  ggtitle('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

## Стопкоронавирус этапы ограничений

panel5 <- panelview(1 ~ stopcovid, 
                    data = data3, 
                    index = c("region","week"), 
                    xlab = "Неделя", 
                    ylab = "Регион") 

cairo_pdf(file = 'stopcovid1_v1903.pdf', width = 10, height = 12)
panel5 + theme_minimal() + 
  scale_fill_discrete(name = "Стопкороновирус",
                      labels = c('0 этап', '1 этап', '2 этап', '3 этап')) +
  theme(legend.position = "bottom") + 
  ggtitle('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

## Пропуска на передвижения из НПА

panel6 <- panelview(1 ~ pass, 
                    data = data3, 
                    index = c("region","week"), 
                    xlab = "Неделя", 
                    ylab = "Регион") 

cairo_pdf(file = 'pass1_v1903.pdf', width = 10, height = 12)
panel6 + theme_minimal() + 
  scale_fill_discrete(name = "Система пропусков", 
                      labels = c("Пропуск в данных","Действуют ограничения", "Нет ограничений")) +
  theme(legend.position = "bottom") + 
  ggtitle('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
dev.off()

