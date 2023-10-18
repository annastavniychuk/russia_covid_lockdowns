# Impact of the pandemic COVID-19 re-opening orders on the unemployment claims: Evidence from Russian regions

The repository contains datasets on restrictive measures in Russia during the coronavirus pandemic. 

Datasets collected by the authors for the article ["The effect of the removal of regional anti-COVID restrictive measures on the dynamics of applications for unemployment benefits in Russia".](https://doi.org/10.3897/popecon.7.e90445)

**Abstract**

This paper assesses changes in the dynamics of applications for unemployment benefits in response to the abolition of regional restrictive measures during the first wave of COVID-19 spread in Russia. This assessment is interesting from the perspective of developing anti-crisis support measures for the population. The assessment is based on ﻿weekly-regional panel data using the ﻿staggered difference-in-differences method. ﻿After the lifting of restrictive measures, the number of new applications for unemployment benefits does not decrease significantly. The result remains robust when an alternative measure of the stringency of restrictions is used, such as an indicator for the validity period of digital passes instead of data on the stages of lifting restrictions. A comparison of official data on the effect of restrictive measures with the Yandex self-isolation index is provided.

**For citation:**
*Suchkova OV, Stavniychuk AY, Kalashnov GY, Osavolyuk A (2023) The ﻿effect of the removal of regional anti-COVID restrictive measures on the dynamics of applications for unemployment benefits in Russia. Population and Economics 7(2): 1-22. https://doi.org/10.3897/popecon.7.e90445*


``` 
├── code
│   └── panel_graphs.R -- script merges datasets from the data folder, pre-processes and makes graphics for the figures folder
│   
├── data
│   ├── labor_data.xlsx -- data aggregated at the regional level on citizens' applications for unemployment benefits during the 2020 pandemic,
│   │   collected by Rostrud and the Center for Economic Development, as well as data on restrictive measures and COVID-19 incidence rates in the regions,
│   │   collected by the authors of the article.
│   ├── self_isolation_data.csv -- Yandex self-isolation index (day-city)
│   │   https://yandex.ru/company/researches/2020/podomam
│   │   https://yandex.ru/company/services_news/2020/2020-03-30
│   ├── pass_data.csv -- regional restrictive measures (lockdowns) (week-region)
│   ├── electronic_pass_regime_data.xlsx -- data on the timing of the introduction and cancellation of electronic passes in the regions of Russia
│   │   (collected from regional news sites and Garant materials)
│   └── city_dictionary.csv -- dictionary with city and region names to aggregate and merge pass.csv and Таблица_самоизоляция_по_дням_2022_11_30_02_37.csv
│   
└── figures -- panelview graphs
    ├── pass.pdf 
    ├── pass1.pdf 
    ├── stopcovid.pdf 
    ├── stopcovid1.pdf 
    ├── yandex.pdf 
    └── yandex1.pdf 

pass_data.csv
├── level -- data on restrictive measures introduced or removed by the heads of subjects of the Russian Federation, 
│   depending on the epidemiological situation in the region. 
│   The data was collected (https://github.com/go95/covid_labor) from an interactive map from the official resource for informing
│   the public about coronavirus. https://стопкоронавирус.рф/information/ (Accessed 12/13/2020) At the time of writing, the map
│   shows statistics on new cases of coronavirus detection in the regions. 
│   You can get acquainted with the type of map with which we worked in the study using the web archive of the page at the link
│   http://web.archive.org/web/20200615124941/https://xn--80aesfpebagmfblc0a.xn--p1ai/information/ 
│   
└── pass -- date of introduction and removal of the electronic pass regime, which we collected from regional news sites                               
    (https://docs.google.com/spreadsheets/d/1TKZL2P0oDZ6VoyRGPIW3Tstkfi3vsopov9kzdSCuQp0/edit?usp=sharing) and legal acts, 
    summarized in the material prepared by the experts of the Garant company https://base.garant.ru/77398959/
  ```

# Влияние снятия региональных ограничительных антиковидных мер на динамику заявок на пособие по безработице в России 

Репозиторий содержит наборы данных об ограничительных мерах в России во время пандемии коронавируса. 

Наборы данных, собранные авторами для статьи [«Влияние снятия региональных ограничительных антиковидных мер на динамику заявок на пособие по безработице в России».](https://doi.org/10.3897/popecon.7.e90445)

**Аннотация**

В работе дается оценка изменения динамики заявок на  пособие по  безработице в  ответ на отмену региональных ограничительных мер в первую волну распространения COVID-19 в России, что интересно с точки зрения разработки мер антикризисной поддержки населения. Оценка дается на панельных данных методом ступенчатой разности разностей в разрезе «регион-неделя». После снятия ограничительных мер число новых заявок на пособие по безработице значимо не снижается. Результат остается устойчивым при использовании альтернативной меры жесткости ограничений  — индикатора для периода действия электронных пропусков вместо данных об этапах снятия ограничений. Приводится сравнение официальных данных о действии ограничительных мер с индексом самоизоляции Яндекса.

**Для цитирования:** 
*Suchkova OV, Stavniychuk AY, Kalashnov GY, Osavolyuk A (2023) The ﻿effect of the removal of regional anti-COVID restrictive measures on the dynamics of applications for unemployment benefits in Russia. Population and Economics 7(2): 1-22. https://doi.org/10.3897/popecon.7.e90445*

``` 
├── code
│   └── panel_graphs.R -- скрипт, который мёрджит наборы данных из папки data, предварительно их обрабатывает и строит графики для папки figures
│   
├── data
│   ├── labor_data.xlsx -- агрегированные на уровне регионов данные о заявках граждан на пособие по безработице в период пандемии 2020 г., собранные Рострудом и ЦПУР,
│   │   а также данные об ограничительных мерах и показателях заболеваемости COVID-19 в регионах, собранные автрами статьи.
│   ├── self_isolation_data.csv -- индекс самоизоляции Яндекса (разбивка день-город) 
│   │   https://yandex.ru/company/researches/2020/podomam
│   │   https://yandex.ru/company/services_news/2020/2020-03-30
│   ├── pass_data.csv -- региональные ограничительные меры (разбивка неделя-регион)
│   ├── electronic_pass_regime_data.xlsx -- данные о сроках введения и отмены электронных пропусков в регионах России 
│   │   (собраны из региональных новостных сайтов  и  материалов компании "Гарант")
│   └── city_dictionary.csv -- словарь с названиями городов и регионов, чтобы агрегировать данные на уровень региона и 
│       мерждить файлы pass.csv и Таблица_самоизоляция_по_дням_2022_11_30_02_37.csv
│   
└── figures -- panelview графики 
    ├── pass.pdf 
    ├── pass1.pdf 
    ├── stopcovid.pdf 
    ├── stopcovid1.pdf 
    ├── yandex.pdf 
    └── yandex1.pdf 
  ```

В файле `labor_data.xlsx` представлены агрегированные на уровне регионов данные о заявках граждан на пособие по безработице в период пандемии 2020 г., собранные Рострудом и ЦПУР, а также данные об ограничительных мерах и показателях заболеваемости COVID-19 в регионах. Данные охватывают период с 06.04.2020 г. по 26.10.2020 г. и имеют еженедельную частотность.

|Переменная|Описание|Источник|
|---|---|---|
|week|Неделя (указана дата начала недели)|   |
|region|Название региона|   |
|oced|Название сферы деятельности по справочнику ИАС ОБВ «Работа в России»|   |
|unemployed|Количество заявок на пособие по безработице в регионе за неделю по сферам деятельности, указанным по справочнику ИАС ОБВ «Работа в России»| |
|cv_gender|Количество заявок на пособие по безработице от мужчин в регионе за неделю по сферам деятельности, указанным по справочнику ИАС ОБВ «Работа в России»|   |
|cv_birthday_2000|Количество заявок на пособие по безработице в регионе за неделю по сферам деятельности от граждан, родившихся в 2000 году и позднее|   |
|cv_birthday_1990|Количество заявок на пособие по безработице в регионе за неделю по сферам деятельности от граждан, родившихся в 1990 году и позднее|   |
|cv_birthday_1980|Количество заявок на пособие по безработице в регионе за неделю по сферам деятельности от граждан, родившихся в 1980 году и позднее|   |
|cv_birthday_1970|Количество заявок на пособие по безработице в регионе за неделю по сферам деятельности от граждан, родившихся в 1970 году и позднее|   |
|cv_birthday_1960|Количество заявок на пособие по безработице в регионе за неделю по сферам деятельности от граждан, родившихся в 1960 году и позднее|   |
|employed_in_industry_jan|Среднесписочная численность работников без внешних совместителей по полному кругу организаций (по отраслям) за январь 2020 года|[Росстат](https://fedstat.ru/indicator/57848)|
|population|Численность населения региона на начало 2020 года|[Росстат](https://rosstat.gov.ru/bgd/regl/b20_111/Main.htm)|
|men| | |
|women| | |
|infections|Число выявленных случаев заболевания COVID-19 в разрезе регионов|[Данные собраны с помощью Yandex DataLens из источника стопкоронавирус.рф](https://datalens.yandex.ru/marketplace/f2eb8io5l5q4rp84feo1)|
|deaths|Число смертей от COVID-19 в разрезе регионов|[Данные собраны с помощью Yandex DataLens из источника стопкоронавирус.рф](https://datalens.yandex.ru/marketplace/f2eb8io5l5q4rp84feo1)|
|healed|Число выздоровлений от COVID-19 в разрезе регионов|[Данные собраны с помощью Yandex DataLens из источника стопкоронавирус.рф](https://datalens.yandex.ru/marketplace/f2eb8io5l5q4rp84feo1)|
|tests|Еженедельное число проведенных тестов на COVID-19 в разрезе регионов|Данные по ТОП-15 регионам по числу проведенных тестов и ТОП-15 регионам по числу проведенных тестов на 100 000 человек населения публикуются в рамках пресс-релиза [Роспотребнадзора](https://www.rospotrebnadzor.ru/), таким образом, можно восстановить данные по 23 регионам России (некоторые из них пересекаются). Наблюдения по регионам, которые не постоянно находились в ТОП-15, были восстановлены посредством интерполяции|
|Rt|Коэффициент распространения инфекции - это показатель, определяющий среднее количество людей, которых инфицирует один больной до его изоляции. Данный коэффициент рассчитывается на основе данных по приросту новых случаев за последние 8 суток и используется для принятия решений о переходе к первому, второму или третьему этапу снятия ограничений. Коэффициент рассчитывался на оперативных данных числа выявленных случаев заболевания по формуле: $R_t=\frac{X_8+X_7+X_6+X_5}{X_1+X_2+X_3+X_4}$, где $X_i$ - количество зарегистрированных больных в регионе за $i$-е сутки|Рассчитано авторами на основе оперативных данных стопкоронавирус.рф.|
|level|Этап снятия ограничительных мер в регионе в зависимости от эпидемиологической ситуации. Использовалась градация жесткости мер: 0 – Данные доступны с 8 июня 2020 г.|Данные собраны с интерактивной карты с помощью веб архива страницы https://стопкоронавирус.рф/information/|

В файле `pass_data.csv`
|Переменная|Описание|Источник|
|---|---|---|
|level|Данные об ограничительных мерах, введенных или снятых главами субъектов РФ в зависимости от эпидемиологической ситуации в регионе|Данные были собраны с интерактивной карты с официального интернет-ресурса для информирования населения по вопросам коронавируса. Портал стопкоронавирус.рф. Ситуация с СOVID-19 в регионах. https://стопкоронавирус.рф/information/ (Дата обращения 13.12.2020 г.) На момент написания рукописи на карте обозначена статистика о новых случая выявления коронавируса в регионах. Ознакомиться с видом карты, с которым мы работали в исследовании, можно с помощью веб-архива страницы по ссылке http://web.archive.org/web/20200615124941/https://xn--80aesfpebagmfblc0a.xn--p1ai/information/|
|pass|Данные о введении и отмене режима электронных пропусков|Данные были собраны нами с региональных новостных сайтов (https://docs.google.com/spreadsheets/d/1TKZL2P0oDZ6VoyRGPIW3Tstkfi3vsopov9kzdSCuQp0/edit?usp=sharing) и нормативно-правовых актов, а также с помощью материалов, подготовленных экспертами компании "Гарант" https://base.garant.ru/77398959/|




