# Impact of the pandemic COVID-19 re-opening orders on the unemployment claims: Evidence from Russian regions

The repository contains datasets on restrictive measures in Russia during the coronavirus pandemic. Datasets collected by the authors for the article "Impact of the pandemic COVID-19 re-opening orders on the unemployment claims: Evidence from Russian regions".

``` 
├── code
│   └── panel_graphs.R -- script merge datasets from the data folder, pre-processes and makes graphics for the figures folder
├── data
│   ├── Таблица_самоизоляция_по_дням_2022_11_30_02_37.csv -- Yandex self-isolation index (day-city)
│   │   https://yandex.ru/company/researches/2020/podomam
│   │   https://yandex.ru/company/services_news/2020/2020-03-30
│   ├── pass1.csv -- regional restrictive measures (lockdowns) (week-region)
│   ├── Пропускной режим и самоизоляция 2020.xlsx -- data on the timing of the introduction and cancellation of electronic passes in the regions of Russia
│   │   (collected from regional news sites and Garant materials)
│   └── city.csv -- dictionary with city and region names to aggregate and merge pass.csv and Таблица_самоизоляция_по_дням_2022_11_30_02_37.csv
└── figures -- panelview graphs
    ├── pass.pdf 
    ├── pass1.pdf 
    ├── stopcovid.pdf 
    ├── stopcovid1.pdf 
    ├── yandex.pdf 
    └── yandex1.pdf 

pass1.csv
  ├── column level -- data on restrictive measures introduced or removed by the heads of subjects of the Russian Federation, 
  │   depending on the epidemiological situation in the region. 
  │   The data was collected (https://github.com/go95/covid_labor) from an interactive map from the official resource for informing
  │   the public about coronavirus. https://стопкоронавирус.рф/information/ (Accessed 12/13/2020) At the time of writing, the map
  │   shows statistics on new cases of coronavirus detection in the regions. 
  │   You can get acquainted with the type of map with which we worked in the study using the web archive of the page at the link
  │   http://web.archive.org/web/20200615124941/https://xn--80aesfpebagmfblc0a.xn--p1ai/information/ 
  └── column pass -- date of introduction and removal of the electronic pass regime, which we collected from regional news sites          
      (https://docs.google.com/spreadsheets/d/1TKZL2P0oDZ6VoyRGPIW3Tstkfi3vsopov9kzdSCuQp0/edit?usp=sharing) and legal acts, 
      summarized in the material prepared by the experts of the Garant company https://base.garant.ru/77398959/
  ```

# Влияние снятия региональных ограничительных антиковидных мер на динамику заявок на пособие по безработице в России 

Репозиторий содержит наборы данных об ограничительных мерах в России во время пандемии коронавируса. Наборы данных, собранные авторами для статьи «Влияние снятия региональных ограничительных антиковидных мер на динамику заявок на пособие по безработице в России».

``` 
├── code
│   └── panel_graphs.R -- скрипт, который мёрджит наборы данных из папки data, предварительно их обрабатывает и строит графики для папки figures
├── data
│   ├── Таблица_самоизоляция_по_дням_2022_11_30_02_37.csv -- индекс самоизоляции Яндекса (разбивка день-город) 
│   │   https://yandex.ru/company/researches/2020/podomam
│   │   https://yandex.ru/company/services_news/2020/2020-03-30
│   ├── pass1.csv -- региональные ограничительные меры (разбивка неделя-регион)
│   ├── Пропускной режим и самоизоляция 2020.xlsx -- данные о сроках введения и отмены электронных пропусков в регионах России 
│   │   (собраны из региональных новостных сайтов  и  материалов компании "Гарант")
│   └── city.csv -- словарь с названиями городов и регионов, чтобы агрегировать данные на уровень региона и 
│       мерждить файлы pass.csv и Таблица_самоизоляция_по_дням_2022_11_30_02_37.csv
└── figures -- panelview графики 
    ├── pass.pdf 
    ├── pass1.pdf 
    ├── stopcovid.pdf 
    ├── stopcovid1.pdf 
    ├── yandex.pdf 
    └── yandex1.pdf 

pass1.csv
  ├── столбец level -- данные об ограничительных мерах, введенных или снятых главами субъектов РФ в зависимости от эпидемиологической ситуации в регионе. 
  │   Данные были собраны с интерактивной карты с официального интернет-ресурса для информирования населения по вопросам коронавируса.  
  │   Портал стопкоронавирус.рф. Ситуация с СOVID-19 в регионах. https://стопкоронавирус.рф/information/ (Дата обращения 13.12.2020 г.) 
  │   На момент написания рукописи на карте обозначена статистика о новых случая выявления коронавируса в регионах. 
  │   Ознакомиться с видом карты, с которым мы работали в исследовании, можно с помощью веб-архива страницы по ссылке  
  │   http://web.archive.org/web/20200615124941/https://xn--80aesfpebagmfblc0a.xn--p1ai/information/ 
  └── столбец pass -- данные о введении и отмене режима электронных пропусков, собранные нами с региональных новостных сайтов         
      (https://docs.google.com/spreadsheets/d/1TKZL2P0oDZ6VoyRGPIW3Tstkfi3vsopov9kzdSCuQp0/edit?usp=sharing) и нормативно-правовых актов, 
      обобщенных экспертами компании "Гарант" в материале https://base.garant.ru/77398959/
  ```
