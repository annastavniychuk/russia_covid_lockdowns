# Impact of the pandemic COVID-19 re-opening orders on the unemployment claims: Evidence from Russian regions

The repository contains datasets on restrictive measures in Russia during the coronavirus pandemic. Datasets collected by the authors for the article "Impact of the pandemic COVID-19 re-opening orders on the unemployment claims: Evidence from Russian regions" (in Russian).

``` 

├── code
│   └── panel_graphs.R -- script merge datasets from the data folder, pre-processes and makes graphics for the figures folder
├── data
│   ├── Таблица_самоизоляция_по_дням_2022_11_30_02_37.csv -- Yandex self-isolation index (day-city)
│   ├── pass1.csv -- regional restrictive measures (lockdowns) (week-region)
│   └── city.csv -- dictionary with city and region names to aggregate and merge pass.csv and Таблица_самоизоляция_по_дням_2022_11_30_02_37.csv
└── figures
    ├── pass.pdf -- 
    ├── pass1.pdf -- 
    ├── stopcovid.pdf -- 
    ├── stopcovid1.pdf -- 
    ├── yandex.pdf -- 
    └── yandex1.pdf -- 

pass1.csv
  ├── column level -- data on restrictive measures introduced or removed by the heads of subjects of the Russian Federation, 
  │   depending on the epidemiological situation in the region. 
  │   The data was collected (https://github.com/go95/covid_labor) from an interactive map from the official resource for informing
  │   the public about coronavirus. https://стопкоронавирус.рф/information/ (Accessed 12/13/2020) At the time of writing, the map
  │   shows statistics on new cases of coronavirus detection in the regions. 
  │   You can get acquainted with the type of map with which we worked in the study using the web archive of the page at the link
  │   http://web.archive.org/web/20200615124941/https://xn--80aesfpebagmfblc0a.xn--p1ai/information/ 
  └── column pass -- data on the introduction of the electronic pass regime, which we collected from regional news sites          
      (https://docs.google.com/spreadsheets/d/1TKZL2P0oDZ6VoyRGPIW3Tstkfi3vsopov9kzdSCuQp0/edit?usp=sharing) and legal acts, summarized in the material 
      prepared by the experts of the Garant company https://base.garant.ru/77398959/
  ```

# Влияние снятия региональных ограничительных антиковидных мер на динамику заявок на пособие по безработице в России 

Репозиторий содержит наборы данных об ограничительных мерах в России во время пандемии коронавируса. Наборы данных, собранные авторами для статьи «Влияние снятия региональных ограничительных антиковидных мер на динамику заявок на пособие по безработице в России».
