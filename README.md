# Impact of the pandemic COVID-19 re-opening orders on the unemployment claims: Evidence from Russian regions

The repository contains datasets on restrictive measures in Russia during the coronavirus pandemic. 

Datasets collected by the authors for the article ["The effect of the removal of regional anti-COVID restrictive measures on the dynamics of applications for unemployment benefits in Russia".](https://doi.org/10.3897/popecon.7.e90445)

**Abstract**

This paper assesses changes in the dynamics of applications for unemployment benefits in response to the abolition of regional restrictive measures during the first wave of COVID-19 spread in Russia. This assessment is interesting from the perspective of developing anti-crisis support measures for the population. The assessment is based on ﻿weekly-regional panel data using the ﻿staggered difference-in-differences method. ﻿After the lifting of restrictive measures, the number of new applications for unemployment benefits does not decrease significantly. The result remains robust when an alternative measure of the stringency of restrictions is used, such as an indicator for the validity period of digital passes instead of data on the stages of lifting restrictions. A comparison of official data on the effect of restrictive measures with the Yandex self-isolation index is provided.

**For citation:**
*Suchkova OV, Stavniychuk AY, Kalashnov GY, Osavolyuk A (2023) The ﻿effect of the removal of regional anti-COVID restrictive measures on the dynamics of applications for unemployment benefits in Russia. Population and Economics 7(2): 1-22. https://doi.org/10.3897/popecon.7.e90445*

---

# Влияние снятия региональных ограничительных антиковидных мер на динамику заявок на пособие по безработице в России 

Репозиторий содержит наборы данных об ограничительных мерах в России во время пандемии коронавируса. 

Наборы данных, собранные авторами для статьи [«Влияние снятия региональных ограничительных антиковидных мер на динамику заявок на пособие по безработице в России».](https://doi.org/10.3897/popecon.7.e90445)

**Аннотация**

В работе дается оценка изменения динамики заявок на  пособие по  безработице в  ответ на отмену региональных ограничительных мер в первую волну распространения COVID-19 в России, что интересно с точки зрения разработки мер антикризисной поддержки населения. Оценка дается на панельных данных методом ступенчатой разности разностей в разрезе «регион-неделя». После снятия ограничительных мер число новых заявок на пособие по безработице значимо не снижается. Результат остается устойчивым при использовании альтернативной меры жесткости ограничений  — индикатора для периода действия электронных пропусков вместо данных об этапах снятия ограничений. Приводится сравнение официальных данных о действии ограничительных мер с индексом самоизоляции Яндекса.

**Для цитирования:** 
*Suchkova OV, Stavniychuk AY, Kalashnov GY, Osavolyuk A (2023) The ﻿effect of the removal of regional anti-COVID restrictive measures on the dynamics of applications for unemployment benefits in Russia. Population and Economics 7(2): 1-22. https://doi.org/10.3897/popecon.7.e90445*

---

Мы максимально приветствуем использование собранной нами базы данных в любых исследованиях, однако просим указывать источник. 

Контакт для связи [annastavnychuk@gmail.com](mailto:annastavnychuk@gmail.com)

В файле `labor_data.xlsx` представлены агрегированные на уровне регионов данные о заявках граждан на пособие по безработице в период пандемии 2020 г., собранные Рострудом и ЦПУР, а также данные об ограничительных мерах, показателях заболеваемости COVID-19 и демографии в регионах, собранные авторами. Данные по безработным охватывают период с 06.04.2020 г. по 26.10.2020 г. и имеют еженедельную частотность.

Ниже для удобства приведено описание переменных:

|Переменная|Описание|Периодичность|Источник|
|---|---|---|---|
|week|Неделя (указана дата начала недели)|   |
|region|Название региона|   |
|oced|Название сферы деятельности по справочнику ИАС ОБВ «Работа в России»|   |
|unemployed|Количество заявок на пособие по безработице в регионе за неделю по сферам деятельности, указанным по справочнику ИАС ОБВ «Работа в России»|Ежедневная, агрегировали до недели|Обработано в рамках хакатона PandemicDataHack, организованного ИНИД и Рострудом. «Регистрируемая безработица в России: обезличенные микроданные о характеристиках граждан и полученных услугах за 2017-2021 гг.». Роструд; обработка: Инфраструктура научно-исследовательских данных, АНО «ЦПУР», 2021.|
|cv_gender|Количество заявок на пособие по безработице от мужчин в регионе за неделю по сферам деятельности, указанным по справочнику ИАС ОБВ «Работа в России»|Ежедневная, агрегировали до недели|Обработано в рамках хакатона PandemicDataHack, организованного ИНИД и Рострудом. «Регистрируемая безработица в России: обезличенные микроданные о характеристиках граждан и полученных услугах за 2017-2021 гг.». Роструд; обработка: Инфраструктура научно-исследовательских данных, АНО «ЦПУР», 2021.|
|cv_birthday_2000|Количество заявок на пособие по безработице в регионе за неделю по сферам деятельности от граждан, родившихся в 2000 году и позднее|Ежедневная, агрегировали до недели|Обработано в рамках хакатона PandemicDataHack, организованного ИНИД и Рострудом. «Регистрируемая безработица в России: обезличенные микроданные о характеристиках граждан и полученных услугах за 2017-2021 гг.». Роструд; обработка: Инфраструктура научно-исследовательских данных, АНО «ЦПУР», 2021.|
|cv_birthday_1990|Количество заявок на пособие по безработице в регионе за неделю по сферам деятельности от граждан, родившихся в 1990 году и позднее|Ежедневная, агрегировали до недели|Обработано в рамках хакатона PandemicDataHack, организованного ИНИД и Рострудом. «Регистрируемая безработица в России: обезличенные микроданные о характеристиках граждан и полученных услугах за 2017-2021 гг.». Роструд; обработка: Инфраструктура научно-исследовательских данных, АНО «ЦПУР», 2021.|
|cv_birthday_1980|Количество заявок на пособие по безработице в регионе за неделю по сферам деятельности от граждан, родившихся в 1980 году и позднее|Ежедневная, агрегировали до недели|Обработано в рамках хакатона PandemicDataHack, организованного ИНИД и Рострудом. «Регистрируемая безработица в России: обезличенные микроданные о характеристиках граждан и полученных услугах за 2017-2021 гг.». Роструд; обработка: Инфраструктура научно-исследовательских данных, АНО «ЦПУР», 2021.|
|cv_birthday_1970|Количество заявок на пособие по безработице в регионе за неделю по сферам деятельности от граждан, родившихся в 1970 году и позднее|Ежедневная, агрегировали до недели|Обработано в рамках хакатона PandemicDataHack, организованного ИНИД и Рострудом. «Регистрируемая безработица в России: обезличенные микроданные о характеристиках граждан и полученных услугах за 2017-2021 гг.». Роструд; обработка: Инфраструктура научно-исследовательских данных, АНО «ЦПУР», 2021.|
|cv_birthday_1960|Количество заявок на пособие по безработице в регионе за неделю по сферам деятельности от граждан, родившихся в 1960 году и позднее|Ежедневная, агрегировали до недели|Обработано в рамках хакатона PandemicDataHack, организованного ИНИД и Рострудом. «Регистрируемая безработица в России: обезличенные микроданные о характеристиках граждан и полученных услугах за 2017-2021 гг.». Роструд; обработка: Инфраструктура научно-исследовательских данных, АНО «ЦПУР», 2021.|
|population|Численность населения региона на начало 2020 года, показатель изменяется по регионам, но не меняется в течение 2020 года|Ежегодная|[Росстат](https://rosstat.gov.ru/bgd/regl/b20_111/Main.htm)|
|men|Численность мужчин в регионе на начало 2020 года, показатель изменяется по регионам, но не меняется в течение 2020 года|Ежегодная|[Росстат](https://rosstat.gov.ru/bgd/regl/b20_111/Main.htm)|
|women|Численность женщин в регионе на начало 2020 года, показатель изменяется по регионам, но не меняется в течение 2020 года|Ежегодная|[Росстат](https://rosstat.gov.ru/bgd/regl/b20_111/Main.htm)|
|infections|Число выявленных случаев заболевания COVID-19 в разрезе регионов|Ежедневная, агрегировали до недели|[Данные собраны с помощью Yandex DataLens из источника стопкоронавирус.рф](https://datalens.yandex.ru/marketplace/f2eb8io5l5q4rp84feo1)|
|deaths|Число смертей от COVID-19 в разрезе регионов|Ежедневная, агрегировали до недели|[Данные собраны с помощью Yandex DataLens из источника стопкоронавирус.рф](https://datalens.yandex.ru/marketplace/f2eb8io5l5q4rp84feo1)|
|healed|Число выздоровлений от COVID-19 в разрезе регионов|Ежедневная, агрегировали до недели|[Данные собраны с помощью Yandex DataLens из источника стопкоронавирус.рф](https://datalens.yandex.ru/marketplace/f2eb8io5l5q4rp84feo1)|
|tests| Прирост (изменение по сравнению с предыдущим периодом) числа проведенных тестов на COVID-19|Еженедельная|Данные по ТОП-15 регионам по числу проведенных тестов и ТОП-15 регионам по числу проведенных тестов на 100 000 человек населения публикуются в рамках пресс-релиза [Роспотребнадзора](https://www.rospotrebnadzor.ru/), таким образом, можно восстановить данные по 23 регионам России (некоторые из них пересекаются). Наблюдения по регионам, которые не постоянно находились в ТОП-15, были восстановлены посредством интерполяции|
|Rt|Коэффициент распространения инфекции - это показатель, определяющий среднее количество людей, которых инфицирует один больной до его изоляции|Ежедневная, агрегировали до недели|Рассчитано авторами на основе оперативных данных стопкоронавирус.рф. Данный коэффициент рассчитывается на основе данных по приросту новых случаев за последние 8 суток и используется для принятия решений о переходе к первому, второму или третьему этапу снятия ограничений. Коэффициент рассчитывался на оперативных данных числа выявленных случаев заболевания по формуле: $R_t=\frac{X_8+X_7+X_6+X_5}{X_1+X_2+X_3+X_4}$, где $X_i$ - количество зарегистрированных больных в регионе за $i$-е сутки|
|level|Этап снятия ограничительных мер в регионе в зависимости от эпидемиологической ситуации. Данные доступны с 8 июня 2020 г.|Еженедельная|Данные были собраны с интерактивной карты с официального интернет-ресурса для информирования населения по вопросам коронавируса. Портал стопкоронавирус.рф. Ситуация с СOVID-19 в регионах. https://стопкоронавирус.рф/information/ (Дата обращения 13.12.2020 г.) На момент написания рукописи на карте обозначена статистика о новых случая выявления коронавируса в регионах. Ознакомиться с видом карты, с которым мы работали в исследовании, можно с помощью веб-архива страницы по ссылке http://web.archive.org/web/20200615124941/https://xn--80aesfpebagmfblc0a.xn--p1ai/information/|
|yandex|Индекс самоизоляции Яндекса. Чем выше значение индекса, тем лучше соблюдается самоизоляция. Для расчета индекса применяются обезличенные данные об использовании приложений Яндекса (количество маршрутов в Яндекс Навигаторе и Яндекс Метро, время использования Яндекс.Эфир, Дзен, КиноПоиск и т. д.).|Ежедневная, агрегировали до недели|[Данные собраны с помощью Yandex DataLens](https://datalens.yandex.ru/marketplace/f2eb8io5l5q4rp84feo1).  «Все показатели сводятся к единой [шкале](https://yandex.ru/company/services_news/2020/2020-03-30), на которой 0 соответствует уровню часа пик в типичный будний день, а 5 — показателям, которые обычно бывают глубокой ночью» . Аналитики Яндекса приводят следующую [шкалу](https://yandex.ru/company/researches/2020/podomam) активности населения: «0-2,4 балла – на улице очень много людей; 2,5–2,9 балла – на улице много людей; 3–3,5 балла – на улице есть люди; 3,6–3,9 балла – большинство людей дома; 4–5 баллов – на улице почти никого»|
|pass|Данные об ограничительных мерах, введенных или снятых главами субъектов РФ в зависимости от эпидемиологической ситуации в регионе|Еженедельная|Данные были собраны с интерактивной карты с официального интернет-ресурса для информирования населения по вопросам коронавируса. Портал стопкоронавирус.рф. Ситуация с СOVID-19 в регионах. https://стопкоронавирус.рф/information/ (Дата обращения 13.12.2020 г.) На момент написания рукописи на карте обозначена статистика о новых случая выявления коронавируса в регионах. Ознакомиться с видом карты, с которым мы работали в исследовании, можно с помощью веб-архива страницы по ссылке http://web.archive.org/web/20200615124941/https://xn--80aesfpebagmfblc0a.xn--p1ai/information/|



