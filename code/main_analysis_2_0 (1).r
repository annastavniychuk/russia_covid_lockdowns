library(dplyr)
library(plm)
library(coefplot)
library(stargazer)
library(readxl)
library(sandwich)
library(fixest)
library(DIDmultiplegt)
library(TwoWayFEWeights)
library(panelView)
library(ggplot2)
library(bacondecomp)
library(didimputation)
library(did)
library(modelsummary)
library(stringr)


setwd('~/covid_labor/')

get_data <- function() {
  data <- read.csv('final_short.csv')
  data$week <- as.character(data$week)
  
  data <- data[!(data$oced %in% c('Логистика')),]
  data[data$week < '2020-06-08', 'level'] <- 0
  data <- data %>% filter(data$week >= '2020-06-08')
  data$oced <- as.factor(data$oced)
  data$region <- as.factor(data$region)
  data$week <- as.Date(data$week)
  data <- data[data$region != 'Байконур',]
  data <- data[data$region != 'Чукотский АО',]
  data <- data[data$region != 'Москва',]
  data <- data[data$oced != 'None',]
  
  grid <- expand.grid(oced=unique(data$oced), region=unique(data$region), week=unique(data$week))
  data <- left_join(grid, data)
  data <- data %>% group_by(region, week) %>% mutate(level=max(level, na.rm=TRUE), Rt=max(Rt, na.rm=TRUE))
  data$unemployed <- coalesce(data$unemployed, 0)
#  data$cv_birthday_1960 <- coalesce(data$cv_birthday_1960, "tst")
#  data$cv_birthday_1970 <- coalesce(data$cv_birthday_1970, "tst")
#  data$cv_birthday_1980 <- coalesce(data$cv_birthday_1980, "tst")
#  data$cv_birthday_1990 <- coalesce(data$cv_birthday_1990, "tst")
##  data$cv_birthday_2000 <- coalesce(data$cv_birthday_2000, "tst")
#  data$cv_gender <- coalesce(data$cv_birthday_2000, "tst")
  
  
  data$level <- coalesce(data$level, 0)
  data <- data %>% group_by(region, oced) %>% arrange(week) %>% mutate(level = cummax(level))
  data$level_binary <- as.integer(data$level > 1)
  #data$level_binary[data$level == -1] <- NA
  #data$level_binary_0[data$level == -1] <- NA
  data <- data %>% group_by(region, oced) %>% arrange(week) %>% mutate(adoption_date = week[which.max(c(level_binary, TRUE))])
  data$adoption_date[is.na(data$adoption_date)] <- '2020-08-31'
  
  data <- data %>% group_by(region, oced) %>% arrange(week) %>% mutate(lagged_level_binary=lag(level_binary))
  data
}

data <- get_data()

grid2 <- data %>% group_by(region, oced) %>% summarize(m_u=min(unemployed)) %>% filter(m_u > 5) %>% select(region, oced)
data <- left_join(grid2, data)

get_panel_view <- function(data) {
  for_panel_view <- data %>% group_by(region, week) %>% summarize(unemployed=sum(unemployed), level=max(level))
  sorting <- data %>% group_by(region) %>%
    summarize(adoption_date = max(adoption_date)) %>% arrange(adoption_date) %>%
    mutate(id=row_number())
  for_panel_view <- inner_join(for_panel_view, sorting)
  
  week_breaks <- 1 + 3 * 0:((length(unique(data$week)) - 1) / 3)
  # важно! + sort!
  # Повернуть, сделат белым
  #return(for_panel_view)
  panelview(
    for_panel_view,
    unemployed ~ level,
    theme.bw=TRUE,
    color = c("#000000","#444444", "#888888", "white"),
    background='white',
    index=c('id', 'week'),
  ) + scale_y_continuous(breaks=sorting$id, labels = rev(sorting$region), name='Регион') +
    scale_x_continuous(breaks=week_breaks, labels = unique(data$week)[week_breaks], name='Неделя') + 
    scale_fill_discrete(labels=c('0 этап', '1 этап', '2 этап', '3 этап'), type = c("#000000","#444444", "#888888", "white")) +
    labs(title=NULL)  
}

get_panel_view(data)

# they use slitely different parallel trend assumptions... + ?

data2 <- data %>% filter(week >= '2020-06-08', week < '2020-09-01') %>% group_by(week, region, adoption_date) %>% summarize( # , week < '2020-09-01'
  level_binary=max(level_binary),
  unemployed=sum(unemployed),
  log_unemp=log(sum(unemployed)),
  adoption_date=first(adoption_date),
  population=max(population, na.rm=T)
) %>% group_by(week) %>% mutate(
  log_unemp_w=log_unemp* population * 83 / sum(population), log_unemp=log_unemp, w=population * 83 / sum(population),
  period=as.factor(pmax(-1, week - adoption_date))) #  

cs <- aggte(att_gt('log_unemp', idname='region', tname='week', gname='adoption_date', data=data2 %>%
                     mutate(week=as.numeric(week)/7, region=as.numeric(region), adoption_date=as.numeric(adoption_date)/7), control_group = "notyettreated"), type='dynamic')
mod_cs <- list(
  tidy = data.frame(term=as.character(cs$egt), estimate=cs$att.egt, std.error=cs$se.egt, conf.low=cs$att.egt - 1.96*cs$se.egt, conf.high=cs$att.egt + 1.96*cs$se.egt),
  glance = data.frame()
  )
class(mod_cs) <- "modelsummary_list"
cs_w <- aggte(att_gt('log_unemp', idname='region', tname='week', gname='adoption_date', data=data2 %>%
                       mutate(week=as.numeric(week)/7, region=as.numeric(region), adoption_date=as.numeric(adoption_date)/7), control_group = "notyettreated", weightsname='w'), type='dynamic')
mod_cs_w <- list(
  tidy = data.frame(term=as.character(cs_w$egt), estimate=cs_w$att.egt, std.error=cs_w$se.egt, conf.low=cs_w$att.egt - 1.96*cs_w$se.egt, conf.high=cs_w$att.egt + 1.96*cs_w$se.egt),
  glance = data.frame()
)
class(mod_cs_w) <- "modelsummary_list"

bs <- did_imputation(data2 %>%
                       mutate(week=as.numeric(week)/7, region=as.numeric(region), adoption_date=as.numeric(adoption_date)/7), 'log_unemp', idname='region', tname='week', gname='adoption_date', horizon=TRUE, pretrends=TRUE)
mod_bs <- list(
  tidy = bs,
  glance = data.frame())
class(mod_bs) <- "modelsummary_list"
bs_w <- did_imputation(data2 %>%
                         mutate(week=as.numeric(week)/7, region=as.numeric(region), adoption_date=as.numeric(adoption_date)/7), 'log_unemp', idname='region', tname='week', gname='adoption_date', wname='w', horizon=TRUE, pretrends=TRUE)
mod_bs_w <- list(
  tidy = bs_w,
  glance = data.frame())
class(mod_bs_w) <- "modelsummary_list"

sunab <- feols(log_unemp ~ sunab(adoption_date, week) | region + week, data=data2)
sunab_w <- feols(log_unemp ~ sunab(adoption_date, week) | region + week, weights=data2$w, data=data2)
twfe <- feols(log_unemp ~ period | region + week, data=data2 %>% filter(week >= '2020-06-08', week < '2020-09-01'))
twfe_w <- feols(log_unemp ~ period | region + week, weights=data2$w, data=data2)
sunab_pois <- fepois(unemployed ~ sunab(adoption_date, week) | region + week, data=data2)
twfe_pois <- fepois(unemployed ~ period | region + week, data=data2)

simple_twfe <- feols(log_unemp ~ level_binary | region + week, data=data2 %>% filter(week >= '2020-06-08', week < '2020-09-01'))
simple_twfe_w <- feols(log_unemp ~ level_binary | region + week, weights=data2$w, data=data2)
simple_twfe_pois <- fepois(unemployed ~ level_binary | region + week, data=data2)

modelsummary(list('TWFE'=simple_twfe, 'TWFE pop weights'=simple_twfe_w, 'TWFE poisson'=simple_twfe_pois), output='simple.docx') # 
modelsummary(list("Callaway Sant'Anna"=mod_cs, "Callaway Sant'Anna pop weights"=mod_cs_w), coef_rename=coef_rename, output='callaway.docx')
modelsummary(list('Borusyak et al.'=mod_bs, 'Borusyak et al. pop weights'=mod_bs_w), coef_rename=coef_rename, output='borus.docx')
modelsummary(list("Sun Abraham"=sunab, "Sun Abraham pop weight"=sunab_w, "Sun Abraham poisson"=sunab_pois), coef_rename=coef_rename, output='sunab.docx')
modelsummary(list('TWFE'=twfe, 'TWFE pop weights'=twfe_w, 'TWFE poisson'=twfe_pois), coef_rename=coef_rename, output='twfe.docx')

coef_rename <- function(coef) {
  print(coef)
  n <- as.numeric(str_extract(coef, '-?[0-9]+'))
  print(n)
  names(n) < coef
  if (!(5 %in% n)) {
    n <- n / 7
  }
  print(n)
  return(as.character(n))
}

background = list(geom_vline(xintercept = 0, color = 'black'))
modelplot(list('TWFE'=twfe, 'TWFE pop weights'=twfe_w, 'TWFE poisson'=twfe_pois), coef_rename=coef_rename, coef_omit='63|70|77|84', background=background) + facet_grid(model~.) + coord_flip() + theme(legend.position="none") + xlab('Новые зарегистированные безработные\n(доля к последней неделе без ограничений')
modelplot(list('Borusyak et al.'=mod_bs, 'Borusyak et al. pop weights'=mod_bs_w), coef_rename=coef_rename, coef_omit='9|1[0-9]', background=background) + facet_grid(model~.) + coord_flip() + theme(legend.position="none") + xlab('Новые зарегистированные безработные\n(доля к последней неделе без ограничений')
modelplot(list("Callaway Sant'Anna"=mod_cs, "Callaway Sant'Anna pop weights"=mod_cs_w), coef_rename=coef_rename, coef_omit='9|1[0-9]', background=background) + facet_grid(model~.) + coord_flip() + theme(legend.position="none") + xlab('Новые зарегистированные безработные\n(доля к последней неделе без ограничений')
modelplot(list("Sun Abraham"=sunab, "Sun Abraham pop weight"=sunab_w, "Sun Abraham poisson"=sunab_pois), coef_rename=coef_rename, coef_omit='9|1[0-9]', background=background) + facet_grid(model~.) + coord_flip() + theme(legend.position="none") + xlab('Новые зарегистированные безработные\n(доля к последней неделе без ограничений')

###########################

get_data <- function() {
  data <- read.csv('final_short.csv')
  data$week <- as.character(data$week)
  
  data <- data[!(data$oced %in% c('Логистика', 'Начало трудовой деятельности')),] #У нас есть спецраздел -- начало трудовой деятельности
  data[data$week < '2020-06-08', 'level'] <- 0
  data <- data %>% filter(data$week >= '2020-06-08')
  data$oced <- as.factor(data$oced)
  data$region <- as.factor(data$region)
  data$week <- as.Date(data$week)
  data <- data[data$region != 'Байконур',]
  data <- data[data$region != 'Чукотский АО',]
  data <- data[data$region != 'Москва',]
  data <- data[data$oced != 'None',]
  
  grid <- expand.grid(oced=unique(data$oced), region=unique(data$region), week=unique(data$week))
  data <- left_join(grid, data)
  data <- data %>% group_by(region, week) %>% mutate(level=max(level, na.rm=TRUE), Rt=max(Rt, na.rm=TRUE))
  data$unemployed <- coalesce(data$unemployed, 0)
  #  data$cv_birthday_1960 <- coalesce(data$cv_birthday_1960, "tst")
  #  data$cv_birthday_1970 <- coalesce(data$cv_birthday_1970, "tst")
  #  data$cv_birthday_1980 <- coalesce(data$cv_birthday_1980, "tst")
  #  data$cv_birthday_1990 <- coalesce(data$cv_birthday_1990, "tst")
  ##  data$cv_birthday_2000 <- coalesce(data$cv_birthday_2000, "tst")
  #  data$cv_gender <- coalesce(data$cv_birthday_2000, "tst")
  
  
  data$level <- coalesce(data$level, 0)
  data <- data %>% group_by(region, oced) %>% arrange(week) %>% mutate(level = cummax(level))
  data$level_binary <- as.integer(data$level > 1)
  #data$level_binary[data$level == -1] <- NA
  #data$level_binary_0[data$level == -1] <- NA
  data <- data %>% group_by(region, oced) %>% arrange(week) %>% mutate(adoption_date = week[which.max(c(level_binary, TRUE))])
  data$adoption_date[is.na(data$adoption_date)] <- max(data$week)
  
  data <- data %>% group_by(region, oced) %>% arrange(week) %>% mutate(lagged_level_binary=lag(level_binary))
  data
}

data <- get_data()

grid2 <- data %>% group_by(region, oced) %>% summarize(m_u=min(unemployed)) %>% filter(m_u > 5) %>% select(region, oced)
data <- left_join(grid2, data)

get_panel_view <- function(data) {
  for_panel_view <- data %>% group_by(region, week) %>% summarize(unemployed=sum(unemployed), level=max(level))
  sorting <- data %>% group_by(region) %>%
    summarize(adoption_date = max(adoption_date)) %>% arrange(adoption_date) %>%
    mutate(id=row_number())
  for_panel_view <- inner_join(for_panel_view, sorting)
  
  week_breaks <- 1 + 3 * 0:((length(unique(data$week)) - 1) / 3)
  # важно! + sort!
  # Повернуть, сделат белым
  #return(for_panel_view)
  panelview(
    for_panel_view,
    unemployed ~ level,
    theme.bw=TRUE,
    color = c("#000000","#444444", "#888888", "white"),
    background='white',
    index=c('id', 'week'),
  ) + scale_y_continuous(breaks=sorting$id, labels = rev(sorting$region), name='Регион') +
    scale_x_continuous(breaks=week_breaks, labels = unique(data$week)[week_breaks], name='Неделя') + 
    scale_fill_discrete(labels=c('0 этап', '1 этап', '2 этап', '3 этап'), type = c("#000000","#444444", "#888888", "white")) +
    labs(title=NULL)  
}

get_panel_view(data)


################## they use slitely different parallel trend assumptions... + ?

data2 <- data %>% filter(week >= '2020-06-08') %>% group_by(week, region, adoption_date) %>% summarize( # , week < '2020-09-01'
  level_binary=max(level_binary),
  unemployed=sum(unemployed),
  log_unemp=log(sum(unemployed)),
  adoption_date=first(adoption_date),
  population=max(population, na.rm=T)
) %>% group_by(week) %>% mutate(
  log_unemp_w=log_unemp* population * 83 / sum(population), log_unemp=log_unemp, w=population * 83 / sum(population),
  period=as.factor(pmax(0, week - adoption_date))) #  

cs <- aggte(att_gt('log_unemp', idname='region', tname='week', gname='adoption_date', data=data2 %>%
                     mutate(week=as.numeric(week)/7, region=as.numeric(region), adoption_date=as.numeric(adoption_date)/7), control_group = "notyettreated"), type='dynamic')
mod_cs <- list(
  tidy = data.frame(term=as.character(cs$egt), estimate=cs$att.egt, std.error=cs$se.egt, conf.low=cs$att.egt - 1.96*cs$att.egt, conf.high=cs$att.egt + 1.96*cs$att.egt),
  glance = data.frame()
)
class(mod_cs) <- "modelsummary_list"
cs_w <- aggte(att_gt('log_unemp', idname='region', tname='week', gname='adoption_date', data=data2 %>%
                       mutate(week=as.numeric(week)/7, region=as.numeric(region), adoption_date=as.numeric(adoption_date)/7), control_group = "notyettreated", weightsname='w'), type='dynamic')
mod_cs_w <- list(
  tidy = data.frame(term=as.character(cs_w$egt), estimate=cs_w$att.egt, std.error=cs_w$se.egt, conf.low=cs_w$att.egt - 1.96*cs_w$att.egt, conf.high=cs_w$att.egt + 1.96*cs_w$att.egt),
  glance = data.frame()
)
class(mod_cs_w) <- "modelsummary_list"

bs <- did_imputation(data2 %>%
                       mutate(week=as.numeric(week)/7, region=as.numeric(region), adoption_date=as.numeric(adoption_date)/7), 'log_unemp', idname='region', tname='week', gname='adoption_date', horizon=TRUE, pretrends=TRUE)
mod_bs <- list(
  tidy = bs,
  glance = data.frame())
class(mod_bs) <- "modelsummary_list"
bs_w <- did_imputation(data2 %>%
                         mutate(week=as.numeric(week)/7, region=as.numeric(region), adoption_date=as.numeric(adoption_date)/7), 'log_unemp', idname='region', tname='week', gname='adoption_date', wname='w', horizon=TRUE, pretrends=TRUE)
mod_bs_w <- list(
  tidy = bs_w,
  glance = data.frame())
class(mod_bs_w) <- "modelsummary_list"

sunab <- feols(log_unemp ~ sunab(adoption_date, week) | region + week, data=data2)
sunab_w <- feols(log_unemp ~ sunab(adoption_date, week) | region + week, weights=data2$w, data=data2)
twfe <- feols(log_unemp ~ period | region + week, data=data2)
twfe_w <- feols(log_unemp ~ period | region + week, weights=data2$w, data=data2)
sunab_pois <- fepois(unemployed ~ sunab(adoption_date, week) | region + week, data=data2)
twfe_pois <- fepois(unemployed ~ period | region + week, data=data2)

simple_twfe <- feols(log_unemp ~ level_binary | region + week, data=data2)
simple_twfe_w <- feols(log_unemp ~ level_binary | region + week, weights=data2$w, data=data2)
simple_twfe_pois <- fepois(unemployed ~ level_binary | region + week, data=data2)

modelsummary(list('TWFE'=simple_twfe, 'TWFE pop weights'=simple_twfe_w, 'TWFE poisson'=simple_twfe_pois))#, output='simple.docx') # 
modelsummary(list("Callaway Sant'Anna"=mod_cs, "Callaway Sant'Anna pop weights"=mod_cs_w), coef_rename=coef_rename, output='callaway.docx')
modelsummary(list('Borusyak et al.'=mod_bs, 'Borusyak et al. pop weights'=mod_bs_w), coef_rename=coef_rename, output='borus.docx')
modelsummary(list("Sun Abraham"=sunab, "Sun Abraham pop weight"=sunab_w, "Sun Abraham poisson"=sunab_pois), coef_rename=coef_rename, output='sunab.docx')
modelsummary(list('TWFE'=twfe, 'TWFE pop weights'=twfe_w, 'TWFE poisson'=twfe_pois), coef_rename=coef_rename, output='twfe.docx')

coef_rename <- function(coef) {
  n <- as.numeric(str_extract(coef, '-?[0-9]+'))
  names(n) < coef
  if (!(5 %in% n)) {
    n <- n / 7
  }
  print(n)
  return(as.character(n))
}

background = list(geom_vline(xintercept = 0, color = 'black'))
modelplot(list('TWFE'=twfe, 'TWFE pop weights'=twfe_w, 'TWFE poisson'=twfe_pois), coef_rename=coef_rename, coef_omit='9|1[0-9]', background=background) + facet_grid(model~.) + coord_flip() + theme(legend.position="none") + xlab('Новые зарегистированные безработные\n(доля к последней неделе без ограничений')
modelplot(list('Borusyak et al.'=mod_bs, 'Borusyak et al. pop weights'=mod_bs_w), coef_rename=coef_rename, coef_omit='9|1[0-9]', background=background) + facet_grid(model~.) + coord_flip() + theme(legend.position="none") + xlab('Новые зарегистированные безработные\n(доля к последней неделе без ограничений')
modelplot(list("Callaway Sant'Anna"=mod_cs, "Callaway Sant'Anna pop weights"=mod_cs_w), coef_rename=coef_rename, coef_omit='9|1[0-9]', background=background) + facet_grid(model~.) + coord_flip() + theme(legend.position="none") + xlab('Новые зарегистированные безработные\n(доля к последней неделе без ограничений')
modelplot(list("Sun Abraham"=sunab, "Sun Abraham pop weight"=sunab_w, "Sun Abraham poisson"=sunab_pois), coef_rename=coef_rename, coef_omit='9|1[0-9]', background=background) + facet_grid(model~.) + coord_flip() + theme(legend.position="none") + xlab('Новые зарегистированные безработные\n(доля к последней неделе без ограничений')



##########################


"wooldridge"
"bacon, staggered"
# STRANGE BORUSYAK BEHAVIOUR


a <- twowayfeweights(data2, 'log_unemp', 'region', 'week', 'level_binary', 'feTR') # BUGGG!!!
a$week <- a$T
res <- bacon(log_unemp ~ level_binary, id_var='region', time_var='week', data=data2 %>% mutate(week=as.numeric(week)))

res$treated <- as.Date(res$treated, origin='1970-01-01')
res$untreated <- as.Date(res$untreated, origin='1970-01-01')
weights <- full_join(
  res %>% group_by(treated) %>% summarize(t_weight=sum(weight)),
  res %>% group_by(untreated) %>% summarize(ut_weight=sum(weight)),
  by=c(treated='untreated')
)

weights <- left_join(
  weights,
  data2 %>% group_by(adoption_date) %>% summarize(n=length(unique(region))),
  by=c(treated='adoption_date')
)

weights[is.na(weights)] <- 0
weights$weight_bc <- (weights$t_weight - weights$ut_weight) / weights$n
weights <- weights %>% arrange(treated)

plot(weights$weight_bc)

panelview(a, weight ~ weight, type='outcome', index= c('G', 'week'))

data_oced <- data %>% filter(week >= '2020-06-08', week < '2020-09-01') %>% group_by(week, region, adoption_date, oced) %>% summarize( # , week < '2020-09-01'
  level_binary=max(level_binary),
  unemployed=sum(unemployed),
  log_unemp=log(sum(unemployed)),
  adoption_date=first(adoption_date),
  population=max(population, na.rm=T)
) %>% group_by(week) %>% mutate(
  log_unemp_w=log_unemp* population * 83 / sum(population), log_unemp=log_unemp, w=population * 83 / sum(population),
  period=as.factor(pmax(0, week - adoption_date))) #  


models <- list()
for (ind in unique(data_oced$oced)) {
  #if (ind == 'Химическая, нефтехимическая, топливная промышленность') {
  #  next
  #}
  print(ind)
  cs <- aggte(att_gt('log_unemp', idname='region', tname='week', gname='adoption_date', data=tmp %>%
                       mutate(week=as.numeric(week)/7, region=as.numeric(region), adoption_date=as.numeric(adoption_date)/7), control_group = "notyettreated"), type='simple', na.rm=TRUE)
  mod_cs <- list(
    tidy = data.frame(term='ATT', estimate=cs$overall.att, std.error=cs$overall.se, conf.low=cs$overall.att - 1.96*cs$overall.se, conf.high=cs$overall.att + 1.96*cs$overall.se),
    glance = data.frame()
  )
  print(mod_cs)
  mod_bs <- list(
    tidy = bs,
    glance = data.frame())
  class(mod_cs) <- "modelsummary_list"
  models[[ind]] <- mod_cs
  
}
models <- models[order(sapply(models, function(x) {x$tidy$estimate[[9]]}))]
models <- models[order(sapply(models, function(x) {x$coeftable['ATT', 'Estimate']}))]
models <- models[order(sapply(models, function(x) {x$tidy$estimate}))]
models <- models[order(sapply(models, function(x) {x$estimate}))]
modelplot(models, background=background) + facet_grid(model~.) + xlab('Новые зарегистированные безработные\n(доля к последней неделе без ограничений')







get_data <- function() {
  data <- read.csv('final_class.csv')
  data$week <- as.character(data$week)
  
  data <- data[!(data$oced %in% c('Логистика', 'Начало трудовой деятельности')),]
  data[data$week < '2020-06-08', 'level'] <- 0
  data <- data %>% filter(data$week >= '2020-06-08')
  data$oced <- as.factor(data$oced)
  data$region <- as.factor(data$region)
  data$week <- as.Date(data$week)
  data <- data[data$region != 'Байконур',]
  data <- data[data$region != 'Чукотский АО',]
  data <- data[data$region != 'Москва',]
  data <- data[data$oced != 'None',]
  
  grid <- expand.grid(oced=unique(data$oced), region=unique(data$region), week=unique(data$week))
  data <- left_join(grid, data)
  data <- data %>% group_by(region, week) %>% mutate(level=max(level, na.rm=TRUE), Rt=max(Rt, na.rm=TRUE))
  data$unemployed <- coalesce(data$unemployed, 0)
  #  data$cv_birthday_1960 <- coalesce(data$cv_birthday_1960, "tst")
  #  data$cv_birthday_1970 <- coalesce(data$cv_birthday_1970, "tst")
  #  data$cv_birthday_1980 <- coalesce(data$cv_birthday_1980, "tst")
  #  data$cv_birthday_1990 <- coalesce(data$cv_birthday_1990, "tst")
  ##  data$cv_birthday_2000 <- coalesce(data$cv_birthday_2000, "tst")
  #  data$cv_gender <- coalesce(data$cv_birthday_2000, "tst")
  
  
  data$level <- coalesce(data$level, 0)
  data <- data %>% group_by(region, oced) %>% arrange(week) %>% mutate(level = cummax(level))
  data$level_binary <- as.integer(data$level > 1)
  #data$level_binary[data$level == -1] <- NA
  #data$level_binary_0[data$level == -1] <- NA
  data <- data %>% group_by(region, oced) %>% arrange(week) %>% mutate(adoption_date = week[which.max(c(level_binary, TRUE))])
  data$adoption_date[is.na(data$adoption_date)] <- '2020-08-31'
  
  data <- data %>% group_by(region, oced) %>% arrange(week) %>% mutate(lagged_level_binary=lag(level_binary))
  data
}
data <- get_data()
grid2 <- data %>% group_by(region, oced) %>% summarize(m_u=min(unemployed)) %>% filter(m_u > 5) %>% select(region, oced)
data <- left_join(grid2, data)


data_oced <- data %>% filter(week >= '2020-06-08', week < '2020-09-01') %>% group_by(week, region, adoption_date, oced) %>% summarize( # , week < '2020-09-01'
  level_binary=max(level_binary),
  unemployed=sum(unemployed),
  log_unemp=log(sum(unemployed)),
  adoption_date=first(adoption_date),
  population=max(population, na.rm=T),
  employed_in_industry_jan = sum(employed_in_industry_jan)
) %>% group_by(week) %>% mutate(
  log_unemp_w=log_unemp* population * 83 / sum(population), log_unemp=log_unemp, w=employed_in_industry_jan * 83 / sum(employed_in_industry_jan),
  period=as.factor(pmax(0, week - adoption_date))) #  



models <- list()
for (ind in unique(data_oced$oced)) {
  print(ind)
  tmp <- data_oced %>% filter(oced == ind)
  twfe <- feols(log_unemp ~ logit_binary | region + week, data=data2 %>% filter(week >= '2020-06-08', week < '2020-09-01'))
  models[[ind]] <- twfe
  
}
models <- models[order(sapply(models, function(x) {x$tidy$estimate[[9]]}))]
models <- models[order(sapply(models, function(x) {x$coeftable['ATT', 'Estimate']}))]
models <- models[order(sapply(models, function(x) {x$tidy$estimate}))]
models <- models[order(sapply(models, function(x) {x$estimate}))]
modelplot(models, background=background) + facet_grid(model~.) + xlab('Новые зарегистированные безработные\n(доля к последней неделе без ограничений')








lag(x, n = 1L, default = NA, order_by = NULL, ...)
twfe <- feols(log_unemp ~ level_binary| region + week, data2)

ggplot(data=data.frame(y=y, x=x, se=se)) + geom_line(aes(x=x, y=y)) +
  geom_line(aes(x=x, y=y - 2 * se), linetype='dashed') + geom_line(aes(x=x, y=y + 2 * se), linetype='dashed') +
  xlab('Недель до ослабления ограничительных мер') + ylab('Новые зарегистрированные безработные (% к последнему дню ограничений)') + theme_bw()

industries <- unique(data$oced)
ind_delete <- c("Химическая, нефтехимическая, топливная промышленность")

industries <- c('Продажи, закупки, снабжение, торговля', 'Производство', 'Работы, не требующие квалификации', 'Рабочие специальности','Образование, наука',
                'Транспорт, автобизнес, логистика, склад, ВЭД',
                'Финансы, кредит, страхование, пенсионное обеспечение',
                'Информационные технологии, телекоммуникации, связь',
                'Здравоохранение, спорт, красота, социальное обеспечение')

industries3 <- c('Образование, наука', 'Продажи, закупки, снабжение, торговля', 'Здравоохранение, спорт, красота, социальное обеспечение', 'Транспорт, автобизнес, логистика, склад, ВЭД')

industries <- setdiff(unique(data$oced), ind_delete)[1:27]
industries <- industries[22:length(industries)]
industries <- industries[2]
results <- list()

for (ind in industries) {
  print(ind)
  data_tmp <- data %>% filter(oced == ind) %>% group_by(week, region) %>%
    summarize(level_binary=max(level_binary), unemp=sum(unemployed),
              log_unemp=log(sum(unemployed - cv_birthday_1990)) - log(cv_birthday_1990),
              population=mean(population), employed_in_industry_jan=mean(employed_in_industry_jan)) # %>%
  # group_by(week, ) %>% summarize(.groups='keep', region=region, level_binary=level_binary, log_unemp=log_unemp * employed_in_industry_jan * 83 / sum(employed_in_industry_jan))# CANNOT WEIGHT
  a <- did_multiplegt(data_tmp, 'log_unemp', 'region', 'week', 'level_binary', placebo=9, dynamic=15, cluster='region', brep=500, covariance=T, parallel = T)
  results <- c(results, list(a))
} # need weights... need them... WEIGHT BY BASELINE? or b. also, what to do with resampling? treat as 0? but no...


for (ind in industries[21:28]) {
  print(ind)
  data_tmp <- data %>% filter(oced == ind) %>% group_by(week, region) %>%
    summarize(level_binary=max(level_binary), unemp=sum(unemployed),
              log_unemp=log(sum(unemployed)))
  # group_by(week, ) %>% summarize(.groups='keep', region=region, level_binary=level_binary, log_unemp=log_unemp * employed_in_industry_jan * 83 / sum(employed_in_industry_jan))# CANNOT WEIGHT
  a <- did_multiplegt(data_tmp, 'log_unemp', 'region', 'week', 'level_binary', placebo=9, dynamic=8, cluster='region', brep=500, covariance=T, parallel = T)
  results <- c(results, list(a))
} # need weights... need them... WEIGHT BY BASELINE? or b. also, what to do with resampling? treat as 0? but no...




output_data_f <- data.frame()
output_data_p <- data.frame()# LOOKS OK, but NEED TO RENORM! -- robustness check to another specification
for (i in 1:length(results)) {
  a <- results[[i]]
  industry <- industries[i]
  effect <- c(results[[i]]$effect, unlist(data.frame(results[[i]]) %>% select(starts_with('dynamic'))))
  N_effect <- c(results[[i]]$N_effect, unlist(data.frame(results[[i]]) %>% select(starts_with('N_dynamic'))))
  unlist(data.frame(results[[1]]) %>% select(starts_with('cov_placebo')))
  se <- c(results[[i]]$se_effect, unlist(data.frame(results[[i]]) %>% select(starts_with('se_dynamic'))))
  cov_mat <- diag(se ** 2)
  cov_mat[upper.tri(cov_mat)] <- unlist(data.frame(results[[i]]) %>% select(starts_with('cov_effect')))
  cov_mat[lower.tri(cov_mat)] <- t(cov_mat)[lower.tri(cov_mat)]
  
  placebo <- rev(unlist(data.frame(results[[i]]) %>% select(starts_with('placebo'))))
  N_placebo <- rev(unlist(data.frame(results[[i]]) %>% select(starts_with('N_placebo'))))
  p_se <- rev(unlist(data.frame(results[[i]]) %>% select(starts_with('se_placebo'))))
  p_cov_mat <- diag(p_se ** 2)
  p_cov_mat[upper.tri(p_cov_mat)] <- unlist(data.frame(results[[i]]) %>% select(starts_with('cov_placebo')))
  p_cov_mat[lower.tri(p_cov_mat)] <- t(p_cov_mat)[lower.tri(p_cov_mat)]
  
  output_data_f <- rbind(output_data_f, data.frame(y=effect, x=1:16, se=se, industry=industry, N=N_effect, cov_mat=cov_mat))
  output_data_p <- rbind(output_data_p, data.frame(y=placebo, x=-1:-9, se=p_se, industry=industry, N=N_placebo, cov_mat=p_cov_mat))
}

write.csv(output_data_p, 'did_results_covar_p_7.csv', row.names=F) # these are log!
write.csv(output_data_f, 'did_results_covar_f_7.csv', row.names=F) # these are log!


i <- 1
placebo <- rev(unlist(data.frame(results[[i]]) %>% select(starts_with('placebo'))))
N_placebo <- rev(unlist(data.frame(results[[i]]) %>% select(starts_with('N_placebo'))))
p_se <- rev(unlist(data.frame(results[[i]]) %>% select(starts_with('se_placebo'))))
p_cov_mat <- diag(p_se ** 2)
p_cov_mat[upper.tri(p_cov_mat)] <- unlist(data.frame(results[[i]]) %>% select(starts_with('cov_placebo')))
p_cov_mat[lower.tri(p_cov_mat)] <- t(p_cov_mat)[lower.tri(p_cov_mat)]

unlist(data.frame(results[[1]]) %>% select(starts_with('cov_effect')))

write.csv(output_data, 'did_results_log_weighted.csv')



results <- list()

for (ind in industries) {
  data_tmp <- data %>% filter(oced == ind) %>% group_by(week, region) %>% summarize(level_binary=max(level_binary), unemp=sum(unemployed), log_unemp=log(sum(unemployed)))
  a <- did_multiplegt(data_tmp, 'unemp', 'region', 'week', 'level_binary', placebo=9, dynamic=15, brep=100, parallel=T)
  results <- c(results, list(a))
}

output_data <- data.frame() # LOOKS OK, but NEED TO RENORM! -- robustness check to another specification
for (i in 1:18) {
  a <- results2[[i]]
  industry <- industries[i]
  x <- -9:16
  y <- 100 + 100 * unlist(a[c('placebo_9', 'placebo_8', 'placebo_7', 'placebo_6', 'placebo_5', 'placebo_4', 'placebo_3', 'placebo_2', 'placebo_1', 'N_placebo_3', 'effect', 'dynamic_1', 'dynamic_2', 'dynamic_3', 'dynamic_4', 'dynamic_5', 'dynamic_6', 'dynamic_7', 'dynamic_8')])
  se <- 100 * unlist(a[c('se_placebo_9', 'se_placebo_8', 'se_placebo_7', 'se_placebo_6', 'se_placebo_5', 'se_placebo_4', 'se_placebo_3', 'se_placebo_2', 'se_placebo_1', 'N_placebo_3', 'se_effect', 'se_dynamic_1', 'se_dynamic_2', 'se_dynamic_3', 'se_dynamic_4', 'se_dynamic_5', 'se_dynamic_6', 'se_dynamic_7', 'se_dynamic_8')])
  y['N_placebo_3'] <- 100
  se['N_placebo_3'] <- 0
  output_data <- rbind(output_data, data.frame(x=x, y=y, se=se, industry=industry))
}

write.csv(output_data, 'did_results_log_weighted.csv')


results <- list()
for (ind in industries) {
  data2 <- data  %>% filter(oced == ind) %>%  group_by(week, region) %>% summarize(level_binary=max(level_binary), unemp=sum(unemployed), log_unemp=log(sum(unemployed)), adoption_date=first(adoption_date), employed_in_industry_jan=max(data$employed_in_industry_jan, na.rm=T)) %>% 
    group_by(week) %>% mutate(log_unemp=log_unemp * data$employed_in_industry_jan * 83 / sum(data$employed_in_industry_jan))
  
  a <- did_multiplegt(data_tmp, 'unemp', 'region', 'week', 'level_binary', placebo=9, dynamic=15, brep=100, parallel=T, covariance=T)
  results <- c(results, list(a))
}

write.csv(output_data, 'did_results_log_weighted.csv')
ggplot(data=output_data) + geom_line(aes(x=x, y=y, group=industry, color=industry)) +
  xlab('Время до ослабления карантина') + ylab('Новые зарегистрированные безработные (% к последнему дню карантина)')  


# SUN ABRAHAM ESTIMATES

data2 <- data %>% group_by(week, region) %>% summarize(level_binary=max(level_binary), log_unemp_cum=log(sum(unemployed_cum)), log_unemp=log(sum(unemployed)), adoption_date=first(adoption_date), adoption_date_0=first(adoption_date_0))

model2 <- feols(log_unemp ~ sunab(adoption_date_0, week) | week + region, data2)

model2 <- feols(log_unemp ~ sunab_att(adoption_date, week, ref.p=-20:-1) | week + region, data2)


data_fe <- data %>% group_by(week) %>% mutate_at(vars(-week, -region, -oced, -adoption_date, -adoption_date_0, -nontriv_adoption_date_0, -cv_industry), function(x) {x - mean(x)})

#model3 <- feols(log_unemp ~ sunab(adoption_date, week) | region, data_fe[data_fe$oced == 'ЖКХ, эксплуатация',])
model3 <- feols(log_unemp ~ sunab(adoption_date, week, ref.p=-20:-1) + Rt + log_infections | region  + week, data_fe[data_fe$oced == 'Пищевая промышленность',])

# cum????


non_missing <- data %>% group_by(region, oced) %>% summarize(unemployed=min(unemployed)) %>% filter(unemployed > 5)
non_miss_data <- data %>% inner_join(non_missing[c('oced', 'region')])

#  + Rt
model4 <- feols(unemployed ~ sunab(adoption_date, week, ref.p=-20:-1, no_agg=TRUE):oced | region^oced + week + oced[log_infections], non_miss_data, weights = non_miss_data$employed_in_industry_jan)
res2 <- aggregate(model4, r'{.*?oced(.*)}')

model4 <- feols(log(unemployed) ~ sunab(adoption_date, week, ref.p=-2:-1):oced | region^oced + week^oced + oced[log_infections], non_miss_data, weights = non_miss_data$employed_in_industry_jan, subset=non_miss_data$oced == 'ТОРГОВЛЯ ОПТОВАЯ И РОЗНИЧНАЯ; РЕМОНТ АВТОТРАНСПОРТНЫХ СРЕДСТВ И МОТОЦИКЛОВ')

data2 <- data %>% group_by(week, region) %>% summarize(level_binary=max(level_binary), unemployed=sum(unemployed), adoption_date=first(adoption_date), employed_in_industry_jan=sum(employed_in_industry_jan), infections=mean(infections), population=max(population, na.rm=T)) # , Rt=mean(Rt)
model5 <- feols(log(unemployed) ~ sunab(adoption_date, week, ref.p=-2:-1) | region + week, data2, weights = data2$population) #, weights = 1/data2$population)
res2 <- aggregate(model4, r'{.*?oced(.*)}', use_weights = T)

model4 <- fepois(unemployed ~ sunab(adoption_date, week, ref.p=-20:-1, no_agg=TRUE)*oced + Rt | region^oced + week + oced[log_infections], non_miss_data, nthreads=4, mem.clean=TRUE, verbose=1)
res2 <- aggregate(model4, r'{.*?oced(.*)}')
# проверить этот последний NA

model4_men <- feols(log_unemp_men ~ sunab(adoption_date, week, ref.p=-20:-1, no_agg=TRUE)*oced + Rt | region^oced + week + oced[log_infections], data_fe)
res3 <- aggregate(model4_men, r'{.*?oced(.*)}')

model4_women <- feols(log_unemp ~ sunab(adoption_date, week, ref.p=-20:-1, no_agg=TRUE)*oced + Rt | region^oced + week + oced[log_infections], data_fe)
res4 <- aggregate(model4_women, r'{.*?oced(.*)}')


res <- data.frame()
for (ind in industries) {
  data_tmp <- data_fe[data_fe$oced == ind,]
  print(nrow(data_tmp))
  model4 <- feols(log_unemp ~ sunab_att(adoption_date, week, ref.p=-20:-1) + Rt + log_infections | region + week, data_tmp)
  att <- model4$coeftable['ATT',]
  # att <- feols(log_unemp ~ level_binary + Rt + log_infections | region + week, data_tmp)$coeftable['level_binary',]
  res <- rbind(res, att)
}

a <- summary(model4)
# Туризм!

data3 <- data
data3$log_unemp <- log(data$unemployed)
data3$group_var <- paste0(data$region, data$oced)
data_tr <- data.frame(i(data2$oced, data2$level_binary))
data3 <- cbind(data3, data_tr)

b <- twowayfeweights(data3, 'log_unemp', 'group_var', 'week', names(data_tr)[1], 'feTR', other_treatments = names(data_tr)[-1])
b <- twowayfeweights(data3, 'log_unemp', 'group_var', 'week', names(data_tr)[1], 'feTR')



# better plot, conf int?


model2 <- feols(log(unemp_rate) ~ sunab(adoption_date, week) | industry[log(infections)] + region + week, data_fe)



# twowayfeweights(data2, 'log_unemp', 'oced', 'week', 'level_binary', 'feTR')
# program myself?
# clustered errors
# SUN
# HATouile

model_with_fe <- feols(log(unemployed) ~ level_binary:C(oced) | region + oced[log(infections) + Rt] + week, data) # how to cluster fucking 


model_with_interactive_fe <- feols(log(unemployed) ~ level_binary:C(oced) | region + region^oced + oced[log(infections) + Rt] + week, data)  # how to cluster fucking

ord <- names(model_with_interactive_fe$coefficients)
ord <- ord[order(model_with_interactive_fe$coefficients)]
ord <- ord[ord != 'Rt']
ord <- substring(ord, 21)


coefplot(list(model_with_interactive_fe, model_with_fe), drop='Rt', order=ord) 




model_with_lag <- feols(log(unemployed) ~ Rt + C(oced):lagged_level_binary | region^oced[log(infections)] + week, data)


model_names <- model_with_lag$coefficients[grepl('level_binary', names(model_with_lag$coefficients), fixed = TRUE)]
model_names <- names(model_names[order(model_names)])
namesportal_cp2 <- substr(model_names, 8, nchar(model_names) - 20)

cp2 <- coefplot(model_with_lag, predictors='lagged_level_binary', sort='magnitude', decreasing=FALSE, xlab='Влияние лага карантина на заявки по безработице, %', 
                #title='Модели по справочнику ИАС ОБВ «Работа в России»', 
                title=NULL, 
                outerCI=0, ylab='Отрасль', zeroColor='red') + 
  theme(text = element_text(size=20)) + scale_y_discrete(labels=namesportal_cp2)
cp2
cairo_pdf(file = '2.pdf', width = 21, height = 12)
cp2
dev.off()

model_men <- feols(log(cv_gender) ~ log(infections):C(oced) + Rt + C(oced):level_binary + C(oced) + C(region) + C(week), data[data$cv_gender!=0,])
model_women <- feols(log(unemployed - cv_gender) ~ log(infections):C(oced) + Rt + C(oced):level_binary + C(oced) + C(region) + C(week), data[data$cv_gender!=data$unemployed,])


model_names <- model_men$coefficients[grepl('level_binary', names(model_men$coefficients), fixed = TRUE)]
model_names <- names(model_names[order(model_names)])
namesportal_cp5 <- substr(model_names, 8, nchar(model_names) - 13)

namesportal_cp5 <- c('Начало трудовой деятельности', 
                     'Логистика', 
                     'Домашний персонал', 
                     'Кадровая служба, управление персоналом', 
                     'Административная работа, секретариат', 
                     'Консалтинг, стратегич. развитие, управление', 
                     'Образование, наука', 
                     'Юриспруденция',
                     'Здравоохр., спорт, красота, соц. обеспечение', 
                     'Высший менеджмент', 
                     'Химич., нефтехимич., топливная промышл.', 
                     'Маркетинг, реклама, PR', 
                     'Финансы, кредит, страхование, пенс. обеспеч.', 
                     'Услуги населению, сервисное обслуживание', 
                     'Металлургия, металлообработка', 
                     'Сельское хозяйство, экология, ветиринария', 
                     'Добывающая промышленность', 
                     'Гос. служба, некоммерч. организации', 
                     'Работы, не требующие квалификации', 
                     'Легкая промышленность',
                     'Искусство, культура и развлечения', 
                     'Машиностроение', 
                     'Туризм, гостиницы, рестораны',
                     'Лесная, деревообр., целл.-бумаж. пром.', 
                     'Пищевая промышленность', 
                     'Электроэнергетика', 
                     'ИТ, телекоммуникации, связь', 
                     'Безопасность, службы охраны', 
                     'ЖКХ, эксплуатация', 
                     'Рабочие специальности', 
                     'Производство', 
                     'Строительство, ремонт, строймат., недвиж.', 
                     'Транспорт, автобизнес, логистика, склад, ВЭД', 
                     'Продажи, закупки, снабжение, торговля')
cp5 <- multiplot(model_men, model_women, predictors='level_binary', sort='magnitude', decreasing=FALSE, lwdInner=1, xlab='Влияние карантина на заявки по безработице, %', 
                 #title='Модели по справочнику ИАС ОБВ «Работа в России»', 
                 title=NULL,
                 outerCI=0, ylab='Отрасль', zeroColor='red', names = c('Подвыборка мужчин', 'Подвыборка женщин')) + 
  scale_colour_discrete("Модель") + theme(text = element_text(size=20)) + scale_y_discrete(labels=namesportal_cp5)
cp5
cairo_pdf(file = '5.pdf', width = 21, height = 12)
cp5
dev.off()

younger_30 <- lm(log(cv_birthday_1990) ~ log(infections):C(oced) + Rt + C(oced):level_binary + C(oced) + C(region) + C(week), data[data$cv_birthday_1990 != 0,])
older_30 <- lm(log(unemployed - cv_birthday_1990) ~ log(infections):C(oced) + Rt + C(oced):level_binary + C(oced) + C(region) + C(week), data[data$unemployed != data$cv_birthday_1990,])

model_names <- younger_30$coefficients[grepl('level_binary', names(younger_30$coefficients), fixed = TRUE)]
model_names <- names(model_names[order(model_names)])
namesportal_cp6 <- substr(model_names, 8, nchar(model_names) - 13)

cp6 <- multiplot(younger_30, older_30, predictors='level_binary', sort='magnitude', decreasing=FALSE, lwdInner=1, xlab='Влияние карантина на заявки по безработице, %', 
                 #title='Модели по справочнику ИАС ОБВ «Работа в России»', 
                 title=NULL, 
                 outerCI=0, ylab='Отрасль', zeroColor='red', names = c('Подвыборка моложе 30', 'Подвыборка старше 30')) + 
  scale_colour_discrete("Модель") + theme(text = element_text(size=20)) + scale_y_discrete(labels=namesportal_cp6)
cp6
cairo_pdf(file = '6.pdf', width = 21, height = 12)
cp6
dev.off()

age_20 <- lm(log(cv_birthday_1990) ~ log(infections):C(oced) + Rt + level_binary + C(oced) + C(region) + C(week), data[data$cv_birthday_1990 != 0,])
age_30 <- lm(log(cv_birthday_1980 - cv_birthday_1990) ~ log(infections):C(oced) + Rt + level_binary + C(oced) + C(region) + C(week), data[data$cv_birthday_1980 != data$cv_birthday_1990,])
age_40 <- lm(log(cv_birthday_1970 - cv_birthday_1980) ~ log(infections):C(oced) + Rt + level_binary + C(oced) + C(region) + C(week), data[data$cv_birthday_1970 != data$cv_birthday_1980,])
age_50 <- lm(log(cv_birthday_1960 - cv_birthday_1970) ~ log(infections):C(oced) + Rt + level_binary + C(oced) + C(region) + C(week), data[data$cv_birthday_1960 != data$cv_birthday_1970,])
age_60 <- lm(log(unemployed - cv_birthday_1960) ~ log(infections):C(oced) + Rt + level_binary + C(oced) + C(region) + C(week), data[data$unemployed != data$cv_birthday_1960,])
multiplot(age_20, age_30, age_40, age_50, age_60, predictors='level_binary', sort='magnitude', decreasing=FALSE, xlab='Влияние карантина на заявки по безработице, %', title='Модели по справочнику ИАС ОБВ «Работа в России»', outerCI=0, ylab='Отрасль', zeroColor='red')

# cv_

propensity <- mean(data$level_binary[!is.na(data$level_binary)])

data$level_binary_placebo <- NA
data$level_binary_placebo[!is.na(data$level_binary)] <- rbinom(sum(!is.na(data$level_binary)), 1, propensity)

placebo <- lm(log(unemployed) ~ log(infections):C(oced) + Rt + C(oced):level_binary_placebo + C(oced) + C(region) + C(week), data)

model_names <- placebo$coefficients[grepl('level_binary', names(placebo$coefficients), fixed = TRUE)]
model_names <- names(model_names[order(model_names)])
namesportal_cp4 <- substr(model_names, 8, nchar(model_names) - 21)


cp4 <- coefplot(placebo, predictors='level_binary_placebo', sort='magnitude', decreasing=FALSE, xlab='Влияние плацебо-карантина на заявки по безработице, %', 
                #title='Модели по справочнику ИАС ОБВ «Работа в России»', 
                title=NULL, 
                outerCI=0, ylab='Отрасль', zeroColor='red') + theme(text = element_text(size=20)) + scale_y_discrete(labels=namesportal_cp4) 
cp4
cairo_pdf(file = '4.pdf', width = 21, height = 12)
cp4
dev.off()

# horovitz-tompson
# controls:
#   + bartik style
# bartik style interactions
# log + norm
# other identification things
# double robustness

# выбросить логистику и начало трудовой деятельности
# проверить, откуда пропущенные значения
# poisson?
# Rt after or before?

# МОЛОДЕЖЬ!!!



did_wald(data) {
  
}
