

############################################################################################
################# Re estimates of COVID-19 in Chile                      ###################
################# Cuadrado C.                                            ###################
################# Escuela de Salud Pública. Universidad de Chile         ###################
################# v1 Last Update: Junio 17, 2020                          ###################
############################################################################################

# Load libraries

library("R0")
library("EpiEstim")
library("tidyr")
library("dplyr")
library("readr")
library("ggplot2")
library("readxl")
library("googlesheets4")

# Set working directory
setwd("~/Dropbox/COVID19/Estimaciones Re")

# R estimation by Regions ---------------------------------

# Download data by region
cases <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto13/CasosNuevosCumulativo_std.csv")
admin.codes <- read_excel("~/Documents/GitHub/UC-UCH-COVID-lab/underreporting/cut_2018_v03.xls")

# Recode region names and codes
admin.codes.reg <- admin.codes %>% rename(region=`Nombre Región`, codigo=`Código Región`) %>% 
                                   dplyr::select(region, codigo) %>% group_by(codigo) %>% summarise_all(first)
admin.codes.reg$region <- ifelse(admin.codes.reg$region=="La Araucanía","Araucanía", admin.codes.reg$region)
admin.codes.reg$region <- ifelse(admin.codes.reg$region=="Aysén del General Carlos Ibáñez del Campo","Aysén", admin.codes.reg$region)
admin.codes.reg$region <- ifelse(admin.codes.reg$region=="Libertador General Bernardo O'Higgins","O’Higgins", admin.codes.reg$region)
admin.codes.reg$region <- ifelse(admin.codes.reg$region=="Magallanes y de la Antártica Chilena","Magallanes", admin.codes.reg$region)
admin.codes.reg$region <- ifelse(admin.codes.reg$region=="Metropolitana de Santiago","Metropolitana", admin.codes.reg$region)

# Reshape and clean data

cases_reg <- cases %>% rename(region=Region,
                              date=Fecha,
                              new_cases=Total) %>% group_by(region) %>%  
                        dplyr::mutate(date = lubridate::ymd(date),
                                      week = lubridate::isoweek(date),
                                      time = seq(1,length(date)),
                                      new_cases = tidyr::replace_na(new_cases,0),
                                      region=ifelse(region=="Total", "Chile", region)) 

  
cases_reg <- left_join(cases_reg, admin.codes.reg, by="region")
cases_reg$codigo <- ifelse(cases_reg$region=="Chile", 0, as.numeric(cases_reg$codigo))
table(cases_reg$region, cases_reg$codigo)

# Example for Antofagasta (Biweekly Re)
t_start <- seq(2, nrow(cases_reg %>% filter(codigo==2 & !is.na(new_cases)))-13)   
t_end <- t_start + 13       

rm(Re)
Re <- estimate_R(incid = cases_reg %>% filter(codigo==2 & !is.na(new_cases)) %>% dplyr::pull(new_cases),
                 method="uncertain_si",
                 config = make_config(list(
                   mean_si = 5, std_si= 3.8,
                   min_mean_si = 3, max_mean_si = 7,
                   std_std_si = 0.5, std_mean_si= 1, 
                   min_std_si = 1.8, max_std_si = 5.8,
                   t_start = t_start, 
                   t_end = t_end)))
Re_cases <- Re
plot(Re)
cbind(tail(Re$R$`Mean(R)`, n=1),tail(Re$R$`Quantile.0.025(R)`, n=1),tail(Re$R$`Quantile.0.975(R)`, n=1))
ggsave("Re Antofagasta completo.png", width = 10*2.5, height = 8*2.5, units = "cm", dpi=300, limitsize = FALSE)


# Create empty dataset for loop
R0_data <- as.data.frame(matrix(NA, nrow = 1,ncol = ncol(Re$R)+1))

# Assign names to variables
colnames(R0_data) <- c(colnames(Re$R),"codigo")

# Loop over each region
# for (i in 1:nrow(table(cases_reg$codigo))){
for (i in 0:nrow(table(cases_reg$codigo))){
  t_start <- seq(2, nrow(cases_reg %>% filter(codigo==i))-13)   
  t_end <- t_start + 13    
  R0est <- estimate_R(incid = cases_reg %>% filter(codigo==i) %>% dplyr::pull(new_cases),
                      method="uncertain_si",
                      config = make_config(list(
                        mean_si = 5, std_si= 3.8,
                        min_mean_si = 3, max_mean_si = 7,
                        std_std_si = 0.5, std_mean_si= 1, 
                        min_std_si = 1.8, max_std_si = 5.8,
                        t_start = t_start, 
                        t_end = t_end)))
  Re <- R0est$R
  data_temp <- Re %>% mutate(codigo=i) 
  R0_data <- bind_rows(R0_data %>% filter(!is.na(codigo)),data_temp)
  rm(data_temp)  
}

# Save
R0_data<-left_join(R0_data %>% mutate(time=t_end) %>% filter(!is.na(`Mean(R)`)),
                       cases_reg)                  
save(R0_data, file="Re_region.R")

readr::write_excel_csv2(R0_data %>% filter(!is.na(region)),
                        paste0("Re_region.R",tail(R0_data$date,n=1),".csv"))
readr::write_excel_csv2(R0_data %>% filter(!is.na(region)) %>%  dplyr::select(date, week, region, `Mean(R)`,`Quantile.0.025(R)`,`Quantile.0.975(R)`),
                        paste0("~/Documents/GitHub/covid19-data/analisis/Re/data/Re_region_",tail(R0_data$date,n=1),".csv"))

# Graficos - Region
ggplot(R0_data %>% filter(time>15 & codigo!=0), aes(x=t_end, y=`Mean(R)`))+
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin=`Quantile.0.05(R)`,ymax=`Quantile.0.95(R)`), alpha = 0.5) +
  geom_hline(yintercept=1, linetype="solid", color = "darkgray", size=0.6) +
  # scale_y_continuous(limits=c(0,7)) +
  labs(title="Número de reproducción efectivo (Re) por Región.",
       y="Re",x="Días desde inicio del brote",
       caption = paste0("El número de reproducción efectivo (Re) indica cuantos nuevos casos produce de manera directa cada caso conocido.
                         Cálculos en base a un intervalo serial de 5 días (rango 3-7) y un período de 14 días. 
                         Cuadrado C. Escuela de Salud Pública. Universidad de Chile. Update: ",tail(R0_data$date,n=1))) +
  theme_minimal() +
  theme_minimal() +
  facet_wrap(region ~ .,scales = "free") 
ggsave("Re region completo.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
ggsave("~/Documents/GitHub/covid19-data/analisis/Re/graphs/Re region completo.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)

# Graficos - Region ultimas 2 semanas
ggplot(R0_data %>% filter(time>tail(R0_data$time,n=1)-14 & codigo!=0), aes(x=t_end, y=`Mean(R)`, group=region))+
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin=`Quantile.0.05(R)`,ymax=`Quantile.0.95(R)`), alpha = 0.5) +
  geom_hline(yintercept=1, linetype="solid", color = "darkgray", size=0.6) +
  scale_y_continuous(limits=c(0,2.5)) +
  labs(title="Número de reproducción efectivo (Re) por Región. Últimas 2 semanas.",
       y="Re",x="Días desde inicio del brote",
       caption = paste0("El número de reproducción efectivo (Re) indica cuantos nuevos casos produce de manera directa cada caso conocido.
                         Cálculos en base a un intervalo serial de 5 días (rango 3-7) y un período de 14 días. 
                         Cuadrado C. Escuela de Salud Pública. Universidad de Chile. Update: ",tail(R0_data$date,n=1))) +
  theme_minimal() +
  theme_minimal() +
  facet_wrap(region ~ .) 
ggsave("Re region ultimas 2 semanas.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
ggsave("~/Documents/GitHub/covid19-data/analisis/Re/graphs/Re region ultimas 2 semanas.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)

# # R estimation by Regions - death data ---------------------------------
# 
# # Download data by region
# deaths <- read_csv("https://raw.githubusercontent.com/jorgeperezrojas/covid19-data/master/csv/muertes.csv")
# 
# # Reshape and clean data
# death_long <- gather(deaths, date, deaths, "04/01/2020":rev(names(deaths))[1])
# death_long
# 
# death_reg <- death_long %>% group_by(region) %>%  dplyr::mutate(date = lubridate::mdy(date),
#                                                                 new_deaths = deaths-lag(deaths),
#                                                                 week = lubridate::isoweek(date),
#                                                                 time = seq(1,length(date)),
#                                                                 new_deaths = tidyr::replace_na(new_deaths,0)) 
# 
# death_reg <- bind_rows(death_reg, death_reg %>% dplyr::group_by(date, week) %>%  dplyr::summarise(new_deaths=sum(new_deaths,na.rm = T),
#                                                                                                   deaths=sum(deaths,na.rm = T)) %>%
#                          dplyr::mutate(region="Chile",codigo=0,
#                                        time = seq(1,length(date))))
# 
# 
# # Example for RM (Biweekly Re)
# t_start <- seq(2, nrow(death_reg %>% filter(codigo==13 & deaths>0  & !is.na(new_deaths)))-13)   
# t_end <- t_start + 13       
# 
# rm(Re)
# Re <- estimate_R(incid = death_reg %>% filter(codigo==13 & deaths>0) %>% dplyr::pull(new_deaths),
#                  method="uncertain_si",
#                  config = make_config(list(
#                    mean_si = 6.48, std_si= 3.83,
#                    min_mean_si = 4.48, max_mean_si = 8.48,
#                    std_std_si = 0.5, std_mean_si= 1, 
#                    min_std_si = 1.83, max_std_si = 5.83,
#                    t_start = t_start, 
#                    t_end = t_end)))
# plot(Re)
# Re_death <- Re
# cbind(tail(Re$R$`Mean(R)`, n=1),tail(Re$R$`Quantile.0.025(R)`, n=1),tail(Re$R$`Quantile.0.975(R)`, n=1))
# 
# # Create empty dataset for loop
# R0_data <- as.data.frame(matrix(NA, nrow = 1,ncol = ncol(Re$R)+1))
# 
# # Assign names to variables
# colnames(R0_data) <- c(colnames(Re$R),"codigo")
# 
# # Loop over each region
# for (i in 0:(nrow(table(death_reg$codigo)))){
#   t_start <- seq(2, nrow(death_reg %>% filter(codigo==i))-13)   
#   t_end <- t_start + 13    
#   R0est <- estimate_R(incid = death_reg %>% filter(codigo==i) %>% dplyr::pull(new_deaths),
#                       method="uncertain_si",
#                       config = make_config(list(
#                         mean_si = 5, std_si= 3.8,
#                         min_mean_si = 3, max_mean_si = 7,
#                         std_std_si = 0.5, std_mean_si= 1, 
#                         min_std_si = 1.8, max_std_si = 5.8,
#                         t_start = t_start, 
#                         t_end = t_end)))
#   Re <- R0est$R
#   data_temp <- Re %>% mutate(codigo=i) 
#   R0_data <- bind_rows(R0_data %>% filter(!is.na(codigo)),data_temp)
#   rm(data_temp)  
# }
# 
# # Save data 
# 
# R0_data<-left_join(R0_data %>% mutate(time=t_end) %>% filter(!is.na(`Mean(R)`)),
#                    cases_reg)                  
# save(R0_data, file="Re_deaths_region.R")
# 
# readr::write_excel_csv2(R0_data %>% filter(!is.na(region)),
#                         paste0("Re_deaths_region.R",tail(R0_data$date,n=1),".csv"))
# readr::write_excel_csv2(R0_data %>% filter(!is.na(region)) %>%  dplyr::select(date, week, region, `Mean(R)`,`Quantile.0.025(R)`,`Quantile.0.975(R)`),
#                         paste0("~/Documents/GitHub/covid19-data/analisis/Re/data/Re_deaths_region.R",tail(R0_data$date,n=1),".csv"))
# 
# # Graficos - Region
# ggplot(R0_data %>% filter(time>20 & codigo!=0), aes(x=t_end, y=`Mean(R)`))+
#   geom_line() + 
#   geom_point() + 
#   geom_ribbon(aes(ymin=`Quantile.0.05(R)`,ymax=`Quantile.0.95(R)`), alpha = 0.5) +
#   geom_hline(yintercept=1, linetype="solid", color = "darkgray", size=0.6) +
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits=c(0,15)) +
#   labs(title="Número de reproducción efectivo (Re) por Región.",
#        y="Re",x="Días desde inicio del brote",
#        caption = paste0("El número de reproducción efectivo (Re) indica cuantos nuevos casos produce de manera directa cada caso conocido.
#                          Cálculos en base a un intervalo serial de 5 días (rango 3-7) y un período de 14 días.
#                          Se utilizan las nuevas muertes COVID-19 reportadas por región para este análisis.
#                          Cuadrado C. Escuela de Salud Pública. Universidad de Chile. Update: ",tail(R0_data$date,n=1))) +
#   theme_minimal() +
#   theme_minimal() +
#   facet_wrap(region ~ .) 
# ggsave("Re deaths region completo.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
# ggsave("~/Documents/GitHub/covid19-data/analisis/Re/graphs/Re deaths region completo.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
# 
# 
# # Graficos - Region ultimas 2 semanas
# ggplot(R0_data %>% filter(time>tail(R0_data$time,n=1)-14 & codigo!=0), aes(x=t_end, y=`Mean(R)`, group=region))+
#   geom_line() + 
#   geom_point() + 
#   geom_ribbon(aes(ymin=`Quantile.0.05(R)`,ymax=`Quantile.0.95(R)`), alpha = 0.5) +
#   geom_hline(yintercept=1, linetype="solid", color = "darkgray", size=0.6) +
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits=c(0,3)) +
#   labs(title="Número de reproducción efectivo (Re) por Región. Últimas 2 semanas.",
#        y="Re",x="Días desde inicio del brote",
#        caption = paste0("El número de reproducción efectivo (Re) indica cuantos nuevos casos produce de manera directa cada caso conocido.
#                          Cálculos en base a un intervalo serial de 5 días (rango 3-7) y un período de 14 días.
#                          Se utilizan las nuevas muertes COVID-19 reportadas por región para este análisis.
#                          Cuadrado C. Escuela de Salud Pública. Universidad de Chile. Update: ",tail(R0_data$date,n=1))) +
#   theme_minimal() +
#   theme_minimal() +
#   facet_wrap(region ~ .) 
# ggsave("Re deaths region ultimas 2 semanas.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
# ggsave("~/Documents/GitHub/covid19-data/analisis/Re/graphs/Re deaths region ultimas 2 semanas.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
# 
# # Comparación Re para Chile con datos de casos, UCI y muertes ---------------------
# 
# # Estimaciones Re por casos para Chile
# t_start <- seq(2, nrow(cases_reg %>% filter(codigo==0))-13)   
# t_end <- t_start + 13       
# 
# rm(Re_cases)
# Re_cases <- estimate_R(incid = cases_reg %>% filter(codigo==0) %>% dplyr::pull(new_cases),
#                  method="uncertain_si",
#                  config = make_config(list(
#                    mean_si = 6.48, std_si= 3.83,
#                    min_mean_si = 4.48, max_mean_si = 8.48,
#                    std_std_si = 0.5, std_mean_si= 1, 
#                    min_std_si = 1.83, max_std_si = 5.83,
#                    t_start = t_start, 
#                    t_end = t_end)))
# cbind(tail( Re_cases$R$`Mean(R)`, n=1),tail(Re_cases$R$`Quantile.0.025(R)`, n=1),tail( Re_cases$R$`Quantile.0.975(R)`, n=1))
# 
# # Estimaciones Re por muertes para Chile
# t_start <- seq(2, nrow(death_reg %>% filter(codigo==0))-13)   
# t_end <- t_start + 13       
# 
# rm(Re_death)
# Re_death <- estimate_R(incid = death_reg %>% filter(codigo==0) %>% dplyr::pull(new_deaths),
#                        method="uncertain_si",
#                        config = make_config(list(
#                          mean_si = 6.48, std_si= 3.83,
#                          min_mean_si = 4.48, max_mean_si = 8.48,
#                          std_std_si = 0.5, std_mean_si= 1, 
#                          min_std_si = 1.83, max_std_si = 5.83,
#                          t_start = t_start, 
#                          t_end = t_end)))
# cbind(tail( Re_death$R$`Mean(R)`, n=1),tail(Re_death$R$`Quantile.0.025(R)`, n=1),tail( Re_death$R$`Quantile.0.975(R)`, n=1))
# 
# 
# Re_cases2 <- Re_cases$R %>% dplyr::select(t_start, t_end, `Mean(R)`, `Quantile.0.05(R)`,`Quantile.0.95(R)`) %>%
#              mutate(Metodo="Casos confirmados")
# Re_death2 <- Re_death$R %>% dplyr::select(t_start, t_end, `Mean(R)`, `Quantile.0.05(R)`,`Quantile.0.95(R)`) %>%
#              mutate(Metodo="Muertes",
#                      t_end=t_end+12)
# 
# Re_comparison <- bind_rows(Re_cases2,Re_death2)
# 
# ggplot(Re_comparison %>% filter(t_end>=24), aes(x=t_end, y=`Mean(R)`, color=Metodo, group=Metodo))+
#   geom_line() + 
#   geom_point() + 
#   geom_ribbon(aes(ymin=`Quantile.0.05(R)`,ymax=`Quantile.0.95(R)`), alpha = 0.5) +
#   geom_hline(yintercept=1, linetype="solid", color = "darkgray", size=0.6) +
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits=c(0,4)) +
#   labs(title="Número de reproducción efectivo (Re) en Chile",
#        y="Re",x="Días desde inicio del brote",
#        caption = paste0("El número de reproducción efectivo (Re) indica cuantos nuevos casos produce de manera directa cada caso conocido.
#                          Cálculos en base a un intervalo serial de 6.5 días (rango 4.5-8.5) y un período de 14 días.
#                          Se utilizan las nuevas muertes y nuevos casos confirmados COVID-19 reportadas por región para este análisis.
#                          Cuadrado C. Escuela de Salud Pública. Universidad de Chile. Update: ",tail(R0_data$date,n=1))) +
#   theme_minimal() 
# ggsave("Re comparasion casos y muertes Chile.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
# ggsave("~/Documents/GitHub/covid19-data/analisis/Re/graphs/Re comparasion casos y muertes Chile.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
# 
# 

# R estimation by county ------------------------------------------------------------

# Download data by county
cases_muni <- read_csv("https://raw.githubusercontent.com/jorgeperezrojas/covid19-data/master/csv/long_confirmados_comunas_interpolado.csv")
data <-read_sheet("https://docs.google.com/spreadsheets/d/1IjirW9zoW5H-x8AnQcQaKXZXiS1tCuTs0PxSkNxYE0g/edit#gid=0",
                  sheet = "Sheet1", range = cell_rows(c(2, NA)))

# Reshape and clean data
cases_muni$positivos_acumulados <- ifelse(cases_muni$positivos_acumulados=="-",0,cases_muni$positivos_acumulados)
cases_muni$positivos_acumulados <- as.numeric(cases_muni$positivos_acumulados)
cases_muni <- cases_muni %>% group_by(comuna) %>% arrange(lubridate::mdy(fecha)) %>%  
                                    dplyr::mutate(
                                            cases=positivos_acumulados,
                                            date = lubridate::mdy(fecha),
                                            new_cases = cases-lag(cases),
                                            week = lubridate::isoweek(date),
                                            time = seq(1,length(date)),
                                            new_cases=tidyr::replace_na(new_cases,0),
                                            new_cases=ifelse(new_cases<0,0,new_cases),
                                            time = seq(1,length(date))) %>% ungroup() %>% arrange(comuna,date)

# Example for Recoleta (Weekly Re)
t_start <- seq(2, nrow(cases_muni %>% filter(comuna=="Calama"))-13)   
t_end <- t_start + 13       

rm(Re)
Re <- estimate_R(incid = cases_muni %>% filter(comuna=="Calama") %>% dplyr::pull(new_cases),
                 method="uncertain_si",
                 config = make_config(list(
                   mean_si = 5, std_si= 3.8,
                   min_mean_si = 3, max_mean_si = 7,
                   std_std_si = 0.5, std_mean_si= 1, 
                   min_std_si = 1.8, max_std_si = 5.8,
                   t_start = t_start, 
                   t_end = t_end)))
plot(Re)  
cbind(tail(Re$R$`Mean(R)`, n=1),tail(Re$R$`Quantile.0.025(R)`, n=1),tail(Re$R$`Quantile.0.975(R)`, n=1))
ggsave("Re Calama completo.png", width = 10*2.5, height = 8*2.5, units = "cm", dpi=300, limitsize = FALSE)

# Create empty dataset for loop
R0_data_muni <- as.data.frame(matrix(NA, nrow = 1,ncol = ncol(Re$R)+1))

# Assign names to variables
colnames(R0_data_muni) <- c(colnames(Re$R),"comuna")
R0_data_muni

comunas <- unique(cases_muni$comuna)

# Loop over each region
# for (i in 1:nrow(table(cases_reg$codigo))){
for (i in comunas){
  skip_to_next <- FALSE
  tryCatch(estimate_R(incid = cases_muni %>% dplyr::filter(comuna==i) %>% dplyr::pull(new_cases),
                      method="uncertain_si",
                      config = make_config(list(
                        mean_si = 5, std_si= 3.8,
                        min_mean_si = 3, max_mean_si = 7,
                        std_std_si = 0.5, std_mean_si= 1, 
                        min_std_si = 1.8, max_std_si = 5.8,
                        t_start = t_start, 
                        t_end = t_end))), error = function(e) { skip_to_next <<- TRUE})
  if(skip_to_next) { next }     

  t_start <- seq(2, nrow(cases_muni %>% dplyr::filter(comuna==i))-13)   
  t_end <- t_start + 13    
  R0est <- estimate_R(incid = cases_muni %>% dplyr::filter(comuna==i) %>% dplyr::pull(new_cases),
                      method="uncertain_si",
                      config = make_config(list(
                        mean_si = 5, std_si= 3.8,
                        min_mean_si = 3, max_mean_si = 7,
                        std_std_si = 0.5, std_mean_si= 1, 
                        min_std_si = 1.8, max_std_si = 5.8,
                        t_start = t_start, 
                        t_end = t_end)))
  Re <- R0est$R
  data_temp <- Re %>% mutate(comuna=i) 
  R0_data_muni <- bind_rows(R0_data_muni,data_temp)
  rm(data_temp)
}

R0_data_muni2<-left_join(R0_data_muni %>% mutate(time=t_end) %>% filter(!is.na(`Mean(R)`)),
                        cases_muni %>% dplyr::select(comuna,codigo_comuna, region, codigo_region, date, time))                  

save(R0_data_muni2, file="Re_muni.R")
readr::write_excel_csv2(R0_data_muni2 %>% dplyr::select(date, comuna, region, `Mean(R)`,`Quantile.0.025(R)`,`Quantile.0.975(R)`),
                        paste0("~/Documents/GitHub/covid19-data/analisis/Re/data/Re_muni_",tail(R0_data_muni2$date,n=1),".csv"))

tableReMuni <- R0_data_muni2 %>%  group_by(comuna) %>% arrange(date) %>% 
                                  slice(tail(row_number(), 1)) %>% 
                                  dplyr::select(date, comuna, region, `Mean(R)`,`Quantile.0.025(R)`,`Quantile.0.975(R)`) %>% 
                                  filter(region=="Metropolitana" & `Quantile.0.025(R)`>=1) %>% arrange(desc(`Mean(R)`))
readr::write_excel_csv2(tableReMuni,
                        paste0("Table_Re_muni_",tail(R0_data_muni2$date,n=1),".csv"))

load(file="Re_muni.R")

# Graficos - Todas las comunas
# ggplot(R0_data_muni2, aes(x=t_end, y=`Mean(R)`, group=comuna))+
#   geom_line() + 
#   geom_point() + 
#   geom_ribbon(aes(ymin=`Quantile.0.05(R)`,ymax=`Quantile.0.95(R)`), alpha = 0.5) +
#   geom_hline(yintercept=1, linetype="solid", color = "darkgray", size=0.6) +
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(0,4)) +
#   labs(y="Re",x="Días desde inicio del brote",
#        caption = paste0("Cuadrado C. Escuela de Salud Pública. Universidad de Chile. Update: ",tail(R0_data_muni2$date,n=1))) +
#   theme_minimal() +
#   facet_wrap(comuna ~ region)
# ggsave("Re municipios.png", width = 10*2.5, height = 10*2.5, units = "cm", dpi=300, limitsize = FALSE)

# Graficos - Región Metropolitana

# Agregar data de cuarentena
data$comuna <- zoo::na.locf(data$...1 ) # Rellenar NA de comuna
data <- data %>% slice(.,1:(which(data == "Cordones sanitarios", arr.ind=TRUE)[1]-2))  # Eliminar data redundante
data$...2 <- data$...1 <- NULL

# Limpiar base colapsando por comuna y asignando 1 a las comunas con al menos una parte de ella en cuarentena en cada día
data$comuna <- sub(pattern=" urbano",replacement = "", x=data$comuna)  # Eliminar "urbano" de algunas comunas
data <- data %>% group_by(comuna) %>% mutate_each(~as.numeric(.)) %>%
                 summarise_all(~sum(.)) %>%  ungroup() %>%
                 mutate_at(vars(-comuna),~ifelse(.>0,1,0))

# Pasar a formato long y arreglar fechas
cuarentena_long <- tidyr:::gather(data, dia, cuarentena,colnames(data)[2]:tail(colnames(data),n=1), factor_key=F)
cuarentena_long <- cuarentena_long %>% separate(dia, sep ="-", c("day","month"))  %>%
  mutate(year=2020,
         month=tolower(month),
         month=ifelse(month=="mar",3,
                      ifelse(month=="abr",4,
                             ifelse(month=="may",5,
                                    ifelse(month=="june",6,
                                           ifelse(month=="july",7,NA))))),
         date=lubridate::make_date(year, month, day),
         semana=lubridate::week(date))

cuarentena_long <- cuarentena_long %>% dplyr::select(-day,-month,-year)
cuarentena_long  <- cuarentena_long %>% dplyr::select(comuna,date,cuarentena)
cuarentena_long$comuna <- stringi::stri_trans_general(cuarentena_long$comuna,"Latin-ASCII") # Eliminar acentos
R0_data_muni2$comuna <- stringi::stri_trans_general(R0_data_muni2$comuna ,"Latin-ASCII") # Eliminar acentos

R0_data_muni3 <- left_join(R0_data_muni2,cuarentena_long)
R0_data_muni3$cuarentena <- tidyr::replace_na(R0_data_muni3$cuarentena,0)

# Subset RM
R0_data_muniRM <- R0_data_muni3 %>% filter(region=="Metropolitana")

ggplot(R0_data_muniRM,
       aes(x=t_end, y=`Mean(R)`, group=comuna))+
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin=`Quantile.0.05(R)`,ymax=`Quantile.0.95(R)`), alpha = 0.5) +
  geom_hline(yintercept=1, linetype="solid", color = "darkgray", size=0.6) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(0,4)) +
  labs(title="Número de reproducción efectivo (Re) en comunas RM",
       y="Re",x="Días desde inicio del brote",
       # caption = paste0("El número de reproducción efectivo (Re) indica cuantos nuevos casos produce de manera directa cada caso conocido.
       #                   En color naranja se indica los períodos bajo cuarentena de cada comuna. Cálculos en base a un intervalo serial de 5 días (rango 3-7) y un período de 14 días.
       #                   Cuadrado C. Escuela de Salud Pública. Universidad de Chile. Update: ",tail(R0_data_muniRM$date,n=1))) +
        caption = paste0("El número de reproducción efectivo (Re) indica cuantos nuevos casos produce de manera directa cada caso conocido.
                         Cuadrado C. Escuela de Salud Pública. Universidad de Chile. Update: ",tail(R0_data_muniRM$date,n=1))) +
  theme_minimal() +
  # geom_segment(aes(y=-Inf, yend=Inf,x=t_end, xend=t_end, alpha=cuarentena),
  #              inherit.aes = F,colour="coral", size=4)+
  # scale_alpha_continuous(range=c(0,0.1))+
  # guides(alpha=F) +
  facet_wrap(comuna ~ ., scales = "free") 
# ggsave("Re municipios RM total.png", width = 10*3, height = 10*3, units = "cm", dpi=300, limitsize = FALSE)
ggsave("~/Documents/GitHub/covid19-data/analisis/Re/graphs/Re municipios RM total.png", width = 10*3, height = 10*3, units = "cm", dpi=300, limitsize = FALSE)

# Graficos - Región Metropolitana

# Subset RM solo últimos 14 días
R0_data_muniRM <- R0_data_muni3 %>% filter(region=="Metropolitana" & time>tail(R0_data_muni3$time,n=1)-14)

ggplot(R0_data_muniRM,
aes(x=t_end, y=`Mean(R)`, group=comuna))+
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin=`Quantile.0.05(R)`,ymax=`Quantile.0.95(R)`), alpha = 0.5) +
  geom_hline(yintercept=1, linetype="solid", color = "darkgray", size=0.6) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(0,3)) +
  labs(title="Número de reproducción efectivo (Re) en comunas RM, últimas 2 semanas",
       y="Re",x="Días desde inicio del brote",
       caption = paste0("El número de reproducción efectivo (Re) indica cuantos nuevos casos produce de manera directa cada caso conocido.
                         Cálculos en base a un intervalo serial de 5 días (rango 3-7) y un período de 14 días. 
                         Cuadrado C. Escuela de Salud Pública. Universidad de Chile. Update: ",tail(R0_data_muniRM$date,n=1))) +
  theme_minimal() +
  theme_minimal() +
  # geom_segment(aes(y=-Inf, yend=Inf,x=t_end, xend=t_end, alpha=cuarentena),
  #              inherit.aes = F,colour="coral", size=4)+
  # scale_alpha_continuous(range=c(0,0.1))+
  # guides(alpha=F) +
  facet_wrap(comuna ~ .)
# ggsave("Re municipios RM ultimas 2 semanas.png", width = 10*2.5, height = 10*2.5, units = "cm", dpi=300, limitsize = FALSE)
ggsave("~/Documents/GitHub/covid19-data/analisis/Re/graphs/Re municipios RM ultimas 2 semanas.png", width = 10*2.5, height = 10*2.5, units = "cm", dpi=300, limitsize = FALSE)

# Graficos de Re por comuna para cada región últimas 2 semanas
for (i in 1:length(unique(R0_data_muni2$codigo_region))){
ggplot(R0_data_muni2 %>% filter(codigo_region==i & time>tail(R0_data_muni3$time,n=1)-14), 
       aes(x=t_end, y=`Mean(R)`, group=comuna))+
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin=`Quantile.0.05(R)`,ymax=`Quantile.0.95(R)`), alpha = 0.5) +
  geom_hline(yintercept=1, linetype="solid", color = "darkgray", size=0.6) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(0,3)) +
  labs(y="Re",x="Días desde inicio",
       caption = paste0("Cuadrado C. Escuela de Salud Pública. Universidad de Chile. Update: ",tail(R0_data_muni2$date,n=1))) +
  theme_minimal() +
  facet_wrap(comuna ~ .)
  # ggsave(paste0("Re municipios - Region ",i,".png"), width = 10*2.5, height = 10*2.5, units = "cm", dpi=300, limitsize = FALSE)
  ggsave(paste0("~/Documents/GitHub/covid19-data/analisis/Re/graphs/Re municipios - Region",i,".png"), width = 10*2.5, height = 10*2.5, units = "cm", dpi=300, limitsize = FALSE)
}

# Re por Servicio de Salud ------------------------------------------------------------

# Download data by county

cases_muni <- read_csv("https://raw.githubusercontent.com/jorgeperezrojas/covid19-data/master/csv/long_confirmados_comunas_interpolado.csv")

SS_provincia_comuna <- read_excel("SS provincia comuna.xls", 
                                  col_types = c("numeric", "text", "text", 
                                                "text", "text", "numeric", "text", 
                                                "numeric"))
# Reshape and clean data
cases_muni$positivos_acumulados <- ifelse(cases_muni$positivos_acumulados=="-",0,cases_muni$positivos_acumulados)
cases_muni$positivos_acumulados <- as.numeric(cases_muni$positivos_acumulados)
cases_muni <- cases_muni %>% group_by(comuna) %>% arrange(lubridate::mdy(fecha)) %>%  
  dplyr::mutate(
    cases=positivos_acumulados,
    date = lubridate::mdy(fecha),
    new_cases = cases-lag(cases),
    week = lubridate::isoweek(date),
    time = seq(1,length(date)),
    new_cases=tidyr::replace_na(new_cases,0),
    new_cases=ifelse(new_cases<0,0,new_cases),
    time = seq(1,length(date))) %>% ungroup() %>% arrange(comuna,date)

# Merge with health regions (SS) dataset
cases_ss <- left_join(cases_muni, SS_provincia_comuna, by = "codigo_comuna") %>% 
            group_by(ss, codigo_ss, date, time) %>% summarise(new_cases=sum(new_cases, na.rm = T))

# Collapse data by health region (SS)

# Example for SSMN (Weekly Re)
t_start <- seq(2, nrow(cases_ss %>% filter(codigo_ss==9))-13)   
t_end <- t_start + 13       

rm(Re)
Re <- estimate_R(incid = cases_ss %>% filter(codigo_ss==9) %>% dplyr::pull(new_cases),
                 method="uncertain_si",
                 config = make_config(list(
                   mean_si = 5, std_si= 3.8,
                   min_mean_si = 3, max_mean_si = 7,
                   std_std_si = 0.5, std_mean_si= 1, 
                   min_std_si = 1.8, max_std_si = 5.8,
                   t_start = t_start, 
                   t_end = t_end)))
plot(Re)  
cbind(tail(Re$R$`Mean(R)`, n=1),tail(Re$R$`Quantile.0.025(R)`, n=1),tail(Re$R$`Quantile.0.975(R)`, n=1))

# Create empty dataset for loop
R0_data_ss <- as.data.frame(matrix(NA, nrow = 1,ncol = ncol(Re$R)+1))

# Assign names to variables
colnames(R0_data_ss) <- c(colnames(Re$R),"ss")
R0_data_ss

ss <- unique(cases_ss$ss)
ss

# Loop over each region
# for (i in 1:nrow(table(cases_reg$codigo))){
for (i in ss){
  skip_to_next <- FALSE
  tryCatch(estimate_R(incid = cases_ss %>% dplyr::filter(ss==i) %>% dplyr::pull(new_cases),
                      method="uncertain_si",
                      config = make_config(list(
                        mean_si = 5, std_si= 3.8,
                        min_mean_si = 3, max_mean_si = 7,
                        std_std_si = 0.5, std_mean_si= 1, 
                        min_std_si = 1.8, max_std_si = 5.8,
                        t_start = t_start, 
                        t_end = t_end))), error = function(e) { skip_to_next <<- TRUE})
  if(skip_to_next) { next }     
  
  t_start <- seq(2, nrow(cases_ss %>% dplyr::filter(ss==i))-13)   
  t_end <- t_start + 13    
  R0est <- estimate_R(incid = cases_ss %>% dplyr::filter(ss==i) %>% dplyr::pull(new_cases),
                      method="uncertain_si",
                      config = make_config(list(
                        mean_si = 5, std_si= 3.8,
                        min_mean_si = 3, max_mean_si = 7,
                        std_std_si = 0.5, std_mean_si= 1, 
                        min_std_si = 1.8, max_std_si = 5.8,
                        t_start = t_start, 
                        t_end = t_end)))
  Re <- R0est$R
  data_temp <- Re %>% mutate(ss=i) 
  R0_data_ss <- bind_rows(R0_data_ss,data_temp)
  rm(data_temp)
}


# Buscando el bug de Ñuble

cases_nuble <- left_join(cases_muni %>% filter(region=="Ñuble") , SS_provincia_comuna, by = "codigo_comuna") %>% 
  group_by(ss, codigo_ss, date, time) %>% summarise(new_cases=sum(new_cases, na.rm = T))

cases_nuble$ss <- replace_na(cases_nuble$ss,"Ñuble")

# Example for Ñuble (Weekly Re)
t_start <- seq(2, nrow(cases_nuble)-13)   
t_end <- t_start + 13   

rm(Re)
Re <- estimate_R(incid = cases_nuble %>% dplyr::pull(new_cases),
                 method="uncertain_si",
                 config = make_config(list(
                   mean_si = 5, std_si= 3.8,
                   min_mean_si = 3, max_mean_si = 7,
                   std_std_si = 0.5, std_mean_si= 1, 
                   min_std_si = 1.8, max_std_si = 5.8,
                   t_start = t_start, 
                   t_end = t_end)))
plot(Re)  
cbind(tail(Re$R$`Mean(R)`, n=1),tail(Re$R$`Quantile.0.025(R)`, n=1),tail(Re$R$`Quantile.0.975(R)`, n=1))

Re <- R0est$R
data_temp <- Re %>% mutate(ss="Ñuble") 
R0_data_ss <- bind_rows(R0_data_ss,data_temp)

R0_data_ss2<-left_join(R0_data_ss %>% mutate(time=t_end) %>% filter(!is.na(`Mean(R)`)),
                       cases_ss %>% dplyr::select(ss, date, time))                  

tableReSS <- R0_data_ss2 %>%  group_by(ss) %>% arrange(date) %>% 
  slice(tail(row_number(), 1)) %>% 
  dplyr::select(date, ss, `Mean(R)`,`Quantile.0.025(R)`,`Quantile.0.975(R)`) %>% 
  arrange(desc(`Mean(R)`))

save(R0_data_ss2, file="Re_SS.R")
readr::write_excel_csv2(R0_data_ss2 %>% dplyr::select(date, ss, codigo_ss, `Mean(R)`,`Quantile.0.025(R)`,`Quantile.0.975(R)`),
                        paste0("~/Documents/GitHub/covid19-data/analisis/Re/data/Re_SS_",tail(R0_data_muni2$date,n=1),".csv"))

load(file="Re_SS.R")
readr::write_excel_csv2(tableReSS,
                        paste0("Table_Re_SS_",tail(R0_data_muni2$date,n=1),".csv"))

# Graficos - Servicios de salud
ggplot(R0_data_ss2 %>% filter(time>20), aes(x=t_end, y=`Mean(R)`, group=ss))+
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin=`Quantile.0.05(R)`,ymax=`Quantile.0.95(R)`), alpha = 0.5) +
  geom_hline(yintercept=1, linetype="solid", color = "darkgray", size=0.6) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits=c(0,5)) +
  labs(title="Número de reproducción efectivo (Re) en Servicios de Salud",
       y="Re",x="Días desde inicio del brote",
       caption = paste0("*El número de reproducción efectivo (Re) indica cuantos nuevos casos produce cada caso conocido \n 
                        Cuadrado C. Escuela de Salud Pública. Universidad de Chile. Update: ",tail(R0_data_muni2$date,n=1))) +
  theme_minimal() +
  theme_minimal() +
  facet_wrap(ss ~ .) 
# ggsave("Re SS completo.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
ggsave("~/Documents/GitHub/covid19-data/analisis/Re/graphs/Re SS completo.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)


# Graficos - Servicios de salud
ggplot(R0_data_ss2 %>% filter(time>tail(R0_data_ss2$time,n=1)-14), aes(x=t_end, y=`Mean(R)`, group=ss))+
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin=`Quantile.0.05(R)`,ymax=`Quantile.0.95(R)`), alpha = 0.5) +
  geom_hline(yintercept=1, linetype="solid", color = "darkgray", size=0.6) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits=c(0,3)) +
  labs(title="Número de reproducción efectivo (Re) en Servicios de Salud, últimas 2 semanas",
       y="Re",x="Días desde inicio del brote",
       caption = paste0("*El número de reproducción efectivo (Re) indica cuantos nuevos casos produce cada caso conocido \n 
                        Cuadrado C. Escuela de Salud Pública. Universidad de Chile. Update: ",tail(R0_data_muni2$date,n=1))) +
  theme_minimal() +
  theme_minimal() +
  facet_wrap(ss ~ .) 
# ggsave("Re SS ultimas 2 semanas.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
ggsave("~/Documents/GitHub/covid19-data/analisis/Re/graphs/Re SS ultimas 2 semanas.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)

                  