
############################################################################################
################# Under-reporting estimates & case-fatality of COVID-19 in Chile ###########
################# Cuadrado C.                                                    ###########
################# Escuela de Salud Pública. Universidad de Chile         ###################
################# v1 Last Update: June 17, 2020                          ###################
############################################################################################
#
# Initial code based on: Russell et al. Using a delay-adjusted case fatality ratio to estimate under-reporting. First online: 22-03-2020 | Last update: 08-04-2020. Available at: https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html?fbclid=IwAR31V4DbTkUDkJJKpfJMI1M7sYxt16EMQ9yRH5Y-lV0lAIH2mbkfkFZ5zeE
# Original estimation method: Nishiura et al. Early epidemiological assessment of the virulence of emerging infectious diseases: A case study of an influenza pandemic. PLoS One 2009;4.

######### cCFR and Under-reporting estimation based on LSHTM method ---------------------------------------------------

### Clean work space
rm(list = ls(all.names = TRUE))

# Set up paths and parameters ---------------------------------------------

# Load libraries
library(tidyverse)
library(padr)
library(tidyr)

# Set parameters
# zmeanHDT <- 13 # Original parameters used by Russel et al
# zmedianHDT <- 9.1 # Original parameters used by Russel et al

zmeanHDT <- 12.23785 # Data for Chile week 12 to 20. Time from confirmation to death
zmedianHDT <- 10 # Data for Chile week 12 to 20. Time from confirmation to death
# zmeanHDT <- 14.34823 # Data for Chile week 12 to 20. Time from confirmation to death
# zmedianHDT <- 13 # Data for Chile week 12 to 20. Time from confirmation to death

# Estimate lognormal parameters from mean and median
muHDT <- log(zmedianHDT)
sigmaHDT <- sqrt(2*(log(zmeanHDT) - muHDT))
cCFRBaseline <- 1.38
cCFREstimateRange <- c(1.23, 1.53)
#cCFRIQRRange <- c(1.3, 1.4)

# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, muHDT, sigmaHDT) - plnorm(x, muHDT, sigmaHDT)
}


# Define CFR function -----------------------------------------------------

# Function to work out correction CFR
scale_cfr <- function(data_1_in, delay_fun){
  case_incidence <- data_1_in$new_cases
  death_incidence <- data_1_in$new_deaths
  date <- tail(data_1_in$date,1)
  cumulative_known_t <- 0 # cumulative cases with known outcome at time tt
  # Sum over cases up to time tt
  for(ii in 1:nrow(data_1_in)){
    known_i <- 0 # number of cases with known outcome at time ii
    for(jj in 0:(ii - 1)){
      known_jj <- (case_incidence[ii - jj]*delay_fun(jj))
      known_i <- known_i + known_jj
    }
    cumulative_known_t <- cumulative_known_t + known_i # Tally cumulative known
  }
  # naive CFR value
  b_tt <- sum(death_incidence)/sum(case_incidence) 
  # corrected CFR estimator
  p_tt <- sum(death_incidence)/cumulative_known_t
  data.frame(date=date, nCFR = b_tt, cCFR = p_tt, total_deaths = sum(death_incidence), 
             cum_known_t = round(cumulative_known_t), total_cases = sum(case_incidence))
}

#### Run analysis for Chile by date  -----------------------------------------------------------

# Load data
deaths <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto37/Defunciones_std.csv")
cases <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo_std.csv")

# Tidy data
deaths <- deaths %>% arrange(Fecha) %>% group_by(Fecha) %>% dplyr::select(Total) %>% slice(tail(row_number(), 1)) %>%
                     rename(date=Fecha, new_deaths=Total) %>% ungroup() %>% mutate(new_deaths=tidyr::replace_na(new_deaths,0))

cases <- cases %>% rename(region= Region, date=Fecha, cases=Total) %>% group_by(region) %>%
  dplyr::mutate(date = lubridate::ymd(date),
                new_cases = cases-lag(cases),
                week = lubridate::isoweek(date),
                new_cases=tidyr::replace_na(new_cases,0),
                new_cases=ifelse(new_cases<0,0,new_cases),
                time = seq(1,length(date))) %>% ungroup() %>% filter(region=="Total") %>% dplyr::select(-region)

covidchile <- left_join(cases,deaths)

covidchile <- covidchile %>% mutate(new_deaths=tidyr::replace_na(new_deaths,0),
                                    deaths = cumsum(new_deaths)) %>% filter(deaths>0)
covidchile

# Create matrix of results
m.ur.cfr.Chile <- as.data.frame(matrix(NA, nrow = nrow(covidchile), ncol = 15))

# Assign names to variables
colnames(m.ur.cfr.Chile) <- c("date","nCFR","cCFR","total_deaths","cum_known_t" ,"total_cases",             
                               "nCFR_UQ","nCFR_LQ","cCFR_UQ", "cCFR_LQ","underreporting_estimate",
                                "lower","upper","quantile25","quantile75")

# Loop over each day
for (i in 1:(nrow(covidchile)-2)){
  m.ur.cfr.Chile[i,] <- covidchile %>%
  # padr::pad() %>%
  slice(., 1:(n()-i)) %>%
  dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
  dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
  dplyr::filter(cum_deaths > 0) %>%
  dplyr::select(-cum_deaths) %>%
  dplyr::do(scale_cfr(., delay_fun = hospitalisation_to_death_truncated)) %>%
  dplyr::filter(cum_known_t > 0 & cum_known_t >= total_deaths)  %>%
  dplyr::mutate(nCFR_UQ = binom.test(total_deaths, total_cases)$conf.int[2],
                nCFR_LQ = binom.test(total_deaths, total_cases)$conf.int[1],
                cCFR_UQ = binom.test(total_deaths, cum_known_t)$conf.int[2],
                cCFR_LQ = binom.test(total_deaths, cum_known_t)$conf.int[1],
                underreporting_estimate = cCFRBaseline / (100*cCFR),
                lower = cCFREstimateRange[1] / (100 * cCFR_UQ),
                upper = cCFREstimateRange[2] / (100 * cCFR_LQ),
                quantile25 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[1],
                quantile75 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[2],
                date=date)
}

# Fix date format
m.ur.cfr.Chile$date <- zoo::as.Date(m.ur.cfr.Chile$date)

# Clean results
m.ur.cfr.Chile<- m.ur.cfr.Chile %>% dplyr::mutate(underreporting_estimate = ifelse(underreporting_estimate <= 1, underreporting_estimate, 1)) %>%
                                    dplyr::mutate(upper = ifelse(upper <= 1, upper, 1)) %>%
                                    dplyr::mutate(underreporting_estimate = signif(underreporting_estimate, 2)) %>%
                                    dplyr::mutate(lower = signif(lower, 2)) %>%
                                    dplyr::mutate(upper = signif(upper, 2)) %>%
                                    dplyr::mutate(underreporting_estimate_clean = paste0(underreporting_estimate*100,
                                                                                         "% (",lower*100,"% - ",upper*100,"%)"))

m.ur.cfr.Chile <- m.ur.cfr.Chile %>% arrange(date) %>% mutate(new_cases=total_cases-lag(total_cases),
                                                              expected_cases = new_cases*(1+underreporting_estimate)) %>%
                                     slice(., 7:(n()-7)) # Filtro primeras 7 y últimos 7 observaciones

m.ur.cfr.Chile.clean <- m.ur.cfr.Chile %>% dplyr::filter(!is.na(nCFR) & !is.na(new_cases))%>%
                          dplyr::select(date, new_cases, total_cases, total_deaths, underreporting_estimate, lower,
                                        upper) %>%
                          dplyr::mutate(underreporting_estimate = 1-underreporting_estimate) %>%
                          dplyr::mutate(lower2 = 1-upper) %>%
                          dplyr::mutate(upper = 1-lower) %>%
                          dplyr::mutate(lower = lower2) %>%
                          dplyr::mutate(underreporting_estimate = signif(underreporting_estimate, 2)) %>%
                          dplyr::mutate(lower = signif(lower, 2)) %>%
                          dplyr::mutate(upper = signif(upper, 2)) %>%
                          dplyr::mutate(underreporting_estimate_clean = paste0(underreporting_estimate*100,
                                                                               "% (",lower*100,"% - ",upper*100,"%)"),
                                        expected_new_cases = round(new_cases*(1+underreporting_estimate))) %>% 
                          dplyr::arrange(desc(date)) %>%
                          dplyr::select(date, underreporting_estimate, lower, upper, underreporting_estimate_clean)
m.ur.cfr.Chile.clean
readr::write_excel_csv2(m.ur.cfr.Chile.clean,
                       "~/Documents/GitHub/covid19-data/analisis/Subreporte/Subreporte_Chile.csv")

m.case.fatality.Chile.clean  <- m.ur.cfr.Chile %>% dplyr::filter(!is.na(nCFR) & !is.na(new_cases))%>%
                                dplyr::select(date, new_cases, total_cases, total_deaths,
                                              nCFR, nCFR_LQ, nCFR_UQ, cCFR, cCFR_LQ, cCFR_UQ) %>%
                                mutate(letalidad_caso_cruda = paste0(signif(nCFR,3)*100, "% (",signif(nCFR_LQ,3)*100,"% - ",signif(nCFR_UQ,3)*100,"%)"),
                                       letalidad_caso_aj_retraso = paste0(signif(cCFR,3)*100, "% (",signif(cCFR_LQ,3)*100,"% - ",signif(cCFR_UQ,3)*100,"%)")) %>%
                                dplyr::select(-new_cases, -total_cases, -total_deaths)
m.case.fatality.Chile.clean
readr::write_excel_csv2(m.case.fatality.Chile.clean,
                        "~/Documents/GitHub/covid19-data/analisis/Letalidad/Letalidad_Chile.csv")

# Graph results
underreport <- ggplot(m.ur.cfr.Chile %>% filter(!is.na(underreporting_estimate)), aes(date, 1-underreporting_estimate)) + geom_line() + geom_point() +
       geom_ribbon(aes(ymin=1-upper,ymax=1-lower), alpha = 0.5) +
       scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(0,1), labels = scales::percent_format(accuracy = 1L)) +
       labs(y="Subreporte de casos sintomáticos COVID-19 (% del total de casos)",x="Fecha", 
           title = "Estimación de subreporte de casos COVID-19, Chile",
           caption = paste("Cuadrado C. Escuela de Salud Pública. Universidad de Chile. Update:",Sys.Date()))
underreport

cCFR <- ggplot(m.ur.cfr.Chile %>% filter(!is.na(cCFR)), aes(date, cCFR)) + geom_line() + geom_point() +
        geom_ribbon(aes(ymax=cCFR_UQ,ymin=cCFR_LQ), alpha = 0.5) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(0,0.1), labels = scales::percent_format(accuracy = 1L)) +
        labs(y="Tasa de letalidad ajustada por retraso",x="Fecha", 
             title = "Estimación de letalidad COVID-19 ajustada por retraso, Chile",
             caption = paste("Cuadrado C. Escuela de Salud Pública. Universidad de Chile.",Sys.Date()))
cCFR   

cCFR2 <- ggplot(m.ur.cfr.Chile %>% filter(!is.na(cCFR)), aes(date, cCFR)) + 
  geom_line(aes(color="Ajustada por retraso")) + geom_point(aes(color="Ajustada por retraso")) +
  geom_ribbon(aes(ymax=cCFR_UQ,ymin=cCFR_LQ, fill="band"), show.legend=FALSE, alpha = 0.2) +
  geom_line(aes(date, nCFR, color="Cruda")) + geom_point(aes(date, nCFR, color="Cruda")) +
  geom_ribbon(aes(ymax=nCFR_UQ,ymin=nCFR_LQ, fill="band2"), show.legend=FALSE, alpha = 0.2) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(0,0.05), labels = scales::percent_format(accuracy = 1L)) +
  labs(y="Tasa de letalidad ajustada por retraso",x="Fecha") +
  scale_color_manual(values = c("firebrick3","goldenrod1"), name="Tasa de letalidad") +
  scale_fill_manual(values = c("firebrick3","goldenrod1")) +
  theme_minimal() +
  theme(legend.position = "bottom")
cCFR2

    underreport
    # ggsave("underreport.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    ggsave("~/Documents/GitHub/covid19-data/analisis/Subreporte/subreporte Chile.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    
    cCFR
    # ggsave("cCFR.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    ggsave("~/Documents/GitHub/covid19-data/analisis/Letalidad/letalidad Chile.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    
    cCFR2
    # ggsave("cCFR2.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    ggsave("~/Documents/GitHub/covid19-data/analisis/Letalidad/letalidad Chile 2.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    

#### Run analysis for Chile by region ------------------------------------------------------

    # Load data
    cases <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo_std.csv")
    deaths <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto14/FallecidosCumulativo_std.csv")
    
    # Tidy data
    cases <- cases %>% rename(region= Region, date=Fecha, cases=Total) %>% group_by(region) %>%
      dplyr::mutate(date = lubridate::ymd(date),
                    new_cases = cases-lag(cases),
                    week = lubridate::isoweek(date),
                    time = seq(1,length(date)),
                    new_cases=tidyr::replace_na(new_cases,0),
                    new_cases=ifelse(new_cases<0,0,new_cases),
                    time = seq(1,length(date))) %>% ungroup() %>% arrange(region,date)
    
    deaths <- deaths %>% rename(region= Region, date=Fecha, deaths=Total) %>% group_by(region) %>%
                    dplyr::mutate(date = lubridate::ymd(date),
                                  new_deaths = deaths-lag(deaths),
                                  week = lubridate::isoweek(date),
                                  new_deaths=tidyr::replace_na(new_deaths,0),
                                  new_deaths=ifelse(new_deaths<0,0,new_deaths)) %>% ungroup() %>% arrange(region,date)
    
    region.data <- left_join(cases, deaths)
    
    region.data.desc <- region.data %>% 
      dplyr::arrange(region, date) %>% 
      dplyr::ungroup() %>%
      mutate(new_deaths=tidyr::replace_na(new_deaths,0),
             region=ifelse(region=="Total","Chile",region))  %>%
      dplyr::select(date, region, new_cases, new_deaths)

    region.data.desc
    
    # Do analysis
    
    underreport.byregion <- region.data.desc %>%
      dplyr::group_by(region) %>%
      slice(., 1:(n()-7)) %>% # últimas 7 observaciones
      # dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
      #               new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
      dplyr::group_by(region) %>%
      dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
      dplyr::filter(cum_deaths > 0) %>%
      dplyr::select(-cum_deaths) %>%
      dplyr::do(scale_cfr(., delay_fun = hospitalisation_to_death_truncated)) %>%
      dplyr::filter(cum_known_t > 0 & cum_known_t >= total_deaths)  %>%
      dplyr::mutate(nCFR_UQ = binom.test(total_deaths, total_cases)$conf.int[2],
                    nCFR_LQ = binom.test(total_deaths, total_cases)$conf.int[1],
                    cCFR_UQ = binom.test(total_deaths, cum_known_t)$conf.int[2],
                    cCFR_LQ = binom.test(total_deaths, cum_known_t)$conf.int[1],
                    underreporting_estimate = cCFRBaseline / (100*cCFR),
                    lower = cCFREstimateRange[1] / (100 * cCFR_UQ),
                    upper = cCFREstimateRange[2] / (100 * cCFR_LQ),
                    quantile25 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[1],
                    quantile75 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[2]
      ) %>% 
      dplyr::filter(total_deaths > 10)
    underreport.byregion
    
    underreport.byregionfinal <- underreport.byregion %>%
      dplyr::select(region, total_cases, total_deaths, underreporting_estimate, lower,
                    upper) %>%
      dplyr::mutate(underreporting_estimate = ifelse(underreporting_estimate>1,1,underreporting_estimate),
                    underreporting_estimate = 1-underreporting_estimate,
                    upper2 = ifelse(upper>1,1,upper),
                    upper = 1-lower,
                    lower = 1-upper2,
                    underreporting_estimate = signif(underreporting_estimate, 2),
                    lower = signif(lower, 2),
                    upper = signif(upper, 2)) %>%
      dplyr::select(-upper2) %>%
      dplyr::ungroup(region) %>%
      dplyr::mutate(region = region %>% stringr::str_replace_all("_", " ")) %>% 
      dplyr::mutate(underreporting_estimate_clean = paste0(underreporting_estimate*100,
                                                           "% (",lower*100,"% - ",upper*100,"%)")) %>% 
      dplyr::arrange(desc(underreporting_estimate))
    
    underreport.byregionfinal
    readr::write_excel_csv2(underreport.byregionfinal %>% filter(region!="Chile") %>% dplyr::select(-total_cases,-total_deaths),
                            "~/Documents/GitHub/covid19-data/analisis/Subreporte/Subreporte_regiones.csv")
    
    m.case.fatality.region.clean  <- underreport.byregion %>% filter(region!="Chile") %>%
      dplyr::select(region, nCFR, nCFR_LQ, nCFR_UQ, cCFR, cCFR_LQ, cCFR_UQ) %>%
      mutate(letalidad_caso_cruda = paste0(signif(nCFR,3)*100, "% (",signif(nCFR_LQ,3)*100,"% - ",signif(nCFR_UQ,3)*100,"%)"),
             letalidad_caso_aj_retraso = paste0(signif(cCFR,3)*100, "% (",signif(cCFR_LQ,3)*100,"% - ",signif(cCFR_UQ,3)*100,"%)"))
    m.case.fatality.region.clean
    readr::write_excel_csv2(m.case.fatality.region.clean,
                            "~/Documents/GitHub/covid19-data/analisis/Letalidad/Letalidad_regiones.csv")
    
   underreport.reg <- ggplot(data=underreport.byregionfinal %>% filter(total_deaths>10 & underreporting_estimate>0 & region!="Chile"), 
                             aes(reorder(region, -underreporting_estimate, sum),underreporting_estimate))+
      geom_col(fill = "#FF6666") +
      geom_errorbar(aes(ymin = lower, ymax = upper), width=0.2) +
      geom_text(aes(label = scales::percent(underreporting_estimate), 
                    y = underreporting_estimate, 
                    group = region),
                position = position_dodge(width = 0.9),
                vjust = 1.5) +
      labs(y="Subreporte casos sintomáticos COVID-19 (% del total de casos)",x="", 
           title = "Estimación de subreporte de casos COVID-19 en Chile por Región",
           caption = paste("Cuadrado C. Escuela de Salud Pública. Universidad de Chile. 
                           Líneas negras verticales representan el intervalo de credibilidad (95%) de la estimación.
                           Se grafica subreporte para regiones con más de 10 muertes acumuladas y 
                           subreporte estimado mayor a 0% promedio.
                           Update:",Sys.Date())) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(0,1), labels = scales::percent_format(accuracy = 1L)) +
      theme(axis.text.x = element_text(angle = 45)) +
      theme_minimal()
    
    underreport.reg
    # ggsave("underreport.reg.png")
    ggsave("~/Documents/GitHub/covid19-data/analisis/Subreporte/Subreporte por Región Chile.png", width = 12*2.5, height = 9*2.5, units = "cm", dpi=300, limitsize = FALSE)
    
    fatality.reg <- ggplot(data=m.case.fatality.region.clean, aes(reorder(region, -cCFR, sum),cCFR))+
      geom_col(fill = "#FF6666") +
      geom_errorbar(aes(ymin = cCFR_LQ, ymax = cCFR_UQ), width=0.2) +
      geom_text(aes(label = scales::percent(cCFR), 
                    y = cCFR, 
                    group = region),
                position = position_dodge(width = 0.9),
                vjust = 1.5) +
      labs(y="Tasa de letalidad ajustada por retraso",x="Región", 
           title = "Estimación de letalidad COVID-19 ajustada por retraso por Región, Chile",
           caption = paste("Cuadrado C. Escuela de Salud Pública. Universidad de Chile. 
                            Líneas negras verticales representan el intervalo de credibilidad (95%) de la estimación.
                           Update:",Sys.Date())) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(0,0.1), labels = scales::percent_format(accuracy = 1L)) +
      theme(axis.text.x = element_text(angle = 45)) +
      theme_minimal()
    
    fatality.reg
    # ggsave("fatality.reg.png")
    ggsave("~/Documents/GitHub/covid19-data/analisis/Letalidad/Letalidad por Región Chile.png", width = 12*2.5, height = 9*2.5, units = "cm", dpi=300, limitsize = FALSE)
    
#### Run analysis for Chile by day and region ------------------------------------------------------
    
    # Load data
    cases <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo_std.csv")
    deaths <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto14/FallecidosCumulativo_std.csv")
    
    # Tidy data
    cases <- cases %>% rename(region= Region, date=Fecha, cases=Total) %>% group_by(region) %>%
      dplyr::mutate(date = lubridate::ymd(date),
                    new_cases = cases-lag(cases),
                    week = lubridate::isoweek(date),
                    time = seq(1,length(date)),
                    new_cases=tidyr::replace_na(new_cases,0),
                    new_cases=ifelse(new_cases<0,0,new_cases),
                    time = seq(1,length(date))) %>% ungroup() %>% arrange(region,date)
    
    deaths <- deaths %>% rename(region= Region, date=Fecha, deaths=Total) %>% group_by(region) %>%
      dplyr::mutate(date = lubridate::ymd(date),
                    new_deaths = deaths-lag(deaths),
                    week = lubridate::isoweek(date),
                    new_deaths=tidyr::replace_na(new_deaths,0),
                    new_deaths=ifelse(new_deaths<0,0,new_deaths)) %>% ungroup() %>% arrange(region,date)
    
    region.data <- left_join(cases, deaths)
    
    region.data.desc <- region.data %>% 
      dplyr::arrange(region, date) %>% 
      dplyr::ungroup() %>%
      mutate(new_deaths=tidyr::replace_na(new_deaths,0),
             deaths=tidyr::replace_na(deaths,0),
             region=ifelse(region=="Total","Chile",region))  %>%
      dplyr::select(date, region, cases, new_cases, deaths, new_deaths)
    
    region.data.desc
    
    # Do analysis
    
    # Create matrix of results
    m.ur.cfr.region.Chile <- as.data.frame(matrix(NA, nrow = nrow(region.data.desc), 
                                                  ncol = 16))
    current <- as.data.frame(matrix(NA, nrow = nrow(region.data.desc %>% filter(region=="Antofagasta")), 
                                    ncol = 16))
    
    # Assign names to variables
    colnames(m.ur.cfr.region.Chile) <- c("date","nCFR","cCFR","total_deaths","cum_known_t" ,"total_cases",             
                                         "nCFR_UQ","nCFR_LQ","cCFR_UQ", "cCFR_LQ","underreporting_estimate",
                                         "lower","upper","quantile25","quantile75","region")
    
    colnames(current) <- c("date","nCFR","cCFR","total_deaths","cum_known_t" ,"total_cases",             
                           "nCFR_UQ","nCFR_LQ","cCFR_UQ", "cCFR_LQ","underreporting_estimate",
                           "lower","upper","quantile25","quantile75","region")
    
    # Loop over each day
    
    region.data.desc <- region.data.desc %>% filter(region!="Atacama" & region!="Aysén") # Filter regions without deaths
    
    for (j in unique(region.data.desc$region)){
      for (i in 1:(nrow(region.data.desc %>%
                       dplyr::filter(region==j) %>%
                       filter(deaths > 1))-7)){
      current[i,] <- region.data.desc %>%
          dplyr::filter(region==j) %>%
          dplyr::filter(deaths > 1) %>%
          # padr::pad() %>%
          slice(., 1:(n()-i)) %>%
          dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                        new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
          dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
          dplyr::filter(cum_deaths > 0) %>%
          dplyr::select(-cum_deaths) %>%
          dplyr::do(scale_cfr(., delay_fun = hospitalisation_to_death_truncated)) %>%
          dplyr::filter(cum_known_t > 0 & cum_known_t >= total_deaths)  %>%
          dplyr::mutate(nCFR_UQ = binom.test(total_deaths, total_cases)$conf.int[2],
                        nCFR_LQ = binom.test(total_deaths, total_cases)$conf.int[1],
                        cCFR_UQ = binom.test(total_deaths, cum_known_t)$conf.int[2],
                        cCFR_LQ = binom.test(total_deaths, cum_known_t)$conf.int[1],
                        underreporting_estimate = cCFRBaseline / (100*cCFR),
                        lower = cCFREstimateRange[1] / (100 * cCFR_UQ),
                        upper = cCFREstimateRange[2] / (100 * cCFR_LQ),
                        quantile25 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[1],
                        quantile75 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[2],
                        region = j,
                        date = zoo::as.Date(date))
      }
      m.ur.cfr.region.Chile <- bind_rows(m.ur.cfr.region.Chile, current)
    }
    
    m.ur.cfr.region.Chile <- m.ur.cfr.region.Chile %>% filter(complete.cases(m.ur.cfr.region.Chile)) %>% 
                                                       mutate(date=zoo::as.Date(date))
    
    m.ur.cfr.region.Chile
    rm(current)
  
    # Clean results
    m.ur.cfr.region.Chile <- left_join(m.ur.cfr.region.Chile, region.data.desc)
    m.ur.cfr.region.Chile <- m.ur.cfr.region.Chile %>% dplyr::select(-total_deaths, -total_cases)  %>% 
                            dplyr::mutate(underreporting_estimate = ifelse(underreporting_estimate <= 1, underreporting_estimate, 1),
                                          upper = ifelse(upper <= 1, upper, 1),
                                          underreporting_estimate = signif(underreporting_estimate, 2),
                                          lower = signif(lower, 2), 
                                          upper = signif(upper, 2),
                                          underreporting_estimate_clean = paste0(underreporting_estimate*100,
                                                                                 "% (",lower*100,"% - ",upper*100,"%)"),
                                          expected_cases = new_cases*(1+underreporting_estimate)) %>%
                            arrange(region, date) %>% group_by(region) %>% 
                            slice(., 7:(n()-7)) # Filtro primeras 7 y últimos 7 observaciones
    
    
    m.ur.cfr.region.Chile.clean <- 
      m.ur.cfr.region.Chile %>% dplyr::filter(!is.na(nCFR) & !is.na(new_cases))%>%
                                dplyr::select(date, new_cases, cases, deaths, underreporting_estimate, lower,
                                                upper) %>%
                                dplyr::mutate(underreporting_estimate = 1-ifelse(underreporting_estimate <= 1, underreporting_estimate, 1)) %>%
                                dplyr::mutate(lower = 1-ifelse(upper <= 1, upper, 1)) %>%
                                dplyr::mutate(upper = 1-lower) %>%
                                dplyr::mutate(underreporting_estimate = signif(underreporting_estimate, 2)) %>%
                                dplyr::mutate(lower = signif(lower, 2)) %>%
                                dplyr::mutate(upper = signif(upper, 2)) %>%
                                dplyr::mutate(underreporting_estimate_clean = paste0(underreporting_estimate*100,
                                                                                       "% (",lower*100,"% - ",upper*100,"%)")) %>% 
                                dplyr::arrange(desc(date))
    
    m.ur.cfr.region.Chile.clean
    readr::write_excel_csv2(m.ur.cfr.region.Chile.clean %>% filter(region!="Chile"),
                            "~/Documents/GitHub/covid19-data/analisis/Subreporte/Subreporte_Regiones_serie.csv")
    
    m.case.fatality.region.Chile.clean  <- 
      m.ur.cfr.region.Chile %>% dplyr::filter(!is.na(nCFR))%>%
                                dplyr::select(date, cases, deaths,
                                              nCFR, nCFR_LQ, nCFR_UQ, cCFR, cCFR_LQ, cCFR_UQ) %>%
                                dplyr::mutate(letalidad_caso_cruda = paste0(signif(nCFR,3)*100, "% (",signif(nCFR_LQ,3)*100,"% - ",signif(nCFR_UQ,3)*100,"%)"),
                                       letalidad_caso_aj_retraso = paste0(signif(cCFR,3)*100, "% (",signif(cCFR_LQ,3)*100,"% - ",signif(cCFR_UQ,3)*100,"%)"))
    m.case.fatality.region.Chile.clean
    readr::write_excel_csv2(m.case.fatality.region.Chile.clean %>% filter(region!="Chile"),
                            "~/Documents/GitHub/covid19-data/analisis/Letalidad/Letalidad_Regiones_serie.csv")
    
    # Graph results
    underreport_reg_ts <- ggplot(m.ur.cfr.region.Chile.clean %>% filter(deaths>0), 
                                 aes(date, underreporting_estimate, color=region)) + geom_line() + geom_point() +
      # geom_ribbon(aes(ymin=lower,ymax=upper), alpha = 0.5) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(0,1), labels = scales::percent_format(accuracy = 1L)) +
      labs(y="Subreporte de casos sintomáticos COVID-19 (% del total de casos)",x="Fecha", colour="Región",
           title = "Estimación de subreporte de casos COVID-19, Chile",
           caption = paste("Cuadrado C. Escuela de Salud Pública. Universidad de Chile.
                           Se grafican tiempo-región con 1 o más fallecidos acumulados.
                           Update:",Sys.Date()))+ facet_wrap(region~.) +
      theme_minimal() + theme(legend.position = "none")
    underreport_reg_ts
    
    cCFR_reg_ts <- ggplot(m.case.fatality.region.Chile.clean %>% filter(deaths>0), 
                          aes(date, cCFR, color=region)) + geom_line() + geom_point() +
      # geom_ribbon(aes(ymax=cCFR_UQ,ymin=cCFR_LQ), alpha = 0.1) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits=c(0,0.1), labels = scales::percent_format(accuracy = 1L)) +
      labs(y="Tasa de letalidad ajustada por retraso",x="Fecha", colour="Región",
           title = "Estimación de letalidad COVID-19 ajustada por retraso, Regiones de Chile",
           caption = paste("Cuadrado C. Escuela de Salud Pública. Universidad de Chile.
                           Se grafican tiempo-región con 1 o más fallecidos acumulados.
                           Update:",Sys.Date())) + facet_wrap(region~.) +
      theme_minimal() + theme(legend.position = "none")
    cCFR_reg_ts  
    
    nCFR_reg_ts <- ggplot(m.case.fatality.region.Chile.clean %>% filter(deaths>0), 
                          aes(date, nCFR, color=region)) + geom_line() + geom_point() +
      # geom_ribbon(aes(ymax=nCFR_UQ,ymin=nCFR_LQ), alpha = 0.1) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits=c(0,0.05), labels = scales::percent_format(accuracy = 1L)) +
      labs(y="Tasa de letalidad cruda",x="Fecha",  colour="Región",
           title = "Estimación de letalidad COVID-19 cruda, Regiones de Chile",
           caption = paste("Cuadrado C. Escuela de Salud Pública. Universidad de Chile.
                           Se grafican tiempo-región con 1 o más fallecidos acumulados.
                           Update:",Sys.Date())) + facet_wrap(region~.) +
      theme_minimal() + theme(legend.position = "none")
    nCFR_reg_ts
    
    underreport_reg_ts
    # ggsave("underreport_reg_ts.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    ggsave("~/Documents/GitHub/covid19-data/analisis/Subreporte/subreporte por region y tiempo.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    
    cCFR_reg_ts
    # ggsave("cCFR_reg_ts.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    ggsave("~/Documents/GitHub/covid19-data/analisis/Letalidad/letalidad por region y tiempo.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    
    nCFR_reg_ts
    # ggsave("nCFR_reg_ts.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    ggsave("~/Documents/GitHub/covid19-data/analisis/Letalidad/letalidad por region y tiempo 2.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    
  