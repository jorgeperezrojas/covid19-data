
############################################################################################
################# Under-reporting estimates & case-fatality of COVID-19 in Chile ###########
################# Cuadrado C.                                                    ###########
################# Escuela de Salud Pública. Universidad de Chile         ###################
################# v1 Last Update: June 4, 2020                         ###################
############################################################################################
#
# Code based on: Russell et al. Using a delay-adjusted case fatality ratio to estimate under-reporting. First online: 22-03-2020 | Last update: 08-04-2020. Available at: https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html?fbclid=IwAR31V4DbTkUDkJJKpfJMI1M7sYxt16EMQ9yRH5Y-lV0lAIH2mbkfkFZ5zeE
# Original estimation method: Nishiura et al. Early epidemiological assessment of the virulence of emerging infectious diseases: A case study of an influenza pandemic. PLoS One 2009;4.

######### cCFR and Under-reporting estimation based on LSHTM method ---------------------------------------------------

# Set up paths and parameters ---------------------------------------------

# Load libraries
library(tidyverse)
library(padr)

# Set parameters
zmeanHDT <- 13 # Original parameters used by Russel et al
zsdHDT <- 12.7
# zmeanHDT <- 11.92 # Data for Chile RM April 22
# zsdHDT <- 8.44 # Data for Chile RM April 22
zmedianHDT <- 9.1
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
  data.frame(nCFR = b_tt, cCFR = p_tt, total_deaths = sum(death_incidence), 
             cum_known_t = round(cumulative_known_t), total_cases = sum(case_incidence))
}



#### Load data -----------------------------------------------------

# Dowload database directly from ECDC
httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
allDat <- read.csv(tf)

# Organize data
allDatDesc <- allDat %>% 
  dplyr::arrange(countriesAndTerritories, dateRep) %>% 
  dplyr::mutate(dateRep = lubridate::dmy(dateRep))%>% 
  dplyr::rename(date = dateRep, new_cases = cases, new_deaths = deaths, country = countriesAndTerritories) %>%
  dplyr::select(date, country, new_cases, new_deaths) %>%
  dplyr::filter(country != "CANADA", 
                country != "Cases_on_an_international_conveyance_Japan") %>% 
  dplyr::arrange(country,date)


#### Run analysis for all-countries  -----------------------------------------------------------

allTogetherClean2 <- allDatDesc %>%
  dplyr::group_by(country) %>%
  padr::pad() %>%
  dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
  dplyr::group_by(country) %>%
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


reportDataFinal <- allTogetherClean2 %>%
  dplyr::select(country, total_cases, total_deaths, underreporting_estimate, lower,
                upper) %>%
  dplyr::mutate(underreporting_estimate = ifelse(underreporting_estimate <= 1, underreporting_estimate, 1)) %>%
  dplyr::mutate(upper = ifelse(upper <= 1, upper, 1)) %>%
  dplyr::mutate(underreporting_estimate = signif(underreporting_estimate, 2)) %>%
  dplyr::mutate(lower = signif(lower, 2)) %>%
  dplyr::mutate(upper = signif(upper, 2)) %>%

  dplyr::ungroup(country) %>%
  dplyr::mutate(country = country %>% stringr::str_replace_all("_", " ")) %>% 
  dplyr::mutate(underreporting_estimate_clean = paste0(underreporting_estimate*100,
                                                       "% (",lower*100,"% - ",upper*100,"%)"))

reportDataFinal

#### Run analysis for Chile by date  -----------------------------------------------------------

# Create matrix of results
m.ur.cfr.Chile <- as.data.frame(matrix(NA, nrow = 1+nrow(allDatDesc %>%
                          dplyr::filter(country=="Chile")), 
                    ncol = 14))

# Assign names to variables
colnames(m.ur.cfr.Chile) <- c("nCFR","cCFR","total_deaths","cum_known_t" ,"total_cases",             
                               "nCFR_UQ","nCFR_LQ","cCFR_UQ", "cCFR_LQ","underreporting_estimate",
                                "lower","upper","quantile25","quantile75")

# Loop over each day
for (i in 1:nrow(allDatDesc %>% 
                     dplyr::filter(country=="Chile"))){
  m.ur.cfr.Chile[i,] <- allDatDesc %>%
  dplyr::filter(country=="Chile") %>%
  padr::pad() %>%
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
                quantile75 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[2])
}

# Add date variable
m.ur.cfr.Chile$date <- allDatDesc %>% dplyr::filter(country=="Chile") %>% padr::pad() %>% dplyr::arrange(desc(date)) %>%  dplyr::pull(date)

# Clean results
m.ur.cfr.Chile<- m.ur.cfr.Chile %>% dplyr::mutate(underreporting_estimate = ifelse(underreporting_estimate <= 1, underreporting_estimate, 1)) %>%
                                    dplyr::mutate(upper = ifelse(upper <= 1, upper, 1)) %>%
                                    dplyr::mutate(underreporting_estimate = signif(underreporting_estimate, 2)) %>%
                                    dplyr::mutate(lower = signif(lower, 2)) %>%
                                    dplyr::mutate(upper = signif(upper, 2)) %>%
                                    dplyr::mutate(underreporting_estimate_clean = paste0(underreporting_estimate*100,
                                                                                         "% (",lower*100,"% - ",upper*100,"%)"))

m.ur.cfr.Chile <- m.ur.cfr.Chile %>% arrange(date) %>% mutate(new_cases=total_cases-lag(total_cases),
                                                              expected_cases = new_cases*(1+underreporting_estimate))

m.ur.cfr.Chile.clean <- m.ur.cfr.Chile %>% dplyr::filter(!is.na(nCFR) & !is.na(new_cases))%>%
                          dplyr::select(date, new_cases, total_cases, total_deaths, underreporting_estimate, lower,
                                        upper) %>%
                          dplyr::mutate(underreporting_estimate = 1-ifelse(underreporting_estimate <= 1, underreporting_estimate, 1)) %>%
                          dplyr::mutate(lower = 1-ifelse(upper <= 1, upper, 1)) %>%
                          dplyr::mutate(upper = 1-lower) %>%
                          dplyr::mutate(underreporting_estimate = signif(underreporting_estimate, 2)) %>%
                          dplyr::mutate(lower = signif(lower, 2)) %>%
                          dplyr::mutate(upper = signif(upper, 2)) %>%
                          dplyr::mutate(underreporting_estimate_clean = paste0(underreporting_estimate*100,
                                                                               "% (",lower*100,"% - ",upper*100,"%)"),
                                        expected_new_cases = round(new_cases*(1+underreporting_estimate))) %>% 
                          dplyr::arrange(desc(date))
m.ur.cfr.Chile.clean
readr::write_excel_csv2(m.ur.cfr.Chile.clean,
                       "~/Documents/GitHub/covid19-data/analisis/Subreporte/Subreporte_Chile.csv")

m.case.fatality.Chile.clean  <- m.ur.cfr.Chile %>% dplyr::filter(!is.na(nCFR) & !is.na(new_cases))%>%
                                dplyr::select(date, new_cases, total_cases, total_deaths,
                                              nCFR, nCFR_LQ, nCFR_UQ, cCFR, cCFR_LQ, cCFR_UQ) %>%
                                mutate(letalidad_caso_cruda = paste0(signif(nCFR,3)*100, "% (",signif(nCFR_LQ,3)*100,"% - ",signif(nCFR_UQ,3)*100,"%)"),
                                       letalidad_caso_aj_retraso = paste0(signif(cCFR,3)*100, "% (",signif(cCFR_LQ,3)*100,"% - ",signif(cCFR_UQ,3)*100,"%)"))
m.case.fatality.Chile.clean
readr::write_excel_csv2(m.case.fatality.Chile.clean,
                        "~/Documents/GitHub/covid19-data/analisis/Letalidad/Letalidad_Chile.csv")

# Graph results
underreport <- ggplot(m.ur.cfr.Chile %>% filter(!is.na(underreporting_estimate)), aes(date, 1-underreporting_estimate)) + geom_line() + geom_point() +
       geom_ribbon(aes(ymin=1-upper,ymax=1-lower), alpha = 0.5) +
       scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(0,1), labels = scales::percent) +
       labs(y="Subreporte de casos sintomáticos COVID-19 (% del total de casos)",x="Fecha", 
           title = "Estimación de subreporte de casos COVID-19, Chile",
           caption = paste("Cuadrado C. Escuela de Salud Pública. Universidad de Chile. Update:",Sys.Date()))

underreport

cCFR <- ggplot(m.ur.cfr.Chile %>% filter(!is.na(cCFR)), aes(date, cCFR)) + geom_line() + geom_point() +
        geom_ribbon(aes(ymax=cCFR_UQ,ymin=cCFR_LQ), alpha = 0.5) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(0,0.1), labels = scales::percent) +
        labs(y="Tasa de letalidad ajustada por retraso",x="Fecha", 
        # labs(y="Delay-adjusted case fatality ratio (cCFR)",x="Date", 
             title = "Estimación de letalidad COVID-19 ajustada por retraso, Chile",
             caption = paste("Cuadrado C. Escuela de Salud Pública. Universidad de Chile.",Sys.Date()))
cCFR   

    underreport
    ggsave("underreport.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    ggsave("~/Documents/GitHub/covid19-data/analisis/Subreporte/subreporte Chile.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    
    cCFR
    ggsave("cCFR.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    ggsave("~/Documents/GitHub/covid19-data/analisis/Letalidad/letalidad Chile.png", width = 12*2.5, height = 12*2.5, units = "cm", dpi=300, limitsize = FALSE)
    


# Analysis by region ------------------------------------------------------

    cases <- read_csv("https://raw.githubusercontent.com/jorgeperezrojas/covid19-data/master/csv/confirmados.csv")
    deaths <- read_csv("https://raw.githubusercontent.com/jorgeperezrojas/covid19-data/master/csv/muertes.csv")
    
    library(tidyr)
    deaths_long <- gather(deaths, date, deaths, "04/01/2020":rev(names(deaths))[1])
    deaths_long
    
    cases_long <- gather(cases, date, cases, "03/07/2020":rev(names(cases))[1])
    cases_long
    
    region.data <- left_join(deaths_long,cases_long)
    
    region.data.desc <- region.data %>% 
      dplyr::arrange(region, date) %>% 
      dplyr::mutate(date = lubridate::mdy(date))%>% 
      dplyr::group_by(region) %>%
      dplyr::mutate(new_cases = cases-lag(cases), 
                    new_deaths = deaths-lag(deaths)) %>%
      dplyr::ungroup() %>%
      dplyr::select(date, region, new_cases, new_deaths)

    region.data.desc
    
    # Do analysis
    
    underreport.byregion <- region.data.desc %>%
      dplyr::group_by(region) %>%
      padr::pad() %>%
      dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                    new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
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
      dplyr::mutate(underreporting_estimate = 1-underreporting_estimate,
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
    readr::write_excel_csv2(underreport.byregionfinal,
                            "~/Documents/GitHub/covid19-data/analisis/Subreporte/Subreporte_regiones.csv")
    
    m.case.fatality.region.clean  <- underreport.byregion %>% 
      dplyr::select(region, total_cases, total_deaths,
                    nCFR, nCFR_LQ, nCFR_UQ, cCFR, cCFR_LQ, cCFR_UQ) %>%
      mutate(letalidad_caso_cruda = paste0(signif(nCFR,3)*100, "% (",signif(nCFR_LQ,3)*100,"% - ",signif(nCFR_UQ,3)*100,"%)"),
             letalidad_caso_aj_retraso = paste0(signif(cCFR,3)*100, "% (",signif(cCFR_LQ,3)*100,"% - ",signif(cCFR_UQ,3)*100,"%)"))
    m.case.fatality.region.clean
    readr::write_excel_csv2(m.case.fatality.region.clean,
                            "~/Documents/GitHub/covid19-data/analisis/Letalidad/Letalidad_regiones.csv")
    
    underreport.reg <- ggplot(data=underreport.byregionfinal, aes(reorder(region, -underreporting_estimate, sum),underreporting_estimate))+
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
                            Se gráfican regiones con más de 10 fallecimientos a la fecha.
                           Update:",Sys.Date())) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(0,1), labels = scales::percent) +
      theme(axis.text.x = element_text(angle = 45)) +
      theme_minimal()
    
    underreport.reg
    ggsave("underreport.reg.png")
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
                            Se gráfican regiones con más de 10 fallecimientos a la fecha.
                           Update:",Sys.Date())) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(0,0.1), labels = scales::percent) +
      theme(axis.text.x = element_text(angle = 45)) +
      theme_minimal()
    
    fatality.reg
    ggsave("fatality.reg.png")
    ggsave("~/Documents/GitHub/covid19-data/analisis/Letalidad/Letalidad por Región Chile.png", width = 12*2.5, height = 9*2.5, units = "cm", dpi=300, limitsize = FALSE)
    
 