#############################     THE POLITICAL CONSEQUENCES OF INTERACTIONS WITH CRIMINAL JUSTICE SYSTEM     #############################
#
#DISSERTATION
#Author: Benjamín Muñoz
#Data Source Nº 2: World Prison Brief
##R Code Nº 1B: WPB Data Preparation
#Objective: Creation of comparative dataset
#Date: October 31, 2023
#
#####################################################     STEP 00:  WORKING SPACE     #####################################################

## Remove objects from working environment
rm(list=ls())

## Set options
options(scipen = 1000000)

## Load packages
library(pdftools)      # Import PDFs
library(purrr)         # Iterations      
library(rvest)         # Web scraping
library(tidyverse)     # Multiple tools


###################################################     STEP 01:  IMPORTING DATASET     ###################################################

## Import PDFs
wppl_01 <- pdftools::pdf_text(pdf = "2_Data/1_Raw_Data/2_World_Prison_Brief/world_prison_population_list_14th_edition.pdf")
wppl_02 <- pdftools::pdf_text(pdf = "2_Data/1_Raw_Data/2_World_Prison_Brief/world_prison_population_list_13th_edition.pdf")
wpre_01 <- pdftools::pdf_text(pdf = "2_Data/1_Raw_Data/2_World_Prison_Brief/world_pre-trial_list_4th_edn_final.pdf")


####################################################     STEP 02: DATA MANAGEMENT 1     ####################################################

# 1) WORLD PRISON POPULATION LIST

## Create list to store results
list_01 <- list()

## Run loop to extract pages 
for (i in 1:length(wppl_01)) {
  wppl_01[i] %>%
    strsplit(., "\n") %>% 
    unlist()  %>% 
    as.data.frame() %>%
    dplyr::rename(orig = ".") %>%
    dplyr::filter(orig != "") -> list_01[[i]]
}

## Create a data.frame with the tables
bind_rows(list_01) %>% 
  ### Identify relevant tables
  dplyr::mutate(check1 = dplyr::if_else(stringr::str_detect(orig, "Part 1: Prison population totals, rates and trends per country") | 
                                          stringr::str_detect(orig, "Part 2: Prison population totals, rates and trends by continent"), 
                                        1, 0),
                check2 = cumsum(check1)) %>% 
  ### Drop irrelevant lines
  dplyr::filter(check2 == 1) %>%
  dplyr::select(-check1, -check2) %>% 
  ### Identify lines to remove (Subdivisions of Tables and Number Pages)
  dplyr::mutate(check3 = if_else(orig %in% c("Northern Africa", "Western Africa", "Central Africa",
                                             "Eastern Africa", "Southern Africa", "Northern America",
                                             "Central America", "Caribbean", "South America",
                                             "Western Asia", "Central Asia", "Southern Asia",
                                             "South-Eastern Asia", "Eastern Asia", "Northern Europe",
                                             "Southern Europe", "Western Europe", "Central & Eastern Europe",
                                             "Europe/Asia", "Oceania"), 1, 0),
                check4 = cumsum(check3),
                check5 = ifelse(stringr::str_replace_all(string = orig, pattern =  "\\s", replacement = "") %in% 
                                  c("3","4","5","6","7","8","9","10","11","12","13","14","15","16",
                                    "587prisonersinTRNCinSeptember2020.","in2019."), 0, check4), 
                check6 = str_count(str_replace_all(orig, "\\s", ""), "[a-zA-Z]")/str_length(str_replace_all(orig, "\\s", "")) *100) %>%
  ### Drop irrelevant lines
  dplyr::filter(check5 != 0)  %>% 
  dplyr::filter(check6 < 80) %>%
  dplyr::select(-check3,-check5,-check6)-> wppl_01a

## Create a new version of the data.frame
wppl_01a %>%
  ### Create region and subregion variables
  dplyr::mutate(subregion = car::recode(check4, "1='Northern Africa';2='Western Africa';
                             3='Central Africa'          ; 4='Eastern Africa'    ; 5='Southern Africa'; 
                             6='Northern America'        ; 7='Central America'   ; 8='Caribbean'; 
                             9='South America'           ;10='Western Asia'      ;11='Central Asia';
                            12='Southern Asia'           ;13='South-Eastern Asia';14='Eastern Asia';
                            15='Northern Europe'         ;16='Southern Europe'   ;17='Western Europe';
                            18='Central & Eastern Europe';19='Europe/Asia'       ;20='Oceania'"),
                region = car::recode(subregion,"c('Northern Africa','Western Africa','Central Africa',
                            'Eastern Africa','Southern Africa')='Africa';c('Northern America',
                            'Central America','Caribbean','South America')='America';c('Western Asia',
                            'Central Asia','Southern Asia','South-Eastern Asia','Eastern Asia',
                            'Europe/Asia')='Asia';c('Northern Europe','Southern Europe','Western Europe',
                            'Central & Eastern Europe')='Europe';'Oceania'='Oceania'")) %>%
  dplyr::select(-check4) %>% 
  ### Create new version of origin (replace with |)
  dplyr::mutate(orig1 = stringr::str_replace_all(string = orig,  pattern = "c\\. ", replacement = ""),
                orig2 = stringr::str_replace_all(string = orig1, pattern = "\\s+", replacement = "\\|"),
                orig3 = str_replace_all(orig2, "(?<=[\\p{L}.()])\\|(?=\\p{L}(?!c\\.))", "%") %>%
                  str_replace_all(., "(?<=[\\p{L}.()])\\|(?=[().])", "%"),
                orig4 = str_replace(orig3, pattern = "\\|&\\|", replacement = "%&%"),
                orig5 = str_replace(orig4, pattern = fixed("/|"), replacement = "%"),
                orig6 = str_replace_all(orig5, "\\|:", "%:"),
                orig7 = car::recode(orig6,"'Equatorial%Guinea|500|.15|799,000|63|not%available'=
                                    'Equatorial%Guinea|500|.15|799,000|63|NA|NA|';
                                    'Grenadines|2005|364|334'       = '|2005|364|334';
                                    'Grenadines|2010|413|379'       = '|2010|413|379';
                                    '(France)|2005|600|293'         = '|2005|600|293';
                                    '(France)|2010|715|306'         = '|2010|715|306';
                                    'Wales|2005|75,979|142'         = '|2005|75,979|142';
                                    'Wales|2010|84,725|153'         = '|2010|84,725|153';
                                    'Ireland|2005|1,301|76'         = '|2005|1,301|76';
                                    'Ireland|2010|1,465|81'         = '|2010|1,465|81';
                                    '(Denmark)|2005|10|20'          = '|2005|10|20';
                                    '(Denmark)|2010|10|20'          = '|2010|10|20';
                                    'Federation|2005|1,435|64'      = '|2005|1,435|64';
                                    'Republika%Srpska|2005|955|68'  = '|2005|955|68';
                                    'Republika%Srpska|2010|1,046|75'= '|2010|1,046|75';
                                    'Micronesia|2005|72|68'         = '|2005|72|68';
                                    '(USA)|2005|149|235'            = '|2005|149|235';
                                    '(USA)|2011|162|304'            = '|2011|162|304'"),
                orig8 = str_replace_all(orig7, ":\\|", ":%"),
                orig9 = str_replace_all(orig8, "average\\|", ""),
                orig10 = str_replace_all(orig9, "Feb/Mar\\|21","Feb/Mar%21"),
                orig11 = str_replace_all(orig10, "\\|-\\|","\\|")) %>%  
  dplyr::select(subregion,region,orig11) -> wppl_01b

## Repair final case
wppl_01b$orig11[594]  <- "UK:%England%&|87,699|22.3.24|60.30m|145|2000|64,602|124"

## Final version of the data.frame
wppl_01b %>%
  ### Same number of "|" symbols
  dplyr::mutate(orig12 = str_replace(orig11, "^\\|(\\d)", "|||||\\1")) %>% 
  ### Select relevant variables
  dplyr::select(subregion,region,orig12) %>% 
  ### Splitting into multiple columns
  tidyr::separate(data = ., col = orig12, 
                  into = c("country", "prison", "date","population1","rate1",
                           "year","population2","rate2"), sep = "\\|", 
                  extra = "merge", fill = "right") -> wppl_01c

## Repairing variables
wppl_01c %>%
  ### Rename variables
  dplyr::rename(country_0 = country, prison_A0 = prison, 
                year_A0 = date, population_A0 = population1, rate_A0 = rate1, 
                year_B0 = year, prison_B0 = population2, rate_B0 = rate2) %>% 
  ### 1) Country
  dplyr::mutate(country_1 = str_replace_all(country_0, "%", " "),
                extra1    = str_detect(country_1, "\\(USA\\)|\\(UK\\)|\\(Netherlands\\)|
                                      \\(NZ\\)|\\(France\\)|\\(Denmark\\)|\\(China\\)"),
                extra2    = ifelse(country_0 %in% c("French%Guiana%Guyane","Faeroes"), TRUE, extra1),  
                country_2 = str_trim(str_replace_all(country_1, "\\(USA\\)|\\(UK\\)|\\(Netherlands\\)|
                                                    \\(NZ\\)|\\(France\\)|\\(Denmark\\)|\\(China\\)",
                                                     "")),
                country_3 = car::recode(country_2,"'Guinea (Republic of)'='Republic of Guinea';
                                      'Congo (Republic of)'='Republic of Congo';
                                      'Dem. Republic of Congo'='Democratic Republic of Congo';
                                      'Swaziland/eSwatini'='Eswatini';'U.S.A.'='United States of America';
                                      'Antigua & Barbuda' = 'Antigua and Barbuda';'St Kitts & Nevis' = 'St Kitts and Nevis';
                                      'St Vincent & the'='St Vincent and the Grenadines';'Trinidad & Tobago' = 'Trinidad and Tobago';
                                      'Cayman Is'='Cayman Islands';'French Guiana Guyane'='French Guiana';
                                      'Korea (Republic of)'='Republic of Korea';'Faeroes'='Faroe Islands';
                                      'Cyprus*'='Cyprus';'San Marino*'='San Marino';'France*'='France';
                                      'Liechtenstein*'='Liechtenstein';'Monaco*'='Monaco';'Moldova*'='Moldova';
                                      'Ukraine*'='Ukraine';'Georgia*'='Georgia';
                                      'Federated States of'='Federated States of Micronesia';
                                      'Cook Islands (NZ)'='Cook Islands';'Fr. Polynesia'='French Polynesia';
                                      'Northern Mariana Is.'='Northern Mariana Islands'"),
                ### 2) Prison Population
                prison_A1 = str_replace_all(prison_A0, ",", ""),
                prison_A2 = str_replace_all(prison_A1, "\\*", ""),
                ### 3) Year
                year_A1   = car::recode(year_A0, "'19'='.19';'20'='.20';'2018-19'='.18';'.21/22'='22';
                                       '2022-23'='2022';'21'='.21';'mid-21'='.21';'mid-22'='.22';'mid-23'='.23';
                                       'Feb/Mar%21'='.21';'mid-16'='.16';'mid-19'='.19';'mid-20'='.20'"),
                
                year_A2 = str_sub(year_A1, -3, -1),
                year_A3 = str_replace(year_A2, pattern = ".", "20"),
                ### 4) Total Population
                population_A1 = ifelse(str_sub(population_A0, -1, -1) == "m", 1,
                                       ifelse(population_A0 == "", "", 0)),
                population_A2 = ifelse(test = population_A1 == 1, 
                                       yes = as.numeric(str_replace_all(population_A0,"m|,",""))*1000000,
                                       no = ifelse(test = population_A1 == 0,
                                                   yes = as.numeric(str_replace_all(population_A0,",", "")),
                                                   no = population_A1)),
                ### 5) Prison Rate
                rate_A1 = as.numeric(str_replace_all(rate_A0, pattern = "\\*", replacement = "")),
                rate_A2 = ifelse(is.na(rate_A1)==T, "", rate_A1),
                ### 6) Year Trend
                year_B1 = car::recode(year_B0, "'2000-01'='2001';'2005-06'='2006';
                                              '2010-11'='2011';'2013-14'='2014';
                                              '2015-16'='2016';'2019-20'='2020'"),
                ### 7) Prison Population Trend
                prison_B1 = ifelse(str_sub(prison_B0, -1, -1) == "m", 1,
                                   ifelse(prison_B0 == "", "", 0)),
                prison_B2 = ifelse(test = prison_B1 == 1, 
                                   yes = as.numeric(str_replace_all(prison_B0,"m|,|\\*",""))*1000000,
                                   no = ifelse(test = prison_B1 == 0,
                                               yes = as.numeric(str_replace_all(prison_B0,",|\\*", "")),
                                               no = prison_B1)),
                ### 8) Prison Rate Trend
                rate_B1 = str_replace_all(rate_B0, pattern = "\\*",""),
                rate_B2 = str_replace_all(rate_B1, pattern = ",", ""),
                rate_B3 = str_replace_all(rate_B2, pattern = "-", "0"),
                rate_B4 = ifelse(rate_B3 == "", "NA", rate_B3)) -> wppl_01d

## Final modifications to Country
wppl_01d$country_4 <- ifelse(wppl_01d$country_3 == "Cote d’Ivoire", "Ivory Coast", 
                             ifelse(wppl_01d$country_3 == "UK: England &", 'UK: England and Wales', 
                                    ifelse(wppl_01d$country_3 == "UK: Northern", "UK: Northern Ireland",
                                           wppl_01d$country_3)))
wppl_01d$country_4[332] <- "British Virgin Islands"
wppl_01d$country_4[630] <- "Bosnia and Herzegovina (Federation)"
wppl_01d$country_4[634] <- "Bosnia and Herzegovina (Republika Srpska)"

## Impute values for country
wppl_01d$country_5 <- wppl_01d$country_4

for (i in 2:dim(wppl_01d)[1]) {
  wppl_01d$country_5[i] <- ifelse(wppl_01d$country_4[i] == "",
                                  wppl_01d$country_5[i-1],
                                  wppl_01d$country_4[i])
}

## Select variables
wppl_01d %>% 
  dplyr::select(subregion, region, country_5, year_A3, prison_A2,population_A2, rate_A2, 
                year_B1, prison_B2,               rate_B4) %>% 
  dplyr::rename(country = country_5, year_1 = year_A3, prison_1 = prison_A2, 
                population_1 = population_A2, rate_1 = rate_A2,
                year_2 = year_B1, prison_2 = prison_B2, rate_2 = rate_B4) -> wppl_01e

## Identify current values
wppl_01e %>% 
  dplyr::select(subregion,region,country,year_1, prison_1, population_1, rate_1) %>%
  dplyr::filter(year_1 != "") %>%
  dplyr::rename(year = year_1, prison = prison_1, population = population_1, rate = rate_1) -> wppl_01f1

## Identify historical values
wppl_01e %>% 
  dplyr::mutate(population_2 = NA) %>% 
  dplyr::select(subregion,region,country,year_2, prison_2, population_2, rate_2) %>%
  dplyr::filter(is.na(prison_2)==F) %>%
  dplyr::rename(year = year_2, prison = prison_2, population = population_2, rate = rate_2) %>%
  dplyr::mutate(prison = as.character(prison), population = as.character(population)) -> wppl_01f2

## Merge data.frames
wppl_01g <- bind_rows(wppl_01f1, wppl_01f2)

wppl_01g %>%
  dplyr::mutate(year       = as.numeric(year),
                prison     = as.numeric(prison),
                population = as.numeric(population),
                rate       = as.numeric(rate)) -> wppl_01h

wppl_01h %>% 
  dplyr::mutate(rate       = ifelse(is.na(rate)       == TRUE,1086, rate),
                population = ifelse(is.na(population) == TRUE, round(prison/rate*100000), population)
  ) %>%
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  dplyr::mutate(population = ifelse(population == Inf, NA, population))-> wppl_01i

wppl_01i %>% dplyr::mutate(year = ifelse(year == 202, 2022, year)) -> wppl_01i

## Remove objects
rm(i, list_01, wppl_01, wppl_01a, wppl_01b, wppl_01c, wppl_01d, wppl_01e, wppl_01f1, wppl_01f2, wppl_01g, wppl_01h)


####################################################     STEP 03: DATA MANAGEMENT 2     ####################################################

# 2) WORLD PRISON POPULATION LIST (13th ED)

## Create list to store results
list_02 <- list()

## Run loop to extract pages 
for (i in 1:length(wppl_02)) {
  wppl_02[i] %>%
    strsplit(., "\n") %>% 
    unlist()  %>% 
    as.data.frame() %>%
    dplyr::rename(orig = ".") %>%
    dplyr::filter(orig != "") -> list_02[[i]]
}

## Create a data.frame with the tables
bind_rows(list_02) %>% 
  ### Identify relevant tables
  dplyr::mutate(check1 = dplyr::if_else(stringr::str_detect(orig, "Part 1: Prison population totals, rates and trends per country") | 
                                          stringr::str_detect(orig, "Part 2: Prison population totals, rates and trends by continent"), 
                                        1, 0),
                check2 = cumsum(check1)) %>% 
  ### Drop irrelevant lines
  dplyr::filter(check2 == 1) %>%
  dplyr::select(-check1, -check2) %>% 
  ### Identify lines to remove (Subdivisions of Tables and Number Pages)
  dplyr::mutate(check3 = if_else(orig %in% c("Northern Africa", "Western Africa", "Central Africa",
                                             "Eastern Africa", "Southern Africa", "Northern America",
                                             "Central America", "Caribbean", "South America",
                                             "Western Asia", "Central Asia", "Southern Asia",
                                             "South-Eastern Asia", "Eastern Asia", "Northern Europe",
                                             "Southern Europe", "Western Europe", "Central & Eastern Europe",
                                             "Europe/Asia", "Oceania"), 1, 0),
                check4 = cumsum(check3),
                check5 = ifelse(stringr::str_replace_all(string = orig, pattern =  "\\s", replacement = "") %in% 
                                  c("3","4","5","6","7","8","9","10","11","12","13","14","15","16",
                                    "587prisonersinTRNCinSeptember2020.","in2019."), 0, check4), 
                check6 = str_count(str_replace_all(orig, "\\s", ""), "[a-zA-Z]")/str_length(str_replace_all(orig, "\\s", "")) *100) %>%
  ### Drop irrelevant lines
  dplyr::filter(check5 != 0)  %>% 
  dplyr::filter(check6 < 80) %>%
  dplyr::select(-check3,-check5,-check6)-> wppl_02a

## Create a new version of the data.frame
wppl_02a %>%
  ### Create region and subregion variables
  dplyr::mutate(subregion = car::recode(check4, "1='Northern Africa';2='Western Africa';
                             3='Central Africa'          ; 4='Eastern Africa'    ; 5='Southern Africa'; 
                             6='Northern America'        ; 7='Central America'   ; 8='Caribbean'; 
                             9='South America'           ;10='Western Asia'      ;11='Central Asia';
                            12='Southern Asia'           ;13='South-Eastern Asia';14='Eastern Asia';
                            15='Northern Europe'         ;16='Southern Europe'   ;17='Western Europe';
                            18='Central & Eastern Europe';19='Europe/Asia'       ;20='Oceania'"),
                region = car::recode(subregion,"c('Northern Africa','Western Africa','Central Africa',
                            'Eastern Africa','Southern Africa')='Africa';c('Northern America',
                            'Central America','Caribbean','South America')='America';c('Western Asia',
                            'Central Asia','Southern Asia','South-Eastern Asia','Eastern Asia',
                            'Europe/Asia')='Asia';c('Northern Europe','Southern Europe','Western Europe',
                            'Central & Eastern Europe')='Europe';'Oceania'='Oceania'")) %>%
  dplyr::select(-check4) %>% 
  ### Create new version of origin (replace with |)
  dplyr::mutate(orig1 = stringr::str_replace_all(string = orig,  pattern = "c\\. ", replacement = ""),
                orig2 = stringr::str_replace_all(string = orig1, pattern = "\\s+", replacement = "\\|"),
                orig3 = str_replace_all(orig2, "(?<=[\\p{L}.()])\\|(?=\\p{L}(?!c\\.))", "%") %>%
                  str_replace_all(., "(?<=[\\p{L}.()])\\|(?=[().])", "%"),
                orig4 = str_replace(orig3, pattern = "\\|&\\|", replacement = "%&%"),
                orig5 = str_replace(orig4, pattern = fixed("/|"), replacement = "%"),
                orig6 = str_replace_all(orig5, "\\|:", "%:"),
                orig7 = car::recode(orig6,"'Equatorial%Guinea|500|.15|799,000|63|not%available'=
                                    'Equatorial%Guinea|500|.15|799,000|63|NA|NA|';
                                    'Grenadines|2005|364|334'       = '|2005|364|334';
                                    'Grenadines|2010|413|379'       = '|2010|413|379';
                                    '(France)|2005|600|293'         = '|2005|600|293';
                                    '(France)|2010|715|306'         = '|2010|715|306';
                                    'Wales|2005|75,979|142'         = '|2005|75,979|142';
                                    'Wales|2010|84,725|153'         = '|2010|84,725|153';
                                    'Ireland|2005|1,301|76'         = '|2005|1,301|76';
                                    'Ireland|2010|1,465|81'         = '|2010|1,465|81';
                                    '(Denmark)|2005|10|20'          = '|2005|10|20';
                                    '(Denmark)|2010|10|20'          = '|2010|10|20';
                                    'Federation|2005|1,435|64'      = '|2005|1,435|64';
                                    'Republika%Srpska|2005|955|68'  = '|2005|955|68';
                                    'Republika%Srpska|2010|1,046|75'= '|2010|1,046|75';
                                    'Micronesia|2005|72|68'         = '|2005|72|68';
                                    '(USA)|2005|149|235'            = '|2005|149|235';
                                    '(USA)|2011|162|304'            = '|2011|162|304'"),
                orig8 = str_replace_all(orig7, ":\\|", ":%"),
                orig9 = str_replace_all(orig8, "average\\|", ""),
                orig10 = str_replace_all(orig9, "Feb/Mar\\|21","Feb/Mar%21"),
                orig11 = str_replace_all(orig10, "\\|-\\|","\\|")) %>%  
  dplyr::select(subregion,region,orig11) -> wppl_02b

## Repair final case
wppl_02b$orig11[577] <- "UK:%England%&|78,789|1.10.21|60.07m|131|2000|64,602|124"

## Final version of the data.frame
wppl_02b %>%
  ### Same number of "|" symbols
  dplyr::mutate(orig12 = str_replace(orig11, "^\\|(\\d)", "|||||\\1")) %>% 
  ### Select relevant variables
  dplyr::select(subregion,region,orig12) %>% 
  ### Splitting into multiple columns
  tidyr::separate(data = ., col = orig12, 
                  into = c("country", "prison", "date","population1","rate1",
                           "year","population2","rate2"), sep = "\\|", 
                  extra = "merge", fill = "right") -> wppl_02c

## Repairing variables
wppl_02c %>%
  ### Rename variables
  dplyr::rename(country_0 = country, prison_A0 = prison, 
                year_A0 = date, population_A0 = population1, rate_A0 = rate1, 
                year_B0 = year, prison_B0 = population2, rate_B0 = rate2) %>% 
  ### 1) Country
  dplyr::mutate(country_1 = str_replace_all(country_0, "%", " "),
                extra1    = str_detect(country_1, "\\(USA\\)|\\(UK\\)|\\(Netherlands\\)|
                                      \\(NZ\\)|\\(France\\)|\\(Denmark\\)|\\(China\\)"),
                extra2    = ifelse(country_0 %in% c("French%Guiana%Guyane","Faeroes"), TRUE, extra1),  
                country_2 = str_trim(str_replace_all(country_1, "\\(USA\\)|\\(UK\\)|\\(Netherlands\\)|
                                                    \\(NZ\\)|\\(France\\)|\\(Denmark\\)|\\(China\\)",
                                                     "")),
                country_3 = car::recode(country_2,"'Guinea (Republic of)'='Republic of Guinea';
                                      'Congo (Republic of)'='Republic of Congo';
                                      'Dem. Republic of Congo'='Democratic Republic of Congo';
                                      'Swaziland/eSwatini'='Eswatini';'U.S.A.'='United States of America';
                                      'Antigua & Barbuda' = 'Antigua and Barbuda';'St Kitts & Nevis' = 'St Kitts and Nevis';
                                      'St Vincent & the'='St Vincent and the Grenadines';'Trinidad & Tobago' = 'Trinidad and Tobago';
                                      'Cayman Is'='Cayman Islands';'French Guiana Guyane'='French Guiana';
                                      'Korea (Republic of)'='Republic of Korea';'Faeroes'='Faroe Islands';
                                      'Cyprus*'='Cyprus';'San Marino*'='San Marino';'France*'='France';
                                      'Liechtenstein*'='Liechtenstein';'Monaco*'='Monaco';'Moldova*'='Moldova';
                                      'Ukraine*'='Ukraine';'Georgia*'='Georgia';
                                      'Federated States of'='Federated States of Micronesia';
                                      'Cook Islands (NZ)'='Cook Islands';'Fr. Polynesia'='French Polynesia';
                                      'Northern Mariana Is.'='Northern Mariana Islands'"),
                ### 2) Prison Population
                prison_A1 = str_replace_all(prison_A0, ",", ""),
                prison_A2 = str_replace_all(prison_A1, "\\*", ""),
                ### 3) Year
                year_A1   = car::recode(year_A0, "'19'='.19';'20'='.20';'2018-19'='.18';'.21/22'='22';
                                       '2022-23'='2022';'21'='.21';'mid-21'='.21';'mid-22'='.22';'mid-23'='.23';
                                       'Feb/Mar%21'='.21';'mid-16'='.16';'mid-19'='.19';'mid-20'='.20'"),
                
                year_A2 = str_sub(year_A1, -3, -1),
                year_A3 = str_replace(year_A2, pattern = ".", "20"),
                ### 4) Total Population
                population_A1 = ifelse(str_sub(population_A0, -1, -1) == "m", 1,
                                       ifelse(population_A0 == "", "", 0)),
                population_A2 = ifelse(test = population_A1 == 1, 
                                       yes = as.numeric(str_replace_all(population_A0,"m|,",""))*1000000,
                                       no = ifelse(test = population_A1 == 0,
                                                   yes = as.numeric(str_replace_all(population_A0,",", "")),
                                                   no = population_A1)),
                ### 5) Prison Rate
                rate_A1 = as.numeric(str_replace_all(rate_A0, pattern = "\\*", replacement = "")),
                rate_A2 = ifelse(is.na(rate_A1)==T, "", rate_A1),
                ### 6) Year Trend
                year_B1 = car::recode(year_B0, "'2000-01'='2001';'2005-06'='2006';
                                              '2010-11'='2011';'2013-14'='2014';
                                              '2015-16'='2016';'2019-20'='2020'"),
                ### 7) Prison Population Trend
                prison_B1 = ifelse(str_sub(prison_B0, -1, -1) == "m", 1,
                                   ifelse(prison_B0 == "", "", 0)),
                prison_B2 = ifelse(test = prison_B1 == 1, 
                                   yes = as.numeric(str_replace_all(prison_B0,"m|,|\\*",""))*1000000,
                                   no = ifelse(test = prison_B1 == 0,
                                               yes = as.numeric(str_replace_all(prison_B0,",|\\*", "")),
                                               no = prison_B1)),
                ### 8) Prison Rate Trend
                rate_B1 = str_replace_all(rate_B0, pattern = "\\*",""),
                rate_B2 = str_replace_all(rate_B1, pattern = ",", ""),
                rate_B3 = str_replace_all(rate_B2, pattern = "-", "0"),
                rate_B4 = ifelse(rate_B3 == "", "NA", rate_B3)) -> wppl_02d

## Final modifications to Country
wppl_02d$country_4 <- ifelse(wppl_02d$country_3 == "Cote d’Ivoire", "Ivory Coast", 
                             ifelse(wppl_02d$country_3 == "UK: England &", 'UK: England and Wales', 
                                    ifelse(wppl_02d$country_3 == "UK: Northern", "UK: Northern Ireland",
                                           wppl_02d$country_3)))
wppl_02d$country_4[319] <- "British Virgin Islands"
wppl_02d$country_4[613] <- "Bosnia and Herzegovina (Federation)"
wppl_02d$country_4[617] <- "Bosnia and Herzegovina (Republika Srpska)"

## Impute values for country
wppl_02d$country_5 <- wppl_02d$country_4

for (i in 2:dim(wppl_02d)[1]) {
  wppl_02d$country_5[i] <- ifelse(wppl_02d$country_4[i] == "",
                                  wppl_02d$country_5[i-1],
                                  wppl_02d$country_4[i])
}

## Select variables
wppl_02d %>% 
  dplyr::select(subregion, region, country_5, year_A3, prison_A2,population_A2, rate_A2, 
                year_B1, prison_B2,               rate_B4) %>% 
  dplyr::rename(country = country_5, year_1 = year_A3, prison_1 = prison_A2, 
                population_1 = population_A2, rate_1 = rate_A2,
                year_2 = year_B1, prison_2 = prison_B2, rate_2 = rate_B4) -> wppl_02e

## Identify current values
wppl_02e %>% 
  dplyr::select(subregion,region,country,year_1, prison_1, population_1, rate_1) %>%
  dplyr::filter(year_1 != "") %>%
  dplyr::rename(year = year_1, prison = prison_1, population = population_1, rate = rate_1) -> wppl_02f1

## Identify historical values
wppl_02e %>% 
  dplyr::mutate(population_2 = NA) %>% 
  dplyr::select(subregion,region,country,year_2, prison_2, population_2, rate_2) %>%
  dplyr::filter(is.na(prison_2)==F) %>%
  dplyr::rename(year = year_2, prison = prison_2, population = population_2, rate = rate_2) %>%
  dplyr::mutate(prison = as.character(prison), population = as.character(population)) -> wppl_02f2

## Merge data.frames
wppl_02g <- bind_rows(wppl_02f1, wppl_02f2)

wppl_02g %>%
  dplyr::mutate(year       = as.numeric(year),
                prison     = as.numeric(prison),
                population = as.numeric(population),
                rate       = as.numeric(rate)) -> wppl_02h

wppl_02h %>% 
  dplyr::mutate(population = ifelse(is.na(population) == TRUE, round(prison/rate*100000), population)
  ) %>%
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  dplyr::mutate(population = ifelse(population == Inf, NA, population))-> wppl_02i

## Remove objects
rm(i, list_02, wppl_02, wppl_02a, wppl_02b, wppl_02c, wppl_02d, wppl_02e, wppl_02f1, wppl_02f2, wppl_02g, wppl_02h)


####################################################     STEP 04: DATA MANAGEMENT 3     ####################################################

# 3) WORLD PRE-TRIAL/REMAND IMPRISONMENT LIST

## Create list to store results
list_03 <- list()

## Run loop to extract pages 
for (i in 1:length(wpre_01)) {
  wpre_01[i] %>%
    strsplit(., "\n") %>% 
    unlist()  %>% 
    as.data.frame() %>%
    dplyr::rename(orig = ".") %>%
    dplyr::filter(orig != "") -> list_03[[i]]
}

## Create a data.frame with the tables
bind_rows(list_03) %>% 
  ### Identify relevant tables
  dplyr::mutate(check1 = dplyr::if_else(stringr::str_detect(orig, "Table 1                                                   AFRICA") | 
                                          stringr::str_detect(orig, "Table 6             Pre-trial/remand prison population levels – summary by continent"), 
                                        1, 0),
                check2 = cumsum(check1)) %>%
  ### Drop irrelevant lines
  dplyr::filter(check2 == 1) %>%
  dplyr::select(-check1, -check2) %>%
  ### Identify lines to remove (Subdivisions of Tables and Number Pages)
  dplyr::mutate(check3 = if_else(orig %in% c("Northern Africa", "Western Africa", "Central Africa",
                                             "Eastern Africa", "Southern Africa", "Northern America",
                                             "Central America", "Caribbean", "South America",
                                             "Western Asia", "Central Asia", "Southern Asia",
                                             "South Eastern Asia", "Eastern Asia", "Northern Europe",
                                             "Southern Europe", "Western Europe", "Central & Eastern Europe",
                                             "Europe/Asia", "Oceania"), 1, 0),
                check4 = cumsum(check3),
                check5 = ifelse(stringr::str_replace_all(string = orig, pattern =  "\\s", replacement = "") %in% 
                                  c("3","4","5","6","7","8","9","10","11","12","13","14","15"), 0, check4), 
                check6 = str_count(str_replace_all(orig, "\\s", ""), "[a-zA-Z]")/str_length(str_replace_all(orig, "\\s", "")) *100) %>%
  ### Drop irrelevant lines
  dplyr::filter(check5 != 0) %>% dplyr::filter(check6 < 80) %>%
  dplyr::select(-check3,-check5,-check6) -> wpre_01a

## Create a new version of the data.frame
wpre_01a %>%
  ### Create region and subregion variables
  dplyr::mutate(subregion = car::recode(check4, "1='Northern Africa';2='Western Africa';
                             3='Central Africa'          ; 4='Eastern Africa'    ; 5='Southern Africa'; 
                             6='Northern America'        ; 7='Central America'   ; 8='Caribbean'; 
                             9='South America'           ;10='Western Asia'      ;11='Central Asia';
                            12='Southern Asia'           ;13='South-Eastern Asia';14='Eastern Asia';
                            15='Northern Europe'         ;16='Southern Europe'   ;17='Western Europe';
                            18='Central & Eastern Europe';19='Europe/Asia'       ;20='Oceania'"),
                region = car::recode(subregion,"c('Northern Africa','Western Africa','Central Africa',
                            'Eastern Africa','Southern Africa')='Africa';c('Northern America',
                            'Central America','Caribbean','South America')='America';c('Western Asia',
                            'Central Asia','Southern Asia','South-Eastern Asia','Eastern Asia',
                            'Europe/Asia')='Asia';c('Northern Europe','Southern Europe','Western Europe',
                            'Central & Eastern Europe')='Europe';'Oceania'='Oceania'")) %>%
  dplyr::select(-check4) %>%
  ### Create new version of origin (replace with |)
  dplyr::mutate(orig1 = stringr::str_replace_all(string = orig,  pattern = "c\\. ", replacement = ""),
                orig2 = stringr::str_replace_all(string = orig1, pattern = "\\s+", replacement = "\\|"),
                orig3 = str_replace_all(orig2, "(?<=[\\p{L}.()])\\|(?=\\p{L}(?!c\\.))", "@") %>%
                  str_replace_all(., "(?<=[\\p{L}.()])\\|(?=[().])", "@"),
                orig4 = str_replace(orig3, pattern = "\\|&\\|", replacement = "@&@"),
                orig5 = str_replace(orig4, pattern = fixed("/|"), replacement = "|"),
                orig6 = str_replace_all(orig5, "\\|:", "%:"),
                orig7 = car::recode(orig6,"'Guinea@Bissau|133|18.2.17|67.9%|1.92m|7|not@available'=
                                    'Guinea@Bissau|133|18.2.17|67.9%|1.92m|7|NA|NA|NA|NA';
                                    'Central@African@Republic*|384|11.11|70.2%|4.51m|-|not@available'=
                                    'Central@African@Republic|384|11.11|70.2%|4.51m|-|NA|NA|NA|NA';
                                    'Comoros|42|31.12.15|29.0%|779,000|5|not@available'=
                                    'Comoros|42|31.12.15|29.0%|779,000|5|NA|NA|NA|NA';
                                    'Rwanda|**|4,354|5.17|7.5%|11.95m|36|2000|c.109,250|95%|1,300'=
                                    'Rwanda|4,354|5.17|7.5%|11.95m|36|2000|c.109,250|95%|1,300';
                                    'Namibia|***|3,910|.15|54%|2.43m|161|2001|250|5.2%|13'=
                                    'Namibia|3,910|.15|54%|2.43m|161|2001|250|5.2%|13';
                                    'Sint@Maarten@(Neth)|17|9.7.07|18.1%|39,470|43|not@available'=
                                    'Sint@Maarten@(Neth)|17|9.7.07|18.1%|39,470|43|NA|NA|NA|NA';
                                    'Suriname|505|2.11|50%|528,000|96|not@available'=
                                    'Suriname|505|2.11|50%|528,000|96|NA|NA|NA|NA';
                                    'Syria|5,353|.04|50.5%|17.68m|30|not@available'=
                                    'Syria|5,353|.04|50.5%|17.68m|30|NA|NA|NA|NA';
                                    'Laos|6,000|.16|67%|6.91m|87|not@available'=
                                    'Laos|6,000|.16|67%|6.91m|87|NA|NA|NA|NA';
                                    'Palau|4|30.4.05|4.1%|20,000|20|not@available'=
                                    'Palau|4|30.4.05|4.1%|20,000|20|NA|NA|NA|NA'"),
                orig8 = str_replace_all(orig7, ":\\|", ":@"),
                orig9 = str_replace_all(orig8, "avrge\\|", "."),
                orig10 = str_replace_all(orig9, "Feb/Mar\\|21","Feb/Mar%21"),
                orig11 = str_replace_all(orig10, "\\|-\\|","\\|NA\\|") ) %>%    
  dplyr::select(subregion,region,orig11) -> wpre_01b

## Final version of the data.frame
wpre_01b %>%
  ### Same number of "|" symbols
  dplyr::mutate(orig12 = str_replace(orig11, "^\\|(\\d)", "||||||\\1")) %>%
  ### Select relevant variables
  dplyr::select(subregion,region,orig12) %>%
  ### Splitting into multiple columns
  tidyr::separate(data = ., col = orig12, 
                  into = c("country_0", "pretrial_A0", "year_A0", "percentage_A0", "population_A0", "rate_A0",
                           "year_B0","pretrial_B0", "percentage_B0", "rate_B0"), sep = "\\|", 
                  extra = "merge", fill = "right") -> wpre_01c

## Repairing variables
wpre_01c %>%
  ### 1) Country
  dplyr::mutate(country_1 = str_replace_all(country_0, "@", " "),
                extra1    = str_detect(country_1, "\\(USA\\)|\\(UK\\)|\\(Netherlands\\)|
                                      \\(NZ\\)|\\(France\\)|\\(Denmark\\)|\\(China\\|
                                                     \\(Fr\\)|\\(Neth\\)"),
                extra2    = ifelse(country_0 %in% c("French@Guiana"), TRUE, extra1),  
                country_2 = str_trim(str_replace_all(country_1, "\\(USA\\)|\\(UK\\)|\\(Netherlands\\)|
                                                    \\(NZ\\)|\\(France\\)|\\(Denmark\\)|\\(China\\)|
                                                     \\(Fr\\)|\\(Neth\\)","")),
                country_3 = car::recode(country_2,"'Guinea (Republic of)'='Republic of Guinea';
                                        'Congo (Republic of)'='Republic of Congo';
                                        'Democratic Republic'='Democratic Republic of Congo';
                                        'Swaziland'='Eswatini';'U.S.A.'='United States of America';
                                        'Antigua & Barbuda' = 'Antigua and Barbuda';'St Kitts & Nevis' = 'St Kitts and Nevis';
                                        'St Vincent & the'='St Vincent and the Grenadines';'Trinidad & Tobago' = 'Trinidad and Tobago';
                                        'Cayman Is'='Cayman Islands';'Korea (Republic of)'='Republic of Korea';
                                        'Faeroes'='Faroe Islands';'Ukraine*'='Ukraine';
                                        'Federated States of'='Federated States of Micronesia';
                                        'Cook Islands (NZ)'='Cook Islands';
                                        'French Polynesia (Fr)'='French Polynesia';
                                        'New Caledonia (Fr)'='New Caledonia'"),
                ### 2) Prison Population
                pretrial_A1 = str_replace_all(pretrial_A0, ",", ""),
                pretrial_A2 = str_replace_all(pretrial_A1, "\\*", ""),
                ### 3) Year
                year_A1   = car::recode(year_A0, "'2013/14'='.14';'2017/18'='.18';
                                        'mid-15'='.15';'mid-16'='.16';'mid-18'='.18'"),
                year_A2 = str_sub(year_A1, -3, -1),
                year_A3 = str_replace(year_A2, pattern = ".", "20"),
                ### 4) Percentage
                percentage_A1 = str_replace_all(percentage_A0, "%", ""),
                ### 5) Total Population
                population_A1 = ifelse(str_sub(population_A0, -1, -1) == "m", 1,
                                       ifelse(population_A0 == "", "", 0)),
                population_A2 = ifelse(test = population_A1 == 1, 
                                       yes = as.numeric(str_replace_all(population_A0,"m|,",""))*1000000,
                                       no = ifelse(test = population_A1 == 0,
                                                   yes = as.numeric(str_replace_all(population_A0,",", "")),
                                                   no = population_A1)),
                ### 6) Prison Rate
                rate_A1 = as.numeric(str_replace_all(rate_A0, pattern = "\\*", replacement = "")),
                rate_A2 = ifelse(is.na(rate_A1)==T, "", rate_A1),
                rate_A3 = ifelse(country_3 == "Central African Republic", "NA", rate_A2),
                ### 7) Year Trend
                year_B1 = car::recode(year_B0, "'2000/01'='2001';'2004/05'='2005';
                                      '2005/06'='2006';'2010/11'='2011';'2015/16'='2016'"),
                ### 8) Prison Population Trend
                pretrial_B1 = str_replace_all(pretrial_B0, pattern = "w|c.", ""),
                pretrial_B2 = ifelse(str_sub(pretrial_B1, -1, -1) == "m", 1,
                                     ifelse(pretrial_B1 == "", "", 0)),
                pretrial_B3 = ifelse(test = pretrial_B2 == 1, 
                                     yes = as.numeric(str_replace_all(pretrial_B1,"m|,|\\*",""))*1000000,
                                     no = ifelse(test = pretrial_B2 == 0,
                                                 yes = as.numeric(str_replace_all(pretrial_B1,",|\\*", "")),
                                                 no = pretrial_B2)),
                ### 9) Percentage
                percentage_B1 = str_replace_all(percentage_B0, "%", ""),
                ### 10) Prison Rate Trend
                rate_B1 = str_replace_all(rate_B0, pattern = "\\*",""),
                rate_B2 = str_replace_all(rate_B1, pattern = ",", ""),
                rate_B3 = str_replace_all(rate_B2, pattern = "-", "0"),
                rate_B4 = ifelse(rate_B3 == "", "NA", rate_B3)
  ) -> wpre_01d

## Final modifications to Country
wpre_01d$country_4 <- ifelse(wpre_01d$country_3 == "Cote d’Ivoire", "Ivory Coast", 
                             wpre_01d$country_3)

wpre_01d$country_4[251] <- "British Virgin Islands"
wpre_01d$country_4[455] <- "UK: England and Wales"
wpre_01d$country_4[491] <- "Bosnia and Herzegovina (Federation)"
wpre_01d$country_4[495] <- "Bosnia and Herzegovina (Republika Srpska)"

## Impute values for country
wpre_01d$country_5 <- wpre_01d$country_4

for (i in 2:dim(wpre_01d)[1]) {
  wpre_01d$country_5[i] <- ifelse(wpre_01d$country_4[i] == "",
                                  wpre_01d$country_5[i-1],
                                  wpre_01d$country_4[i])
}

## Select variables
wpre_01d %>% 
  dplyr::select(subregion, region, country_5, 
                year_A3, pretrial_A2, percentage_A1, population_A2, rate_A3,
                year_B1, pretrial_B3, percentage_B1,                rate_B4) %>% 
  dplyr::rename(country = country_5, year_1 = year_A3, pretrial_1 = pretrial_A2,
                percentage_1 = percentage_A1, population_1 = population_A2, rate_1 = rate_A3,
                year_2 = year_B1, pretrial_2 = pretrial_B3, percentage_2 = percentage_B1, rate_2 = rate_B4) -> wpre_01e

## Identify current values
wpre_01e %>% 
  dplyr::select(subregion,region,country,year_1, pretrial_1, percentage_1, population_1, rate_1) %>%
  dplyr::filter(year_1 != "") %>%
  dplyr::rename(year = year_1, pretrial = pretrial_1, percentage = percentage_1,
                population = population_1, rate = rate_1) -> wpre_01f1

## Identify historical values
wpre_01e %>% 
  dplyr::mutate(population_2 = NA) %>% 
  dplyr::select(subregion,region,country,year_2, pretrial_2, percentage_2, population_2, rate_2) %>%
  dplyr::filter(is.na(pretrial_2)==F) %>%
  dplyr::rename(year = year_2, pretrial = pretrial_2, percentage = percentage_2, 
                population = population_2, rate = rate_2) %>%
  dplyr::mutate(pretrial = as.character(pretrial), population = as.character(population)) -> wpre_01f2

## Merge data.frames
wpre_01g <- bind_rows(wpre_01f1, wpre_01f2)

wpre_01g %>%
  dplyr::mutate(year       = as.numeric(year),
                pretrial   = as.numeric(pretrial),
                percentage = as.numeric(percentage),
                population = as.numeric(population),
                rate       = as.numeric(rate)) -> wpre_01h

wpre_01h %>% 
  dplyr::mutate(population = ifelse(is.na(population) == TRUE, round(pretrial/rate*100000), population)
  ) %>%
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) -> wpre_01i


## Remove objects
rm(i, list_03, wpre_01, wpre_01a, wpre_01b, wpre_01c, wpre_01d, wpre_01e, wpre_01f1, wpre_01f2, wpre_01g, wpre_01h)


######################################################     STEP 05: WEB SCRAPING     ######################################################

## General website
wpb_00 <- read_html(x = "https://www.prisonstudies.org/world-prison-brief-data")

## Extract countries
wpb_00 %>% html_nodes("a") %>% html_attr("href") %>% str_subset(string = ., pattern = "/country/") -> list_00


# 1) BACKGROUND INFORMATION

## Create function for all the information
extractor_01 <- purrr::safely(function(link){
  
  ### Verbose
  message("Getting page ",i)
  Sys.sleep(5)
  
  ### Create the new link
  wpblink <- read_html(x = paste0("https://www.prisonstudies.org",link))
  
  ### Extract tables
  wpblink %>% html_nodes("table") -> subset_nodes
  
  ### Combine Available information
  bind_cols(
    #### 1) Variable Name
    subset_nodes[1] %>% html_table() %>% as.data.frame() %>%
      select(X2) %>% dplyr::rename(country = X2),
    #### 2) General Description
    subset_nodes[2] %>% html_table() %>% as.data.frame() %>% 
      dplyr::mutate(X2 = str_squish(str_replace_all(X2, pattern = "\n|Circa", replacement = "")),
                    X3 = str_replace_all(X2, pattern = "(\\d)\\s+(?=\\d)", replacement = "\\1"))  %>%
      dplyr::arrange(X1) %>% select(X1,X3) %>%
      spread(key = X1, value = X3) %>%
      dplyr::rename_with(~stringr::str_to_lower(.) %>% stringr::str_replace_all(" ", "_")), 
    #### 3) Contemporaneous Values
    subset_nodes[3] %>% html_table() %>% as.data.frame() %>% 
      dplyr::mutate(X1a = str_remove_all(X1, "\\([^()]*\\)"),
                    X1b = str_squish(str_replace(X1a, "/.*", "")),
                    X2a = str_squish(str_replace_all(X2, pattern = "\n|Circa", replacement = "")),
                    X2b = str_replace_all(X2a, pattern = "(\\d)\\s+(?=\\d)", replacement = "\\1"),
                    X2c = str_replace(X2b, pattern = "\\s+(?=[a-zA-Z\\(])", replacement = "@") )  %>%
      tidyr::separate(col = X2c, into = c("X2c_1", "X2c_2"), sep = "@", remove = TRUE, extra = "merge") %>% 
      dplyr::mutate(X1b_3 = X1b,               X1b_4 = str_c(X1b, " Date"),
                    X2c_3 = str_squish(X2c_1), X2c_4 = str_squish(X2c_2)) %>%
      dplyr::arrange(X1b_3) %>% select(X1b_3,X2c_3, X1b_4, X2c_4) %>%
      pivot_longer(cols = c(X2c_3, X2c_4), names_to = "X1", values_to = "X3") %>% 
      dplyr::mutate(X2 = ifelse(X1 == "X2c_3", X1b_3, X1b_4)) %>%
      dplyr::select(X2, X3) %>%
      spread(key = X2, value = X3) %>%
      dplyr::rename_with(~stringr::str_to_lower(.) %>% stringr::str_replace_all(" ", "_")) )  -> out_df
  ### Define output
  return(out_df)
})

## Create list to store results
list_01a  <- list()
list_01a1 <- list()

## Run loop for iterations
for(i in 1:226){
  list_01a[[i]] <- extractor_01(link = list_00[i])
}
list_01a1[[1]]  <- extractor_01(link = list_00[51])
list_01a1[[2]]  <- extractor_01(link = list_00[77])
list_01a1[[3]]  <- extractor_01(link = list_00[82])
list_01a1[[4]]  <- extractor_01(link = list_00[87])
list_01a1[[5]]  <- extractor_01(link = list_00[89])
list_01a1[[6]]  <- extractor_01(link = list_00[102])
list_01a1[[7]]  <- extractor_01(link = list_00[106])
list_01a1[[8]]  <- extractor_01(link = list_00[120])
list_01a1[[9]]  <- extractor_01(link = list_00[124])
list_01a1[[10]] <- extractor_01(link = list_00[131])
list_01a1[[11]] <- extractor_01(link = list_00[150])
list_01a1[[12]] <- extractor_01(link = list_00[152])
list_01a1[[13]] <- extractor_01(link = list_00[159])
list_01a1[[14]] <- extractor_01(link = list_00[179])
list_01a1[[15]] <- extractor_01(link = list_00[187])
list_01a1[[16]] <- extractor_01(link = list_00[215])

## Save the data
write_rds(x = list_01a,  file = "2_Data/2_Processed_Data/2_World_Prison_Brief/World_Prison_Brief_Webpage_01.rds")
write_rds(x = list_01a1, file = "2_Data/2_Processed_Data/2_World_Prison_Brief/World_Prison_Brief_Webpage_01a.rds")

## Combine all 'result' data frames into one
df_01a  <- do.call(bind_rows, lapply(list_01a, function(x) x$result))
df_01a1 <- do.call(bind_rows, lapply(list_01a1, function(x) x$result))

df_01a2 <- dplyr::bind_rows(df_01a, df_01a1)

## Cleaning the variables
df_01a2 %>%
  dplyr::rename(address     = contact_address,
                dir_prison  = `head_of_prison_administration_(and_title)`,
                ministry    = ministry_responsible,
                adm_prison  = prison_administration,
                pris_tot_0  = prison_population_total,
                pris_toty_0 = prison_population_total_date,
                pris_rate_0 = prison_population_rate,
                pris_raty_0 = prison_population_rate_date,
                pre_porc_0  = `pre-trial_detainees`,
                pre_porcy_0 = `pre-trial_detainees_date`,
                fem_porc_0  = female_prisoners,
                fem_porcy_0 = female_prisoners_date,
                juv_porc_0  = juveniles,
                juv_porcy_0 = juveniles_date,
                for_porc_0  = foreign_prisoners,
                for_porcy_0 = foreign_prisoners_date,
                est_numb_0  = number_of_establishments,
                est_numy_0  = number_of_establishments_date,
                off_cap_0   = official_capacity_of_prison_system,
                off_capy_0  = official_capacity_of_prison_system_date,
                off_occ_0   = occupancy_level,
                off_occy_0  = occupancy_level_date ) %>%
  dplyr::mutate(country     = str_squish(str_replace_all(country, "\\s+", " ")),
                ministry    = str_squish(str_replace_all(ministry, "\\s+", " ")),
                adm_prison  = str_squish(str_replace_all(adm_prison, "\\s+", " ")),
                dir_prison  = str_squish(str_replace_all(dir_prison, "\\s+", " ")),
                address     = str_squish(str_replace_all(address, "\\s+", " ")),
                telephone   = str_squish(str_replace_all(telephone, "\\s+", " ")),
                fax         = str_squish(str_replace_all(fax, "\\s+", " ")),
                website     = str_squish(str_replace_all(website, "\\s+", " ")),
                email       = str_squish(str_replace_all(email, "\\s+", " ")),
                pris_tot_1  = str_squish(str_replace_all(pris_tot_0, "\\*|The", "NA")),
                pris_toty_1 = str_squish(str_replace_all(pris_toty_0, "[a-zA-Z]|&|\\(|\\)|;|,|é|'|-", "")),
                pris_toty_2 = ifelse(str_detect(pris_toty_1, "\\.\\d{4}")==T,
                                     yes = str_extract(pris_toty_1, "\\.\\d{4}"), 
                                     no = ifelse(str_detect(pris_toty_1, "^\\d{4}$")==T,
                                                 yes = paste0(".",str_extract(pris_toty_1, "^\\d{4}$")), 
                                                 no = ifelse(test = str_detect(pris_toty_1, "^\\d{4} \\.{2}$")==T,
                                                             yes = paste0(".",str_replace_all(pris_toty_1, pattern = "\\.| ",
                                                                                              replacement = "")), 
                                                             no = pris_toty_1))),
                pris_toty_3 = car::recode(pris_toty_2, "'.. 2016 80000 120000.'='.2016';
                                          '1.6.24 53397 117850'='.2024';
                                          '2004 9 . 2023 156457 5235 10199 .. .'='.2004';
                                          '2017 .. .'='.2017';'2019 .. . 300 .'='.2019';
                                          '2021/22 4804 2021 4087 2022 .. .'='.2022';
                                          '2022 .'='.2022';'2022 32200 35000'='.2022';
                                          '2022 è'='.2022' ", as.factor = TRUE),
                pris_rate_1  = str_squish(str_replace_all(pris_rate_0, "\\*|The", "NA")),
                pris_raty_1 = str_squish(str_replace_all(pris_raty_0, "[a-zA-Z]|&|\\(|\\)|;|,|é|'|-", "")),
                pris_raty_2 = str_squish(str_replace_all(pris_raty_1, "\\.\\.", "")),
                pris_raty_3 = ifelse(pris_raty_2 == "1.418 2018 . 2340000 165 100000.", "1.418 2018", pris_raty_2),
                pris_raty_4 = ifelse(is.na(pris_raty_3), "NA@NA", str_replace(pris_raty_3, " ", "@")),
                population1 = as.numeric(sapply(str_split(pris_raty_4, "@"), `[`, 1)),
                population2 = ifelse(str_detect(population1, "\\.") == T, population1 *1000000, 
                                     ifelse(population1 == 39, 39*1000000, population1)),
                population3 = ifelse(population2 == 11, 11000000,
                                     ifelse(population2 == 29, 29000000, population2)),
                pris_raty_5 = sapply(str_split(pris_raty_4, "@"), `[`, 2),
                pris_raty_6 = ifelse(pris_raty_5 == "NA", NA, (pris_raty_5)),
                pre_porc_1  = str_squish(str_replace_all(pre_porc_0, "%", "")),
                pre_porc_2  = car::recode(pre_porc_1, "'54.0 2021/22'='54.0';'The'=NA"),
                pre_porcy_1 = str_squish(str_replace_all(pre_porcy_0, "[a-zA-Z]|&|\\(|\\)|;|,|é|'|-", "")),
                pre_porcy_2 = ifelse(str_detect(pre_porcy_1, "\\.\\d{4}")==T,
                                     yes = str_extract(pre_porcy_1, "\\.\\d{4}"), 
                                     no = ifelse(str_detect(pre_porcy_1, "^\\d{4}$")==T,
                                                 yes = paste0(".",str_extract(pre_porcy_1, "^\\d{4}$")), 
                                                 no = ifelse(test = str_detect(pre_porcy_1, "^\\d{4} \\.{2}$")==T,
                                                             yes = paste0(".",str_replace_all(pre_porcy_1, pattern = "\\.| ", 
                                                                                              replacement = "")), 
                                                             no = pre_porcy_1))),
                pre_porcy_3 = car::recode(pre_porcy_2, "'2022/23'='.2023';'/ 200000. 2014 44 2012.'='.2014';'201112'='.2012';
                                          '2023/24'='.2024'",as.factor = TRUE),
                fem_porc_1  = str_squish(str_replace_all(fem_porc_0, "%", "")),
                fem_porcy_1 = str_squish(str_replace_all(fem_porcy_0, "[a-zA-Z]|&|\\(|\\)|;|,|é|'|-|/", "")),
                fem_porcy_2 = ifelse(str_detect(fem_porcy_1, "\\.\\d{4}")==T,
                                     yes = str_extract(fem_porcy_1, "\\.\\d{4}"), 
                                     no = ifelse(str_detect(fem_porcy_1, "^\\d{4}$")==T,
                                                 yes = paste0(".",str_extract(fem_porcy_1, "^\\d{4}$")), 
                                                 no = ifelse(test = str_detect(fem_porcy_1, "^\\d{4} \\.{2}$")==T,
                                                             yes = paste0(".",str_replace_all(fem_porcy_1, pattern = "\\.| ", 
                                                                                              replacement = "")), 
                                                             no = fem_porcy_1))),
                fem_porcy_3 = ifelse(fem_porcy_2 == "2027 2017", ".2017", 
                                     ifelse(fem_porcy_2 == "21.3.14", ".2014",
                                            ifelse(fem_porcy_2 == "201314", ".2014",
                                                   ifelse(fem_porcy_2 == "202324", ".2024",
                                                          ifelse(fem_porcy_2 == ".1020", ".2021",
                                                                 fem_porcy_2))))),
                for_porc_1  = str_squish(str_replace_all(for_porc_0, "%", "")),
                for_porcy_1 = str_squish(str_replace_all(for_porcy_0, "[a-zA-Z]|&|\\(|\\)|;|,|é|'|-|/", "")),
                for_porcy_2 = ifelse(str_detect(for_porcy_1, "\\.\\d{4}")==T,
                                     yes = str_extract(for_porcy_1, "\\.\\d{4}"), 
                                     no = ifelse(str_detect(for_porcy_1, "^\\d{4}$")==T,
                                                 yes = paste0(".",str_extract(for_porcy_1, "^\\d{4}$")), 
                                                 no = ifelse(test = str_detect(for_porcy_1, "^\\d{4} \\.{2}$")==T,
                                                             yes = paste0(".",str_replace_all(for_porcy_1, pattern = "\\.| ", 
                                                                                              replacement = "")), 
                                                             no = for_porcy_1))),
                for_porcy_3 = ifelse(for_porcy_2 == "2021 .", ".2021",
                                     ifelse(for_porcy_2 == "49 2010.", ".2010",
                                            for_porcy_2)),
                juv_porc_1 = str_squish(str_replace_all(juv_porc_0, "[a-zA-Z]|%|\\(", "")),
                juv_porc_2 = ifelse(juv_porc_1 == "5.9 31.12.2016 -", "5.9",
                                    ifelse(juv_porc_1 == "", NA, juv_porc_1)),
                juv_porcy_1 = str_squish(str_replace_all(juv_porcy_0, "[a-zA-Z]|&|\\(|\\)|;|,|é|'|-|/", "")),
                juv_porcy_2 = ifelse(str_detect(juv_porcy_1, "\\.\\d{4}")==T,
                                     yes = str_extract(juv_porcy_1, "\\.\\d{4}"), 
                                     no = ifelse(str_detect(juv_porcy_1, "^\\d{4}$")==T,
                                                 yes = paste0(".",str_extract(juv_porcy_1, "^\\d{4}$")), 
                                                 no = ifelse(test = str_detect(juv_porcy_1, "^\\d{4} \\.{2}$")==T,
                                                             yes = paste0(".",str_replace_all(juv_porcy_1, pattern = "\\.| ", 
                                                                                              replacement = "")), 
                                                             no = juv_porcy_1))),
                juv_porcy_3 = car::recode(juv_porcy_2, "'.'=NA;'. 2014 48 .'='.2014';'.1020'='.2021';'20'='.2016';
                                                        '2023 ‘’'='.2023';''=NA"),
                juv_porcy_4 = str_replace_all(juv_porcy_3, "(\\d{4}) (\\d+)", ".\\1"),
                est_numb_1 = str_squish(str_replace_all(est_numb_0, "[a-zA-Z]|%|\\(|\\*|‘’", "")),
                est_numb_2 = ifelse(est_numb_1 == "", "NA", est_numb_1),
                est_numy_1 = str_squish(str_replace_all(est_numy_0, "[a-zA-Z]|&|\\(|\\)|;|,|é|'|-|/", "")),
                est_numy_2 = str_extract(est_numy_1, "20\\d{2}"),
                off_occ_1  = str_squish(str_replace_all(off_occ_0, "[a-zA-Z]|%|\\(|'", "")),
                off_occ_2  = ifelse(off_occ_1 == "", NA, off_occ_1),
                off_occy_1 = str_squish(str_replace_all(off_occy_0, "[a-zA-Z]|&|\\(|\\)|;|,|é|'|-|/", "")),
                off_occy_2 = str_extract(off_occy_1, "20\\d{2}"),
                off_capy_1 = str_squish(str_replace_all(off_capy_0, "[a-zA-Z]|&|\\(|\\)|;|,|é|'|-|/", "")),
                off_capy_2 = str_extract(off_capy_1, "20\\d{2}")
                ) %>%
  dplyr::select(country:website, pris_tot_1, pris_toty_3, pris_rate_1, pris_raty_6, population3, 
                pre_porc_2, pre_porcy_3, fem_porc_1, fem_porcy_3, for_porc_1, for_porcy_3, 
                juv_porc_2, juv_porcy_4, est_numb_2, est_numy_2, off_occ_2, 
                off_occy_2, off_cap_0, off_capy_2)-> df_01b

## Recode Country
df_01b %>%
dplyr::mutate(country = str_trim(str_replace_all(country, "\\(USA\\)|\\(UK\\)|\\(Netherlands\\)|
                                                    \\(NZ\\)|\\(France\\)|\\(Denmark\\)|\\(China\\)",
                                                             "")),
              country = car::recode(country, "'Anguilla (United Kingdom)'='Anguilla';'Bermuda (United Kingdom)'=
                                    'Bermuda';'Virgin Islands (United Kingdom)'='British Virgin Islands';
                                    'Cape Verde (Cabo Verde)'='Cape Verde';'Cayman Islands (United Kingdom)'=
                                    'Cayman Islands';'Cook Islands (New Zealand)'='Cook Islands';
                                    'Congo (Republic of)'='Republic of Congo';'Curaçao'='Curacao';
                                    'Cyprus (Republic of)'='Cyprus';'Eswatini/Swaziland'='Eswatini';
                                    'Faeroe Islands'='Faroe Islands';'Micronesia, Federated States of'=
                                    'Federated States of Micronesia';'French Guiana/Guyane'='French Guiana';
                                    'Gibraltar (United Kingdom)'='Gibraltar';'Guernsey (United Kingdom)'=
                                    'Guernsey';'Guinea (Republic of)'='Republic of Guinea';
                                    'Ireland, Republic of'='Ireland';'Isle of Man (United Kingdom)'='Isle of Man';
                                    'Jersey (United Kingdom)'='Jersey';'Kosovo/Kosova'='Kosovo';'Moldova (Republic of)'=
                                    'Moldova';'Myanmar (formerly Burma)'='Myanmar';'Republic of (South) Korea'=
                                    'Republic of Korea';'Somalia (Federal Republic of)'='Somalia';
                                    'St. Kitts and Nevis'='St Kitts and Nevis';'St. Lucia'='St Lucia';
                                    'St. Vincent and the Grenadines'='St Vincent and the Grenadines';
                                    'Timor-Leste (formerly East Timor)'='Timor-Leste'
                                    
                                    ")
                ) -> df_01b

df_01b$country[13] <- 'Ivory Coast'
df_01b$country[118] <- 'Bosnia and Herzegovina (Federation)'
df_01b$country[119] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_01b$country[161] <- 'UK: England and Wales'
df_01b$country[162] <- 'UK: Northern Ireland'
df_01b$country[163] <- 'UK: Scotland'

## Final format
df_01b %>%
  dplyr::mutate(pris_tot    = as.numeric(pris_tot_1),
                pris_toty_4 = str_replace_all(pris_toty_3, "\\.", ""),
                pris_toty   = as.numeric(ifelse(pris_toty_4 == "NA", NA, pris_toty_4)),
                pris_rate   = as.numeric(pris_rate_1),
                pris_raty   = as.numeric(pris_raty_6),
                population  = population3,
                pre_porc    = as.numeric(pre_porc_2),
                pre_porcy_4 = str_replace(pre_porcy_3, "0\\.",""),
                pre_porcy   = as.numeric(ifelse(pre_porcy_4 == "201","2010", 
                                     ifelse(pre_porcy_4 == "202", "2020",
                                            pre_porcy_4))),
                fem_porc    = as.numeric(fem_porc_1),
                fem_porcy   = as.numeric(str_replace(fem_porcy_3, "\\.","")),
                for_porc    = as.numeric(for_porc_1),
                for_porcy   = as.numeric(str_replace(for_porcy_3, "\\.","")),
                juv_porc    = as.numeric(juv_porc_2),
                juv_porcy   = as.numeric(str_replace(juv_porcy_4, "\\.","")),
                est_numb    = as.numeric(ifelse(est_numb_2 == "NA", NA, est_numb_2)),
                est_numy    = as.numeric(est_numy_2),
                off_occ     = as.numeric(off_occ_2),
                off_occy    = as.numeric(off_occy_2),
                off_cap     = as.numeric(off_cap_0),
                off_capy    = as.numeric(off_capy_2)
                ) %>%
  dplyr::select(country:website,pris_tot:off_capy,
                -pris_toty_4,-pre_porcy_4)-> df_01c

## Remove objects
rm(list_01a, list_01a1, df_01a, df_01a1, df_01a2, df_01b, i, extractor_01)


# 2) GENERAL PRISON TRENDS

## Create function for all the information
extractor_02 <- purrr::safely(function(link){
  
  ### Verbose
  message("Getting page ", link)
  Sys.sleep(7)
  
  ### Create the new link
  wpblink <- read_html(x = paste0("https://www.prisonstudies.org", link))
  
  ### Extract tables
  wpblink %>% html_nodes("table") -> subset_nodes
  
  ### Conditional extraction based on subset_nodes length
  if (length(subset_nodes) >= 9) {
    ### Combine Available information with Historical Trends
    dplyr::bind_cols(
      #### 1) Variable Name
      subset_nodes[1] %>% html_table() %>% as.data.frame() %>%
        select(X2) %>% dplyr::rename(country = X2),
      
      dplyr::bind_rows(
        #### 2) Prison Trends
        subset_nodes[5] %>% html_table() %>% as.data.frame() %>%
          dplyr::mutate(Year = as.character(Year),
                        Prison.population.total = as.character(Prison.population.total),
                        Prison.population.rate = as.character(Prison.population.rate) ),
        #### 3) Historical Trends
        subset_nodes[9] %>% html_table() %>% as.data.frame() %>%
          dplyr::slice(2) %>%
          dplyr::mutate(across(everything(), ~str_replace_all(., "\n", "@") ) ) %>%
          tidyr::separate_longer_delim(cols = X1:X3, delim = "@") %>%
          dplyr::rename(Year = X1, Prison.population.total = X2,
                        Prison.population.rate = X3) %>%
          dplyr::mutate(Year = as.character(Year),
                        Prison.population.total = as.character(Prison.population.total),
                        Prison.population.rate = as.character(Prison.population.rate) )
      )
    ) -> out_df
  } else {
    ### Combine Available information without Historical Trends
    dplyr::bind_cols(
      #### 1) Variable Name
      subset_nodes[1] %>% html_table() %>% as.data.frame() %>%
        select(X2) %>% dplyr::rename(country = X2),
      
      #### 2) Prison Trends
      subset_nodes[5] %>% html_table() %>% as.data.frame() %>%
        dplyr::mutate(Year = as.character(Year),
                      Prison.population.total = as.character(Prison.population.total),
                      Prison.population.rate = as.character(Prison.population.rate) )
    ) -> out_df
  }
  
  ### Define output
  return(out_df)
})

## Create list to store results
list_01b  <- list()
list_01b1 <- list()
list_02b  <- list()
list_02b1 <- list()

## Run loop for iterations
for(i in 1:226){
  list_01b[[i]] <- extractor_02(link = list_00[i])
}
list_01b1[[1]]   <- extractor_02(link = list_00[2])
list_01b1[[2]]   <- extractor_02(link = list_00[9])
#list_01b1[[3]]   <- extractor_02(link = list_00[17])
#list_01b1[[4]]   <- extractor_02(link = list_00[18])
#list_01b1[[5]]   <- extractor_02(link = list_00[25])
list_01b1[[6]]   <- extractor_02(link = list_00[29])
list_01b1[[7]]   <- extractor_02(link = list_00[31])
list_01b1[[8]]   <- extractor_02(link = list_00[33])
#list_01b1[[9]]   <- extractor_02(link = list_00[47])
#list_01b1[[10]]  <- extractor_02(link = list_00[63])
list_01b1[[11]]  <- extractor_02(link = list_00[144])
list_01b1[[12]]  <- extractor_02(link = list_00[159])
#list_01b1[[13]]  <- extractor_02(link = list_00[165])
list_01b1[[14]]  <- extractor_02(link = list_00[179])
list_01b1[[15]]  <- extractor_02(link = list_00[220])
list_01b1[[16]]  <- extractor_02(link = list_00[222])
list_01b1[[17]]  <- extractor_02(link = list_00[224])
list_01b1[[18]]  <- extractor_02(link = list_00[226])

## Save the data
write_rds(x = list_01b,  file = "2_Data/2_Processed_Data/2_World_Prison_Brief/World_Prison_Brief_Webpage_General_Trends_01.rds")
write_rds(x = list_01b1, file = "2_Data/2_Processed_Data/2_World_Prison_Brief/World_Prison_Brief_Webpage_General_Trends_01a.rds")

## Transform into character strings
for(i in 1:226){
  list_01b[[i]]$result %>% as.data.frame() %>%
    dplyr::mutate(across(everything(), as.character)) %>%
    as.data.frame() -> list_02b[[i]]
}
for(i in 1:18){
  list_01b1[[i]]$result %>% as.data.frame() %>%
    dplyr::mutate(across(everything(), as.character)) %>%
    as.data.frame() -> list_02b1[[i]]
}

## Combine all 'result' data frames into one
df_02a  <- do.call(bind_rows, list_02b)
df_02a1 <- do.call(bind_rows, list_02b1)

df_02a2 <- dplyr::bind_rows(df_02a, df_02a1)
# 220 Countries OK -> 6 No Info

## Cleaning the variables
df_02a2 %>%
  dplyr::rename(year  = Year,
                tot_0 = Prison.population.total,
                rat_0 = Prison.population.rate ) %>%
  dplyr::mutate(tot_1 = as.character(str_squish(str_replace_all(tot_0, ",|c|\\.|\\+|\\*", ""))),
                tot_2 = as.numeric(tot_1),
                tot_3 = ifelse(country == "Rwanda" & year == "1998", 145021,
                               ifelse(country == "Rwanda" & year == "2002", 112000,
                                      ifelse(country == "Rwanda" & year == "2004", 87000,
                                             ifelse(country == "Rwanda" & year == "2006", 82000,
                                                    ifelse(country == "Rwanda" & year == "2008", 59311,
                                                           tot_2))))),
                rat_1 = as.character(str_squish(str_replace_all(rat_0, ",|c|\\.|\\+|\\*", ""))),
                rat_2 = as.numeric(rat_1),
                rat_3 = ifelse(country == "Rwanda" & year == "1998", 1947,
                               ifelse(country == "Rwanda" & year == "2002", 1246,
                                      ifelse(country == "Rwanda" & year == "2004", 941,
                                             ifelse(country == "Rwanda" & year == "2006", 837,
                                                    ifelse(country == "Rwanda" & year == "2008", 576,
                                                           rat_2))))) ) %>%
  dplyr::rename(total = tot_3, rate = rat_3) %>% 
  dplyr::select(country, year, total, rate) -> df_02b

## Recode Country
df_02b %>%
  dplyr::mutate(country = str_trim(str_replace_all(country, "\\(USA\\)|\\(UK\\)|\\(Netherlands\\)|
                                                    \\(NZ\\)|\\(France\\)|\\(Denmark\\)|\\(China\\)",
                                                   "")),
                country = car::recode(country, "'Anguilla (United Kingdom)'='Anguilla';'Bermuda (United Kingdom)'=
                                    'Bermuda';'Virgin Islands (United Kingdom)'='British Virgin Islands';
                                    'Cape Verde  (Cabo Verde)'='Cape Verde';'Cayman Islands  (United Kingdom)'=
                                    'Cayman Islands';'Cook Islands  (New Zealand)'='Cook Islands';
                                    'Congo (Republic of)'='Republic of Congo';'Curaçao'='Curacao';
                                    'Cyprus (Republic of)'='Cyprus';'Eswatini/Swaziland'='Eswatini';
                                    'Faeroe Islands'='Faroe Islands';'Micronesia, Federated States of'=
                                    'Federated States of Micronesia';'French Guiana/Guyane'='French Guiana';
                                    'Gibraltar  (United Kingdom)'='Gibraltar';'Guernsey  (United Kingdom)'=
                                    'Guernsey';'Guinea (Republic of)'='Republic of Guinea';
                                    'Ireland, Republic of'='Ireland';'Isle of Man  (United Kingdom)'='Isle of Man';
                                    'Jersey  (United Kingdom)'='Jersey';'Kosovo/Kosova'='Kosovo';'Moldova (Republic of)'=
                                    'Moldova';'Myanmar (formerly Burma)'='Myanmar';'Republic of (South) Korea'=
                                    'Republic of Korea';'Somalia (Federal Republic of)'='Somalia';
                                    'St. Kitts and Nevis'='St Kitts and Nevis';'St. Lucia'='St Lucia';
                                    'St. Vincent and the Grenadines'='St Vincent and the Grenadines';
                                    'Timor-Leste (formerly East Timor)'='Timor-Leste'
                                    
                                    ")
  ) -> df_02b

df_02b$country[108] <- 'Ivory Coast'
df_02b$country[109] <- 'Ivory Coast'
df_02b$country[110] <- 'Ivory Coast'
df_02b$country[111] <- 'Ivory Coast'
df_02b$country[112] <- 'Ivory Coast'
df_02b$country[113] <- 'Ivory Coast'
df_02b$country[114] <- 'Ivory Coast'
df_02b$country[115] <- 'Ivory Coast'
df_02b$country[116] <- 'Ivory Coast'
df_02b$country[117] <- 'Ivory Coast'
df_02b$country[118] <- 'Ivory Coast'
df_02b$country[1640] <- 'Bosnia and Herzegovina (Federation)'
df_02b$country[1641] <- 'Bosnia and Herzegovina (Federation)'
df_02b$country[1642] <- 'Bosnia and Herzegovina (Federation)'
df_02b$country[1643] <- 'Bosnia and Herzegovina (Federation)'
df_02b$country[1644] <- 'Bosnia and Herzegovina (Federation)'
df_02b$country[1645] <- 'Bosnia and Herzegovina (Federation)'
df_02b$country[1646] <- 'Bosnia and Herzegovina (Federation)'
df_02b$country[1647] <- 'Bosnia and Herzegovina (Federation)'
df_02b$country[1648] <- 'Bosnia and Herzegovina (Federation)'
df_02b$country[1649] <- 'Bosnia and Herzegovina (Federation)'
df_02b$country[1650] <- 'Bosnia and Herzegovina (Federation)'
df_02b$country[1651] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_02b$country[1652] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_02b$country[1653] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_02b$country[1654] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_02b$country[1655] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_02b$country[1656] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_02b$country[1657] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_02b$country[1658] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_02b$country[1659] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_02b$country[1660] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_02b$country[1661] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_02b$country[1662] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_02b$country[2462] <- 'UK: England and Wales'
df_02b$country[2463] <- 'UK: England and Wales'
df_02b$country[2464] <- 'UK: England and Wales'
df_02b$country[2465] <- 'UK: England and Wales'
df_02b$country[2466] <- 'UK: England and Wales'
df_02b$country[2467] <- 'UK: England and Wales'
df_02b$country[2468] <- 'UK: England and Wales'
df_02b$country[2469] <- 'UK: England and Wales'
df_02b$country[2470] <- 'UK: England and Wales'
df_02b$country[2471] <- 'UK: England and Wales'
df_02b$country[2472] <- 'UK: England and Wales'
df_02b$country[2473] <- 'UK: England and Wales'
df_02b$country[2474] <- 'UK: England and Wales'
df_02b$country[2475] <- 'UK: England and Wales'
df_02b$country[2476] <- 'UK: England and Wales'
df_02b$country[2477] <- 'UK: England and Wales'
df_02b$country[2478] <- 'UK: England and Wales'
df_02b$country[2479] <- 'UK: England and Wales'
df_02b$country[2480] <- 'UK: England and Wales'
df_02b$country[2481] <- 'UK: England and Wales'
df_02b$country[2482] <- 'UK: England and Wales'
df_02b$country[2483] <- 'UK: England and Wales'
df_02b$country[2484] <- 'UK: England and Wales'
df_02b$country[2485] <- 'UK: England and Wales'
df_02b$country[2486] <- 'UK: England and Wales'
df_02b$country[2487] <- 'UK: England and Wales'
df_02b$country[2488] <- 'UK: England and Wales'
df_02b$country[2489] <- 'UK: England and Wales'
df_02b$country[2490] <- 'UK: England and Wales'
df_02b$country[2491] <- 'UK: England and Wales'
df_02b$country[2492] <- 'UK: Northern Ireland'
df_02b$country[2493] <- 'UK: Northern Ireland'
df_02b$country[2494] <- 'UK: Northern Ireland'
df_02b$country[2495] <- 'UK: Northern Ireland'
df_02b$country[2496] <- 'UK: Northern Ireland'
df_02b$country[2497] <- 'UK: Northern Ireland'
df_02b$country[2498] <- 'UK: Northern Ireland'
df_02b$country[2499] <- 'UK: Northern Ireland'
df_02b$country[2500] <- 'UK: Northern Ireland'
df_02b$country[2501] <- 'UK: Northern Ireland'
df_02b$country[2502] <- 'UK: Northern Ireland'
df_02b$country[2503] <- 'UK: Northern Ireland'
df_02b$country[2504] <- 'UK: Northern Ireland'
df_02b$country[2505] <- 'UK: Northern Ireland'
df_02b$country[2506] <- 'UK: Northern Ireland'
df_02b$country[2507] <- 'UK: Northern Ireland'
df_02b$country[2508] <- 'UK: Northern Ireland'
df_02b$country[2509] <- 'UK: Northern Ireland'
df_02b$country[2510] <- 'UK: Northern Ireland'
df_02b$country[2511] <- 'UK: Scotland'
df_02b$country[2512] <- 'UK: Scotland'
df_02b$country[2513] <- 'UK: Scotland'
df_02b$country[2514] <- 'UK: Scotland'
df_02b$country[2515] <- 'UK: Scotland'
df_02b$country[2516] <- 'UK: Scotland'
df_02b$country[2517] <- 'UK: Scotland'
df_02b$country[2518] <- 'UK: Scotland'
df_02b$country[2519] <- 'UK: Scotland'
df_02b$country[2520] <- 'UK: Scotland'
df_02b$country[2521] <- 'UK: Scotland'
df_02b$country[2522] <- 'UK: Scotland'
df_02b$country[2523] <- 'UK: Scotland'
df_02b$country[2524] <- 'UK: Scotland'
df_02b$country[2525] <- 'UK: Scotland'
df_02b$country[2526] <- 'UK: Scotland'
df_02b$country[2527] <- 'UK: Scotland'
df_02b$country[2528] <- 'UK: Scotland'
df_02b$country[2529] <- 'UK: Scotland'
df_02b$country[2530] <- 'UK: Scotland'
df_02b$country[2531] <- 'UK: Scotland'
df_02b$country[2532] <- 'UK: Scotland'
df_02b$country[2533] <- 'UK: Scotland'
df_02b$country[2534] <- 'UK: Scotland'
df_02b$country[2535] <- 'UK: Scotland'
df_02b$country[2536] <- 'UK: Scotland'
df_02b$country[2537] <- 'UK: Scotland'
df_02b$country[2538] <- 'UK: Scotland'
df_02b$country[2539] <- 'UK: Scotland'
df_02b$country[2540] <- 'UK: Scotland'

## Recode Year
df_02b$year <- car::recode(df_02b$year, "'1901/02'='1902';'1910/11'='1911';'1920/21'='1921';'1921/22'='1922';
                           '1929-30'='1930';'1929/30'='1930';'1930/31'='1931';'1935/36'='1936';'1940/41'='1941';
                           '1945/46'='1946';'1951-55'='1953';'1956-60'='1958';'1961-65'='1963';'1966-70'='1968';
                           '1971-75'='1973';'1982-83'='1983';'1986-87'='1987';'1988/89'='1989';'1990-91'='1991';
                           '1990/91'='1991';'1993/94'='1994';'1995-96'='1996';'1996/97'='1997';'1999/2000'='2000';
                           '2000-01'='2001'")

df_02b %>%
  dplyr::group_by(country, year) %>%
  dplyr::summarise(prison_H = mean(total),
                   rate_H   = mean(rate),
                   n        = n()) -> df_02b1

## Remove objects
rm(list_01b, list_01b1, list_02b, list_02b1, df_02a, df_02a1, df_02a2, i, extractor_02)


# 3) PRE-TRIAL TRENDS

## Create function for all the information
extractor_03 <- purrr::safely(function(link){
  
  ### Verbose
  message("Getting page ",i)
  Sys.sleep(7)
  
  ### Create the new link
  wpblink <- read_html(x = paste0("https://www.prisonstudies.org",link))
  
  ### Extract tables
  wpblink %>% html_nodes("table") -> subset_nodes
  
  ### Combine Available information
  bind_cols(
    #### 1) Variable Name
    subset_nodes[1] %>% html_table() %>% as.data.frame() %>%
      dplyr::select(X2) %>% dplyr::rename(country = X2),
    #### 2) Prison Trends
    bind_rows(
      subset_nodes[7] %>% html_table() %>% as.data.frame() %>%
        dplyr::slice(2) %>%
        dplyr::mutate(across(everything(), ~str_replace_all(., "\r\n", "@") ) ) %>%
        tidyr::separate_longer_delim(cols = X1:X4, delim = "@"), 
      subset_nodes[7] %>% html_table() %>% as.data.frame() %>%
        dplyr::slice(3) 
    ) ) -> out_df
  ### Define output
  return(out_df)
})

## Create list to store results
list_01c  <- list()
list_01c1 <- list()
list_02c  <- list()
list_02c1 <- list()

## Run loop for iterations
for(i in 1:226){
  list_01c[[i]] <- extractor_03(link = list_00[i])
}
list_01c1[[1]]    <- extractor_03(link = list_00[12])
#list_01c1[[2]]    <- extractor_03(link = list_00[17])
#list_01c1[[3]]    <- extractor_03(link = list_00[18])
#list_01c1[[4]]    <- extractor_03(link = list_00[25])
#list_01c1[[5]]    <- extractor_03(link = list_00[47])
#list_01c1[[6]]    <- extractor_03(link = list_00[63])
list_01c1[[7]]    <- extractor_03(link = list_00[78])
#list_01c1[[8]]    <- extractor_03(link = list_00[87])
list_01c1[[9]]    <- extractor_03(link = list_00[90])
list_01c1[[10]]   <- extractor_03(link = list_00[94])
#list_01c1[[11]]   <- extractor_03(link = list_00[95])
list_01c1[[12]]   <- extractor_03(link = list_00[99])
list_01c1[[13]]   <- extractor_03(link = list_00[101])
#list_01c1[[14]]   <- extractor_03(link = list_00[140])
list_01c1[[15]]   <- extractor_03(link = list_00[142])
list_01c1[[16]]   <- extractor_03(link = list_00[156])
#list_01c1[[17]]   <- extractor_03(link = list_00[165])
list_01c1[[18]]   <- extractor_03(link = list_00[188])
list_01c1[[19]]   <- extractor_03(link = list_00[190])
list_01c1[[20]]   <- extractor_03(link = list_00[192])
list_01c1[[21]]   <- extractor_03(link = list_00[199])
list_01c1[[22]]   <- extractor_03(link = list_00[201])
list_01c1[[23]]   <- extractor_03(link = list_00[203])
#list_01c1[[24]]   <- extractor_03(link = list_00[205])

## Save the data
write_rds(x = list_01c,  file = "2_Data/2_Processed_Data/2_World_Prison_Brief/World_Prison_Brief_Webpage_Pretrial_Trends_01.rds")
write_rds(x = list_01c1, file = "2_Data/2_Processed_Data/2_World_Prison_Brief/World_Prison_Brief_Webpage_Pretrial_Trends_01a.rds")

## Transform into character strings
for(i in 1:226){
  list_01c[[i]]$result %>% as.data.frame() %>%
    dplyr::mutate(across(everything(), as.character)) %>%
    as.data.frame() -> list_02c[[i]]
}
for(i in 1:24){
  list_01c1[[i]]$result %>% as.data.frame() %>%
    dplyr::mutate(across(everything(), as.character)) %>%
    as.data.frame() -> list_02c1[[i]]
}

## Combine all 'result' data frames into one
df_03a  <- do.call(bind_rows, list_02c)
df_03a1 <- do.call(bind_rows, list_02c1)

df_03a2 <- dplyr::bind_rows(df_03a, df_03a1)
# 216 Countries OK -> 10 No Info

df_03a3 <- dplyr::bind_rows(df_03a2,
                            data.frame(country =  rep("Namibia",3), 
                                       X1      = c("2013", "2015", "2021"),
                                       X2      = c("3720", "3910", "4804"),
                                       X3      = c("49.4%","54.0%", "54.0%"),
                                       X4      = c("158", "161", "172")) )

df_03a3 %>% dplyr::filter(X1 != "2013\r\n2015\r\n2021/22") -> df_03a4


## Cleaning the variables
df_03a4 %>%
  dplyr::rename(year   = X1,
                tot_0  = X2,
                perc_0 = X3,
                rat_0  = X4 ) %>% 
  dplyr::mutate(tot_1 = as.character(str_squish(str_replace_all(tot_0, ",|c|\\.|\\+|\\*", ""))),
                tot_2 = as.numeric(tot_1),
                tot_3 = ifelse(country == "Turkey" & year == "2000", 26297,
                               ifelse(country == "Turkey" & year == "2005", 25910,
                                      ifelse(country == "Turkey" & year == "2010", 34827,
                                             ifelse(country == "Turkey" & year == "2012", 32470,
                                                    ifelse(country == "Turkey" & year == "2015", 25590,
                                                           ifelse(country == "Turkey" & year == "2020", 41890,
                                                                  ifelse(country == "Turkey" & year == "2024",
                                                                         56512, tot_2))))))),
                rat_1 = as.character(str_squish(str_replace_all(rat_0, ",|c|\\.|\\+|\\*", ""))),
                rat_2 = as.numeric(rat_1),
                rat_3 = ifelse(country == "Turkey" & year == "2000", 39,
                               ifelse(country == "Turkey" & year == "2005", 36,
                                      ifelse(country == "Turkey" & year == "2010", 47,
                                             ifelse(country == "Turkey" & year == "2012", 32,
                                                    ifelse(country == "Turkey" & year == "2015", 32,
                                                           ifelse(country == "Turkey" & year == "2020", 50,
                                                                  ifelse(country == "Turkey" & year == "2024",
                                                                         66, rat_2))))))),
                perc_1 = as.character(str_squish(str_replace_all(perc_0, ",|c|\\.|\\+|\\*|%", ""))),
                perc_2 = as.numeric(perc_1),
                perc_3 = ifelse(country == "Turkey" & year == "2000", 36.6,
                               ifelse(country == "Turkey" & year == "2005", 47.7,
                                      ifelse(country == "Turkey" & year == "2010", 28.9,
                                             ifelse(country == "Turkey" & year == "2012", 14.4,
                                                    ifelse(country == "Turkey" & year == "2015", 14.4,
                                                           ifelse(country == "Turkey" & year == "2020", 15.7,
                                                                  ifelse(country == "Turkey" & year == "2024",
                                                                         14.9, perc_2))))))),
                
                ) %>% 
  dplyr::rename(total = tot_3, rate = rat_3, percentage = perc_3) %>% 
  dplyr::select(country, year, total, percentage, rate) -> df_03b

## Recode Country
df_03b %>%
  dplyr::mutate(country = str_trim(str_replace_all(country, "\\(USA\\)|\\(UK\\)|\\(Netherlands\\)|
                                                    \\(NZ\\)|\\(France\\)|\\(Denmark\\)|\\(China\\)",
                                                   "")),
                country = car::recode(country, "'Anguilla (United Kingdom)'='Anguilla';'Bermuda (United Kingdom)'=
                                    'Bermuda';'Virgin Islands (United Kingdom)'='British Virgin Islands';
                                    'Cape Verde  (Cabo Verde)'='Cape Verde';'Cayman Islands  (United Kingdom)'=
                                    'Cayman Islands';'Cook Islands  (New Zealand)'='Cook Islands';
                                    'Congo (Republic of)'='Republic of Congo';'Curaçao'='Curacao';
                                    'Cyprus (Republic of)'='Cyprus';'Eswatini/Swaziland'='Eswatini';
                                    'Faeroe Islands'='Faroe Islands';'Micronesia, Federated States of'=
                                    'Federated States of Micronesia';'French Guiana/Guyane'='French Guiana';
                                    'Gibraltar  (United Kingdom)'='Gibraltar';'Guernsey  (United Kingdom)'=
                                    'Guernsey';'Guinea (Republic of)'='Republic of Guinea';
                                    'Ireland, Republic of'='Ireland';'Isle of Man  (United Kingdom)'='Isle of Man';
                                    'Jersey  (United Kingdom)'='Jersey';'Kosovo/Kosova'='Kosovo';'Moldova (Republic of)'=
                                    'Moldova';'Myanmar (formerly Burma)'='Myanmar';'Republic of (South) Korea'=
                                    'Republic of Korea';'Somalia (Federal Republic of)'='Somalia';
                                    'St. Kitts and Nevis'='St Kitts and Nevis';'St. Lucia'='St Lucia';
                                    'St. Vincent and the Grenadines'='St Vincent and the Grenadines';
                                    'Timor-Leste (formerly East Timor)'='Timor-Leste'
                                    
                                    ")
  ) -> df_03b

df_03b$country[48] <- 'Ivory Coast'
df_03b$country[49] <- 'Ivory Coast'
df_03b$country[50] <- 'Ivory Coast'
df_03b$country[51] <- 'Ivory Coast'
df_03b$country[52] <- 'Ivory Coast'
df_03b$country[536] <- 'Bosnia and Herzegovina (Federation)'
df_03b$country[537] <- 'Bosnia and Herzegovina (Federation)'
df_03b$country[538] <- 'Bosnia and Herzegovina (Federation)'
df_03b$country[539] <- 'Bosnia and Herzegovina (Federation)'
df_03b$country[540] <- 'Bosnia and Herzegovina (Federation)'
df_03b$country[541] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_03b$country[542] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_03b$country[543] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_03b$country[544] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_03b$country[545] <- 'Bosnia and Herzegovina (Republika Srpska)'
df_03b$country[546] <- 'Bosnia and Herzegovina (Republika Srpska)'

df_03b$country[789] <- 'UK: England and Wales'
df_03b$country[790] <- 'UK: England and Wales'
df_03b$country[791] <- 'UK: England and Wales'
df_03b$country[792] <- 'UK: England and Wales'
df_03b$country[793] <- 'UK: England and Wales'
df_03b$country[794] <- 'UK: England and Wales'
df_03b$country[795] <- 'UK: Northern Ireland'
df_03b$country[796] <- 'UK: Northern Ireland'
df_03b$country[797] <- 'UK: Northern Ireland'
df_03b$country[798] <- 'UK: Northern Ireland'
df_03b$country[799] <- 'UK: Northern Ireland'
df_03b$country[800] <- 'UK: Northern Ireland'
df_03b$country[801] <- 'UK: Scotland'
df_03b$country[802] <- 'UK: Scotland'
df_03b$country[803] <- 'UK: Scotland'
df_03b$country[804] <- 'UK: Scotland'
df_03b$country[805] <- 'UK: Scotland'
df_03b$country[806] <- 'UK: Scotland'

## Recode Year
df_03b$year <- car::recode(df_03b$year, "'1999/2000'='2000';'2000/01'='2001';'2004/05'='2005';'2005/06'='2006';
                           '2009/10'='2010';'2010/11'='2011';'2011/12'='2012';'2015*'='2015';'2015/16'='2016';
                           '2020*'='2020';'2020/21'='2021';'2022/23'='2023';'2023*'='2023';'2023/24'='2024'")

## Repair cases
vct <- c("Central African Republic", "Chad", "Comoros", "Republic of Congo", "Djibouti", "Mali", "Namibia",
         "South Sudan", "Bhutan", "China", "Laos", "Maldives", "Tajikistan", "Turkmenistan", "Curacao", 
         "Sint Maarten", "British Virgin Islands", "Virgin Islands", "Bahrain", "Iraq", "Oman", "Saudi Arabia",
         "Syria", "United Arab Emirates", "Bermuda", "American Samoa", "Cook Islands", "Kiribati", "Marshall Islands",
         "Federated States of Micronesia", "Palau", "Tuvalu")
df_03b %>%
  dplyr::mutate(total_PTD      = ifelse(country %in% vct, NA, total),
                percentage_PTD = ifelse(country %in% vct, NA, percentage),
                rate_PTD       = ifelse(country %in% vct, NA, rate)) -> df_03b1

## Remove objects
rm(list_01c, list_01c1, list_02c, list_02c1, df_03a, df_03a1, df_03a2, df_03a3, df_03a4, i, extractor_03)
rm(list_00, wpb_00)


######################################################     STEP 06: DATA STORAGE     ######################################################

## Save dataset  
write_csv(x = wppl_01i, file = "2_Data/2_Processed_Data/2_World_Prison_Brief/WPB_Prison_Comparative_Dataset_01.csv", na = "NA")
write_csv(x = wppl_02i, file = "2_Data/2_Processed_Data/2_World_Prison_Brief/WPB_Prison_Comparative_Dataset_02.csv", na = "NA")
write_csv(x = wpre_01i, file = "2_Data/2_Processed_Data/2_World_Prison_Brief/WPB_Pretrial_Comparative_Dataset.csv", na = "NA")
write_csv(x = df_01c,   file = "2_Data/2_Processed_Data/2_World_Prison_Brief/WPB_Web_Contemporaneous.csv", na = "NA")
write_csv(x = df_02b1,  file = "2_Data/2_Processed_Data/2_World_Prison_Brief/WPB_Web_Trends.csv", na = "NA")
write_csv(x = df_03b1,  file = "2_Data/2_Processed_Data/2_World_Prison_Brief/WPB_Web_Pretrial_Trends.csv", na = "NA")


#####################################################     STEP 07:FINAL STRUCTURE     #####################################################

## Creaty empty data.frame
empty_df <- expand.grid(country = unique(c(levels(as.factor(df_01c$country)),   levels(as.factor(df_02b1$country)), 
                                           levels(as.factor(df_03b1$country)),  levels(as.factor(wppl_01i$country)), 
                                           levels(as.factor(wppl_02i$country)), levels(as.factor(wpre_01i$country)) )),
                        year    = unique(c(levels(as.factor(df_02b1$year)),     levels(as.factor(df_03b1$year)),
                                           levels(as.factor(wppl_01i$year)),    levels(as.factor(wppl_02i$year)),
                                           levels(as.factor(wpre_01i$year))))
)

## Rename variables
wppl_01i %>% dplyr::rename(prison_14  = prison,   population_14 = population, rate_14 = rate) -> wppl_01i
wppl_02i %>% dplyr::rename(prison_13  = prison,   population_13 = population, rate_13 = rate) -> wppl_02i
wpre_01i %>% dplyr::rename(pretrial_4 = pretrial, percentage_4  = percentage, population_4 = population, rate_4 = rate) -> wpre_01i
#df_02b   %>% dplyr::rename(prison_H  = total, rate_H = rate) -> df_02b
#df_03b   %>% dplyr::rename(total_PTD  = total, percentage_PTD = percentage, rate_PTD = rate) -> df_03b

## Recode and select relevant variables
wppl_01i %>% dplyr::mutate(year = as.factor(year)) %>% dplyr::select(country, year, subregion, region) -> temp_01
temp_01 %>%
  group_by(country) %>%
  summarise(subregion = first(subregion),
            region    = first(region)) -> temp_01

wppl_01i %>% dplyr::mutate(year = as.factor(year)) %>% dplyr::select(-subregion, -region) -> wppl_01i
wppl_02i %>% dplyr::mutate(year = as.factor(year)) %>% dplyr::select(-subregion, -region) -> wppl_02i
wpre_01i %>% dplyr::mutate(year = as.factor(year)) %>% dplyr::select(-subregion, -region) -> wpre_01i
df_02b1  %>% dplyr::mutate(year = as.factor(year)) %>% dplyr::select(-n) -> df_02b1
df_03b1  %>% dplyr::mutate(year = as.factor(year)) %>% dplyr::select(-total,-percentage,-rate) -> df_03b1

 ## Merge data
dplyr::full_join(x = empty_df, y = temp_01, by = c("country")) %>% 
  dplyr::mutate(subregion = ifelse(country == "Democratic People's Republic of (North) Korea",
                                   "Eastern Asia",
                                   ifelse(country == "Eritrea", "Eastern Africa",
                                          ifelse(country == "Madagascar", "Eastern Africa",
                                                 ifelse(country == "Somalia", "Eastern Africa",
                                                        subregion)))),
                region = ifelse(country == "Democratic People's Republic of (North) Korea",
                                "Asia",
                                ifelse(country == "Eritrea", "Africa",
                                       ifelse(country == "Madagascar", "Africa",
                                              ifelse(country == "Somalia", "Africa",
                                                     region)))) ) %>% 
  dplyr::full_join(x = ., y = wppl_01i, by = c("country", "year")) %>%
  dplyr::full_join(x = ., y = wppl_02i, by = c("country", "year")) %>%
  dplyr::full_join(x = ., y = wpre_01i, by = c("country", "year")) %>% 
  dplyr::full_join(x = ., y = df_02b1,  by = c("country", "year")) %>%
  dplyr::full_join(x = ., y = df_03b1,  by = c("country", "year")) -> final_df

## Save dataset  
write_csv(x = final_df, file = "2_Data/2_Processed_Data/2_World_Prison_Brief/World_Prison_Brief.csv", na = "NA")


################################################     TECHNICAL DETAILS FOR REPLICATION     ################################################

#Macbook Air M2 2022
#macOS Sequoia 15.2
#
#A .Rproj file was used in the development of this Code.
#
#sessionInfo() 
#R version 4.4.2 (2024-10-31)
#Platform: aarch64-apple-darwin20
#Running under: macOS Sequoia 15.2
#
#Matrix products: default
#BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
#LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
#
#locale:
#[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
#time zone: America/Santiago
#tzcode source: internal
#
#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     
#
#other attached packages:
#[1] rvest_1.0.4     pdftools_3.4.0  lubridate_1.9.3 forcats_1.0.0   stringr_1.5.1   dplyr_1.1.4     purrr_1.0.2     readr_2.1.5    
#[9] tidyr_1.3.1     tibble_3.2.1    ggplot2_3.5.1   tidyverse_2.0.0 readxl_1.4.3   
#
#loaded via a namespace (and not attached):
#[1] utf8_1.2.4        generics_0.1.3    xml2_1.3.6        stringi_1.8.4     hms_1.1.3         magrittr_2.0.3    grid_4.4.2       
#[8] timechange_0.3.0  pkgload_1.3.4     fastmap_1.2.0     jsonlite_1.8.8    cellranger_1.1.0  processx_3.8.4    chromote_0.2.0   
#[15] ps_1.7.7          promises_1.3.0    httr_1.4.7        selectr_0.4-2     fansi_1.0.6       scales_1.3.0      abind_1.4-5      
#[22] cli_3.6.3         rlang_1.1.4       crayon_1.5.3      bit64_4.0.5       munsell_0.5.1     withr_3.0.1       tools_4.4.2      
#[29] parallel_4.4.2    tzdb_0.4.0        colorspace_2.1-0  curl_5.2.1        vctrs_0.6.5       R6_2.5.1          lifecycle_1.0.4  
#[36] car_3.1-2         bit_4.0.5         vroom_1.6.5       pkgconfig_2.0.3   later_1.3.2       pillar_1.9.0      gtable_0.3.5     
#[43] glue_1.8.0        Rcpp_1.0.13-1     tidyselect_1.2.1  rstudioapi_0.16.0 websocket_1.4.2   carData_3.0-5     qpdf_1.3.3       
#[50] compiler_4.4.2    askpass_1.2.0