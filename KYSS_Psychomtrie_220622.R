###########################################
# Psychometrie KYSS (APvaardig) Juni 2022 #
###########################################

############
# 1. setup #
############

# 1.1 Packages installeren
install.packages("WriteXLS")
install.packages("readxl")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lavaan")
install.packages("semTools")
install.packages ("semTable")

install.packages("psych")
install.packages ("car")
install.packages ("fields")
install.packages ("gmodels")
install.packages ("rela")
install.packages ("NbClust")
 install.packages("corrplot")
 install.packages("ggcorrplot")
 install.packages("moments")
 install.packages("bookdown")

###Rapportage
install.packages ("tidyr")
install.packages ("data.table")
install.packages ("broom")
install.packages ("kableExtra")
install.packages ("flextable")
install.packages ("rmarkdown")
install.packages('tinytex')


  webshot::install_phantomjs()

# 1.2 Packages inladen
## 1.2.1 core

### Laden en opslaan
library ("WriteXLS")
library(readxl)

### Tidyverse
library ("tidyverse")
library ("ggplot2")
library("dplyr")

### Factoranalyses
library ("lavaan")
library ("semTools")


### Inhoudelijke analyses
library ("psych")
library ("car")
library ("fields")
library ("gmodels")
library ("rela")
library ("NbClust")
 library("corrplot")
 library("ggcorrplot")
 library ("moments")


###Rapportage
library ("tidyr")
library ("data.table")
library ("broom")
library(kableExtra)
library(flextable)
library("bookdown")
  library(tinytex)

install.packages("devtools")
library(devtools)
devtools::install_github("dr-JT/semoutput")
library("semTable")

# 1.2 Data inlezen

# Thuiscomputer

APVaardig_clean <- read_excel("APVaardig_clean_start.xlsx")
View(APVaardig_clean)

KYSS_Goleweb_goed <- read_excel("KYSS_Goleweb_goed.xlsx")
Goleweb_clean <-KYSS_Goleweb_goed

Goleweb_KYSS <- test2

#AP
setwd("C:/Users/p087468/Documents/AP/KYSS")
Goleweb_KYSS <- read.delim("test2.txt")


# 2. Data management

Data_KYSS_Start <- APVaardig_clean %>%
  dplyr::select(Deelgenomen_stpn:Index, K_Plan01:K_Flex06) %>%
  dplyr::filter(!is.na(K_Plan01))


Goleweb_KYSS <-Goleweb_KYSS %>%
  dplyr::filter(Geslacht == 'm' | Geslacht == 'v') %>%
  dplyr::filter(Instelling == 'AP Hogeschool' | Instelling == 'HOGENT' | Instelling == 'Hogeschool PXL - KYSS' | Instelling == 'Odisee' | Instelling == 'Thomas More') %>%
  dplyr::mutate(Geslacht = replace(Geslacht, Geslacht == 'm', 'Male')) %>%
  dplyr::mutate(Geslacht = replace(Geslacht, Geslacht == 'v', 'Female')) %>%
  dplyr::rename (Gender = 'Geslacht', Institution = 'Instelling')



# 3. CFA & Betrouwbaarheid

# 3.1 Creativiteit

## Model
Model_Cre<-'
Creativiteit=~K_Crea01+
K_Crea02+
K_Crea03+
K_Crea04+
K_Crea05+
K_Crea06'

## Fit APVaardig
Fit_Cre <- cfa(model = Model_Cre, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Cre, fit.measures = TRUE, standardized=TRUE)

## Betrouwbaarheid APVaardig
Cre_items <- dplyr::select(Data_KYSS_Start, K_Crea01,K_Crea02, K_Crea03, K_Crea04, K_Crea05, K_Crea06)
  psych::alpha(Cre_items) 

## Fit Goleweb  
Fit_Cre_G <- cfa(model = Model_Cre, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Cre_G, fit.measures = TRUE, standardized=TRUE) 

## Betrouwbaarheid Goleweb
  Cre_items_G <- dplyr::select(Goleweb_KYSS, K_Crea01,K_Crea02, K_Crea03, K_Crea04, K_Crea05, K_Crea06)
  psych::alpha(Cre_items_G)

  devtools::install_github("dr-JT/semoutput")
  library(semoutput)

sem_fitmeasures(Fit_Cre_G)

#3.2 Diversiteit

Model_Div<-'
Diversiteit=~K_Divers01+
K_Divers02+
K_Divers03+
K_Divers04+
K_Divers05+
K_Divers06
'
## Fit APVaardig
Fit_Div <- cfa(model = Model_Div, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Div, fit.measures = TRUE, standardized=TRUE)

## Betrouwbaarheid APVaardig
Div_items <- dplyr::select(Data_KYSS_Start, K_Divers01, K_Divers02, K_Divers03, K_Divers04, K_Divers05, K_Divers06)
  psych::alpha(Div_items) 

## Fit Goleweb
Fit_Div_G <- cfa(model = Model_Div, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Div_G, fit.measures = TRUE, standardized=TRUE)

test2 <-sem_fitmeasures(Fit_Div_G)

test3 <- bind_cols(test, test2)

modindices(Fit_Div_G)

## Fit goleweb 2
Model_Div_2<-'
Diversiteit=~K_Divers01+
K_Divers02+
K_Divers03+
K_Divers04+
K_Divers05+
K_Divers06
K_Divers02 ~~ K_Divers04'

Fit_Div_G2 <- cfa(model = Model_Div_2, data = Goleweb_KYSS, estimator="MLM")
summary(Fit_Div_G2, fit.measures = TRUE, standardized=TRUE)

## Betrouwbaarheid goleweb
Div_items_G <- dplyr::select(Goleweb_KYSS, K_Divers01, K_Divers02, K_Divers03, K_Divers04, K_Divers05, K_Divers06)
  rel_div <- psych::alpha(Div_items_G) 
  
  
colformat_double(flextable(rel_div$total), digits = 2)

names(rel_div$total)  
names(psych::alpha(Div_items_G))
    
#3.3 Flexibiliteit

Model_Fle<-'
Flexibiliteit=~K_Flex01+
K_Flex02+
K_Flex03+
K_Flex04+
K_Flex05+
K_Flex06
'

#Fit APVaardig
Fit_Fle <- cfa(model = Model_Fle, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Fle, fit.measures = TRUE, standardized=TRUE)

#Betrouwbaarheid APVaardig
Fle_items <- dplyr::select(Data_KYSS_Start, K_Flex01, K_Flex02, K_Flex03,  K_Flex04,  K_Flex05, K_Flex06)
  psych::alpha(Fle_items)

#Fit GOLEWEB
Fit_Fle_G <- cfa(model = Model_Fle, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Fle_G, fit.measures = TRUE, standardized=TRUE)

#Betrouwbaarheid GOLEWEB
Fle_items_G <- dplyr::select(Goleweb_KYSS, K_Flex01, K_Flex02, K_Flex03,  K_Flex04,  K_Flex05, K_Flex06)
  psych::alpha(Fle_items_G)
  
  
#3.4 Plannen en organiseren

Model_Pla<-'
Plannen=~K_Plan01+
K_Plan02+
K_Plan03+
K_Plan04+
K_Plan05+
K_Plan06+
K_Plan07
'

#Fit Apvaardig
Fit_Pla <- cfa(model = Model_Pla, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Pla, fit.measures = TRUE, standardized=TRUE)
 
modindices(Fit_Pla)

#Betrouwbaarheid APVaardig
Pla_items <- dplyr::select(Data_KYSS_Start, K_Plan01, K_Plan02, K_Plan03, K_Plan04, K_Plan05, K_Plan06, K_Plan07)
psych::alpha(Pla_items)

#Fit GOLEWEB
Fit_Pla_G <- cfa(model = Model_Pla, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Pla_G, fit.measures = TRUE, standardized=TRUE)

modindices(Fit_Pla)

#Betrouwbaarheid GOLEWEB
Pla_items_G <- dplyr::select(Goleweb_KYSS, K_Plan01, K_Plan02, K_Plan03, K_Plan04, K_Plan05, K_Plan06, K_Plan07)
psych::alpha(Pla_items_G)


# 3.4b Plannen en organiseren model 2 
Model_Pla_2<-'
Plannen=~K_Plan01+
K_Plan02+
K_Plan03+
K_Plan04+
K_Plan05+
K_Plan06+
K_Plan07
K_Plan04 ~~ K_Plan05'

Fit_Pla_2 <- cfa(model = Model_Pla_2, data = Data_KYSS_Start, estimator="MLM")
summary(Fit_Pla_2, fit.measures = TRUE, standardized=TRUE)


#3.5 Resultaatgerichtheid

Model_Res<-'
Resultaatgerichtheid=~K_Result01+
K_Result02+
K_Result03+
K_Result04+
K_Result05+
K_Result06+
K_Result07
'

# Fit APVaardig
Fit_Res <- cfa(model = Model_Res, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Res, fit.measures = TRUE, standardized=TRUE)

# Betrouwbaarheid APVaardig
Res_items <- dplyr::select(Data_KYSS_Start, K_Result01, K_Result02, K_Result03, K_Result04, K_Result05, K_Result06, K_Result07)
  psych::alpha(Res_items)

# Fit GOLEWEB
Fit_Res_G <- cfa(model = Model_Res, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Res_G, fit.measures = TRUE, standardized=TRUE)

# Betrouwbaarheid GOLEWEB
Res_items_G <- dplyr::select(Goleweb_KYSS, K_Result01, K_Result02, K_Result03, K_Result04, K_Result05, K_Result06, K_Result07)
  psych::alpha(Res_items_G)
  
#3.6 Samenwerken

Model_Sam<-'
Samenwerken=~K_Samen01+
K_Samen02+
K_Samen03+
K_Samen04+
K_Samen05+
K_Samen06
'

#Fit Apvaardig
Fit_Sam <- cfa(model = Model_Sam, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Sam, fit.measures = TRUE, standardized=TRUE)

#Betrouwbaarheid APVaardig
  Sam_items <- dplyr::select(Data_KYSS_Start, K_Samen01, K_Samen02, K_Samen03, K_Samen04, K_Samen05, K_Samen06)
  psych::alpha(Sam_items)

#Fit GOLEWEB
Fit_Sam_G <- cfa(model = Model_Sam, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Sam_G, fit.measures = TRUE, standardized=TRUE)

#Betrouwbaarheid GOLEWEB
  Sam_items_G <- dplyr::select(Goleweb_KYSS, K_Samen01, K_Samen02, K_Samen03, K_Samen04, K_Samen05, K_Samen06)
  psych::alpha(Sam_items_G)
  
#3.7 Communiceren

Model_Com<-'
Communicatie=~K_Comm01+
K_Comm02+
K_Comm03+
K_Comm04+
K_Comm05+
K_Comm06+
K_Comm07
'
# Fit APVaardig
Fit_Com <- cfa(model = Model_Com, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Com, fit.measures = TRUE, standardized=TRUE)

# Betrouwbaarheid APVaardig
Com_items <- dplyr::select(Data_KYSS_Start, K_Comm01, K_Comm02, K_Comm03, K_Comm04, K_Comm05, K_Comm06, K_Comm07)
  psych::alpha(Com_items)  

 # Fit GOLEWEB
Fit_Com <- cfa(model = Model_Com, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Com, fit.measures = TRUE, standardized=TRUE)
  
  measurementInvariance(model = Model_Com, data=Goleweb_KYSS, group="Institution", estimator="MLM")

# Betrouwbaarheid GOLEWEB
Com_items <- dplyr::select(Goleweb_KYSS, K_Comm01, K_Comm02, K_Comm03, K_Comm04, K_Comm05, K_Comm06, K_Comm07)
  psych::alpha(Com_items)

#3.8 Digitale vaardigheden
  
Model_Dig<-'
Digitale=~K_DigVa01+
K_DigVa02+
K_DigVa03+
K_DigVa04
'

# Fit APVaardig
Fit_Dig <- cfa(model = Model_Dig, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Dig, fit.measures = TRUE, standardized=TRUE)  

# Betrouwbaarheid APVaardig
Digit_items <- dplyr::select(Data_KYSS_Start, K_DigVa01, K_DigVa02, K_DigVa03, K_DigVa04)
  psych::alpha(Digit_items)

# Fit GOLEWEB
Fit_Dig_G <- cfa(model = Model_Dig, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Dig_G, fit.measures = TRUE, standardized=TRUE)

# Betrouwbaarheid GOLEWEB
Digit_items_G <- dplyr::select(Goleweb_KYSS, K_DigVa01, K_DigVa02, K_DigVa03, K_DigVa04)
  psych::alpha(Digit_items_G)


#3.9 Klantgerichtheid
  
Model_Kla<-'
Klantgerichtheid=~K_Klant01+
K_Klant02+
K_Klant03+
K_Klant04+
K_Klant05+
K_Klant06
'

 # Fit APVaardig
Fit_Kla <- cfa(model = Model_Kla, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Kla, fit.measures = TRUE, standardized=TRUE)

modindices(Fit_Kla)
  

# Fit APVaardig model 2
  
  Model_Kla_2<-'
Klantgerichtheid=~K_Klant01+
K_Klant02+
K_Klant03+
K_Klant04+
K_Klant05+
K_Klant06
K_Klant01 ~~ K_Klant02'
  
  Fit_Kla <- cfa(model = Model_Kla_2, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Kla, fit.measures = TRUE, standardized=TRUE)
  
#Betrouwbaarheid APVaardig
  Klant_items <- dplyr::select(Data_KYSS_Start, K_Klant01, K_Klant02, K_Klant03, K_Klant04, K_Klant05, K_Klant06)
  psych::alpha(Klant_items)
  
 # Fit GOLEWEB
Fit_Kla_G <- cfa(model = Model_Kla, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Kla_G, fit.measures = TRUE, standardized=TRUE)

 #Betrouwbaarheid GOLEWEB
  Klant_items_G <- dplyr::select(Goleweb_KYSS, K_Klant01, K_Klant02, K_Klant03, K_Klant04, K_Klant05, K_Klant06)
  psych::alpha(Klant_items_G)


#3.10 Zelfreflectie
  
Model_Ref<-'
Zelfreflectie=~K_Refl01+
K_Refl02+
K_Refl03+
K_Refl04+
K_Refl05+
K_Refl06
'

#Fit APVaardig
Fit_Ref <- cfa(model = Model_Ref, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Ref, fit.measures = TRUE, standardized=TRUE)

 modindices(Fit_Ref)
 
 
#Fit APVaardig model 02
  
  Model_Ref_02<-'
Zelfreflectie=~K_Refl01+
K_Refl02+
K_Refl03+
K_Refl04+
K_Refl05+
K_Refl06
  K_Refl01 ~~ K_Refl02'
  
  Fit_Ref_02 <- cfa(model = Model_Ref_02, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Ref_02, fit.measures = TRUE, standardized=TRUE)

  Ref_items <- dplyr::select(Data_KYSS_Start, K_Refl01, K_Refl02, K_Refl03, K_Refl04, K_Refl05, K_Refl106)
  psych::alpha(Ref_items)

#Fit GOLEWEB

    Model_Ref_G<-'
Zelfreflectie=~K_Refl01+
K_Refl02+
K_Refl03+
K_Refl04+
K_Refl05+
K_Relf06'

Fit_Ref_G <- cfa(model = Model_Ref_G, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Ref_G, fit.measures = TRUE, standardized=TRUE)

modindices(Fit_Ref_G)

# Fit GOLEWEB model 2
    Model_Ref_G2<-'
Zelfreflectie=~K_Refl01+
K_Refl02+
K_Refl03+
K_Refl04+
K_Refl05+
K_Relf06
K_Refl03~~K_Refl04'

Fit_Ref_G2 <- cfa(model = Model_Ref_G2, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Ref_G2, fit.measures = TRUE, standardized=TRUE)

#Betrouwbaarheid GOLEWEB
  Ref_items_G <- dplyr::select(Goleweb_KYSS, K_Refl01, K_Refl02, K_Refl03, K_Refl04, K_Refl05, K_Relf06)
  psych::alpha(Ref_items_G)


#3.11 Zelfstandigheid
  
Model_Zelf<-'
Zelfstandigheid=~K_Zelf01+
K_Zelf02+
K_Zelf03+
K_Zelf04+
K_Zelf05+
K_Zelf06
'

#Fit APVaardig
Fit_Zelf <- cfa(model = Model_Zelf, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Zelf, fit.measures = TRUE, standardized=TRUE)

 #Betrouwbaarheid APVaardig
  Zelf_items <- dplyr::select(Data_KYSS_Start, K_Zelf01, K_Zelf02, K_Zelf03, K_Zelf04, K_Zelf05, K_Zelf06)
  psych::alpha(Zelf_items)
  
 #Fit GOLEWEB
Fit_Zelf_G <- cfa(model = Model_Zelf, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Zelf_G, fit.measures = TRUE, standardized=TRUE)

  modindices(Fit_Zelf_G)

 #Fit GOLEWEB model 2
Model_Zelf_2<-'
Zelfstandigheid=~K_Zelf01+
K_Zelf02+
K_Zelf03+
K_Zelf04+
K_Zelf05+
K_Zelf06
K_Zelf02~~K_Zelf04'
 Fit_Zelf_G2 <- cfa(model = Model_Zelf_2, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Zelf_G2, fit.measures = TRUE, standardized=TRUE)

 #Betrouwbaarheid GOLEWEB
  Zelf_items_G <- dplyr::select(Goleweb_KYSS, K_Zelf01, K_Zelf02, K_Zelf03, K_Zelf04, K_Zelf05, K_Zelf06)
  psych::alpha(Zelf_items_G)
    
#3.12 Verantwoordelijkheid
  
Model_Ver<-'
Verantwoordelijkheid=~K_Vera01+
K_Vera02+
K_Vera03+
K_Vera04
'
#Fit APVaardig
Fit_Ver <- cfa(model = Model_Ver, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Ver, fit.measures = TRUE, standardized=TRUE)

#Betrouwbaarheid APVaardig
  Vera_items <- dplyr::select(Data_KYSS_Start, K_Vera01, K_Vera02, K_Vera03, K_Vera04)
  psych::alpha(Vera_items)  

#Fit GOLEWEB
Fit_Ver_G <- cfa(model = Model_Ver, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Ver_G, fit.measures = TRUE, standardized=TRUE)

#Betrouwbaarheid GOLEWEB
  Vera_items_G <- dplyr::select(Goleweb_KYSS, K_Vera01, K_Vera02, K_Vera03, K_Vera04)
  psych::alpha(Vera_items_G)


#3.13 Leerbereidheid
  
Model_Leer<-'
Leerbereidheid=~K_Leer01+
K_Leer02+
K_Leer03+
K_Leer04+
K_Leer05+
K_Leer06
'

#Fit APVaardig
Fit_Leer <- cfa(model = Model_Leer, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Leer, fit.measures = TRUE, standardized=TRUE)

#Betrouwbaarheid APVaardig
Leer_items <- dplyr::select(Data_KYSS_Start, K_Leer01, K_Leer02, K_Leer03, K_Leer04, K_Leer05, K_Leer06)
  psych::alpha(Leer_items) 

#Fit GOLEWEB
Fit_Leer_G <- cfa(model = Model_Leer, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Leer_G, fit.measures = TRUE, standardized=TRUE)

#Betrouwbaarheid GOLEWEB
Leer_items_G <- dplyr::select(Goleweb_KYSS, K_Leer01, K_Leer02, K_Leer03, K_Leer04, K_Leer05, K_Leer06)
  psych::alpha(Leer_items_G)

#3.14 Analyseren
  
Model_Ana<-'
Analyseren=~K_Anal01+
K_Anal02+
K_Anal03+
K_Anal04+
K_Anal05
'
#Fit APVaardig
Fit_Ana <- cfa(model = Model_Ana, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Ana, fit.measures = TRUE, standardized=TRUE)

# Betrouwbaarheid APVaardig
  Anal_items <- dplyr::select(Data_KYSS_Start, K_Anal01, K_Anal02, K_Anal03, K_Anal04, K_Anal05)
  psych::alpha(Anal_items) 
  
#Fit GOLEWEB
Fit_Ana_G <- cfa(model = Model_Ana, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Ana_G, fit.measures = TRUE, standardized=TRUE)

# Betrouwbaarheid GOLEWEB
  Anal_items_G <- dplyr::select(Goleweb_KYSS, K_Anal01, K_Anal02, K_Anal03, K_Anal04, K_Anal05)
  psych::alpha(Anal_items_G)

  
#3.15 Kritisch denken
  
Model_Kri<-'
Kritischdenken=~K_Kriti01+
K_Kriti02+
K_Kriti03+
K_Kriti04+
K_Kriti05
'
#Fit APVaardig
Fit_Kri <- cfa(model = Model_Kri, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Kri, fit.measures = TRUE, standardized=TRUE)

# Betrouwbaarheid APVaardig
Kriti_items <- dplyr::select(Data_KYSS_Start, K_Kriti01, K_Kriti02, K_Kriti03, K_Kriti04, K_Kriti05)
  psych::alpha(Kriti_items) 
 
#Fit GOLEWEB
Fit_Kri_G <- cfa(model = Model_Kri, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Kri_G, fit.measures = TRUE, standardized=TRUE)

# Betrouwbaarheid GOLEWEB
Kriti_items_G <- dplyr::select(Goleweb_KYSS, K_Kriti01, K_Kriti02, K_Kriti03, K_Kriti04, K_Kriti05)
  psych::alpha(Kriti_items_G)

#3.16 Inleving
  
Model_Inl<-'
Inleving=~K_Inlev01+
K_Inlev02+
K_Inlev03+
K_Inlev04+
K_Inlev05+
K_Inlev06
'

#Fit APVaardig
Fit_Inl <- cfa(model = Model_Inl, data = Data_KYSS_Start, estimator="MLM")
  summary(Fit_Inl, fit.measures = TRUE, standardized=TRUE)
 
# Betrouwbaarheid APVaardig
Inl_items <- dplyr::select(Data_KYSS_Start, K_Inlev01, K_Inlev02, K_Inlev03, K_Inlev04, K_Inlev05, K_Inlev06)
psych::alpha(Inl_items)

#Fit GOLEWEB
Fit_Inl_G <- cfa(model = Model_Inl, data = Goleweb_KYSS, estimator="MLM")
  summary(Fit_Inl_G, fit.measures = TRUE, standardized=TRUE)

# Betrouwbaarheid GOLEWEB
Inl_items_G <- dplyr::select(Goleweb_KYSS, K_Inlev01, K_Inlev02, K_Inlev03, K_Inlev04, K_Inlev05, K_Inlev06)
psych::alpha(Inl_items_G)



  

  
  
#4. Measurement invariance

#4.1 MI voor geslacht

# Verdeling geslacht analyseren
Frequentie_geslacht <- Goleweb_KYSS %>%
group_by (geslacht.)%>%
count()

flextable(Frequentie_geslacht)

rename(Goleweb_KYSS, Geslacht = geslacht.)

Goleweb_KYSS_mi <- Goleweb_KYSS %>%
 dplyr::filter(Gender == 'Male' | Gender == 'Female')

Frequentie_geslacht_2 <- Goleweb_KYSS_mi %>%
group_by (Geslacht)%>%
count()

flextable(Frequentie_geslacht_2)

##4.1.2. Creativiteit

# configural invariance
Crea_fit1 <- cfa(Model_Cre, data = Goleweb_KYSS_mi, group = "Geslacht", estimator="MLM")

# weak invariance
Crea_fit2 <- cfa(Model_Cre, data = Goleweb_KYSS_mi, group = "Geslacht",
            group.equal = "loadings", estimator="MLM")

# strong invariance
Crea_fit3 <- cfa(Model_Cre, data = Goleweb_KYSS_mi, group = "Geslacht",
            group.equal = c("intercepts", "loadings"), estimator="MLM")

# model comparison tests
lavTestLRT(Crea_fit1, Crea_fit2, Crea_fit3)
MI_creativiteit<-measurementInvariance(model = Model_Cre, data=Goleweb_KYSS, group="Gender", estimator="MLM")

names(MI_creativiteit)


##4.1.3 Diversiteit
# configural invariance
Divers_fit1_mi <- cfa(Model_Div_2, data = Goleweb_KYSS_mi, group = "Geslacht", estimator="MLM")

# weak invariance
Divers_fit2_mi <- cfa(Model_Div_2, data = Goleweb_KYSS_mi, group = "Geslacht",
            group.equal = "loadings", estimator="MLM")

# strong invariance
Divers_fit3_mi <- cfa(Model_Div_2, data = Goleweb_KYSS_mi, group = "Geslacht",
            group.equal = c("intercepts", "loadings"), estimator="MLM")

# model comparison tests
lavTestLRT(Divers_fit1_mi, Divers_fit2_mi, Divers_fit3_mi)
MI_diversiteit<-measurementInvariance(model = Model_Div_2, data=Goleweb_KYSS_mi, group="Geslacht", estimator="MLM")

##4.1.4 Flexibiliteit
MI_Flexibiliteit<-measurementInvariance(model = Model_Fle, data=Goleweb_KYSS_mi, group="Geslacht", estimator="MLM")

##4.1.5 Plannen en organiseren
MI_Plannen<-measurementInvariance(model = Model_Pla, data=Goleweb_KYSS_mi, group="Geslacht", estimator="MLM")

##4.1.6 Resultaatsgerichtheid
MI_Resultaatsgerichtheid<-measurementInvariance(model = Model_Res, data=Goleweb_KYSS_mi, group="Geslacht", estimator="MLM")

##4.1.7 Samenwerken
MI_Samenwerken<-measurementInvariance(model = Model_Sam, data=Goleweb_KYSS_mi, group="Geslacht", estimator="MLM")

##4.1.8 Communicatie
MI_Communicatie<-measurementInvariance(model = Model_Com, data=Goleweb_KYSS_mi, group="Geslacht", estimator="MLM")

##4.1.9 Digitale vaardigheden
MI_Digitale_vaardigheden<-measurementInvariance(model = Model_Dig, data=Goleweb_KYSS_mi, group="Geslacht", estimator="MLM")

##4.1.10 Klantgerichtheid
MI_Klantgerichtheid<-measurementInvariance(model = Model_Kla, data=Goleweb_KYSS_mi, group="Geslacht", estimator="MLM")

##4.1.11 Zelfreflectie
MI_Zelfreflectie<-measurementInvariance(model = Model_Ref_G2, data=Goleweb_KYSS_mi, group="Geslacht", estimator="MLM")

##4.1.12 Zelfstandigheid
MI_Zelfstandigheid<-measurementInvariance(model = Model_Zelf_2, data=Goleweb_KYSS_mi, group="Geslacht", estimator="MLM")

##4.1.13 Verantwoordelijkheid
MI_Verantwoordelijkheid<-measurementInvariance(model = Model_Ver, data=Goleweb_KYSS_mi, group="Geslacht", estimator="MLM")

##4.1.14 Leerbereidheid
MI_Leerbereidheid<-measurementInvariance(model = Model_Leer, data=Goleweb_KYSS_mi, group="Geslacht", estimator="MLM")

##4.1.15 Analyseren
MI_Analyseren<-measurementInvariance(model = Model_Ana, data=Goleweb_KYSS_mi, group="Geslacht", estimator="MLM")

##4.1.16 Kritisch denken
MI_Kritisch_denken<-measurementInvariance(model = Model_Kri, data=Goleweb_KYSS_mi, group="Geslacht", estimator="MLM")

##4.1.17 Inlevingsvermogen
MI_Inlevingsvermogen<-measurementInvariance(model = Model_Inl, data=Goleweb_KYSS_mi, group="Geslacht", estimator="MLM")

#5. Schalen berekenen
Goleweb_schalen<-Goleweb_KYSS_mi%>%
  group_by(Gebruikersnaam)%>%
  mutate(
    Creativiteit = round(mean (c(K_Crea01,K_Crea02, K_Crea03, K_Crea04, K_Crea05, K_Crea06), na.rm = TRUE),2),
    Diversiteit = round(mean (c(K_Divers01, K_Divers02, K_Divers03, K_Divers04, K_Divers05, K_Divers06),na.rm = TRUE),2),
    Flexibiliteit = round(mean (c(K_Flex01, K_Flex02, K_Flex03,  K_Flex04,  K_Flex05, K_Flex06),na.rm = TRUE),2),
    Plannen_organisatie = round(mean (c(K_Plan01, K_Plan02, K_Plan03, K_Plan04, K_Plan05, K_Plan06, K_Plan07),na.rm = TRUE),2),
    Resultaatsgerichtheid = round(mean (c(K_Result01, K_Result02, K_Result03, K_Result04, K_Result05, K_Result06, K_Result07),na.rm = TRUE),2),
    Samenwerken = round(mean (c(K_Samen01, K_Samen02, K_Samen03, K_Samen04, K_Samen05, K_Samen06),na.rm = TRUE),2),
    Communicatie = round(mean (c(K_Comm01, K_Comm02, K_Comm03, K_Comm04, K_Comm05, K_Comm06, K_Comm07),na.rm = TRUE),2),
    Digitale_vaardigheden = round(mean (c(K_DigVa01, K_DigVa02, K_DigVa03, K_DigVa04),na.rm = TRUE),2),
    Klantgerichtheid = round(mean (c(K_Klant01, K_Klant02, K_Klant03, K_Klant04, K_Klant05, K_Klant06),na.rm = TRUE),2),
    Zelfreflectie = round(mean (c(K_Refl01, K_Refl02, K_Refl03, K_Refl04, K_Refl05, K_Relf06),na.rm = TRUE),2),
    Zelfstandigheid = round(mean (c(K_Zelf01, K_Zelf02, K_Zelf03, K_Zelf04, K_Zelf05, K_Zelf06),na.rm = TRUE),2),
    Verantwoordelijkheid = round(mean (c(K_Vera01, K_Vera02, K_Vera03, K_Vera04),na.rm = TRUE),2),
    Leerbereidheid = round(mean (c(K_Leer01, K_Leer02, K_Leer03, K_Leer04, K_Leer05, K_Leer06),na.rm = TRUE),2),
    Analyseren = round(mean (c(K_Anal01, K_Anal02, K_Anal03, K_Anal04, K_Anal05),na.rm = TRUE),2),
    Kritisch_denken = round(mean (c(K_Kriti01, K_Kriti02, K_Kriti03, K_Kriti04, K_Kriti05),na.rm = TRUE),2),
    Inlevingsvermogen = round(mean (c(K_Inlev01, K_Inlev02, K_Inlev03, K_Inlev04, K_Inlev05, K_Inlev06),na.rm = TRUE),2)) %>%
    ungroup()


Goleweb_schalen_clean <- Goleweb_schalen %>%
  dplyr::select(Instelling, Geslacht, Gebruikersnaam, Creativiteit:Inlevingsvermogen)


#6. Beschrijvende statistieken

#6.1. Goleweb
## Algemeen Gemiddelde per schaal
Gemiddelde_Goleweb_KYSS<- Goleweb_schalen_clean%>%
 summarise(Creativiteit= round(mean(Creativiteit, na.rm = TRUE),2),
            Diversiteit= round(mean(Diversiteit, na.rm = TRUE),2),
            Flexibiliteit= round(mean(Flexibiliteit, na.rm = TRUE),2),
            Plannen_organisatie= round(mean(Plannen_organisatie, na.rm = TRUE),2),
            Resultaatsgerichtheid=round(mean(Resultaatsgerichtheid, na.rm = TRUE),2),
            Samenwerken = round(mean(Samenwerken, na.rm = TRUE),2),
            Communicatie = round(mean(Communicatie, na.rm = TRUE),2),
            Digitale_vaardigheden = round(mean(Digitale_vaardigheden, na.rm = TRUE),2),
            Klantgerichtheid = round(mean(Klantgerichtheid, na.rm = TRUE),2),
            Zelfreflectie = round(mean(Zelfreflectie, na.rm = TRUE),2),
            Zelfstandigheid = round(mean(Zelfstandigheid, na.rm = TRUE),2),
            Verantwoordelijkheid = round(mean(Verantwoordelijkheid, na.rm = TRUE),2),
            Leerbereidheid = round(mean(Leerbereidheid, na.rm = TRUE),2),
            Analyseren = round(mean(Analyseren, na.rm = TRUE),2),
            Kritisch_denken = round(mean(Kritisch_denken, na.rm = TRUE),2),
            Inlevingsvermogen = round(mean(Inlevingsvermogen, na.rm = TRUE),2))


## Algemene Mediaan per schaal
Mediaan_Goleweb_KYSS<- Goleweb_schalen_clean%>%
 summarise(Creativiteit= round(median(Creativiteit, na.rm = TRUE),2),
            Diversiteit= round(median(Diversiteit, na.rm = TRUE),2),
            Flexibiliteit= round(median(Flexibiliteit, na.rm = TRUE),2),
            Plannen_organisatie= round(median(Plannen_organisatie, na.rm = TRUE),2),
            Resultaatsgerichtheid=round(median(Resultaatsgerichtheid, na.rm = TRUE),2),
            Samenwerken = round(median(Samenwerken, na.rm = TRUE),2),
            Communicatie = round(median(Communicatie, na.rm = TRUE),2),
            Digitale_vaardigheden = round(median(Digitale_vaardigheden, na.rm = TRUE),2),
            Klantgerichtheid = round(median(Klantgerichtheid, na.rm = TRUE),2),
            Zelfreflectie = round(median(Zelfreflectie, na.rm = TRUE),2),
            Zelfstandigheid = round(median(Zelfstandigheid, na.rm = TRUE),2),
            Verantwoordelijkheid = round(median(Verantwoordelijkheid, na.rm = TRUE),2),
            Leerbereidheid = round(median(Leerbereidheid, na.rm = TRUE),2),
            Analyseren = round(median(Analyseren, na.rm = TRUE),2),
            Kritisch_denken = round(median(Kritisch_denken, na.rm = TRUE),2),
            Inlevingsvermogen = round(median(Inlevingsvermogen, na.rm = TRUE),2))

## Algemene standaarddeviatie per schaal
Standaarddeviatie_Goleweb_KYSS<- Goleweb_schalen_clean%>%
 summarise(Creativiteit= round(sd(Creativiteit, na.rm = TRUE),2),
            Diversiteit= round(sd(Diversiteit, na.rm = TRUE),2),
            Flexibiliteit= round(sd(Flexibiliteit, na.rm = TRUE),2),
            Plannen_organisatie= round(sd(Plannen_organisatie, na.rm = TRUE),2),
            Resultaatsgerichtheid=round(sd(Resultaatsgerichtheid, na.rm = TRUE),2),
            Samenwerken = round(sd(Samenwerken, na.rm = TRUE),2),
            Communicatie = round(sd(Communicatie, na.rm = TRUE),2),
            Digitale_vaardigheden = round(sd(Digitale_vaardigheden, na.rm = TRUE),2),
            Klantgerichtheid = round(sd(Klantgerichtheid, na.rm = TRUE),2),
            Zelfreflectie = round(sd(Zelfreflectie, na.rm = TRUE),2),
            Zelfstandigheid = round(sd(Zelfstandigheid, na.rm = TRUE),2),
            Verantwoordelijkheid = round(sd(Verantwoordelijkheid, na.rm = TRUE),2),
            Leerbereidheid = round(sd(Leerbereidheid, na.rm = TRUE),2),
            Analyseren = round(sd(Analyseren, na.rm = TRUE),2),
            Kritisch_denken = round(sd(Kritisch_denken, na.rm = TRUE),2),
            Inlevingsvermogen = round(sd(Inlevingsvermogen, na.rm = TRUE),2))

## Algemene MAD per schaal
mad_Goleweb_KYSS<- Goleweb_schalen_clean%>%
 summarise(Creativiteit= round(mad(Creativiteit, na.rm = TRUE),2),
            Diversiteit= round(mad(Diversiteit, na.rm = TRUE),2),
            Flexibiliteit= round(mad(Flexibiliteit, na.rm = TRUE),2),
            Plannen_organisatie= round(mad(Plannen_organisatie, na.rm = TRUE),2),
            Resultaatsgerichtheid=round(mad(Resultaatsgerichtheid, na.rm = TRUE),2),
            Samenwerken = round(mad(Samenwerken, na.rm = TRUE),2),
            Communicatie = round(mad(Communicatie, na.rm = TRUE),2),
            Digitale_vaardigheden = round(mad(Digitale_vaardigheden, na.rm = TRUE),2),
            Klantgerichtheid = round(mad(Klantgerichtheid, na.rm = TRUE),2),
            Zelfreflectie = round(mad(Zelfreflectie, na.rm = TRUE),2),
            Zelfstandigheid = round(mad(Zelfstandigheid, na.rm = TRUE),2),
            Verantwoordelijkheid = round(mad(Verantwoordelijkheid, na.rm = TRUE),2),
            Leerbereidheid = round(mad(Leerbereidheid, na.rm = TRUE),2),
            Analyseren = round(mad(Analyseren, na.rm = TRUE),2),
            Kritisch_denken = round(mad(Kritisch_denken, na.rm = TRUE),2),
            Inlevingsvermogen = round(mad(Inlevingsvermogen, na.rm = TRUE),2))

## Samenbrengen in tabel
Maten <-  c("Gemiddelde", "Mediaan", "Standaardeviatie", "Absolute Mediaan Afwijking")
descriptives_Goleweb_KYSS <- bind_rows(Gemiddelde_Goleweb_KYSS, Mediaan_Goleweb_KYSS, Standaarddeviatie_Goleweb_KYSS, mad_Goleweb_KYSS)

descriptives_Goleweb_KYSS <- bind_cols(Maten, descriptives_Goleweb_KYSS)
#rename(descriptives_Goleweb_KYSS, Maten = '...1')

#descriptives_Goleweb_KYSS_2<-as.data.frame(t(descriptives_Goleweb_KYSS))

Table_descriptives <- flextable(descriptives_Goleweb_KYSS)
Table_descriptives



Kurtosis_Goleweb_information<- Goleweb_schalen_clean%>%
  summarise( Analysing= round(kurtosis(Analyseren, na.rm = TRUE),2),
             Critical_thinking = round(kurtosis(Kritisch_denken, na.rm = TRUE),2),
             Creativity = round(kurtosis(Creativiteit, na.rm = TRUE),2),
             Willingness_to_learn = round(kustosis(Leerbereidheid, na.rm = TRUE),2))

save_as_image(Table_descriptives, path = "descriptives.png")

## Boxplots
Goleweb_wide <-Goleweb_schalen_clean%>%
  select(Gebruikersnaam, Creativiteit:Inlevingsvermogen)

Goleweb_long <- Goleweb_wide %>%
  pivot_longer(-Gebruikersnaam, names_to = "Schaal", values_to = "Waarde")

ggplot (data = Goleweb_long, mapping = aes(x = Schaal, y = Waarde, fill = Schaal)) +
  geom_boxplot(show.legend = F) +
  stat_summary(fun=mean, geom="point", color="red", size=2, show.legend = F)+
  labs(title="Boxplots beschrijvende statistieken KYSS-schalen",
        x ="KYSS-Schalen", y = "scores (1-5)")+
  scale_fill_grey(start = 0.3, end = 0.9)+
  coord_flip()


## Skewness
skewness_Goleweb_KYSS<- Goleweb_schalen_clean%>%
 summarise(Creativiteit= round(skewness(Creativiteit, na.rm = TRUE),2),
            Diversiteit= round(skewness(Diversiteit, na.rm = TRUE),2),
            Plannen_organisatie= round(skewness(Plannen_organisatie, na.rm = TRUE),2),
            Resultaatsgerichtheid=round(skewness(Resultaatsgerichtheid, na.rm = TRUE),2),
            Samenwerken = round(skewness(Samenwerken, na.rm = TRUE),2),
            Communicatie = round(skewness(Communicatie, na.rm = TRUE),2),
            Digitale_vaardigheden = round(skewness(Digitale_vaardigheden, na.rm = TRUE),2),
            Klantgerichtheid = round(skewness(Klantgerichtheid, na.rm = TRUE),2),
            Zelfreflectie = round(skewness(Zelfreflectie, na.rm = TRUE),2),
            Zelfstandigheid = round(skewness(Zelfstandigheid, na.rm = TRUE),2),
            Verantwoordelijkheid = round(skewness(Verantwoordelijkheid, na.rm = TRUE),2),
            Leerbereidheid = round(skewness(Leerbereidheid, na.rm = TRUE),2),
            Analyseren = round(skewness(Analyseren, na.rm = TRUE),2),
            Kritisch_denken = round(skewness(Kritisch_denken, na.rm = TRUE),2),
            Inlevingsvermogen = round(skewness(Inlevingsvermogen, na.rm = TRUE),2))



#7. Correlaties
Cor_KYSS_Goleweb <- Goleweb_schalen_clean %>%
  dplyr::select(Creativiteit : Inlevingsvermogen) %>%
  dplyr::filter(!is.na(Creativiteit)) %>%
  dplyr::filter(!is.na(Analyseren)) %>%
  dplyr::filter(!is.na(Communicatie)) %>%
  dplyr::filter(!is.na(Digitale_vaardigheden)) %>%
  dplyr::filter(!is.na(Diversiteit)) %>%
  dplyr::filter(!is.na(Flexibiliteit)) %>%
  dplyr::filter(!is.na(Inlevingsvermogen)) %>%
  dplyr::filter(!is.na(Klantgerichtheid)) %>%
  dplyr::filter(!is.na(Kritisch_denken)) %>%
  dplyr::filter(!is.na(Leerbereidheid)) %>%
  dplyr::filter(!is.na(Plannen_organisatie)) %>%
  dplyr::filter(!is.na(Resultaatsgerichtheid)) %>%
  dplyr::filter(!is.na(Samenwerken)) %>%
  dplyr::filter(!is.na(Verantwoordelijkheid)) %>%
  dplyr::filter(!is.na(Zelfreflectie)) %>%
  dplyr::filter(!is.na(Zelfstandigheid))

nrow(Cor_KYSS_Goleweb)


Matrix_KYSS_Goleweb<- cor(Cor_KYSS_Goleweb)

Matrix_KYSS_Goleweb

ggcorrplot(Matrix_KYSS_Goleweb, method = "square", type = "upper", colors=c( "#FF9F23", "White", "#2683C6"), lab=TRUE, lab_col = "grey25", legend.title = " Sterkte correlatie", title = "Samenhang KYSS Schalen in GOLEWEB")

ggsave("correlatie.png", device ="png")

#8. Voorspellende analyses

