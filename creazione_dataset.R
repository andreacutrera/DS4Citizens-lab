################################################################################
###                  La Sostenibilità Ambientale dell'Europa                 ###
################################################################################

### Esploriamo le informazioni disponibili negli open data europei le condizioni
### energetiche e sulla protezione ambientale.
###
### 1. Quali sono i paesi più inquinati?
### 2. Quali sono i paesi che investono più sulla protezione ambientale?
### 3. Quali sono i paesi che utilizzano più energie rinnovabili?

### Alla domanda 1 ---> emissioni di CO_2, Gas Serra, riciclo dei rifiuti;
### Alla domanda 2 ---> pil pro capite, popolazione, educazione
###                     tassazione ed investimenti per protezione ambiente;
### Alla domanda 3 ---> energie rinnovabili;

#########     RISORSA: OPEN DATA database EUROSTAT                     #########
#########     https://ec.europa.eu/eurostat/web/main/data/database     #########

################################################################################
###                                                                          ###
### IMPORTIAMO I DATI SCARICATI NEL FORMATO TABULAR SEPARATED VALUES (.tsv)  ###
###                                                                          ###
################################################################################

################                IMPORTIAMO IL PIL              #################
## https://ec.europa.eu/eurostat/databrowser/view/tec00001/default/table?lang=en
gdp <- read.csv("/Users/andreacutrera/Desktop/lezione DSfoCitizens - Andrea/datasets/gdp.tsv", 
                sep=' ')
colnames(gdp)[1] <- c('stato')
gdp <- gdp[1:41,c('stato','X2018')]
colnames(gdp)[2] <- c('pil_18')
# Current prices, euro per capita [CP_EUR_HAB]
gdp$stato <- substr(gdp$stato, 17, 18)
gdp[29,2] <- substr(gdp[29,2], 3, 7)

gdp$pil_18 <- as.numeric(gdp$pil_18)

str(gdp)

################             IMPORTIAMO LA POPOLAZIONE          ################
## https://ec.europa.eu/eurostat/databrowser/view/tps00001/default/table?lang=en
pop <- read.csv("/Users/andreacutrera/Desktop/lezione DSfoCitizens - Andrea/datasets/population.tsv", sep=' ')
colnames(pop)[1] <- c('stato')
pop <- pop[,c('stato','X2018')]
colnames(pop)[2] <- c('pop_18')
pop$stato <- substr(pop$stato, 5, 6)
pop[c(15,16,20,21,22,34,39,55),2] <- substr(pop[c(15,16,20,21,22,34,39,55),2], 3, 20)

pop$pop_18 <- as.numeric(pop$pop_18)

str(pop)

##############               UNIAMO I DUE DATAFRAMES              ##############
dataframe <- merge(gdp, pop, by = 'stato')


##############      IMPORTIAMO L'EDUCAZIONE DI ALTO LIVELLO      ###############
## https://ec.europa.eu/eurostat/databrowser/view/tps00065/default/table?lang=en
edu <- read.csv("/Users/andreacutrera/Desktop/lezione DSfoCitizens - Andrea/datasets/higher_education.tsv", sep=' ')
colnames(edu)[1] <- c('stato')
#At least upper secondary educational attainment, age group 25-64 --> TOTAL
edu <- edu[77:114,c('stato','X2018')]
colnames(edu)[2] <- c('edu_18')
edu$stato <- substr(edu$stato, 19, 20)
edu[c(2,8,19,23),2] <- substr(edu[c(2,8,19,23),2], 3, 6)

edu$edu_18 <- as.numeric(edu$edu_18)

##############                       MERGE                       ###############
dataframe <- merge(dataframe, edu, by='stato')

#############     IMPORTIAMO LA MEDIA DELLE EMISSIONI DI CO_2      #############
## https://ec.europa.eu/eurostat/databrowser/view/sdg_12_30/default/table?lang=en
co2 <- read.csv("/Users/andreacutrera/Desktop/lezione DSfoCitizens - Andrea/datasets/avg_co2.tsv", sep=" ")
colnames(co2)[1] <- c('stato')
# Average CO2 emissions per km from new passenger cars (source: EEA, DG CLIMA)
co2 <- co2[,c('stato','X2018')]
colnames(co2)[2] <- c('media_co2_18')
co2$stato <- substr(co2$stato, 1, 2)

co2$media_co2_18 <- as.numeric(co2$media_co2_18)

str(co2)


##############                       MERGE                       ###############
dataframe <- merge(dataframe, co2, by='stato')

##############            Rimuoviamo i valori NA                 ###############
dataframe <- na.omit(dataframe)

##############   Rimuoviamo i valori replicati EU, EU, ...       ###############
dataframe <- dataframe[-c(11:22),]

##############          IMPORTIAMO I GAS SERRA PRO CAPITE        ###############
## https://ec.europa.eu/eurostat/databrowser/view/t2020_rd300/default/table?lang=en
ghg <- read.csv("/Users/andreacutrera/Desktop/lezione DSfoCitizens - Andrea/datasets/ghg_pc.tsv", sep=' ')
colnames(ghg)[1] <- c('stato')
# Greenhouse gas emissions per capita
ghg <- ghg[,c('stato','X2018')]
colnames(ghg)[2] <- c('gas_serra_pro_capite_18')
ghg$stato <- substr(ghg$stato, 1, 2)
ghg[c(12,13,22),2] <- substr(ghg[c(12,13,22),2], 3, 6)

ghg$gas_serra_pro_capite_18 <- as.numeric(ghg$gas_serra_pro_capite_18)

str(ghg)

##############                       MERGE                       ###############
dataframe <- merge(dataframe, ghg, by='stato')

#######  IMPORTIAMO LA SPESA PUBBLICA PER LA PROTEZIONE DELL'AMBIENTE    #######
## https://ec.europa.eu/eurostat/databrowser/view/ten00135/default/table?lang=en
env_protection <- read.csv("/Users/andreacutrera/Desktop/lezione DSfoCitizens - Andrea/datasets/env_protection_expenditure.tsv", sep=' ')
colnames(env_protection)[1] <- c('stato')
env_protection <- env_protection[,c('stato','X2018')]
# Million Euro € 
env_protection <- env_protection[1:34,]
colnames(env_protection)[2] <- c('protezione_ambiente_18')
env_protection$stato <- substr(env_protection$stato, 12, 13)
env_protection[c(7,10,12,13,27,32,34),2] <- substr(env_protection[c(7,10,12,13,27,32,34),2], 3, 15)

env_protection$protezione_ambiente_18 <- as.numeric(env_protection$protezione_ambiente_18)

##############                       MERGE                       ###############
dataframe <- merge(dataframe, env_protection, by='stato')

##############            Rimuoviamo i valori NA                 ###############
dataframe <- na.omit(dataframe)

#######    IMPORTIAMO LA TASSAZIONE PER LA PROTEZIONE DELL'AMBIENTE      #######
## https://ec.europa.eu/eurostat/databrowser/view/t2020_rt320/default/table?lang=en
env_tax <- read.csv("/Users/andreacutrera/Desktop/lezione DSfoCitizens - Andrea/datasets/env_tax.tsv", sep=' ')
colnames(env_tax)[1] <- c('stato')
env_tax <- env_tax[,c('stato','X2018')]
# Percentage of gross domestic product (GDP) [PC_GDP]
env_tax <- env_tax[1:33,]
colnames(env_tax)[2] <- c('tasse_ambiente_18')
env_tax$stato <- substr(env_tax$stato, 12, 13)

env_tax$tasse_ambiente_18 <- as.numeric(env_tax$tasse_ambiente_18)

##############                       MERGE                       ###############
dataframe <- merge(dataframe, env_tax, by='stato')

#######    IMPORTIAMO IL RICICLO DEI RIFIUTI AL LIVELLO MUNICIPALE       #######
## https://ec.europa.eu/eurostat/databrowser/view/cei_wm011/default/table?lang=en
riciclo <- read.csv("/Users/andreacutrera/Desktop/lezione DSfoCitizens - Andrea/datasets/recycling.tsv", sep=' ')
colnames(riciclo)[1] <- c('stato')
# share of recycled municipal waste in the total municipal waste generation
riciclo <- riciclo[,c('stato','X2018')]
colnames(riciclo)[2] <- c('riciclo')
riciclo$stato <- substr(riciclo$stato, 8, 9)
riciclo[c(5,7,30,35,37),2] <- substr(riciclo[c(5,7,30,35,37),2], 3, 15)

riciclo$riciclo <- as.numeric(riciclo$riciclo)

##############                       MERGE                       ###############
dataframe <- merge(dataframe, riciclo, by='stato')

##############            Rimuoviamo i valori NA                 ###############
dataframe <- na.omit(dataframe)

#######           IMPORTIAMO I DATI SULLE ENERGIE RINNOVABILI            #######
## https://ec.europa.eu/eurostat/databrowser/view/sdg_07_40/default/table?lang=en
rinnovabili <- read.csv("/Users/andreacutrera/Desktop/lezione DSfoCitizens - Andrea/datasets/renewable_energy.tsv", sep=' ')
colnames(rinnovabili)[1] <- c('stato')
# share of renewable energy consumption in gross final energy consumption 
# according to the Renewable Energy Directive.
rinnovabili <- rinnovabili[,c('stato','X2018')]
colnames(rinnovabili)[2] <- c('energie_rinnovabili_18')
rinnovabili$stato <- substr(rinnovabili$stato, 13, 14)

rinnovabili$energie_rinnovabili_18 <- as.numeric(rinnovabili$energie_rinnovabili_18)

##############                       MERGE                       ###############
dataframe <- merge(dataframe, rinnovabili, by='stato')


View(dataframe)
str(dataframe)
write.table(dataframe,"dataframe.txt", sep=" ",col.names = TRUE, row.names = TRUE)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

