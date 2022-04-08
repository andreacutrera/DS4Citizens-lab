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


###  IMPORTIAMO IL DATAFRAME CHE ABBIAMO CREATO DOPO TUTTA LA MANIPOLAZIONE  ###
dataframe <- read.csv("dataframe.txt", sep=' ')

### Proviamo a vedere come plottare insieme
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

df_scaled <- as.data.frame(lapply(dataframe[,c(2,3,4)], normalize))

str(df_scaled)

#################         CATEGORIZZAIMO QUALCHE VARIABILE      ################

######### CAPIAMO QUALI VARIABILI POTREBBERO ESSERE ASSOCIATE ESPLORANDO UN PO' 
######### LA CORRELAZIONE VARIABILE PER VARIABILE
library(Hmisc)
library(corrplot)
M = rcorr(as.matrix(dataframe[,-1]))
M

correlation = cor(dataframe[,-1], method = c("spearman"))

corrplot(correlation, type = 'lower', order = 'FPC')

################################################################################
# 1. PROTEZIONE AMBIENTE - POPOLAZIONE
# 2. PROTEZIONE AMBIENTE - RICICLO
# 3. EDUCAZIONE - CO_2

########################################### 1. PROTEZIONE AMBIENTE - POPOLAZIONE
df <- dataframe[,c(3,7)]
df$popolazione <- cut(df$pop_18, 
                      breaks = c(0,mean(df$pop_18),max(df$pop_18)), 
                      labels = c("Poco Popolosa","Molto Popolosa"))
df$protezione_ambiente <- cut(df$protezione_ambiente_18, 
                              breaks = c(0,mean(df$protezione_ambiente_18),max(df$protezione_ambiente_18)+1), 
                              labels = c("Bassa Protezione","Alta Protezione"))
df <- df[,-c(1,2)]
t <- table(df)
t
chisq.test(t)

###################################################    ANALISI BIVARIATA    ####  
data <- as.data.frame(cbind(dataframe$protezione_ambiente_18, df$popolazione))
colnames(data) <- c('protezione_ambiente_18', 'popolazione')
ggplot(data, aes(y=protezione_ambiente_18, group=popolazione)) +
  geom_boxplot(alpha=0.2, color='#3498DB', fill='#3498DB')


################################################### 2. PROTEZIONE AMBIENTE - RICICLO
df <- dataframe[,c(7,9)]
df$percentuale_riciclo <- cut(df$riciclo, 
                              breaks = c(-1,mean(df$riciclo),max(df$riciclo)+1), 
                              labels = c("Poco riciclo","Molto Riciclo"))
df$protezione_ambiente <- cut(df$protezione_ambiente_18, 
                              breaks = c(0,mean(df$protezione_ambiente_18),max(df$protezione_ambiente_18)+1), 
                              labels = c("Bassa Protezione","Alta Protezione"))
df <- df[,-c(1,2)]
t <- table(df)
t
chisq.test(t)

###################################################    ANALISI BIVARIATA    ####  
data <- as.data.frame(cbind(dataframe$riciclo, df$protezione_ambiente))
colnames(data) <- c('riciclo', 'protezione_ambiente')
ggplot(data, aes(y=riciclo, group=protezione_ambiente)) +
  geom_boxplot(alpha=0.2, color='#3498DB', fill='#3498DB')


################################################### 3. EDU - CO2
df <- dataframe[,c(4,5)]
df$educazione <- cut(df$edu_18, 
                    breaks = c(49,78.425,82.900,88.375,95), 
                    labels = c("Basso","Medio","Alto", "Molto Alto"))
df$co2 <- cut(df$media_co2_18, 
                breaks = c(105,114.800,121.200,127.675,133), 
                labels = c("Poco","Medio","Molto","Tantissimo"))
df <- df[,-c(1,2)]
t <- table(df)
t
chisq.test(t)

###################################################    ANALISI BIVARIATA    ####  
data <- as.data.frame(cbind(dataframe$media_co2_18, df$educazione))
colnames(data) <- c('media_co2_18', 'educazione')
ggplot(data, aes(y=media_co2_18, group=educazione)) +
  geom_boxplot(alpha=0.2, color='#3498DB', fill='#3498DB')


################################################################################

###################################################   REGRESSIONE LINEARE   ####  



######  POPOLAZIONE E PROTEZIONE AMBIENTE
ggplot(dataframe[dataframe[,'pop_18'] < 5000000,], aes(x=pop_18, y=protezione_ambiente_18)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  xlab('Popolazione') +
  ylab("Spesa pubblica per l'ambiente")

regressione <- lm(protezione_ambiente_18 ~ pop_18, data=dataframe[dataframe[,'pop_18'] < 5000000,])
summary(regressione)

ggplot(dataframe[dataframe[,'pop_18'] > 5000000,], aes(x=pop_18, y=protezione_ambiente_18)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  xlab('Popolazione') +
  ylab("Spesa pubblica per l'ambiente")

regressione <- lm(protezione_ambiente_18 ~ pop_18, data=dataframe[dataframe[,'pop_18'] > 5000000,])
summary(regressione)

ggplot(dataframe, aes(x=pop_18, y=protezione_ambiente_18)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  xlab('Popolazione') +
  ylab("Spesa pubblica per l'ambiente")

regressione <- lm(protezione_ambiente_18 ~ pop_18, data=dataframe)
summary(regressione)



######  PROTEZIONE AMBIENTE e RICICLO
ggplot(dataframe[dataframe[,'protezione_ambiente_18'] < 20000,], aes(x=protezione_ambiente_18, y=riciclo)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  xlab("Spesa pubblica per l'ambiente") +
  ylab("Percentuale di riciclo")

regressione <- lm(protezione_ambiente_18 ~ pop_18, data=dataframe[dataframe[,'protezione_ambiente_18'] < 20000,])
summary(regressione)

ggplot(dataframe, aes(x=protezione_ambiente_18, y=riciclo)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  xlab("Spesa pubblica per l'ambiente") +
  ylab("Percentuale di riciclo")

regressione <- lm(protezione_ambiente_18 ~ pop_18, data=dataframe)
summary(regressione)

######  EDU e CO_2
ggplot(dataframe, aes(x=edu_18, y=media_co2_18)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  xlab("Spesa pubblica per l'ambiente") +
  ylab("Media emissione CO_2")

##########  REGRESSIONE MULTIPLA

regressione <- lm(media_co2_18 ~., data=dataframe[,-1])
summary(regressione)
regressione <- lm(gas_serra_pro_capite_18 ~., data=dataframe[,-1])
summary(regressione)
regressione <- lm(energie_rinnovabili_18 ~., data=dataframe[,-1])
summary(regressione)
regressione <- lm(riciclo ~., data=dataframe[,-1])
summary(regressione)
