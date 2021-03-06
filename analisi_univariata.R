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

################################################################################
###                                                                          ###
###                           ANALISI UNIVARIATA                             ###
###                                                                          ###
################################################################################

############################################   DESCRITTIVA   ###################
mean(dataframe$pil_18) / 1000 # 31.6 K di ricchezza prodotta in media da ogni cittadino europeo
mean(dataframe$pop_18) / 1000000 # IN MILIONI

mean(dataframe$edu_18) # 80% di persone hanno un'educazione di 
                       # livello superiore nella fascia d'età 25-64 anni

mean(dataframe$media_co2_18) #emissione media di co2 per chilometro
mean(dataframe$gas_serra_pro_capite_18) #in equivalenti di co_2
mean(dataframe$protezione_ambiente_18) # milioni
mean(dataframe$tasse_ambiente_18) # percentuale
mean(dataframe$riciclo) # percentuale
mean(dataframe$energie_rinnovabili_18) # percentuale

colnames(dataframe)[2]
mean(dataframe[,2])
median(dataframe[,2])
min(dataframe[,2])
max(dataframe[,2])

#x <- list(colnames(dataframe)[2], mean(dataframe[,2]), median(dataframe[,2]), min(dataframe[,2]), max(dataframe[,2]))
#y <- list(colnames(dataframe)[3], mean(dataframe[,3]), median(dataframe[,3]), min(dataframe[,3]), max(dataframe[,3]))
#z <- rbind(x,y)
descrittiva <- list()
for (i in 2:length(dataframe)) {
  x <- list(colnames(dataframe)[i], round(mean(dataframe[,i]),2), median(dataframe[,i]), min(dataframe[,i]), max(dataframe[,i]))
  descrittiva <- rbind(descrittiva, x)
}
statistica_descrittiva <- as.data.frame(descrittiva)
colnames(statistica_descrittiva) <- c("Variabile",
                                      "Media",
                                      "Mediana",
                                      "Minimo",
                                      "Massimo")
rownames(statistica_descrittiva) <- c(1:9)


#modo veloce
summary(dataframe)
################################################################################

#bar chart
library(ggplot2)

a <-ggplot(dataframe, aes(x=reorder(stato, -pil_18), y=pil_18)) +
  geom_bar(stat="identity", fill="#7894FF", alpha=.6, width=.4) +
  xlab("STATO") +
  ylab("Prodotto Interno Lordo") +
  theme_bw()
b <- ggplot(dataframe, aes(x=reorder(stato, -pop_18), y=pop_18)) +
  geom_bar(stat="identity", fill="#7894FF", alpha=.6, width=.4) +
  xlab("STATO") +
  ylab("Popolazione") +
  theme_bw()

library(ggpubr)
ggarrange(a,b, ncol = 1, nrow = 2)


d <-ggplot(dataframe, aes(x=reorder(stato, -edu_18), y=edu_18)) +
  geom_bar(stat="identity", fill="#7894FF", alpha=.6, width=.4) +
  xlab("STATO") +
  ylab("Educazione di Alto Livello") +
  theme_bw()
e <- ggplot(dataframe, aes(x=reorder(stato, -tasse_ambiente_18), y=tasse_ambiente_18)) +
  geom_bar(stat="identity", fill="#7894FF", alpha=.6, width=.4) +
  xlab("STATO") +
  ylab("Tasse per protezione ambiente") +
  theme_bw()
f <- ggplot(dataframe, aes(x=reorder(stato, -protezione_ambiente_18), y=protezione_ambiente_18)) +
  geom_bar(stat="identity", fill="#7894FF", alpha=.6, width=.4) +
  xlab("STATO") +
  ylab("Spesa pubblica per protezione ambiente") +
  theme_bw()

ggarrange(e,f, ncol = 1, nrow = 2)
ggarrange(d, ncol = 1, nrow = 1)

g <- ggplot(dataframe, aes(x=reorder(stato, -media_co2_18), y=media_co2_18)) +
  geom_bar(stat="identity", fill="#CFA00E", alpha=.6, width=.4) +
  xlab("STATO") +
  ylab("Media emissione CO2") +
  theme_bw()
h <- ggplot(dataframe, aes(x=reorder(stato, -gas_serra_pro_capite_18), y=gas_serra_pro_capite_18)) +
  geom_bar(stat="identity", fill="#CFA00E", alpha=.6, width=.4) +
  xlab("STATO") +
  ylab("Gas serra pro capite") +
  theme_bw()

ggarrange(g,h, ncol = 1, nrow = 2)

i <- ggplot(dataframe, aes(x=reorder(stato, -energie_rinnovabili_18), y=energie_rinnovabili_18)) +
  geom_bar(stat="identity", fill="#55ABFF", alpha=.6, width=.4) +
  xlab("STATO") +
  ylab("Energie Rinnovabili") +
  theme_bw()
j <- ggplot(dataframe, aes(x=reorder(stato, -riciclo), y=riciclo)) +
  geom_bar(stat="identity", fill="#55ABFF", alpha=.6, width=.4) +
  xlab("STATO") +
  ylab("Tasso di Riciclo") +
  theme_bw()

ggarrange(i,j, ncol = 1, nrow = 2)


### Proviamo a vedere come plottare insieme
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

df_scaled <- as.data.frame(lapply(dataframe[,c(2,3,4)], normalize))

str(df_scaled)

pil <- cbind(dataframe$stato, df_scaled$pil_18)
popol <- cbind(dataframe$stato, df_scaled$pop_18)
educ <- cbind(dataframe$stato, df_scaled$edu_18)

df <- as.data.frame(rbind(pil, popol))
df <- as.data.frame(rbind(df, educ))

colnames(df) <- c("stato", "valore")
df$valore <- as.numeric(df$valore)  
df$variable <- c(replicate(28, "PIL"), replicate(28, "POP"), replicate(28, "EDU"))

###### PLOT DELLE VARIABILI INSIEME
ggplot(df, aes(x=reorder(stato, -valore), y=valore, fill=variable)) +
  geom_bar(stat="identity", alpha=.6, width=.8, position = "dodge") +
  xlab("STATO") +
  theme_bw()


############################################  DISTRIBUZIONE  ###################
b1 <-ggplot(dataframe, aes(y=pil_18)) +
  geom_boxplot(varwidth = TRUE, alpha=0.2, color='#3498DB', fill='#3498DB')+
  coord_flip()
h1 <-ggplot(dataframe, aes(y=pil_18)) +
  geom_histogram(alpha=0.5, color='#3498DB', fill='#3498DB', position = 'identity')+
  coord_flip()
b2 <-ggplot(dataframe, aes(y=pop_18)) +
  geom_boxplot(varwidth = TRUE, alpha=0.2, color='#3498DB', fill='#3498DB')+
  coord_flip()
h2 <-ggplot(dataframe, aes(y=pop_18)) +
  geom_histogram(alpha=0.5, color='#3498DB', fill='#3498DB', position = 'identity')+
  coord_flip()
b3 <-ggplot(dataframe, aes(y=edu_18)) +
  geom_boxplot(varwidth = TRUE, alpha=0.2, color='#3498DB', fill='#3498DB')+
  coord_flip()
h3 <-ggplot(dataframe, aes(y=edu_18)) +
  geom_histogram(alpha=0.5, color='#3498DB', fill='#3498DB', position = 'identity')+
  coord_flip()

ggarrange(h1, b1, h2, b2, h3, b3, ncol = 2, nrow = 3)

b4 <-ggplot(dataframe, aes(y=media_co2_18)) +
  geom_boxplot(varwidth = TRUE, alpha=0.2, color='#F39C12', fill='#F39C12')+
  coord_flip()
h4 <-ggplot(dataframe, aes(y=media_co2_18)) +
  geom_histogram(alpha=0.5, color='#F39C12', fill='#F39C12', position = 'identity')+
  coord_flip()
b5 <-ggplot(dataframe, aes(y=gas_serra_pro_capite_18)) +
  geom_boxplot(varwidth = TRUE, alpha=0.2, color='#F39C12', fill='#F39C12')+
  coord_flip()
h5 <-ggplot(dataframe, aes(y=gas_serra_pro_capite_18)) +
  geom_histogram(alpha=0.5, color='#F39C12', fill='#F39C12', position = 'identity')+
  coord_flip()

ggarrange(h4, b4, h5, b5, ncol = 2, nrow = 2)

b6 <-ggplot(dataframe, aes(y=protezione_ambiente_18)) +
  geom_boxplot(varwidth = TRUE, alpha=0.2, color='#27AE60', fill='#27AE60')+
  coord_flip()
h6 <-ggplot(dataframe, aes(y=protezione_ambiente_18)) +
  geom_histogram(alpha=0.5, color='#27AE60', fill='#27AE60', position = 'identity')+
  coord_flip()
b7 <-ggplot(dataframe, aes(y=tasse_ambiente_18)) +
  geom_boxplot(varwidth = TRUE, alpha=0.2, color='#27AE60', fill='#27AE60')+
  coord_flip()
h7 <-ggplot(dataframe, aes(y=tasse_ambiente_18)) +
  geom_histogram(alpha=0.5, color='#27AE60', fill='#27AE60', position = 'identity')+
  coord_flip()

ggarrange(h6, b6, h7, b7, ncol = 2, nrow = 2)

b8 <-ggplot(dataframe, aes(y=riciclo)) +
  geom_boxplot(varwidth = TRUE, alpha=0.2, color='#5D6D7E', fill='#5D6D7E')+
  coord_flip()
h8 <-ggplot(dataframe, aes(y=riciclo)) +
  geom_histogram(alpha=0.5, color='#5D6D7E', fill='#5D6D7E', position = 'identity')+
  coord_flip()
b9 <-ggplot(dataframe, aes(y=energie_rinnovabili_18)) +
  geom_boxplot(varwidth = TRUE, alpha=0.2, color='#5D6D7E', fill='#5D6D7E')+
  coord_flip()
h9 <-ggplot(dataframe, aes(y=energie_rinnovabili_18)) +
  geom_histogram(alpha=0.5, color='#5D6D7E', fill='#5D6D7E', position = 'identity')+
  coord_flip()

ggarrange(h8, b8, h9, b9, ncol = 2, nrow = 2)

############### VARIANZA = (sommatoria (x - media)^2) / (n -1)
sigma <- sum((dataframe$pil_18 - mean(dataframe$pil_18))**2)/(length(dataframe$pil_18)-1)
sigma

var(dataframe$pil_18)
var(dataframe$pop_18)
var(dataframe$edu_18)

var(dataframe$media_co2_18)
var(dataframe$gas_serra_pro_capite_18)
var(dataframe$protezione_ambiente_18)
var(dataframe$tasse_ambiente_18)
var(dataframe$riciclo)
var(dataframe$energie_rinnovabili_18)

############### DEVIAZIONE STANDARD = radice_quadrata(varianza)
std <- sigma**(1/2)
std

sd(dataframe$pil_18)

sd(dataframe$pop_18)
sd(dataframe$edu_18)

sd(dataframe$media_co2_18)
sd(dataframe$gas_serra_pro_capite_18)
sd(dataframe$protezione_ambiente_18)
sd(dataframe$tasse_ambiente_18)
sd(dataframe$riciclo)
sd(dataframe$energie_rinnovabili_18)

