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
                      breaks = c(348449,2623396,7936150,12844213,82792352), 
                      labels = c("Bassa","Media","Grande", "Molto Grande"))
df$protezione_ambiente <- cut(df$protezione_ambiente_18, 
                              breaks = c(169,694.7,2246.3,10423.2,72547), 
                              labels = c("Poco","Medio","Molto","Tantissimo"))
df <- df[,-c(1,2)]
t <- table(df)
t
chisq.test(t)

###################################################    ANALISI BIVARIATA    ####  
data <- as.data.frame(cbind(dataframe$protezione_ambiente_18, as.factor(df$popolazione)))
colnames(data) <- c('protezione_ambiente_18', 'popolazione')
boxplot(protezione_ambiente_18 ~ popolazione, data = data, xlab = 'Popolazione', ylab = 'Spesa Pubblica')
#Salvo la categorizzazione
variabili_categroriche <- df

library(gplots)
plotmeans(protezione_ambiente_18 ~ popolazione, data=data)
aggregate(protezione_ambiente_18 ~ popolazione, data, function(x) c(Media=mean(x), sd = sd(x), n = length(x)))
summary(aov(protezione_ambiente_18 ~ popolazione, data=data))

################################################### 2. PROTEZIONE AMBIENTE - RICICLO
df <- dataframe[,c(7,9)]
df$percentuale_riciclo <- cut(df$riciclo, 
                              breaks = c(-1,27.325,36.850,49.825,68), 
                              labels = c("Basso","Medio","Alto", "Molto Alto"))
df$protezione_ambiente <- cut(df$protezione_ambiente_18, 
                              breaks = c(169,694.7,2246.3,10423.2,72547), 
                              labels = c("Poco","Medio","Molto","Tantissimo"))
df <- df[,-c(1,2)]
t <- table(df)
t
chisq.test(t)

###################################################    ANALISI BIVARIATA    ####  
data <- as.data.frame(cbind(dataframe$riciclo, as.factor(df$protezione_ambiente)))
colnames(data) <- c('riciclo', 'protezione_ambiente')
boxplot(riciclo ~ protezione_ambiente, data = data, xlab = 'Spesa Pubblica', ylab = 'Riciclo')


plotmeans(riciclo ~ protezione_ambiente, data=data)
aggregate(riciclo ~ protezione_ambiente, data, function(x) c(Media=mean(x), sd = sd(x), n = length(x)))
summary(aov(riciclo ~ protezione_ambiente, data=data))
#Salvo la categorizzazione
variabili_categroriche <- cbind(variabili_categroriche, df)

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
data <- as.data.frame(cbind(dataframe$media_co2_18, as.factor(df$educazione)))
colnames(data) <- c('media_co2_18', 'educazione')
boxplot(media_co2_18 ~ educazione, data = data, xlab = 'Educazione', ylab = 'Emissione media CO_2')


plotmeans(media_co2_18 ~ educazione, data=data)
aggregate(media_co2_18 ~ educazione, data, function(x) c(Media=mean(x), sd = sd(x), n = length(x)))
summary(aov(media_co2_18 ~ educazione, data=data))
#Salvo la categorizzazione
variabili_categroriche <- cbind(variabili_categroriche, df)

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



# PCA E KMEANS Clustering
################################################################################
#PCA
library(factoextra)
df <- dataframe[,-c(1,2,3)]
rownames(df) <- dataframe$stato
res.pca <- prcomp(df, scale = TRUE)
fviz_eig(res.pca)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs

#K-Means CLUSTERING
df_scaled <- as.data.frame(cbind(dataframe[,1], as.data.frame(lapply(dataframe[,-c(1,2,3)], normalize))))
rownames(df_scaled) <- df_scaled[,1]
df_scaled <- df_scaled[,-1]
km.res <- kmeans(df_scaled, 4, nstart = 25)
print(km.res)

dd <- cbind(df_scaled, cluster = km.res$cluster)
head(dd)


km.res$cluster
km.res$size

fviz_cluster(km.res, data = df_scaled,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF5733"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

###### with 3 clusters
km.res <- kmeans(df_scaled, 3, nstart = 25)
print(km.res)

dd <- cbind(df_scaled, cluster = km.res$cluster)

head(dd)


km.res$cluster
km.res$size

fviz_cluster(km.res, data = df_scaled,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF5733"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

####### with 5 clusters
km.res <- kmeans(df_scaled, 5, nstart = 25)
print(km.res)

dd <- cbind(df_scaled, cluster = km.res$cluster)
head(dd)


km.res$cluster
km.res$size

fviz_cluster(km.res, data = df_scaled,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF5733", "#33FF42"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)



################################################################################

