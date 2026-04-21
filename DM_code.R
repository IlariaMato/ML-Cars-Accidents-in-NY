#########################IMPORTAZIONE DEL DATASET###############################
setwd("/Users/ilariamato/Desktop/progetto DM")
b1 <- read.csv("u1.csv", sep=",", dec = ".", stringsAsFactors=TRUE)

#Filtro per stato NY
library("dplyr")
b<-b1%>%filter( State == 'NY')

#####SISTEMO CITY#####
#Tengo solo le città che hanno incidenti frequenti maggiori di 20
#Calcola il numero di incidenti per città
city_counts <- b %>%
  count(City, name = "city_count")

#Conta gli incidenti per città
city_counts <- table(b$City)

#Identifica le città meno frequenti (ad esempio meno di 20 incidenti)
rare_cities <- names(city_counts[city_counts < 20])

#Sostituisci le città rare con "Altro"
b$City <- ifelse(b$City %in% rare_cities, "Altro", b$City)

#Filtra il dataset per escludere le città "Altro"
b <- b %>%
  filter(City != "Altro")

#Conta i livelli unici della variabile 
num_livelli_city <- length(unique(b$City))

#CONCLUSIONE: city 125 LIVELLI DA 477

#rinomino le variabili per facilità di analisi
b <- b %>%rename(pressure=Pressure.in., distance = Distance.mi., temperature= Temperature.F., visibility= Visibility.mi., wind_speed= Wind_Speed.mph. )

#################################Variabili factor###############################
str(b)
b$Street<-factor(b$Street)
b$City<-factor(b$City)
b$State<-factor(b$State)
b$Weather_Condition<-factor(b$Weather_Condition)
b$Sunrise_Sunset<-factor(b$Sunrise_Sunset)
b$month<-factor(b$month)
b$day<-factor(b$day)
b$Crossing<-factor(b$Crossing)
b$Junction<-factor(b$Junction)
b$Stop<-factor(b$Stop)
b$Traffic_Signal<-factor(b$Traffic_Signal)
b$visibility<-b$visibility+1 #per rendere visibility positivo
str(b)

#verifichiamo eventuali anomalie nel dataset mediante statistiche descrittive di base
summary(b)
#non sono presenti anomalie nelle variabili

#################################1. Missing values##############################
library(VIM)
missingness<- aggr(b, col=c('blue','red'), numbers=TRUE, sortVars=TRUE, labels=names(b), cex.axis=.7,gap=3)

library(Hmisc)
#vediamo il numero di missing esatto per ogni variabile
describe(b)

#imputiamo con la media
b$wind_speed.imp.avg=impute(b$wind_speed, mean)
b$temperature.imp.avg=impute(b$temperature, mean)
b$pressure.imp.avg=impute(b$pressure, mean)
b$visibility.imp.avg=impute(b$visibility, mean)
b$Weather_Condition.imp.avg=impute(b$Weather_Condition)
b$Sunrise_Sunset.imp.avg=impute(b$Sunrise_Sunset)

#rinomino le variabili che sono state imputate, a come erano chiamate prima
#da qua tolgo le variabili inutili e scendo a da 20 a 15 variabili

b<-b%>%select(-wind_speed, -temperature, -pressure,-visibility,-Weather_Condition,-Sunrise_Sunset, -State, -Street, -Start_Time, -Description, -time)
b <- b %>%
  rename(wind_speed=wind_speed.imp.avg, temperature=temperature.imp.avg, pressure=pressure.imp.avg, visibility=visibility.imp.avg, Weather_Condition=Weather_Condition.imp.avg, Sunrise_Sunset=Sunrise_Sunset.imp.avg)

#vedo se si sono tolti i missing
missingness<- aggr(b, col=c('blue','red'), numbers=TRUE, sortVars=TRUE, labels=names(b), cex.axis=.7,gap=3)

#####SISTEMO WEATHER GROUP dopo missing perchè ha 14 valori missing#####
library(factorMerger)
#1) Weather_Condition
reduce_levels2 <- mergeFactors(response = b$visibility, factor = b$Weather_Condition)
plot(reduce_levels2 , panel = "GIC",title = "", panelGrid = FALSE )

og=cutTree(reduce_levels2)
b$optimal_group=og
names(b)

#da 40 livelli a 11
b$Weather_group =as.factor(as.numeric(b$optimal_group))
names(b)

b<-b%>%select(-wind_speed, -temperature, -pressure,-visibility,-Weather_Condition,-Sunrise_Sunset, -State, -Street, -Start_Time, -Description, -time)

###########################2.CREAZIONE PRIMO MODELLO############################
#proviamo a creare un primo modello con variabile target visibility, in quanto è solo un 
#identificativo. Proviamo a plottarla
hist(b$visibility)

#passiamo ora alla creazione del modello e ad una prima analisi esplorativa del modello
modello0 <-lm(visibility ~ . - Weather_Condition - optimal_group, data = b)
summary(modello0)
drop1(modello0, test='F')
#Vediamo come ci siano diverse variabili significative, mentre abbiamo 5 variabili poco significative
#Severity, Crossing, Stop, Traffic_signal, Sunrise_Sunset, temperature
#Ne verifichiamo la collinearità

###########################3. Multicollinearità#################################

###############VARIABILI QUANTITATIVE
cov=attr(terms(modello0), "term.labels")
cov
b_covar <- b[,cov] 
#creo un vettore delle sole covariate numeriche tra quelle che usiamo nel modello
nums <- sapply(b_covar, is.numeric)
b_numeric <- b_covar[,nums]
#converto, nel caso non lo fosse già, la variabile target in una variabile numerica
y = as.numeric(b$visibility)
#creo una matrice numerica composta dai valori delle covariate numeriche trovate prima
X<-b_numeric
X=as.matrix(X)
#calcolo TOL e VIF per le covariate numeriche del modello
library(mctest)
mod=lm(y~X)
imcdiag(mod)
# TOL e VIF assumono valori accettabili, non è presente collinearità tra le variabili quantitative
#per avere collinearità il VIF deve essere minore di 5
#il TOL maggiore di 0.04

#pairs panels
library(psych)
pairs.panels(X, lm=TRUE)
#CONCLUSIONE: confermano l'assenza di collinearità tra le variabili quantitative

###############VARIABILI QUALITATIVE
cov=attr(terms(modello0), "term.labels")

library(dplyr)
library(plyr)
b_fac <- b[,cov]%>% dplyr::select_if(is.factor)

combos <- combn(ncol(b_fac),2)
adply(combos, 2, function(x) {
  test <- chisq.test(b_fac[, x[1]], b_fac[, x[2]])
  tab  <- table(b_fac[, x[1]], b_fac[, x[2]])
  out <- data.frame("Row" = colnames(b_fac)[x[1]]
                    , "Column" = colnames(b_fac[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 3)
                    , "n" = sum(table(b_fac[,x[1]], b_fac[,x[2]]))
                    , "u1" =length(unique(b_fac[,x[1]]))-1
                    , "u2" =length(unique(b_fac[,x[2]]))-1
                    , "nMinu1u2" =sum(table(b_fac[,x[1]], b_fac[,x[2]]))* min(length(unique(b_fac[,x[1]]))-1 , length(unique(b_fac[,x[2]]))-1) 
                    , "Chi.Square norm"  =test$statistic/(sum(table(b_fac[,x[1]], b_fac[,x[2]]))* min(length(unique(b_fac[,x[1]]))-1 , length(unique(b_fac[,x[2]]))-1)) 
  )
  
  
  return(out)
  
})   
##le uniche due covariate che danno da pensare sono traffic_signal e sunrise_sunset con 0.714
#ma non è una correlazione cosi forte da poter essere definita una relazione collineare

#CONCLUSIONE: non ci sono variabili qualitative collineari tra loro


#Per comodità teniamo modello 1 dopo modifica (ma è lo stesso del modello0)
modello1 <- lm(visibility ~ . - Weather_Condition - optimal_group, data = b)
summary(modello1)
drop1(modello1, test = "F")
#ora i parametri vengono stimati tutti, la variabili significative rimangono le medesime

#####4. Interazioni####
#proviamo ad inserire qualche interazione
#proviamo a far interagire alcune variabili non significative, magari spiegano qualcosa congiuntamente:
#dal drop1 notiamo che Severity, Crossing, Sunrise_Sunset, Traffic_Signal, Stop e
#temperature; non sono molto significative. Proviamo delle interazioni tra queste variabili

modello2 <- lm(visibility ~ Crossing+Stop + distance + City + temperature*Sunrise_Sunset + pressure + 
                   wind_speed + Weather_group + Junction + Severity+Traffic_Signal  + month + day,  data = b)
summary(modello2)
drop1(modello2, test = "F")
#creando l'interazione tra temperature e Sunrise_Sunset questa risulta significativa, 
#e anche severity diventa signicativa
#le altre continuano a non essere significative in tutte le loro combinazioni

#R^2 0.7575
#R^2 dopo interazione 0.7523

#############5. Trasformazioni variabili per la linearità#######################
#TRASFORMAZIONE DELLA Y DEL MODELLO 2 AGGIORNATO

library(MASS)
bc <- boxcox(modello2, plotit=T)

lambda <- bc$x[which.max(bc$y)]
lambda
#ci viene suggerito lambda=1.6767 -> proviamo a trasformare la variabile risposta con lambda

modello1_cox<-lm(I(visibility^1.67) ~ Stop*Crossing + distance + City + temperature + pressure + 
                   wind_speed + Weather_group + Junction + Severity + Sunrise_Sunset + month + day,  data = b)
summary(modello1_cox)
drop1(modello1_cox, test="F")
#R^2 è passato da 0.759 a 0.7443
#R^2adj è passato da 0.752 a 0.739


AIC(modello1)
AIC(modello2)
AIC(modello1_cox)

#Preferiamo non trasformare y perchè peggiora sia R^2 che AIC
#AIC(modello1) --> 33165.92
#AIC(modello2) --> 33160.94
#AIC(modello1_cox) --> 64096.48

#plot per vedere se ci sono differenze
par(mfrow=c(2,2))
plot(modello1_cox)
plot(modello1)
par(mfrow=c(1,1))
#non notiamo particolari differenze nella distribuzione dei residui -> preferiamo non trasformare

#####scaricamento#####
install.packages(c("colorRamps", "ggpubr", "agricolae", "formula.tools", "mvtnorm"))
library(mvtnorm)
install.packages("devtools")
devtools::install_github("MI2DataLab/factorMerger")
install.packages("/Users/ilariamato/Desktop/factorMerger_0.0.4.tar.gz", repos = NULL, type = "source")
library("factorMerger")
#####################

#TRASFORMAZIONE DELLE X DEL MODELLO 2 AGGIORNATO
library(gam)
#loess
modello_gam_loess <- gam(visibility ~ Crossing+Stop + lo(distance, span = 0.3) + City + temperature*Sunrise_Sunset + lo(pressure, span = 0.3) + 
                           wind_speed + Weather_group + Junction + Severity+Traffic_Signal  + month + day,  data = b)
summary(modello_gam_loess)
plot(modello_gam_loess)


#splines
modello_gam_splines <- gam(visibility ~ Crossing+Stop + s(distance) + City + s(temperature)*Sunrise_Sunset + s(pressure) + 
                             s(wind_speed) + Weather_group + Junction + Severity+Traffic_Signal  + month + day,  data = b)
summary(modello_gam_splines)
plot(modello_gam_splines)


#abbiamo provato ad applicare sia loess che spline al modello per le variabili continue: distance e pressure
#abbiamo visto dai grafici che si potevano applicare trasfomazioni cubiche per migliore il modello
#guardando quindi i grafici abbiamo trasformato come segue:

# distance, pressure -> cubica
modello4 <- lm(visibility ~ Crossing+Stop + distance + I(distance^2) + I(distance^3) + City + temperature*Sunrise_Sunset + pressure + + I(pressure^2) + I(pressure^3) +
                 wind_speed + Weather_group + Junction + Severity+Traffic_Signal  + month + day,  data = b)
summary(modello4)
drop1(modello4, test = "F")

#effettuiamo reset test per verificare di aver applicato le migliori trasformazioni
library(lmtest)
resettest(modello4, type = "fitted",  data = b)
#p-value < 2.2e-16 MOLTO SIGNIFICATIVO le trasformazioni fatte sono molto significative 


############################6. Model selection##################################
library(MASS)
step <- stepAIC(modello4, direction="both")

#mi ha definito come modello migliore questo sottostante:
modello5 <- lm(visibility ~ distance + I(distance^2) + I(distance^3) + City + 
                 pressure + I(pressure^2) + 
                 I(pressure^3) + wind_speed + Weather_group + 
                 Severity + month + day + temperature*Sunrise_Sunset, data = b)
summary(modello5)
drop1(modello5, test = "F")
#dopo ciò SONO TUTTE SIGNIFICATIVE E IL R QUADRO PASSA DA 0.759 A 0.761
#tolgo singolarmente temperature Sunrise_Sunset, ma tengo interazione perchè sono significative insieme
##################TOGLIERE 3 LIVELLI WEATHER_CONDITION CUSPIDE##################
boxplot(b$Weather_Condition, b$visibility)
plot(b$Weather_group)
#provo a vedere di togliere i livelli che hanno maggiore incidenza sul risultato come:
#9,10,11

library(dplyr)
b_filtrato <- b %>%filter(Weather_group != 11)
b_filtrato <- b_filtrato %>%filter(Weather_group != 10)
b_filtrato <- b_filtrato %>%filter(Weather_group != 9)
b_filtrato<-na.omit(b_filtrato)

table(b_filtrato$Weather_group)

modello5.1<- lm(visibility ~ distance + I(distance^2) + I(distance^3) + City + 
                  pressure + I(pressure^2) + 
                  I(pressure^3) + wind_speed + Weather_group + 
                  Severity + month + day + temperature*Sunrise_Sunset, data = b_filtrato)

plot(b_filtrato$Weather_Condition)
plot(modello5.1,3)

#visto che la cuspide non si risolve e Multiple R-squared:  0.5464 passa a Adjusted R-squared:  0.5138
#tengo i tre livelli finali

###########################7. Outliers##########################################
#generiamo plot per vedere se sono presenti outliers
par(mfrow=c(2,2))
plot(modello5,3)#residui standardizzati, outliers di y
plot(modello5,4)#dcook
plot(modello5,5)#leverage, outliers di x
par(mfrow=c(1,1))

#verifichiamo se ci sono punti influenti sul modello con i DFFITS
dffits1 <- dffits(modello5) #|DFFITS| > 2*sqrt(p/n)
cutoff <- 2*sqrt(length(modello5$coefficients)/nrow(b)) #cutoff

#estraiamo le osservazioni influenti
influential <- b[abs(dffits1) >= cutoff,]
b_filtered <- b[abs(dffits1) < cutoff, ] #osservazioni non influenti

#costruiamo un modello sui dati filtrati
modello6 <- lm(visibility ~ distance + I(distance^2) + I(distance^3) + City + 
               pressure + I(pressure^2) + 
               I(pressure^3) + wind_speed + Weather_group + 
               Severity + month + day + temperature*Sunrise_Sunset, data = b_filtered)


summary(modello6)
drop1(modello6, test="F")
#dopo la rimozione degli outliers R2 DA 0.761 A 0.8382

plot(modello6, 1)

#######################8. Eteroschedasticità####################################
#controlliamo i residui
par(mfrow=c(2,2))
plot(modello6)
par(mfrow=c(1,1))
#notiamo che i residui standardizzati peggiorano leggermente, proviamo a fare test di White (per considerare
#le interazioni)

library(car)
ncvTest(modello6) 
library("lmtest")
bptest(modello6)
#c'è ancora eteroschedasticità perchè p-value molto basso

#applichiamo residui di White per ottenere inferenza robusta
library(sandwich)
coeftest(modello6, vcov=vcovHC(modello6))
drop1(modello6, test="F")

######################9. Creazione modello finale###############################
b_filtered <- na.omit(b_filtered)
modello_final <- lm(visibility ~ distance + I(distance^2) + I(distance^3) + City + 
                    pressure + I(pressure^2) + 
                    I(pressure^3) + wind_speed + Weather_group + 
                    Severity + month + day + temperature*Sunrise_Sunset, data = b_filtered)
summary(modello_final)
#R^2 0.8398 --> 0.84
#R^2adj 0.8357 --> 0.83
drop1(modello_final, test = "F")

#######################10. Bootstrap############################################
library(car)
BOOT.MOD=Boot(modello_final, R=1999)
summary(BOOT.MOD, high.moments=TRUE)
Confint(BOOT.MOD, level=c(.95), type="perc")
hist(BOOT.MOD, legend="separate", type="perc")
#Bootstrap conferma la robustezza dei parametri stimati, ad eccezione di alcuni livelli di Weather_Condition, 
#che difatti non risultano significativi nel summary del modello finale.
#Review.Rating è contenuto all'interno di un'interazione che risulta significativa, mentre il livello Winter
#appartiene ad una variabile decisamente significativa. Abbiamo ottenuto un modello robusto.

#plot per verificare miglioramento modello full vs modello finale
par(mfrow=c(1,2))
plot(b$visibility, modello0$fitted.values, ylim=c(0,100), xlim=c(0,100))
abline(0,1,col="red")

plot(b_filtered$visibility, modello_final$fitted.values, ylim=c(0,100), xlim=c(0,100))
abline(0,1,col="red")
par(mfrow=c(1,1))

#plot diagnostici
par(mfrow=c(2,2))
plot(modello0)
plot(modello_final)
par(mfrow=c(1,1))

install.packages("gvlma")
library(gvlma)
gvlma(modello_final)

#FINITO PARTE 1

#######################COSTRUISCO MODELLO LOGISTICO#############################
logistico1 <- glm(Sunrise_Sunset ~ . - Weather_Condition - optimal_group,
                  data=b_filtered,family = "binomial")
summary(logistico1)
drop1(logistico1, test = "LRT")
#i parametri non significativi Junction, Traffic_signal, visibility 

#proviamo a creare un altro modello con model selection
library(MASS)
logistico2 <- stepAIC(logistico1, direction="both")

#AIC=10127.52
#Sunrise_Sunset ~ Severity + distance + City + Crossing + Junction + 
# Stop + month + day + wind_speed + temperature + pressure + 
# visibility + Weather_group

modello_log2 <- glm (Sunrise_Sunset ~ Severity + distance + City + Crossing + Junction + 
                       Stop + month + day + wind_speed + temperature + pressure + 
                       visibility + Weather_group, data=b,family = "binomial")
summary(modello_log2)
drop1(modello_log2, test = "LRT")

plot(modello_log2)
#facciamo anova per verificare che i due modelli abbiano la stessa capacità esplicativa
anova(logistico1, modello_log2, test = "LRT")

#L'alto p-value (0.5994 > 0,05) indica che non c'è una differenza significativa tra i due modelli
#Il cambiamento molto piccolo nella devianza (-0,32568) suggerisce anche che il modello più 
#semplice (modello_log2) si comporta in modo simile al modello più complesso (logistico1)


# Per vedere OR di base
exp(coef(modello_log2))

#OR = 1: nessuna associazione
#OR > 1: associazione positiva
#OR < 1: associazione negativa




