load("~/Documents/r Wdirectory/Dati_Caria.RData")
summary(dati)
str(dati)
sum(is.na(dati)) #non sono presenti valori mancanti

attach(dati)
library("tidyverse")
library("gridExtra")
#1
tab <-dati %>% 
  count(Arrival) %>%
  mutate(proportion= n/sum(n)) %>%
  mutate(Arrival= reorder(Arrival, proportion))

tab %>%
  ggplot(aes(Arrival, proportion))+ 
  geom_bar(stat = "identity") #La proporzione di biglietti venduti per le  4 diverse destinazioni
#2
tab2 <-dati %>% 
  count(Departure) %>%
  mutate(proportion= n/sum(n)) %>%
  mutate(Departure= reorder(Departure, proportion))


tab2 %>%
  ggplot(aes(Departure, proportion))+ 
  geom_bar(stat = "identity") #percentuale di biglietti venduti in base ai 6 diversi aereoporti di provenienza
#3 
tab3 <- dati %>% 
  count(ModPag) %>%
  mutate(proportion= n/sum(n)) %>%
  mutate(ModPag = reorder(ModPag, proportion))

tab3<-tab3 %>%         
  ggplot(aes(ModPag, proportion))+
  geom_bar(stat = "identity") # quasi il 90% (0.87) dei clienti ha utilizzato 
#la carta di credito per il pagamento

#4
tab4 <- dati %>% 
  count(Luggage) %>%
  mutate(proportion= n/sum(n)) %>%
  mutate(Luggage = reorder(Luggage, proportion))

tab4<- tab4 %>%         
  ggplot(aes(Luggage, proportion))+
  geom_bar(stat = "identity") # il 54% dei clienti ha effettuato una 
#prenotazione senza imbarcare un bagaglio aggiuntivo  
grid.arrange(tab3, tab4, ncol=2)

#5
tab5 <- dati %>% 
  count(PriorBoard) %>%
  mutate(proportion= n/sum(n)) %>%
  mutate(PriorBoard = reorder(PriorBoard, proportion))

tab5<- tab5 %>%         
  ggplot(aes(PriorBoard, proportion))+
  geom_bar(stat = "identity") # il 52% (0.517) dei clienti ha effettuato una 
#prenotazione con imbarco prioritario

#6
tab6 <- dati %>% 
  count(Seat) %>%
  mutate(proportion= n/sum(n)) %>%
  mutate(Seat = reorder(Seat, proportion))

tab6<- tab6 %>%         
  ggplot(aes(Seat, proportion))+
  geom_bar(stat = "identity") # il 51% (0.509) dei clienti ha effettuato una 
#prenotazione del posto a sedere



#7
tab7 <- dati %>% 
  count(Return) %>%
  mutate(proportion= n/sum(n)) %>%
  mutate(Return = reorder(Return, proportion))

tab7<-tab7 %>%         
  ggplot(aes(Return, proportion))+
  geom_bar(stat = "identity") # il 54% (0.537) dei clienti ha effettuato una 
#prenotazione solo andata
grid.arrange(tab5, tab6, tab7, ncol=3)

#8
filter(dati) %>% 
  ggplot(aes(Unit_Ticket, Fidelity, color= Arrival)) +
  geom_point() #il prezzo dei biglietti di Londra è più alto rispetto a quello 
#di Amsterdam e Madrid. Il costo inferiore sembra essere dovuto al fatto che 
#l'indice di fidelizzazione del cliente è basso per quest'ultime, mentre 
#assume valori alti per Londra.

#9
filter(dati) %>% 
  ggplot(aes(Fidelity, Discount, color= Arrival)) +
  geom_point() #i clienti che acquistano i biglietti per Londra risultano essere 
#più fedeli  rispetto a quelli di Amsterdam e Madrid e per questo godono di uno 
#sconto maggiore a dispetto di quest'ultimi

#10
dati  %>% 
  ggplot(aes( y=Fidelity, Departure)) + 
  geom_boxplot(coef=3) +
  geom_jitter(width= 0.1, alpha =0.2)
#l'indice di fedeltà risulta essere mediamente + alto per le prenotazioni 
#con partenza da Venezia 

dati  %>% 
  ggplot(aes( y=Fidelity, Arrival)) + 
  geom_boxplot(coef=3) +
  geom_jitter(width= 0.1, alpha =0.2)
#per quanto riguarda l'areoporto di Arrivo, l'indice di fedeltà risulta essere 
#mediamente più alto per le prenotazioni con arrivo a Londra 



library("lubridate")
dates<-ymd(dati$Data)
dati
dates

month_day_year<-tibble(date = dates, 
                       month = month(dates, label = TRUE),
                       day = day(dates),
                       year = year(dates)) #creo un oggetto tibble per dividere giorno, mese e anno.


head(month_day_year)
#11  graf
dati2= dati 
dati2<- mutate(dati2, month= month_day_year$month ) #inserisco una nuova colonna 
dati2  %>% 
  ggplot(aes(month, Unit_Ticket)) + 
  geom_boxplot(coef=3) +
  geom_jitter(width= 0.1, alpha =0.2)
#le distribuzioni del prezzo del biglietto nel corso dell'anno è molto simile. Il prezzo del biglietto più basso si è registrato a Novembre, il più prezzo più alto è stato registato a maggio, quello più basso a novembre.


#ricodifica dell'indice di fedeltà
dati2$clientFidelity[dati2$Fidelity<=50] = "basso"
dati2$clientFidelity[dati2$Fidelity>50] = "alto"

fedeltà <-as.factor(dati2$clientFidelity)
class(fedeltà)
dati2<- dati2 %>% mutate(Cfidelity= fedeltà)

#eliminazione colonne rindondanti dopo la ricodifica
dati2= dati2[-21] #elimino ClientFidelity
dati2= dati2[-7] #elimino la vecchia colonna Fidelity
dati2= dati2[-19]



#12
tab12 <- dati2 %>% 
  count(Cfidelity) %>%
  mutate(proportion= n/sum(n)) %>%
  mutate(Cfidelity = reorder(Cfidelity, proportion))

tab12 %>%         
  ggplot(aes(Cfidelity, proportion))+
  geom_bar(stat = "identity") # il 66% (0.659) dei clienti ha un 
#indice di fidelizzazione (da 0 a 100)  basso  (minore al 50) mentre il 34 % 
#dei clienti ha un valore alto per questo indice (maggiore di 50)



library(dslabs)


arrival_departures <- count(dati, Departure, Arrival) #creo un nuovo oggetto 
#tibble in cui conto i viaggi in base a aereoporto di partenza e aereoporto 
#di arrivo

arrival_departures
summary(arrival_departures)


#13
p1<-arrival_departures %>% unite(flights, Departure, Arrival) #unisco le colonne 
#Unite tramite la funzione Unite della libreria Tidyr
p1

p1 %>%
  mutate( flights= reorder(flights, n)) %>%
  ggplot(aes(flights, n)) +
  geom_bar(stat= "identity")+
  coord_flip()+
  theme(axis.text.y = element_text(size = 8)) +
  xlab("")
#vediamo che i biglietti maggiormente venduti nel sito sono per il volo Roma-Londra, seguito da Milano-Amsterdam e Roma-Amsterdam



####
index<- dati2$Unit_Ticket; x<- dati2$Unit_Ticket[index]
m <-mean(x) ; s<- sd(x)
c(average=m, sd= s )

norm_dist<- data.frame(x= seq(-4, 4, len=50)*s + m) %>% 
  mutate(density= dnorm(x, m, s))

#14
dati2 %>% ggplot(aes(Unit_Ticket))+
  geom_histogram(aes(y=..density..), binwidth = 1, color = "black")+
  geom_line(aes(x, density), data= norm_dist, lwd=1.5)
##La distribuzione del prezzo dei biglietti venduti sembra essere 
#approssimabile a una normale


###



# PCA
library(FactoMineR) 

attach(dati)
new_dati<- tibble(Unit_Ticket, Fidelity, BookTime, Taxes, Discount, NPax, NBookMonth,NAccWebWeek, NComplaintsYear, NRefundYear,NCancelYear, Seat, PriorBoard, Luggage, Return, Departure, Arrival, ModPag)

new_dati2 <- tibble(Unit_Ticket, Fidelity, BookTime, Taxes, Discount, NPax, NBookMonth,NAccWebWeek, NComplaintsYear, NRefundYear, NCancelYear, Arrival )
res <- PCA(new_dati2,
           scale.unit = TRUE,
           ind.sup = 12000: 23998,
           quanti.sup = 6:10,
           quali.sup = 12,
           ncp=8)


summary(res)
summary(res, ncp=2)

# Descrizione degli assi fattoriali

dimdesc(res)
dimdesc(res, proba=0.01) #seleziona solo  i punti più caratterizzanti (le  variabili)

# Spazio delle variabili
# senza etichette
# label per i punti unità
plot(res, cex=0.8, habillage="Arrival", label = "none")


plot(res, choix="ind", cex=0.8, habillage=12, 
     title="Graph of the individuals", axes=3:4)



# Selezione dei punti unità
plot(res, cex=0.8, habillage=12, select="cos2 0.7", label="none")
# i 5 più importanti (inclusi ex equo)
plot(res, cex=0.8, habillage=12, select="cos2 5", label="none" ) 
# i 5 che contribuiscono di più  (inclusi ex equo)
plot(res, cex=0.8, habillage=12, select="contrib 5", label="none") 

# Selezione delle variabili
plot(res, choix="var", select="contrib 5")

res.hcpc <- HCPC(res)



#suddivisione casuale del dataset in training set e test set 

set.seed(123)
x=model.matrix(Cfidelity ~.,dati2)[,-1]

train = sample(nrow(x), nrow(x)*2/3)
test=(-train)
dati_test = dati2[-train, ] #creo il test set
dati_train = dati2[train,]

y=as.numeric(dati2$Cfidelity)
y.test=as.numeric(y[test])



#lasso

library(glmnet)
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x[train ,],as.numeric(y[train]),alpha=1,lambda=grid) 
plot(lasso.mod)

cv.out= cv.glmnet(x[train ,],as.numeric(y[train]),alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test,])
table(y.test, round(lasso.pred,0)) #778 malclassificati su 8000 --> 0.0971 -->9,7%
mean((lasso.pred - y.test)^2) #MSE 0.07983797

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:19,] 
lasso.coef 
lasso.coef[lasso.coef!=0] #solamente 8 dei coefficenti stimati hanno 
#valore diverso da 0. Per cui il lasso model scelto dalla 
#cross-validation coniene solo 8 variabili

summary(lasso.mod)




#PCR
library("pls")
pcr_model <- pcr(as.numeric(Cfidelity) ~ ., data = dati2, 
                 scale = TRUE, 
                 validation = "CV")

summary(pcr_model)



# Plot the cross validation MSE
validationplot(pcr_model, val.type="MSEP") #direttamente MSE

predplot(pcr_model) #valori osservati vs valori stimati per la prima categoria e la seconda 

coefplot(pcr_model) #il modello con 7 componenti principali ha un coefficiente di regressione negativo molto alto 



# Considerando 7 componenti principali...

pcr_model <- pcr(as.numeric(Cfidelity )~., ncomp = 7,
                 data = dati2,
                 subset= train,
                 scale = TRUE, 
                 validation = "CV")

summary(pcr_model) # 59.19    59.20    59.20    59.24    61.19    61.31    62.18  variabilità percentuale spiegata per la variabile y 
coefplot(pcr_model)


pcr.pred= predict(pcr_model ,x[test,] ,ncomp=7)
predplot(pcr_model) #valori osservati vs valori stimati per la prima categoria, seconda e terza
mean((pcr.pred-y.test)^2) # MSE 0.08625655
table(as.numeric(dati_test$Cfidelity), round(pcr.pred,0)) #777 casi su 8000 sono malclassificati, quindi il modello in un'ottica di previsione funziona bene con un errore di malclassificazione di 794/8000= 0.099 -->9,9% 


###PLS
plsr_model <- plsr(as.numeric(Cfidelity)~., data = dati2 , 
                   subset = train,
                   scale = TRUE, 
                   validation = "CV")




summary(plsr_model)
#  58.90    58.90    58.90    58.92    60.93    61.13  pcr --> percentuale di variabilità spiegata per la variabile y 

#  61.44    64.67    65.91    66.09    66.19    66.22  pls -> la percentuale di variabilità spiegata per la variabile y col modello pls è maggiore
coefplot(plsr_model)
lm2<-lm(as.numeric(dati_train$Cfidelity)~plsr_model$scores[,1:10])
summary(lm2) #Adjusted R-squared:  0.657

# Plot the root mean squared error
validationplot(plsr_model)

# Plot the cross validation MSE
validationplot(plsr_model, val.type="MSEP")

# Plot the R2
validationplot(plsr_model, val.type = "R2") #con 3 componenti principali supero l'80 %

# You can plot the predicted vs measured values 
# using the predplot function 

predplot(plsr_model)



# considerando solo le prime 7 componenti...

plsr_model <- plsr(as.numeric(Cfidelity)~., ncomp = 7,
                   data = dati2,
                   subset= train,
                   scale = TRUE, 
                   validation = "CV")


summary(plsr_model)# la percentuale di varianza in fedelt con 5 componenti 66.23.  
#Risulta più alta della percentuale spiegata col modello finale dal modello PCR

coefplot(plsr_model) 

lm2<-lm(as.numeric(dati_train$Cfidelity)~plsr_model$scores[,1:7])
summary(lm2) #Adjusted R-squared:  0.657 rimane quasi invariato ma questa 
#volta sono state utilizzate solamente le prime 7 componenti principali



pls.pred=predict(plsr_model,x[test,],ncomp=7)


mean((pls.pred-y.test)^2) # MSE = 0.07939584
table(as.numeric(dati_test$Cfidelity), round(pls.pred,0)) # 775 malclassificati su 8000. 775/8000= 0.0968 --> 9,68%





