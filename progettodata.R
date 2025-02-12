setwd("~/Desktop/Progetto data mining")
install.packages("corrplot") 
install.packages("cggplot2")
install.packages("gridExtra")
install.packages("heplots")
install.packages("MASS")
install.packages("car")
install.packages("class")
install.packages("caret")
install.packages("ROCR")
install.packages("tidyverse")

library(ISLR) 
library(class)
library(recipes)
library(caret)
library(ISLR)

data <- read.csv2("garments_worker_productivity.csv", header = T, sep = ",", dec = ".")
View(data)

str(data)
data$quarter<-as.character(data$quarter)
data$department<-as.character(data$department)
data$day<-as.character(data$day)

#Descrizione variabili
#Variabili presenti nel dataset e spiegazione
#01 data : Data in MM-GG-AAAA
#02 quarter : Una porzione del mese.4 blocchi da sette giorni(una settimana), piu un blocco con i giorni rimanenti alla fine del mese
#03 dipartimento : Reparto associato all'istanza (0=sweing & 1=finishing)
#04 giorno : Giorno della settimana
#05 team_no : numero di team associato all'istanza
#06 targeted_productivity : Produttività target stabilita dall'Autorità per ogni squadra per ogni giorno.
#07 smv : Standard Minute Value, è il tempo assegnato per un compito.
#08 wip : Lavori in corso. Include il numero di elementi non finiti per i prodotti.
#09 over_time : Rappresenta la quantità di ore di lavoro straordinario effettuate da ciascun team in minuti.
#10 incentive : Rappresenta l'ammontare dell'incentivo finanziario (in BDT) che consente o motiva un particolare corso d'azione.
#11 idle_time : la quantità di tempo in cui la produzione è stata interrotta per diversi motivi.
#12 idle_men : Il numero di lavoratori inattivi a causa dell'interruzione della produzione.
#13 no_of_style_change : numero di cambiamenti nello stile di un particolare prodotto
#14 no_of_workers : numero di lavoratori in ogni squadra
#15 Produttività_effettiva : la percentuale effettiva di produttività che è stata


data <- data[,-1]
#Nella variabile 
which(data$quarter != "Quarter1" & data$quarter != "Quarter2" & data$quarter != "Quarter3" & data$quarter != "Quarter4")

for(i in 1:1197){
  if(data$quarter[i]=="Quarter1"){
    data$quarter[i]=1} else if(data$quarter[i]=="Quarter2"){
      data$quarter[i]=2} else if(data$quarter[i]=="Quarter3"){
        data$quarter[i]=3} else if(data$quarter[i]=="Quarter4"){
          data$quarter[i]=4} else{
            data$quarter[i]=5}
}

for (i in 1:1197){
  if(data$department[i] == "sweing"){
    data$department[i] = 0 } else {
      data$department[i] = 1}
}


for(i in 1:1197){
  if(data$day[i]=="Monday"){
    data$day[i]=1} else if(data$day[i]=="Tuesday"){
      data$day[i]=2} else if(data$day[i]=="Wednesday"){
        data$day[i]=3} else if(data$day[i]=="Thursday"){
          data$day[i]=4} else if (data$day[i]=="Friday"){
            data$day[i]=5} else {
              data$day[i]=6}
}



str(data)

set.seed(411)

trn_idx <- sample(nrow(data), 0.8*nrow(data))
trn <- data[trn_idx,]
tst <- data[-trn_idx,]

trn_s_idx <- sample(nrow(trn), 0.6*nrow(trn))
trn_s <- trn[trn_s_idx,]
vld <- trn[-trn_s_idx,]

#################################

summary(trn_s)
i_na <- which(is.na(trn_s$wip))



#cor(trn_s$wip[-i_na],trn_s$actual_productivity[-i_na])

#presenza di na elimino 
#RICORDA DI ELIMINARE ANCHE IN VLD E TEST
trn_s <- trn_s[,-7]

#trasformazione in numeric
trn_s$actual_productivity<-as.numeric(trn_s$actual_productivity)
trn_s$quarter <- as.numeric(trn_s$quarter)
trn_s$department <- as.numeric(trn_s$department)
trn_s$day <- as.numeric(trn_s$day)
trn_s$smv <- as.numeric(trn_s$smv)
trn_s$incentive <- as.numeric(trn_s$incentive)
trn_s$number_of_style_change<- as.numeric(trn_s$number_of_style_change)
trn_s$team <- as.numeric(trn_s$team)
trn_s$over_time <- as.numeric(trn_s$over_time)
str(trn_s)
###########################################################
#0-1 trn_s
for(i in 1:574){
  if(trn_s$actual_productivity[i]<=0.7){
    trn_s$actual_productivity[i]=0} else {
      trn_s$actual_productivity[i]=1}
}
#######################################################
#analisi correlazioni
corr<- cor(trn_s[,-13])
mat.cor <- as.matrix(corr)
?rm
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)


#eliminazione variabili altamente correlate
for (i in 1:nrow(mat.cor)){
  for (j in 1:ncol(mat.cor)){
    if(abs(mat.cor[i,j])>=0.9 & mat.cor[i,j]!=1){
      mat.cor[i,j]=0
    }
  }
}



#elimino :
#no_of workers perchè spiegata da smv e department
#idle_time troppi 0 RICORDA:CALCOLA %
#idle_man troppo 0
idle_timezeri<-which(trn_s$idle_time==0)
percidletimezeri<-565/574
idle_menzeri<-which(trn_s$idle_men==0)
percidlemenzeri<-565/574
trn_s <- trn_s[,-c(9,10,12)]

boxplot(trn_s[,1:9])

par(mfrow=c(1,2))
boxplot(trn_s$day) 
hist(trn_s$day) 

boxplot(trn_s$quarter)
hist(trn_s$quarter)

boxplot(trn_s$over_time, main="over_time")
hist(trn_s$over_time, main="over_time")

boxplot(trn_s$smv)
hist(trn_s$smv)

boxplot(trn_s$smv)
hist(trn_s$no_of_style_change)

boxplot(trn_s$targeted_productivity)

#Normalizzazione
for(i in (1:ncol(trn_s))){
  trn_s[,i] = (trn_s[,i]-min(trn_s[,i]))/(max(trn_s[,i])-min(trn_s[,i]))
}
View(trn_s)

boxplot(trn_s$day) 
hist(trn_s$day) 

boxplot(trn_s$quarter)
hist(trn_s$quarter)

boxplot(trn_s$over_time, main="over_time")
hist(trn_s$over_time, main="over_time")

boxplot(trn_s$smv)
hist(trn_s$smv)

############################################################
#0-1 validation e test
for(i in 1:383){
  if(vld$actual_productivity[i]<=0.7){
    vld$actual_productivity[i]=0} else {
      vld$actual_productivity[i]=1}
}

str(vld$actual_productivity)

for(i in 1:240){
  if(tst$actual_productivity[i]<=0.7){
    tst$actual_productivity[i]=0} else {
      tst$actual_productivity[i]=1}
}

for(i in 1:957){
  if(trn$actual_productivity[i]<=0.7){
    trn$actual_productivity[i]=0} else {
      trn$actual_productivity[i]=1}
}

tst$actual_productivity

prop.table(table(trn_s$actual_productivity))

prop.table(table(vld$actual_productivity))

prop.table(table(tst$actual_productivity))
##########################################################


plot_box_all <-list()
variables <- colnames(trn_s)[1:(dim(trn_s)[2]-1)]

library(ggplot2)

trn_s$actual_productivity<-as.factor(trn_s$actual_productivity)

for (i in variables){
  plot_box_all[[i]] <- ggplot(trn_s, aes_string(x = "actual_productivity", y = i, col = "actual_productivity", fill = "actual_productivity")) + 
    geom_boxplot(alpha = 0.2) + 
    theme(legend.position = "none") + 
    scale_color_manual(values = c("blue", "red")) 
  scale_fill_manual(values = c("blue", "red"))
}

library(gridExtra)

do.call(grid.arrange,c(plot_box_all, nrow = 2))

variables_1 <- variables[1:4]

par(mfrow = c(2, 2))
for(i in variables_1) {
  qqnorm(trn_s[trn_s$actual_productivity == 1, i], main = i); qqline(trn_s[trn_s$actual_productivity == 1, i], col = 2)
}

variables_2 <- variables[5:8]

par(mfrow = c(2, 2))
for(i in variables_2) {
  qqnorm(trn_s[trn_s$actual_productivity == 1, i], main = i); qqline(trn_s[trn_s$actual_productivity == 1, i], col = 2)
}

par(mfrow = c(1, 1))

variables_3 <- variables[9:9]

for(i in variables_3) {
  qqnorm(trn_s[trn_s$actual_productivity == 1, i], main = i); qqline(trn_s[trn_s$actual_productivity == 1, i], col = 2)
}

variables_1 <- variables[1:4]

par(mfrow = c(2, 2))
for(i in variables_1) {
  qqnorm(trn_s[trn_s$actual_productivity == 0, i], main = i); qqline(trn_s[trn_s$actual_productivity == 0, i], col = 2)
}

variables_2 <- variables[5:8]

par(mfrow = c(2, 2))
for(i in variables_2) {
  qqnorm(trn_s[trn_s$actual_productivity == 0, i], main = i); qqline(trn_s[trn_s$actual_productivity == 0, i], col = 2)
}

par(mfrow = c(1, 1))

variables_3 <- variables[9:9]

for(i in variables_3) {
  qqnorm(trn_s[trn_s$actual_productivity == 0, i], main = i); qqline(trn_s[trn_s$actual_productivity == 0, i], col = 2)
}

plot_density <- list()

for(i in variables){
  plot_density[[i]] <- ggplot(trn_s, aes_string(x = i, y = "..density..", col = "actual_productivity")) + 
    geom_density(aes(y = ..density..)) + 
    scale_color_manual(values = c("blue", "red")) + 
    theme(legend.position = "none")
}

do.call(grid.arrange, c(plot_density, nrow = 4))

pvalue_shapiro <- matrix(0, nrow = (dim(trn_s)[2]-1), ncol = 2)
rownames(pvalue_shapiro) = colnames(trn_s)[-10]
colnames(pvalue_shapiro) = c(1, 0)

for (i in colnames(trn_s)[-10]){
  pvalue_shapiro[i, 1] <- shapiro.test(trn_s[trn_s$actual_productivity == 1, i])$p.value
  pvalue_shapiro[i, 0] <- shapiro.test(trn_s[trn_s$actual_productivity == 0, i])$p.value
}
pvalue_shapiro

#MODELLI TRAINING SMALL
library(MASS)
model_logit <- glm(actual_productivity ~., data = trn_s, family = binomial)
summary(model_logit)#AIC con tutte le variabili = 596.97
step.model <- stepAIC(model_logit, direction = "both", trace = FALSE)
summary(step.model)#AIC= 593.12
anova(step.model, test="Chisq")
library(mvinfluence)
influencePlot(step.model)
sub_trn_s_1 <- trn_s[-1092,]

model_logit2 <- glm(actual_productivity ~., data = sub_trn_s_1, family = binomial)
summary(model_logit2)
step.model <- stepAIC(model_logit2, direction = "both", trace = FALSE)
summary(step.model)#l'AIC è migliorato di poco = 592.02
anova(step.model, test="Chisq")
###############################################################
#ANALISI NEL VALIDATION
#faccio cleaning che avevo eseguito prima nel trn_s e normalizzo.
#trasformazione in numeric
str(vld)
vld$quarter <- as.numeric(vld$quarter)
vld$department <- as.numeric(vld$department)
vld$day <- as.numeric(vld$day)
vld$smv <- as.numeric(vld$smv)
vld$incentive <- as.numeric(vld$incentive)
vld$number_of_style_change<- as.numeric(vld$number_of_style_change)
vld$team <- as.numeric(vld$team)
vld$over_time <- as.numeric(vld$over_time)
str(vld)
vld <- vld[,-7]
vld <- vld[,-c(9,10,12)]
str(vld)
str(trn_s)
for(i in (1:ncol(vld))){
  vld[,i] = (vld[,i]-min(trn_s[,i]))/(max(trn_s[,i])-min(trn_s[,i]))
}
#KNN SU VALIDATION
K <- c(1,3,5,7, 9, 13,15, 17,19, 21)
accuracy_knn_models <- NULL

# Calcolo accuratezza per ogni valore di K
## knn necessit� di espicitare covariate e risposta in due data frame diversi
# smaller training data
library(class)
X_default_smaller_trn = trn_s[, -10] 
y_default_smaller_trn = trn_s$actual_productivity
# validation data
X_default_vld = vld[, -10] 
y_default_vld = vld$actual_productivity
calc_class_err = function(actual, predicted) { mean(actual != predicted) }

calc_class_err(actual = y_default_vld,
               predicted = knn(train = X_default_smaller_trn,
                               test = X_default_vld, 
                               cl = y_default_smaller_trn, k =5))


k_to_try = 1:25
err_k = rep(x = 0, times = length(k_to_try))
for (i in seq_along(k_to_try)) {
  pred = knn(train = (X_default_smaller_trn),
             test = (X_default_vld), cl = y_default_smaller_trn,
             k = k_to_try[i])
  err_k[i] = calc_class_err(y_default_vld, pred) }
plot(err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20,
     xlab = "k, number of neighbors", ylab = "classification error", 
     main = "(Test) Error Rate vs Neighbors")
# Minimum error
abline(h = min(err_k), col = "darkorange", lty = 3)

## Errore minimo
min(err_k)
## Valori di k che danno l'errore minimo
which(err_k == min(err_k))
## Scelta del k più alto
max(which(err_k == min(err_k)))

table(y_default_vld)
#test con il k migliore= 5
pred = knn(train = (X_default_smaller_trn),
           test = (X_default_vld), cl = y_default_smaller_trn,
           k = max(which(err_k == min(err_k))), prob = TRUE)

# test error rate = 0.2950392
test_error_rate = calc_class_err(actual = y_default_vld, predicted = pred)
test_error_rate

test_error_rate*100

# confusion matrix o matrice di confusione
conf_matrix = table(pred, y_default_vld)
conf_matrix

# Accuracy= 70,49%
accuracy = function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

accuracy(conf_matrix)

100-accuracy(conf_matrix)
#sensibilità=99.6%
#specificità=0%



# Regressione logistica - calcolo delle probabilità a posteriori
library(nnet)
sub_vld_s_1 <- vld[-1150,]
model_logitvld <- glm(actual_productivity ~., data = sub_vld_s_1, family = binomial)
step.model <- stepAIC(model_logitvld, direction = "both", trace = FALSE)
summary(step.model)
pred_logit <- predict(step.model, vld[, -10], type = "response")
pred_logit
# Trasformazioni probabilità in classe con sogli 0.5
pred_logit_class <- ifelse (pred_logit > 0.5, 1, 0) 
pred_logit_class
table("predicted" = pred_logit_class,
      "actual" = vld$actual_productivity)
acc <- sum(diag(table(vld$actual_productivity, pred_logit_class)))/length(pred_logit_class)
acc

#accuracy=0.781
#sensibility=92%
#specificità=48.3%
#CALCOLIAMO CURVE ROC E AUC per logistic
pred_roclogit <- prediction(pred_logit, vld[, 10])
perf_logit <- performance(pred_roclogit,"tpr","fpr")
auc_logit <- performance(pred_roclogit, measure = "auc")@y.values
auc_logit
#auc = 0.8212

#calcoliamo curva roc e auc per knn
prob_knn <- attributes(pred)$prob
# Trasformazione delle proporzioni (proporzione di elementi su K che appartengono alla classe 0) che il KNN resitutisce in probabilità
prob_knn <- 2*ifelse(pred == "0", 1-prob_knn, prob_knn) - 1
pred_rocknn <- prediction(prob_knn, vld[, 10])
perf_knn<- performance(pred_rocknn,"tpr","fpr")
auc_knn <- performance(pred_rocknn, measure = "auc")@y.values
auc_knn
#auc_knn = 0.5581


par(mfrow = c(1, 2))
plot(perf_logit, colworize = TRUE, main = "Regressione Logistica")
plot(perf_knn, colorize = TRUE, main = "14-NN")

###############################################################

#########################################################
trn$quarter <- as.numeric(trn$quarter)
trn$department <- as.numeric(trn$department)
trn$day <- as.numeric(trn$day)
trn$smv <- as.numeric(trn$smv)
trn$incentive <- as.numeric(trn$incentive)
trn$number_of_style_change<- as.numeric(trn$number_of_style_change)
trn$team <- as.numeric(trn$team)
trn$over_time <- as.numeric(trn$over_time)
str(trn)
trn <- trn[,-7]
trn <- trn[,-c(9,10,12)]
for(i in (1:ncol(trn))){
  trn[,i] = (trn[,i]-min(trn[,i]))/(max(trn[,i])-min(trn[,i]))
}
#########################################################
#analisi sul test
str(tst)
tst$quarter <- as.numeric(tst$quarter)
tst$department <- as.numeric(tst$department)
tst$day <- as.numeric(tst$day)
tst$smv <- as.numeric(tst$smv)
tst$incentive <- as.numeric(tst$incentive)
tst$number_of_style_change<- as.numeric(tst$number_of_style_change)
tst$team <- as.numeric(tst$team)
tst$over_time <- as.numeric(tst$over_time)
str(tst)
tst<- tst[,-7]
tst <- tst[,-c(9,10,12)]
str(tst)
for(i in (1:ncol(tst))){
  tst[,i] = (tst[,i]-min(tst[,i]))/(max(tst[,i])-min(tst[,i]))
}
#KNN SU tst
K <- c(1,3,5,7, 9, 13,15, 17,19, 21)
accuracy_knn_models <- NULL

# Calcolo accuratezza per ogni valore di K
X_default_trn = trn[, -10] 
y_default_trn = trn$actual_productivity
X_default_tst = tst[, -10] 
y_default_tst =tst$actual_productivity

max(which(err_k == min(err_k)))

best_k = max(which(err_k == min(err_k)))
pred = knn(train = (X_default_trn),
           test = (X_default_tst), cl = y_default_trn,
           k = 9, prob = TRUE)


# test error rate
test_error_rate = calc_class_err(actual = y_default_tst, predicted = pred)
test_error_rate

test_error_rate*100
#accuracy test = 72.5%
#test erro rate=27.5
# confusion matrix o matrice di confusione
conf_matrix = table(pred, y_default_tst)
conf_matrix

# Accuracy
accuracy = function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

accuracy(conf_matrix)

100-accuracy(conf_matrix)
#sensibilità=0.89
#specificità=0.31


###########################
#test su logistic
pred_logit <- predict(step.model, tst[, -10], type = "response")
pred_logit_class <- ifelse (pred_logit > 0.5, 1, 0) 
table("predicted" = pred_logit_class,
      "actual" = tst$actual_productivity)
acc <- sum(diag(table(tst$actual_productivity, pred_logit_class)))/length(pred_logit_class)
acc

#accuracy=0.7625
#sensibility=0.885
#specificità=0.448
# Regressione logistica - calcolo delle probabilità a posteriori

