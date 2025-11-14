library(corrplot)
library(leaps)
# Obiettivo: Stimare la qualità del vino
ds = read.csv("/Users/lorenzo/Desktop/magistrale/Data_Science/EserciziR/wine_quality/winequality-red.csv", sep = ";",na.strings = "?")

print(dim(ds))

if(anyNA(ds)){
  ds = na.omit(ds)
}

print(dim(ds))
print(summary(ds))

n=nrow(ds)
# divisione in train e test
set.seed(100)
#calcolo 80%
train_size = floor(0.8*n)

train_index = sample(seq_len(n),size = train_size)
ds_train = ds[train_index,]
ds_test = ds[-train_index,]

# controllo le correzioni
cor_matrix = cor(ds_train)
dev.new()
corrplot(cor_matrix, method = "number",type = "upper", tl.col = "black", tl.cex = 0.5)

# notiamo:
#  correlazione che hanno rilevanza con quality le seguenti feature:
#  volatile.acidity, alcohol, sulphates, citric.adic.
#  Completamente scorrelate invece sono: residual.sugar,free.sulfur.dioxide, pH

#calcoliamo i beta stimati -> B = (Xt X)^-1 * X^t * Y

# Metodo con funzioni di R
coeff_lm = lm(quality ~ ., data=ds_train)
summary(coeff_lm)

# Metodo casereccio - ma non troppo
# Creo la matrice X_train
X_train_matrix = model.matrix(quality ~ ., data=ds_train)
# Creo la matrice Y_train
Y_train_matrix = ds_train$quality

# Eseguo i calcoli separatamente
XTX = t(X_train_matrix) %*% X_train_matrix
XTY = t(X_train_matrix) %*% Y_train_matrix
# Calcolo i coefficienti
coefficienti_caserecci = solve(XTX) %*% XTY
print(coefficienti_caserecci)

# Calcolo MSE_train = sommatoria ((Y_train - Y_predetto)^2)
Y_train_predetto = X_train_matrix %*% coeff_lm$coefficients

MSE_train = mean((ds_train$quality - Y_train_predetto)^2)
print(MSE_train)

#    Effettuare una selezione delle feature applicando la backward stepwise 
#    selection, utilizzare C_p come metrica per scegliere il numero di feature.
nfeatures = ncol(ds_train)-1
backward_selection = regsubsets(quality~.,data=ds_train,method = "backward", nvmax = nfeatures)
sum_back = summary(backward_selection)
print(sum_back)
dev.new()
plot(1:11, sum_back$cp,xlab="features",ylab="Cp",type="b")
abline(a=0,b=1,col="red")

# Dal grafico del C_p si nota un netto miglioramento del modello fino a 5 variabili. 
# Il minimo globale si trova in corrispondenza di 7 feature, che scegliamo come modello ottimale. 
# Va notato che il modello a 6 feature rappresenta una valida alternativa parsimoniosa, avendo un C_p
# quasi identico. Oltre le 7 feature, la curva tende a risalire o stabilizzarsi,
# indicando che le variabili aggiuntive sono ridondanti 
# e rischiano di aumentare la varianza dell'errore senza ridurre il bias.

# Estrapoliamo il modello e vediamo che features sono state scelte.
f_scelte_back = sum_back$which[7,]
print(f_scelte_back)

# Intercept - volatile.acidity - chlorides - free.sulfur.dioxide - total.sulfur.dioxide
# pH - sulphates - alcohol 

# Possiamo notare che all'interno del modello M7 sono presenti delle feature che abbiamo
# ritenuto inutili guardando il grafico di correlazione. Il fatto che ora risulti rilevante
# è perchè nella matrice di correlazione si guarda la correlazione di Y con la singola feature.
# Quindi singolarmente non è rilevante, mentre nel complesso sicuramente aggiunge informazioni  
# significative quando combinato con altri regressori.

#    Calcolare l'MSE di training del modello ottenuto, calcolando esplicitamente
#    i valori di Y predetti dal modello di regressione stimato.

# Prendiamo i coefficienti e mettiamoli nella rispettiva matrice (B)
coef_back_matrix = matrix(coef(backward_selection,7),ncol = 1)
# estraiamo i nomi delle colonne presenti nel modello 7 per creare la matrice X
var_back_names = names(coef(backward_selection,7))[-1]
# Y = X %*% B
y_stimati_mod7 = cbind(1, as.matrix(ds_train[,var_back_names,drop=FALSE])) %*% coef_back_matrix
# Calcolo MSE_train_mod7
MSE_train_mod7 = mean((ds_train$quality - y_stimati_mod7)^2)

# d. Calcolare l'MSE di test ottenuto con i due modelli di regressione calcolati
#    ai punti b e c. La differenza tra i due risultati è significativa? 
#    Commentare opportunamente il comportamento osservato.
# 

# - MSE Test modello con tutte le features.
X_test_full_model = model.matrix(quality~.,data=ds_test)
y_test_full_model = X_test_full_model %*% coeff_lm$coefficients

MSE_test_full_model = mean((ds_test$quality-y_test_full_model)^2)
print(MSE_test_full_model)

# - MSE Test con modello M7
X_test_mod7 = cbind(1, as.matrix(ds_test[,var_back_names,drop=FALSE]))
y_test_stimati_mod7 = X_test_mod7 %*% coef_back_matrix

MSE_test_mod7 = mean((ds_test$quality-y_test_stimati_mod7)^2)
print(MSE_test_mod7)

# Gli MSE calcolati sul Test Set per il modello completo e il modello ridotto (M7) 
# risultano estremamente simili (es. 0.42 vs 0.42). Questo dimostra che le feature eliminate 
# dalla backward selection erano effettivamente ridondanti e 
# non contribuivano alla capacità predittiva del modello.