# Esercitazione riassuntiva regressione lineare 

# Si consideri il dataset "exercise_30_10.csv", disponibile sulla piattaforma di 
# e-learning, contenente 200 osservazioni della variabile indipendente Y e di 12 
# feature X_1, X_2, ..., X_12. Nel dataset non sono presenti osservazioni con
# valori NA. Si vuole determinare un modello di regressione lineare che leghi Y 
# alle feature. Dividere il dataset in una parte di train (80% delle osservazioni) 
# e una parte di test (20% delle osservazioni). 

# Importo le librerie 
library(corrplot)
library(leaps) # libreria per redsubsets

# Importo il Dataset nel progetto
ds = read.csv("/Users/-/Desktop/magistrale/Data_Science/exercise_30_10.csv",header = T ,na.strings = "?")
print(head(ds))
print(dim(ds))

# Definisco il seed di "casualità" per dividere train e test.
# Se non scegliessi un valore io, ogni volta che eseguo lo script
# avrei una divisione in train e test differente. 
# E quindi non potrei testare correttametne il modello.
set.seed(1902)

# Ora passo alla divisione del dataset in train e test
#   1. Prendo le righe del dataset 
n = nrow(ds)
#   2. Calcolo l'80% da queste e creo il train_index
#      Uso la funzione floor in modo da non avere problemi se il numero risulta 
#      decimale (es 160.8). La funzione floor arrotonda all'interno inferiore.
train_size = floor(0.8*n)
#   3. Estraggo ora casualmente gli indici di train usando la funzione sample
#      generando una sequanza di train_size con i numeri che vanno da 1 a n
#      dove n sono le righe totali del dataset n = nrow(ds)
train_index = sample(seq_len(n), size = train_size)

#   4. Creo ora i dataset, assegnando gli indici
train = ds[train_index,] 
test = ds[-train_index,]

# Check delle dimensioni
print(nrow(train))  # 160
print(nrow(test))   # 40

# a. Analizzare la correlazione tra le variabili del dataset. In particolare,
#    commentare quali feature potrebbero essere più influenti ai fini del calcolo 
#    del modello di regressione. 

#   1. Calcolo il carrplot con method="number" che è più facile da leggere
#      e verifico le correlazioni. 
#      Osservo i valori che sono o molto vicini a -1 o molto vicini a 1
#      Non considereo quelli vicini a zero
cor_matrix = cor(train)
dev.new()
corrplot(cor_matrix, method = "number",type = "upper", tl.col = "black", tl.cex = 0.7)

#   Il plot ci fa notare subito che le componenti: 1,2,3,7 
#   sono le più correlate con Y rispettivamente (0.59,0.31,0.36,0.33)
#   quindi solo loro che influiranno maggiormente

#   Notiamo una forte correlazione tra le variabili indipendenti 2,4,5

#   8,9 sono quelle meno influenti.


# b. Calcolare i coefficienti del modello di regressione, utilizzando tutte le 12
#    feature, usando sia le funzioni builtin di R che la relazione esplicita per
#    il calcolo dei coefficienti. 

#   b.1 Calcolo dei coeffcienti con le funzioni di R
#     1. la funzione lm prende come paramentri la variabile dipendente, tilde X
#        dove le X rappresentano le variabili indipendenti.
#       DATO CHE DALLA TRACCIA DOBBIAMO CALCOLARE Y RISPETTO A TUTTE LE 12 FEATURES IL COMANDO:
coefficienti_lm = lm(y~.,data=train)
summary(coefficienti_lm)

# Il summary ci da informazioni molto importanti. Andiamole a vedere:

# Residuals: i residui sono gli errori di stima y_i meno y cappuccio di i.
#            Per essi si cerca la simmetria, si vuole che la Median sia molto vicina a zero,
#            in questo caso -0.02884 è ottimo, in quanto il modello non sta ne sottostimando ne sovrastimando

#           1Q e 3Q sono il primo e terzo quartile dovrebbero essere simili in valore assoluto, 
#           in questo caso sono -0.74661, 0.63964. Quindi ragionevolmente simili in val assoluto

#           Stesso discorso per min e max, che devono essere simili in val assoluto. -2.42814, 2.48550.

# Verdetto: I residui sembrano ben distribuiti e centrati su zero.

# Coefficients: analizziamo le categorie

#             Estimate: è la stima di Beta
#             Intercep: Rappresenta l'intercetta, beta zero
#             Std. Error: Misura l'incertezza sulla stima del coefficiente, più è piccolo più la stima è precisa
#             t value: è il rapporto Estimate/Std.Error. Un t-value grande (in positivo o negativo)
#                      suggerisce che la feature è importante
#             Pr(>|t|): mi definisce quando la feature è importante, e rientra nel range del 95%.
#                      se il valore è minore di 0.05 (5%) vicino a zero, allora è significativa,
#                      altrimenti risulta inutile. Il summary lo fa in automatico, assegna 
#                      degli asterischi, (***) (*) sono quelle statisticamente significative

#                      In questo caso vediamo che x4,x5 sono molto alte, mentre x8 risulata in dubbio in quanto
#                      Non è significativa al 5%, ma lo sarebbe al 10%. È un "forse".
#                      Quindi x4,x5,x8 sono perfetti candidati per essere eliminati.

#           "Residual standard error: 1.009 on 147 degrees of freedom." è la stima della devizione standard del rumore,
#           più è basso, meglio è. In questo caso le predizioni si discostano dal valore reale di 1.009 unità

#           Multiple R-squared:  0.9857,	Adjusted R-squared:  0.9846. Mostrano quando spiega il modello (98.46%)
#           Guarda sempre l'Adjusted il quale è calcolato con la penalizzazione delle features, cioè
#           "Multiple R-squared" aumenta sempre se aggiungi feature, mentre "Adjusted R-squared" aumenta solo se
#           le features aggiunte sono significative.

#           "F-statistic: 846.8 on 12 and 147 DF,  p-value: < 2.2e-16" controlla se il modello è inutile
#           se il valore è basso (come in questo caso) il modello è significativo.

# CALCOLO A MANO CON RELAZIONE ESPLICITA
#   La relazione esplicita ci dice che B=(X^T X)^-1 * (X^T Y)
#   La matrice X ha la prima colonna di tutti 1 e poi le restanti. Y è la matrice delle risposte

#   Definizione Matrice X
#   La funzione vede la colonna specificata a sinistra delle ~ e la ignora.
#   Costruisce quindi la matrice X mettendo 1 nella prima colonna e poi i restanti dati
X_train = model.matrix(y~.,data=train)

#   Definizione Vettore Y
Y_train = train$y

XTX = t(X_train) %*% X_train # (X^T X)
XTY = t(X_train) %*% Y_train # (X^T Y)

coefficienti_manual = solve(XTX) %*% XTY # (XTX)^-1 * XTY
print(coefficienti_manual)

#    Verificare che i risultati siano coerenti.
# I Risultati risultano coerenti con quelli calcolati tramite i comandi R

#    Calcolare l'MSE di training del modello ottenuto, calcolando esplicitamente
#    i valori di Y predetti dal modello di regressione stimato.

#    MSE_train = 1/n sommatoria[ (Yi - Ycappuccio_i)^2 ] 

# Per farlo Calcolo prima gli Y predetti (Y cappuccio)
Y_predetti = X_train %*% coefficienti_manual

# N.B. Quando R esegue il comando lm() genera anche gli Y_predetti o Y_cappuccio
#      per accedervi basta fare nome_variabile$fitted.values, nel nostro caso
#      coefficienti_lm$fitted.values

# ora calcolo i redisui (Y - Ycappuccio)
residui = Y_train - Y_predetti

# adesso devo fare la media al quadrato e ottengo l'MSE di training
MSE_train = mean(residui^2)
print(MSE_train)


# c. Effettuare una selezione delle feature applicando la backward stepwise 
#    selection, utilizzare C_p come metrica per scegliere il numero di feature.

#   Per effettuare la backward/forward stepwise, chiamo il metodo regsubsets() 
#   al quale specifico su che dataset agire (data=train), il metodo (method="backward" o method="forward")
#   e il numero massimo di feature da considerare (nvmax=)
backward_selection = regsubsets(y~.,data=train,method = "backward", nvmax = 12)
#ci salviamo il summary, attraverso il quale possiamo accedere a dei valori importanti, come cp,rsq,rss,adjr2,bic
summary_backward = summary(backward_selection)
print(summary_backward$cp)

# Plottimo il grafico per capire chi scegliere
# Creiamo un grafico per scegliere il modello migliore
dev.new()
plot(1:12, summary_backward$cp, type = 'b', 
     xlab = "Numero di Parametri (p')", 
     ylab = "C_p di Mallows",
     main = "Backward Selection con C_p")
# Aggiungiamo la linea y=x (dove Cp = p')
abline(a = 0, b = 1, col = "red", lty = 2)

# Dal grafico notiamo che il modello con RSS basso e numero di parametri ragioneve è 8.
# Quindi il più ottimale secondo il Criterio C_p

# Vediamo quali sono le 8 features che il C_p ha scelto e capiamo se:
#   - Ci sono le features che al punto a abbiamo definito come forti?
#   - Ci sono le features che al punto a abbiamo definito inutili?
f_scelte = summary_backward$which[8,]
print(f_scelte)


# _**_ Dalla stampa notiamo che abbiamo: Intercetta,x1,x2,x3,x6,x7,x10,x11,x12
#      Possiamo affermare che le features definite al punto a come forti sono presenti (1,2,3,7)
#      e che le features che avevamo detto che erano inutili non sono proprio presenti (8,9)

#    Calcolare l'MSE di training del modello ottenuto, calcolando esplicitamente
#    i valori di Y predetti dal modello di regressione stimato.

# Prendiamo il modello che abbiamo appena scelto con C_p e cominciamo a costruire le matrici
# Con coef(backward_selection,8) gli diciamo di considerare il modello M8 e di mettere in colonna le features
coef_back = matrix(coef(backward_selection,8),ncol=1)

# N.B. All'interno è anche presenta l'intercetta, dobbiamo toglierla per il momento. 
# In quanto ci serve estrarre i "nomi" delle colonne, così da poter calcolare y cappuccio

#Estraiamo i nomi delle colonne togliendo l'intercetta [-1]
vars_back_names=names(coef(backward_selection,8))[-1]

# Costruiamo X e prediamo le y -> y_cappuccio = X*B
y_stimati_mod8 = cbind(1, as.matrix(train[,vars_back_names,drop=FALSE])) %*% coef_back

#Calcolo MSE -> 1/n sommatoria((y-y_cappuccio)^2)
MSE_train_mod8 = mean((train$y - y_stimati_mod8)^2)
print(MSE_train_mod8)

#    Si commenti sulle feature scelte in relazione all'analisi di correlazione 
#    effettuata al punto a.
#Già fatto vedi sopra _**_


# d. Calcolare l'MSE di test ottenuto con i due modelli di regressione calcolati
#    ai punti b e c. La differenza tra i due risultati è significativa? 
#    Commentare opportunamente il comportamento osservato.
# 

# Prendo i coefficienti del modello calcolato sui dati di test al punto b
coefficienti_lm_per_test = matrix(coefficienti_lm$coefficients, ncol=1)
# Calcolo la matrice delle X, dalla matrice di test escudendo la prima colonna
x_matrix_full_test = cbind(1,as.matrix(test[,-1]))
#Calcolo Y predette
y_test_full_model = x_matrix_full_test %*% coefficienti_lm_per_test
# Calcolo MSE con modello che ah tutte le feature
MSE_total_f = mean((test$y-y_test_full_model)^2)
print(MSE_total_f)

# Calcolo MSE di test con Modello ad 8 parametri
# Ho gia i nomi delle colonne che mi servono in vars_back_names
X_test_matrix = cbind(1, as.matrix(test[,vars_back_names,drop=FALSE]))

# Calcolo di Y_cappuccio sul test set
y_mod8_prediction = X_test_matrix %*% coef_back

MSE_test_mod8 = mean((test$y-y_mod8_prediction)^2)
print(MSE_test_mod8)

# La Questo risultato era atteso. Il modello completo, che includeva tutte le 12 feature, 
# stava quasi certamente soffrendo di overfitting sul training set: stava cioè imparando non solo le relazioni vere, 
# ma anche il "rumore" introdotto da feature inutili
#Eliminando queste feature rumorose, il modello backward è diventato più semplice (più parsimonioso). 
# Confrontando i due Errori Quadratici Medi (MSE) calcolati sul test set, 
#osserviamo che l'MSE del modello ottenuto dalla backward selection (1.004353) è più basso di quello del modello completo (1.025878).
# Questa semplicità gli ha permesso di generalizzare meglio ai dati "mai visti" del test set, commettendo, in media, 
# un errore di previsione più piccolo.
