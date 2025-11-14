
# Esercitazione riassuntiva regressione lineare

# Si consideri il dataset "exercise_30_10.csv" contenente 200 osservazioni della
# variabile dipendente Y e di 12 regressori. Nel dataset non sono presenti
# osservazioni con valori NA. Si vuole determinare un modello di regressione
# lineare che leghi Y ai regressori.
# Dividere il dataset in una parte di train (80% delle osservazioni) e una parte
# di test (20% delle osservazioni).

# a. Analizzare la correlazione tra le variabili. In base all'analisi effettuata,
#    commentare quali regressori potrebbero essere più influenti ai fini del calcolo
#    del modello di regressione.

# b. Calcolare i coefficienti del modello di regressione, usando sia le funzioni
#    builtin di R che usando la relazione esplicita per il calcolo dei coefficienti.
#    Verificare che i risultati siano coerenti.
#    Calcolare l'MSE di training del modello ottenuto, calcolando esplicitamente
#    i valori della y predetta dal modello.

# c. Effettuare una selezione dei regressori applicando la backward stepwise selection,
#    utilizzare Cp come metrica per scegliere il numero di regressori.
#    Calcolare l'MSE di training del modello ottenuto, calcolando esplicitamente
#    i valori della y predetta dal modello.
#    Si commenti sul numero di regressori scelti in relazione all'analisi di correlazione
#    effettuata al punto a.

# d. Calcolare l'MSE di test ottenuto con i due modelli di regressione calcolati ai punti b e c.
#    La differenza tra i due risultati è significativa? Commentare il comportamento osservato.

setwd("~/GitHub Repos/UNISA-Vault/MastersDegree/Data Science/Exercises")

install.packages('corrplot')
install.packages('leaps')
library('corrplot')
library('leaps')

df = read.csv("exercise_30_10.csv")
n = nrow(df)

set.seed(1902)
train_idxs = sample(seq_len(n), size = 0.8 * n)

train_data  = df[train_idxs, ]
test_data   = df[-train_idxs, ]


## Punto a.

cor_matrix = cor(df)
corrplot(cor_matrix, method='number')
plot(df)

# Dall'analisi di correlazione tra la variabile dipendente e quelle indipendenti
# possiamo notare come quelli che potrebbero essere i più significativi per il
# nostro modello di regressione lineare multiplo potrebbe essere quello composto
# da x1, x2, x3, x7 (abbiamo preso quelli con corr(a,b)>=0.30)
#
# Un altro elemento che possiamo notare dal grafico di correlazione, è la
# presenza di una certa correlazione lineare tra coppie di variabili, come
# x2:x4, x2:x5, x4:x5.
#
# Inoltre vediamo anche come tra i vari regressori, quelli meno significativi e
# che nel modello non influenzerebbero la regressione, ci sono x8 e x9


## Punto b.

# Built-in R functions to evaluate model coefficients
lin_mod       = lm(y~., data=train_data)
lin_mod_summ  = summary(lin_mod)

# Using OLS
Mx = as.matrix(cbind(1, train_data[, colnames(train_data) != 'y']))
vy = as.matrix(train_data[, c('y')])
betas = solve(t(Mx) %*% Mx) %*% t(Mx) %*% vy

# Coefficients evaluated in both ways are equals!

# Calcola l'MSE (Mean Squared Error) del modello lineare sui dati di training
# utilizzando i valori fitted già calcolati dal modello
MSE_lin_mod_train = mean((train_data$y - lin_mod$fitted.values)^2)

# Genera previsioni usando la funzione predict() sui dati di training
predict_lin_mod_train = predict(lin_mod, newdata = train_data)

# Estrae i coefficienti del modello lineare e li trasforma in una matrice colonna
coef_lm = matrix(lin_mod$coefficients, ncol=1)

# Calcola manualmente le previsioni moltiplicando la matrice di design 
# (che include l'intercetta come colonna di 1) per i coefficienti
# Seleziona tutte le colonne tranne 'y' (le variabili predittive)
predict_lin_mod_train = cbind(1, as.matrix(train_data[, colnames(train_data) != 'y'])) %*% coef_lm

# Ricalcola l'MSE usando le previsioni calcolate manualmente
MSE_lin_mod_train = mean((train_data$y - predict_lin_mod_train)^2)  # = 0.9724531


## Punto c.
lin_mod_back      = regsubsets(y~., data=train_data, nvmax = 12, method = 'backward')
lin_mod_back_summ = summary(lin_mod_back)

dev.new()
plot(lin_mod_back_summ$cp, xlab="Number of variables", ylab="C_p", type="l")
points(which.min(lin_mod_back_summ$cp), min(lin_mod_back_summ$cp), col="red", cex=2)

# scegliamo il modello con 8 parametri (da 9 in sù non portano un sostanziale miglioramento delle performance)

coef_back = matrix(coef(lin_mod_back, 8), ncol=1)
vars_back = names(coef(lin_mod_back, 8))[-1]

predict_back_test = cbind(1, as.matrix(test_data[, vars_back, drop=FALSE])) %*% coef_back
MSE_lin_mod_back_test = mean((test_data$y - predict_back_test)^2)


## Punto d.

# Possiamo vedere come tra i due ci sia una differenza in termini di MSE,
# in particolare notiamo:
#  - MSE modello completo - MSE = 0.972
#  - MSE modello backward - MSE = 0.851
#
# Quindi con meno regressori, otteniamo performance un errore quadratico medio
# minore. E soprattutto, possiamo vedere come siano stati inseriti nel secondo
# modello dei regressori tutti altamente significativi (nonostante dalla nostra
# analisi non fossero presenti).
#
# La differenza, però, tra i due MSE ottenunti è piccola.
# Ciò è dovuto al fatto, ovviamente, che i regressori non inclusi nel secondo
# modello non erano significativi nella sua risposta.
#
# L'indice adjusted-R^2 tra i due rimane pressochè invariato:
# rispettivametne 0.985 vs. 0.9845






