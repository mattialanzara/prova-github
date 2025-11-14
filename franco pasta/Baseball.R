# Exercise on subset selection for linear regression
library(ISLR) 
library(leaps)
# In this exercise we use the Hitters dataset from the ISLR library. We wish to 
# predict a baseball player’s Salary on the basis of statistics associated 
# with the player's performance during the year.
# Remove from the dataset the features that are not numeric, as well as any 
# observation with NA values.

# Carico il dataset
ds = Hitters
summary(ds)
# Controllo com'è formato
head(ds)
dim(ds)

# Rimozioni delle righe con valori NA.
# Controlla se ci sono e nel caso le elmina
if(anyNA(ds)){
  ds = na.omit(ds)
}
# Tramite sapply controlla se ogni colonna è numerica, e genera un vettore TRUE/FALSE
colonne_numeriche = sapply(ds,is.numeric)
# Usiamo i valori true per costruire il dataset con solo colonne numeriche
ds = ds[,colonne_numeriche]
# Controlliamo la dimensione del dataset
dim(ds)

# a. Split the dataset in a train part and a test part. Use 75% of the 
#    observations for training, and the remaining 25% for test.

# Imposoto il seed, savlo il numero di righe e quello di colonne
set.seed(100)
n = nrow(ds)
c = ncol(ds)
# Calcolo il 75% del numero di righe
train_size = floor(0.75*n)
# Genero la sequenza del train_size e creo i rispettivi dataset
train_index = sample(seq_len(n), size = train_size)
ds_train = ds[train_index,]
ds_test = ds[-train_index,]

# b. Compute the regression model, using the training set, relating the Salary 
#    variable to the available features. For this task apply best subset selection
#    se the "regsubsets" function, which is part of the leaps library. Check the
#    summary to understand how the features are selected.

# Ricordiamo che Best Subset selection usa tutte le combinazioni possibili, e trova
# il modello mogliore per ogni sottoinsieme di feature (con 1, con 2, ...)
# si usa sempre il comando regdubsets() ma non si specifica il metodo, che di default è exhaustive
# cioè best subset.

#   *N.B.* Specifica sempre nvmax (cioè il numero massimo di feature da valutare) perchè il defaul value è 8
#       e per esempio col nostro dataset che ne ha 17, salteremmo dei sottoinsiemi.

best_subset = regsubsets(Salary~., data=ds_train, nvmax = c-1)
sum_best_subset = summary(best_subset)

# Il summary mostra quali features sono state scelte per ogni modello.
# La riga indica la dimensione del modello e gli asterischi indicano quali feature sono state inserite
# nel ripettivo modello in valutazione
# In particolare notiamo che alcune sono presenti in dei modelli, e poi spariscono per i successivi.


# c. Choose the best subset of features using estimates of the test error based 
#    on C_p, AIC, BIC and adjusted R^2. Produce suitable plots showing the value
#    of the performance indicators vs the number of features. Additionally, use 
#    the builtin "plot.regsubsets" function and comment on the plot it produces. 

# Per risolvere bisogna inanziattuto creare i grafici per CP,BIC, e adjusted R^2
# (Non creiamo AIC poichè è uguale Cp nella regressione, infatti la funzione regsubsets non lo calcola manco).
dev.new()
plot(sum_best_subset$cp, xlab="N features", ylab="Cp val", type="b")

dev.new()
plot(sum_best_subset$bic, xlab="N features", ylab="BIC val", type="b")

dev.new()
plot(sum_best_subset$adjr2, xlab="N features", ylab="adjusted R^2", type="b")

# Dai tre grafici si evince che più o meno coincidono quasi tutti coerentemente sul modello.
# In particolare tutti e 3 mostrano un modello performante con 8 features (M8).
# - Cp mostra una decrescita da 1 a 7, un valore minimo in 8 e poi una ricrescita all'aumentare delle feature
# - BIC mostra sempre una decrescita da 1 a 7, il valore minimo in 8 e poi una repentina salita con l'aumentare
#   delle feature. Cioò avviene proprio perchè BIC è molto severo quando vengono aggiunte delle features.
# - Infine adjusted R^2 mostra il valore migliore in 8, subito dopo di esso abbiamo una sorta di appiattimento;
#   ciò indiche che il modello va in overfitting.

# Creazione del grafico richiesto con bestsubset
# scegliamo una sola scala di visione, poichè è impossibili visualizzarli tutti.
# Abbiamo scelto BIC in questo caso. 
dev.new()
plot(best_subset, scale="bic", main ="Best subset selection using BIC")

# Il grafico è rappresentato come una leaderboard. In alto va il modello migliore
# Il quadratino nero ci mostra se quella feature è stata o meno scelta per quel modello.
# I colori dei quadratini ci mostrano anche quanto sono forti o meno quelle features,
# per esempio wakls risulta essere molto forte, come anche putsOut. 
# Mentre errors e years molto meno o nulla
# Come vediamo il modello migliore è quello con 8 features. 

# d. Compute the regression model using the forward stepwise selection. 
#    Compare the obtained model against the best subset selection one, verifying
#    if there are any differences in the two approaches for a given number of 
#    features (use the "coef" function to look at the coefficients of the model).
#    As in point c, choose the best subset using the test error estimates. 

coeff_forward = regsubsets(Salary~.,data=ds_train, nvmax = c-1, method = "forward")
sum_coeff = summary(coeff_forward)
print(sum_coeff)

dev.new()
plot(1:16,sum_coeff$bic,xlab="N features",ylab="BIC",type="b",main="Forward BIC")

# Tramite la forward notiamo una cosa particolare. I modelli risultati migliori sono 
# M5 e M9. A differenza del bestSubset in cui era M8.
# Infatti tra 1-4 abbiamo una repentina discesa, 5 punto piu basso, poi risalita fino a 8
# a 9 si riscende di nuovo nel punto quasi piu basso (circa come il modello 5) e poi abbiamo una salita repentina.
# La scelta puo ricadere sul modello M5 qualora si volessero meno feature e un'ottima predizione.
# Oppure sul modello M9 se si vuole dare al modello qualche informazione in più.

# Mostriamo per variabili per capire quali sono state scelte.
# Stamperemo entrambi
feature_mod5 = sum_coeff$which[5,]
feature_mod9 = sum_coeff$which[9,]
print(feature_mod5) #Atbat-Hits-Walks-CRBI-PutOuts 
print("##############")
print(feature_mod9) #AtBat-Hits-HmRun-Walks-CHmRun-CRuns-CRBI-CWalks-PutOuts

# Ora la traccia chiede di valutare se per uno stesso numero di feature, esempio 8,
# il metodo del best subset e il metodo forward scelgono le stesse feature.
# Per controllare uso la funzione names cominata con coef

# modello subset con 8 feature
print("FEATURES SUBSET: ")
print(names(coef(best_subset,8)))
# modello forwrd con 8 feature
print("FEATURES FORWARD: ")
print(names(coef(coeff_forward,8)))

# Notiamo che le feature variano di 1, "CWALKS" in subset e "CRBI" in forward
# Cio accade perchè il forward non valuta tutte le combinazioni ma fa M_k+1 = M_k + X_i
# cioè aggiunge al modello attuale la feature che tra tutte minimizza l'errore.
# Mentre subset valuta tutte le possibili combinazioni.

dev.new()
plot(1:16,sum_coeff$cp,xlab="Features",ylab = "CP",main="FORWARD CP",type = "b") 
dev.new()
plot(1:16,sum_coeff$adjr2,xlab="Features",ylab = "R^2",main="FORWARD adjuster R^2",type="b") 

# Plottando tutti e 3 i grafici, noto che il modello migliore è proprio M9
# la conferma arriva dal grafico adjusted R^2 che segna per M9 il vlaore migliore.
# Ovviamente M9 come gia detto sopra risulta il migliore in BIC e CP

# e. Compute the regression model using the step() function, which applies the 
#    BIC to choose the best subset (use "extractAIC" to compute the BIC of the 
#    selected model).

# L'uso della funzione step permette di vedere passaggio per passaggio le variabili che fanno
# aumentare o diminuire il BIC. Lo fa per step, quindi stamperà un sacco di informazioni ad ogni passaggio.

# La funzione ha bisogno di un modello già fatto per partire.
# Prendiamo il modello con tutte le feature sul training_set
full_model_train = lm (Salary~.,data=ds_train)

# L'accortezza da fare è la seguente:
#   Il metodo step usa di default l'AIC che ha penalità k = 2 (Formula AIC = 1/n  (RSS + 2*d*(sigma_cappuccio)^2))
#   La traccia chiede di usare BIC che ha come penalità k = log(n), dove n sono le righe del dataset.
#   Formula BIC = 1/n  (RSS + log(n)*d*(sigma_cappuccio)^2) --> d è il numero di parametri
#   Quindi dovremmo impostare noi il k

n_train = nrow(ds_train)

# Chiamo la funzione step
step_bic_model = step(full_model_train, direction = "both", k = log(n_train))
summary(step_bic_model)

# Prendiamo il valore di BIC finale usando extractAIC come richiesto dalla traccia
# La funzione resittuisce un array in cui il primo sono i gradi di liberta e il secondo il valore BIC
bic_values = extractAIC(step_bic_model, k = log(n_train))
print(paste("Gradi di libertà:", bic_values[1]))
print(paste("Valore BIC finale:", bic_values[2]))

#dopo la stampa noteremo che quando la funzione di step ha finito, mostra il modello
# "vincitore", con le feature scelte.
# Notiamo che il modello differeisce di qualche feature da quello che abbiamo calcolato 
# sopra con la subset. In particolare:
# mod_step = Salary ~ AtBat + Hits + Runs + Walks + CHits + CRuns + CRBI + CWalks + PutOuts

#(vedi grafico "leaderboard") mod_m8_subset = AtBat + Hits + HmRun + Walks + CHmRun + CRuns + CWalks + PutOuts

# f. For all the subsets computed using best subset selection and forward 
#    stepwise, compute the MSE on the test set. Check if the results are 
#    consistent with the ones from the analysis using the test MSE estimates.

# Lo faccio solamente per il miglior modello subset e il miglior forward

# creo la matrice totale con tutte le features del DS (ovviamente senza le non numeriche, che abbiamo tolto all'inizio)
X_test_full = model.matrix(Salary~., data=ds_test)
# Creo la matrice Y_test
Y_test = ds_test$Salary

coef_best_sub_selection = coef(best_subset, 8)
print(coef_best_sub_selection)

# Definisco ora X con solo le features del modello m8 del subset
# Prendo i nomi delle colonne che mi servono
vars_best = names(coef_best_sub_selection)
X_test_best_subset = X_test_full[,vars_best]

# Creo le predizioni -> X %*% B
Y_test_pred_best_subset = X_test_best_subset %*% coef_best_sub_selection
# Calcolo MSE_test_best_subset
MSE_test_best_subset = mean((Y_test - Y_test_pred_best_subset)^2)
print(MSE_test_best_subset)


# MSE_test_forward
# Prendo i nomi delle features che mi servono
coef_fwd = coef(coeff_forward,9)
vars_best_forward = names(coef_fwd)
X_test_forward = X_test_full[,vars_best_forward]

#Calcolo le predizioni
Y_test_prediction_forward = X_test_forward %*% coef_fwd
MSE_test_forward = mean((Y_test - Y_test_prediction_forward)^2)
print(MSE_test_forward)

# MSE_test_best_subset risulta minore, questo dimostra la forza di provare tutte le combinazioni.
# Tuttavia non è sempre usabile con poich+ effettua 2^p combinazioni.
# Il forward tuttavia non si comporta male nella totalità poichè la differenza è minima.
# Resta il fatto che ha comunque un feature in piu. 
# In conclusione, il modello Best Subset M8 è il vincitore definitivo dell'analisi, 
# garantendo il minor errore di previsione con il minor numero di variabili.

# HOMEWORK: add to the comparison the backward stepwise method, and use also the function "stepAIC".

