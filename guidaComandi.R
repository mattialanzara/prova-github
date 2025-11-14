# ==============================================================================
# 🆘 MASTER CHEAT SHEET - DATA SCIENCE & REGRESSIONE
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARICAMENTO E PULIZIA DATI
# ------------------------------------------------------------------------------
library(MASS)       # Dataset esempio
library(ISLR)       # Dataset esempio
library(leaps)      # Regsubsets (Best Subset, Fwd, Bwd)
library(corrplot)   # Grafici correlazione

# A. Caricamento
# ds = read.csv("file.csv", sep=";", na.strings="?")
ds = Hitters # Esempio

# B. Pulizia NA
if(anyNA(ds)){
  ds = na.omit(ds)
}

# C. Rimozione Colonne NON Numeriche (FONDAMENTALE per le matrici)
cols_numeriche = sapply(ds, is.numeric)
ds = ds[, cols_numeriche]

# Controllo finale
print(dim(ds))
# head(ds)

# ------------------------------------------------------------------------------
# 2. TRAIN / TEST SPLIT
# ------------------------------------------------------------------------------
set.seed(123)            # Importante per riproducibilità
n = nrow(ds)
train_pct = 0.75         # Percentuale Train

train_size = floor(train_pct * n)
train_idx  = sample(seq_len(n), size = train_size)

ds_train = ds[train_idx, ]
ds_test  = ds[-train_idx, ]

# ------------------------------------------------------------------------------
# 3. ANALISI CORRELAZIONE (Training Set)
# ------------------------------------------------------------------------------
cor_matrix = cor(ds_train)
# dev.new()
corrplot(cor_matrix, method="number", type="upper", tl.cex=0.7)
# Cerca: Correlazioni alte tra X e Y (buone). Correlazioni alte tra X e X (Multicollinearità).

# ------------------------------------------------------------------------------
# 4. MODELLO FULL: STANDARD (lm) & MATRICIALE (OLS)
# ------------------------------------------------------------------------------

# Definisci il nome della tua variabile Y
nome_y = "Salary"  # <--- CAMBIA QUESTO CON IL NOME DELLA TUA Y

# Creiamo la formula dinamica (es. "Salary ~ .")
form_full = as.formula(paste(nome_y, "~ ."))

# ==================================================
# A. METODO STANDARD (Funzione lm di R)
# ==================================================
# È il metodo che useresti nel mondo reale. Ti dà p-value, R^2, ecc.

mod_lm = lm(form_full, data = ds_train)

# Stampa il riassunto (P-value, R^2, Test F)
print(summary(mod_lm))

# Estrai i coefficienti
beta_lm = coef(mod_lm)
print("Coefficienti (lm):")
print(beta_lm)

# Calcolo MSE Train rapido (usando i residui salvati nel modello)
mse_train_lm = mean(mod_lm$residuals^2)
print(paste("MSE Train (lm):", mse_train_lm))


# ==================================================
# B. APPROCCIO MATRICIALE MANUALE (Richiesto esame)
# ==================================================
# Obiettivo: Calcolare Beta = (X^T X)^-1 X^T Y

# 1. Creazione Matrici
# model.matrix gestisce l'intercetta (colonna di 1) e i fattori (Dummy vars)
# ATTENZIONE: Usa la formula form_full, NON usare data$colonna
X_train_full = model.matrix(form_full, data = ds_train)
Y_train      = ds_train[[nome_y]]

# 2. Calcolo OLS
XTX = t(X_train_full) %*% X_train_full
XTY = t(X_train_full) %*% Y_train

# solve() calcola l'inversa
beta_matrix = solve(XTX) %*% XTY

print("Coefficienti OLS Manuali:")
print(t(beta_matrix))

# 3. Verifica uguaglianza (Controllo Sanity Check)
# Se la differenza è quasi zero, hai fatto tutto giusto!
diff_coef = sum(abs(beta_lm - beta_matrix))
if(diff_coef < 1e-5) {
  print("VERIFICA OK: I coefficienti manuali corrispondono a quelli di lm()!")
} else {
  print("ATTENZIONE: C'è una discrepanza nei coefficienti. Controlla la matrice X.")
}

# 4. Calcolo MSE Train Manuale
y_hat_train = X_train_full %*% beta_matrix
mse_train_matrix = mean((Y_train - y_hat_train)^2)
print(paste("MSE Train (Matrix):", mse_train_matrix))
# ------------------------------------------------------------------------------
# 5. SELEZIONE FEATURE: REGSUBSETS (Best, Fwd, Bwd)
# ------------------------------------------------------------------------------
# nfeature = numero colonne - 1 (la Y)
n_feat = ncol(ds_train) - 1

# SCEGLI IL METODO:
# method = "exhaustive" -> Best Subset (Default)
# method = "forward"    -> Forward Selection
# method = "backward"   -> Backward Selection

reg_model = regsubsets(form_full, data = ds_train, 
                       nvmax = n_feat, 
                       method = "exhaustive") # <--- 

summ_reg = summary(reg_model)

# ------------------------------------------------------------------------------
# 6. GRAFICI PER SCELTA MODELLO (Cp, BIC, AdjR2)
# ------------------------------------------------------------------------------
# dev.new()
par(mfrow=c(2,2))

# CP Mallows (Cerca Minimo)
plot(summ_reg$cp, xlab="Features", ylab="Cp", type="b")
points(which.min(summ_reg$cp), min(summ_reg$cp), col="red", pch=19)

# BIC (Cerca Minimo)
plot(summ_reg$bic, xlab="Features", ylab="BIC", type="b")
points(which.min(summ_reg$bic), min(summ_reg$bic), col="red", pch=19)

# Adj R2 (Cerca Massimo)
plot(summ_reg$adjr2, xlab="Features", ylab="Adj R2", type="b")
points(which.max(summ_reg$adjr2), max(summ_reg$adjr2), col="red", pch=19)

par(mfrow=c(1,1))

# Grafico Scacchiera (Leaderboard)
# dev.new()
plot(reg_model, scale="bic", main="Best Models (BIC)")

# ------------------------------------------------------------------------------
# 7. SELEZIONE FEATURE: STEPWISE con step() (Per BIC)
# ------------------------------------------------------------------------------
# Richiede un modello lm di partenza
mod_start = lm(form_full, data=ds_train)
n_rows = nrow(ds_train)

# k = log(n) per BIC, k = 2 per AIC
step_model = step(mod_start, direction="both", k = log(n_rows))
summary(step_model)

# ------------------------------------------------------------------------------
# 8. CALCOLO MSE TEST (Procedura Manuale Robusta)
# ------------------------------------------------------------------------------
# Questa procedura funziona per QUALSIASI modello (Best, Fwd, Step, etc.)
# Basta sapere il numero di feature scelto (K)

# --- STEP A: Crea la matrice X del TEST SET completa ---
X_test_full = model.matrix(form_full, data = ds_test)
Y_test      = ds_test[[nome_y]]

# --- STEP B: Estrai coefficienti e fai predizione ---

# ESEMPIO: Supponiamo che il modello scelto sia quello con 8 feature
K_scelto = 8 

# 1. Estrai Beta
coef_scelti = coef(reg_model, K_scelto) 

# 2. Estrai Nomi Variabili
nomi_vars = names(coef_scelti)

# 3. Seleziona colonne corrispondenti dalla matrice X Test
X_test_ridotta = X_test_full[, nomi_vars]

# 4. Moltiplica
y_pred_test = X_test_ridotta %*% coef_scelti

# 5. Calcola MSE
mse_test_finale = mean((Y_test - y_pred_test)^2)

print(paste("MSE Test Modello M", K_scelto, ": ", mse_test_finale, sep=""))


# ------------------------------------------------------------------------------
# 9. CONFRONTO FINALE TRA DUE MODELLI
# ------------------------------------------------------------------------------
# Se devi confrontare Best Subset (M8) vs Forward (M9), 
# ripeti lo STEP B per entrambi e usa un IF.

# mse_best = ...
# mse_fwd  = ...

# if(mse_best < mse_fwd){
#   print("Vince Best Subset")
# } else {
#   print("Vince Forward")
# }

# ==============================================================================
# SOS ERRORI COMUNI
# ==============================================================================
# 1. Error: requires numeric matrix/vector arguments
#    -> Controlla se stai moltiplicando una lista (coeff_lm) invece di un vettore.
#    -> Usa as.matrix() o coeff_lm$coefficients.

# 2. Error: non-conformable arguments
#    -> Dimensioni sbagliate. 
#    -> Hai dimenticato l'intercetta nella matrice X? 
#    -> Hai incluso la colonna Y nella matrice X per sbaglio?

# 3. Best Subset si ferma a 8 variabili
#    -> Hai dimenticato nvmax = ncol(ds)-1 dentro regsubsets.