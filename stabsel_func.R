library(car)
library(glmnet)
library(pROC)
library(purrr)


## ---- eval_func --------------------------------------------------------------
# retourne un modèle de regression logistique entrainé
get_glm <- function(X_train, y_train, vars_idx) {
  data <- cbind(as.data.frame(as.matrix(X_train[, vars_idx])),
                data.frame(y = y_train))
  colnames(data) <- c(vars_idx, "y")
  return(glm("y ~ .", data = data, family = "binomial"))
}

# retourne les probabilités des classes
get_predictions <- function(model, X_test, vars_idx) {
  data <- as.data.frame(as.matrix(X_test[, vars_idx]))
  colnames(data) <- vars_idx
  return(predict(model, newdata = data, type = "response"))
}

# retourne le taux d'agrément
get_score <- function(y_test, y_pred) {
  return(sum(y_test == y_pred) / length(y_test) * 100)
}

# estime les classes
# retourne le taux d'agrément
get_performance <- function(model, X_test, y_test, vars_idx) {
  y_pred <- as.integer(get_predictions(model, X_test, vars_idx) > .5)
  return(get_score(y_test, y_pred))
}

# retourne les coefficients (sans l'intercept)
get_coef <- function(model) {
  coefs <- model$coefficients
  coefs <- coefs[setdiff(names(coefs), "(Intercept)")]
  return(coefs)
}

# entraine un modèle de régression logistique non pénalisé (avec un jeu
#   restreint aux variables stables)
# estime les probabilités des classes
# retourne la courbe ROC
get_roc <- function(X_train, y_train, X_test, y_test, vars_idx) {
  model <- get_glm(X_train, y_train, vars_idx)
  y_pred <- get_predictions(model, X_test, vars_idx)
  return(roc(y_test, y_pred))
}
get_rocs <- function(X_train, y_train, X_test, y_test, mods_summary) {
  mods_roc <- lapply(mods_summary$vars.idx,
                     function(vars) get_roc(X_train, y_train, X_test, y_test,
                                            vars))
  names(mods_roc) <- rownames(mods_summary)
  return(mods_roc)
}

# à partir d'une matrice de coefficients (chemins) retourne un data.frame
get_path_as_df <- function(path, colnames) {
  vars <- which(path != 0, arr.ind = TRUE)
  path_df <- data.frame(cbind(path[vars], vars))
  colnames(path_df) <- colnames
  path_df$lambda <- as.integer(path_df$lambda)
  return(path_df)
}


## ---- stab_sel_func ----------------------------------------------------------
# retourne une matrice de 0 / 1 indiquant si le coefficient d'une variable
# est non nul pour un lambda donné
get_selected_vars <- function(X_train, y_train, lambda, sample_size_coef) {
  full_size <- nrow(X_train)
  fold_idx <- sample(full_size, full_size * sample_size_coef)
  X_fold <- X_train[fold_idx, ]
  y_fold <- y_train[fold_idx]
  return(glmnet(X_fold, y_fold, family = "binomial", lambda = lambda)$beta != 0)
}

# retourne un chemin de stabilité
get_stability_path <- function(X_train, y_train, lambda, n_models = 100,
                               sample_size_coef = .5) {
  # liste de n_models matrices de 0 / 1 indiquant la sélection ou non d'une
  # variable pour un lambda donné
  vars_select <- sapply(1:n_models,
                        function(i) get_selected_vars(X_train, y_train, lambda,
                                                      sample_size_coef))
  # passage à une matrice de fréquences des variables
  # (pour des lambda différents)
  return(reduce(vars_select, `+`) / n_models)
}

# retourne un data.frame donnant le score et la taille du support selon le
# seuil de stabilité
run_stability_selection_model <- function(X_train, y_train, X_test, y_test,
                                          stability_path) {
  # on garde la fréquence max sur les différents lambdas de chaque variable
  vars_max_freq <- apply(stability_path, 1, max)
  stability_indices <- seq(.6, 1, by = .05)
  names(stability_indices) <- stability_indices
  # index des variables stables pour différents seuils
  vars_idx <- lapply(stability_indices,
                     function(s_idx) which(vars_max_freq >= s_idx))
  models <- lapply(vars_idx, 
                   function(v_idx) get_glm(X_train, y_train, v_idx))
  # scores des régressions logistiques sans pénalisation associés aux
  # différents seuils
  scores <- sapply(1:length(vars_idx),
                   function(i) get_performance(models[[i]], X_test, y_test, 
                                               vars_idx[[i]]))
  # coefficients
  coefs <- lapply(models, function(model) get_coef(model))
  # NA coefs
  na_coefs <- lapply(coefs, is.na)
  # MAJ coefs, vars
  vars_idx <- lapply(1:length(vars_idx), 
                     function(i) vars_idx[[i]][!na_coefs[[i]]])
  coefs <- lapply(1:length(coefs), function(i) coefs[[i]][!na_coefs[[i]]])
  coefs_p_val <- lapply(vars_idx, function(v_idx) {
    p_val <- Anova(get_glm(X_train, y_train, v_idx))$`Pr(>Chisq)`
    names(p_val) <- names(v_idx)
    return(p_val)
  })
  nzero <- sapply(vars_idx, length)
  return(data.frame(indice = stability_indices, score = scores, nzero = nzero,
                    vars.idx = I(vars_idx), coefs = I(coefs), 
                    coefs.p.val = I(coefs_p_val)))
}


## ---- cv_func ----------------------------------------------------------------
# retourne un data.frame donnant le score et la taille du support selon le
# seuil de stabilité pour n_folds modèles
cv_run_stability_selection_model <- function(X_train, y_train, lambda,
                                             n_folds = 10) {
  # assignation de chaque observation à un fold
  folds_id <- sample(rep(seq(n_folds), length = nrow(X_train)))
  cv_stability_indices_summary <- lapply(1:n_folds, function(fid) {
    whichs <- folds_id == fid
    # restriction du jeu d'entrainement aux observations n'appartenant pas au
    # fold fid
    fold_X_train <- X_train[! whichs, ]
    fold_y_train <- y_train[! whichs]
    # et du jeu de test à celles appartenant à fid
    fold_X_test <- X_train[whichs, ]
    fold_y_test <- y_train[whichs]
    # appel à la fonction générique
    stability_path <- get_stability_path(fold_X_train, fold_y_train, lambda)
    return(run_stability_selection_model(fold_X_train, fold_y_train,
                                         fold_X_test, fold_y_test,
                                         stability_path))
  })
  return(do.call(rbind, cv_stability_indices_summary))
}

# à partir d'un data.frame contenant les résultats de la cross-validation
# retourne les indices optimaux
cv_get_optimal_stability_indices <- function(stability_indices_summary) {
  stability_indices <- unique(stability_indices_summary$indice)
  mean_sd <- lapply(stability_indices, function(sid) {
    scores <-
      stability_indices_summary$score[stability_indices_summary$indice == sid]
    return(data.frame(mean = mean(100 - scores), sd = sd(100 - scores)))
  })
  mean_sd <- do.call(rbind, mean_sd)
  mean_sd$indice <- stability_indices
  # indice de stabilité pour la meilleure classification
  indice_min <- max(mean_sd$indice[which(mean_sd$mean == min(mean_sd$mean))])
  # indice de stabilité pour le meilleur
  # compromis classification / taille du support
  indice_1sd <- max(mean_sd$indice[which(mean_sd$mean <=
                                           mean_sd$mean[[indice_min]] +
                                             mean_sd$sd[[indice_min]])])
  return(list(indice.min = indice_min, indice.1sd = indice_1sd))
}