## ---- stabsel_func -----------------------------------------------------------
source("stabsel_func.R")


## ---- stabsel_data -----------------------------------------------------------
source("stabsel_data.R")


## ---- lasso ------------------------------------------------------------------
lasso_mod <- cv.glmnet(X.train, y.train, type.measure = "class",
                       family = "binomial")
lasso_lambda <- lasso_mod$lambda
lasso_pred <- predict(lasso_mod, newx = X.test, type = "class",
                      s = lasso_mod$lambda.1se)[, 1]
lasso_vars_idx <- which(lasso_mod$glmnet.fit$beta[,
                          which(lasso_mod$lambda == lasso_mod$lambda.1se)] != 0)
lasso_coefs <- get_coef(X.train, y.train, lasso_vars_idx)
# non NA
lasso_na_coefs <- is.na(lasso_coefs)
# MAJ coefs, vars
lasso_vars_idx <- lasso_vars_idx[!lasso_na_coefs]
lasso_coefs <- lasso_coefs[!lasso_na_coefs]
lasso_coefs_p_val <- Anova(get_glm(X.train, y.train, 
                                   lasso_vars_idx))$`Pr(>Chisq)`
names(lasso_coefs_p_val) <- names(lasso_vars_idx)
lasso_summary <- data.frame(indice = NA,
                            score = get_score(y.test, lasso_pred),
                            nzero = length(lasso_vars_idx),
                            vars.idx = I(list(lasso_vars_idx)),
                            coefs = I(list(lasso_coefs)),
                            coefs.p.val = I(list(lasso_coefs_p_val)))
rownames(lasso_summary) <- "Lasso"


## ---- stab_sel ---------------------------------------------------------------
stability_path <- get_stability_path(X.train, y.train, lasso_lambda)
stab_sel_summary <- run_stability_selection_model(X.train, y.train, X.test,
                                                  y.test, stability_path)
rownames(stab_sel_summary) <- paste0("StabSel-", round(stab_sel_summary$indice,
                                                       digits = 2))


## ---- cv_stab_sel ------------------------------------------------------------
cv_stab_sel_summary <- cv_run_stability_selection_model(X.train, y.train,
                                                        lasso_lambda)
best_indices <- cv_get_optimal_stability_indices(cv_stab_sel_summary)
indice_min_idx <- which(stab_sel_summary$indice == best_indices$indice.min)
indice_1sd_idx <- which(stab_sel_summary$indice == best_indices$indice.1sd)
rownames(stab_sel_summary)[[indice_min_idx]] <-
  paste0(rownames(stab_sel_summary)[[indice_min_idx]], " (cv_min)")
rownames(stab_sel_summary)[[indice_1sd_idx]] <-
  paste0(rownames(stab_sel_summary)[[indice_1sd_idx]], " (cv_1sd)")


## ---- paths ------------------------------------------------------------------
# Construction des data.frames des chemins
# De régularisation
reg_path <- get_path_as_df(lasso_mod$glmnet.fit$beta,
                           c("beta", "var", "lambda"))

## De sélection
stab_path <- get_path_as_df(stability_path, c("freq", "var", "lambda"))

# Jointure avec les fréquences/probabilités de sélection maximales pour la
# coloration
max_freq <- by(stab_path, list(stab_path$var),
               function(df) data.frame(var = unique(df$var),
                                       max_freq = max(df$freq)))
max_freq <- as.data.frame(do.call(rbind, max_freq))
max_freq$var <- as.factor(max_freq$var)
max_freq$max_freq <- as.numeric(max_freq$max_freq)
reg_path <- merge(reg_path, max_freq, by = "var", all.x = TRUE)
reg_path[is.na(reg_path$max_freq), "max_freq"] <- 0
stab_path <- merge(stab_path, max_freq, by = "var")

# cast en factor avec un ordre de levels precis pour que les chemins des
# variables les plus stables apparaissent biens
reg_path$var <- factor(reg_path$var,
                       levels = unique(reg_path[order(reg_path$max_freq),
                                                "var"]))
stab_path$var <- factor(stab_path$var,
                        levels = unique(stab_path[order(stab_path$max_freq),
                                                  "var"]))


## ---- save -------------------------------------------------------------------
save(lasso_summary, stab_sel_summary, file = "data/summaries.Rdata")
save(reg_path, stab_path, file = "data/paths.Rdata")