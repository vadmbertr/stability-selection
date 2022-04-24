## ---- stabsel_func -----------------------------------------------------------
source("stabsel_func.R")


library(ggplot2)


## ---- fig_paths --------------------------------------------------------------
get_fig_stab <- function(stab_path) {
  # meme couleur de .8 à 1
  stab_path$color <- sapply(stab_path$max_freq, function(f) min(f, .8))
  ggplot(data = stab_path) +
    geom_line(aes(x = lambda, y = freq, group = var, color = color)) +
    scale_color_gradient(low = "yellow", high = "red", limits = c(.6, .8),
                         guide = "none") +
    labs(x = "Lambda index", y = "Probabilité de sélection")
}

get_fig_reg <- function(reg_path) {
  # meme couleur de .8 à 1
  reg_path$color <- sapply(reg_path$max_freq, function(f) min(f, .8))
  ggplot(data = reg_path) +
    geom_line(aes(x = lambda, y = beta, group = var, color = color)) +
    scale_color_gradient(low = "yellow", high = "red", limits = c(0.6, .8),
                         guide = "none") +
    labs(x = "Lambda index", y = "Coefficient")
}


## ---- fig_roc ----------------------------------------------------------------
get_fig_roc <- function(mods_roc) {
  ggroc(mods_roc) +
    labs(x = "Spécificité", y = "Sensibilité") +
    scale_color_discrete(name = "Modèle") +
    theme_bw()
}


## ---- fig_score --------------------------------------------------------------
get_fig_nz_score <- function(mods_summary) {
  ggplot(data = mods_summary) +
    geom_point(aes(x = nzero, y = score, colour = rownames(mods_summary),
                   shape = rownames(mods_summary))) +
    scale_shape_manual(name = "Modèle", values = 1:nrow(mods_summary)) +
    scale_color_discrete(name = "Modèle") +
    labs(x = "Taille du support", y = "Taux d'agrément") +
    theme_bw()
}


## ---- fig_coef ---------------------------------------------------------------
get_coef_df <- function(mods_summary, coef_name) {
  df <-  lapply(rownames(mods_summary), function(mod_name) {
    r <- mods_summary[mod_name, ]
    f <- data.frame(model = mod_name, coef = r[, coef_name])
    colnames(f) <- c("model", coef_name)
    rownames(f) <- NULL
    return(f)
  })
  df <- do.call(rbind, df)
  return(df)
}

get_fig_coef <- function(mods_summary) {
  df <- get_coef_df(mods_summary, "coef")
  ggplot(data = df) +
    geom_boxplot(aes(y = coef, fill = model)) +
    scale_fill_discrete(name = "Modèle") +
    scale_x_continuous(breaks = NULL) +
    labs(y = "Coefficients", x = "Modèle") +
    theme_bw()
}

get_fig_signif_coef <- function(X_train, y_train, mods_summary, 
                                signif_level = 0.001) {
  signif_coefs <- lapply(mods_summary$vars.idx, function(v_idx) {
    model <- get_glm(X_train, y_train, v_idx)
    coefs <- get_coef(model)
    return(coefs[Anova(model)$`Pr(>Chisq)`<= signif_level])
  })
  mods_summary$signif.coef <- signif_coefs
  
  df <- get_coef_df(mods_summary, "signif.coef")
  ggplot(data = df) +
    geom_boxplot(aes(y = signif.coef, fill = model)) +
    scale_fill_discrete(name = "Modèle") +
    scale_x_continuous(breaks = NULL) +
    labs(y = "Coefficients significatifs", x = "Modèle") +
    theme_bw()
}