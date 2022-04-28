## ---- stabsel_func -----------------------------------------------------------
source("stabsel_func.R")


library(ggmosaic)
library(ggplot2)


## ---- fig_paths --------------------------------------------------------------
get_fig_stab <- function(stab_path) {
  # meme couleur de .8 à 1
  stab_path$color <- sapply(stab_path$max_freq, function(f) min(f, .8))
  ggplot(data = stab_path) +
    geom_line(aes(x = lambda, y = freq, group = var, color = color)) +
    scale_color_gradient(low = "yellow", high = "red", limits = c(.6, .8),
                         guide = "none") +
    labs(x = "Lambda index", y = "Probabilité de sélection") +
    theme_bw()
}

get_fig_reg <- function(reg_path) {
  # meme couleur de .8 à 1
  reg_path$color <- sapply(reg_path$max_freq, function(f) min(f, .8))
  ggplot(data = reg_path) +
    geom_line(aes(x = lambda, y = beta, group = var, color = color)) +
    scale_color_gradient(low = "yellow", high = "red", limits = c(0.6, .8),
                         guide = "none") +
    labs(x = "Lambda index", y = "Coefficient") +
    theme_bw()
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
get_coefs_df <- function(mods_summary, signif_level) {
  df <-  lapply(rownames(mods_summary), function(mod_name) {
    r <- mods_summary[mod_name, ]
    coef <- unlist(r$coefs)
    vars_idx <- unlist(r$vars.idx)
    if (!is.null(signif_level)) {
      idx <- unlist(r$coefs.p.val) <= signif_level
      coef <- coef[idx]
      vars_idx <- vars_idx[idx]
    }
    f <- data.frame(model = mod_name, coef = coef, vars.idx = vars_idx)
    rownames(f) <- NULL
    return(f)
  })
  df <- do.call(rbind, df)
  return(df)
}

get_fig_coefs <- function(mods_summary, signif_level = NULL) {
  df <- get_coefs_df(mods_summary, signif_level)
  ggplot(data = df) +
    geom_boxplot(aes(y = coef, fill = model)) +
    scale_fill_discrete(name = "Modèle") +
    scale_x_continuous(breaks = NULL) +
    labs(y = "Coefficients", x = "Modèle") +
    theme_bw()
}

signif_codes <- c("0.001" = "***", "0.01" = "**", "0.05" = "*")
get_signif_code <- function(signif_level) {
  return(signif_codes[[as.character(signif_level)]])
}

get_fig_signif_coefs <- function(mods_summary, signif_level) {
  get_fig_coefs(mods_summary, signif_level) +
    labs(y = paste0("Coefficients ", get_signif_code(signif_level)))
}


get_mosaic <- function(X_train, y_train, var_idx, coef, mod_name) {
  df <- data.frame(x = as.factor(X_train[, var_idx]), y = as.factor(y_train))
  levels(df$x) <- c("Absent", "Présent")
  levels(df$y) <- c("Sensible", "Résistante")
  ggplot(data = df) +
    geom_mosaic(aes(x = product(y, x), fill = y)) +
    labs(title = paste0(mod_name, " : ", 
                        " motif ", var_idx, 
                        ", beta = ", round(coef, 2)),
         x = "Motif génomique", y = "Souche") +
    scale_fill_discrete(name = "Souche") +
    theme_bw() +
    theme(axis.text.y = element_text(angle = 90, hjust = .5, vjust = .5),
          plot.title = element_text(size = 10))
}