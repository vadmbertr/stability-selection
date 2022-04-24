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

get_fig_auc <- function(mods_roc) {
  mods_auc <- data.frame(auc = sapply(mods_roc, function(mod_roc) mod_roc$auc),
                         model = names(mods_roc))
  ggplot(data = mods_auc) +
    geom_bar(aes(x = model, y = auc, fill = model), stat = "identity") +
    scale_fill_discrete(name = "Modèle") +
    labs(x = "Modèle", y = "AUC") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
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

get_fig_amp_score <- function(mods_summary) {
  df <-  lapply(rownames(mods_summary), function(mod_name) {
    r <- mods_summary[mod_name, ]
    f <- data.frame(model = mod_name, score = r$score, coef = r$coef)
    colnames(f) <- c("model", "score", "coef")
    rownames(f) <- NULL
    return(f)
  })
  df <- do.call(rbind, df)
  df$score <- as.ordered(df$score)
  ggplot(data = df) +
    geom_boxplot(aes(x = coef, y = score, fill = model),
                 position = position_dodge(1)) +
    labs(x = "Coefficients", y = "Taux d'agrément") +
    scale_x_continuous(limits = c(-25, 25)) +
    theme_bw()
}