# =======================================================================
# ANÁLISIS DE CONGLOMERADOS SECTOR-ENTIDAD: CAPACIDADES TECNOLÓGICAS
# Y DERRAMAS DE CONOCIMIENTO EN MÉXICO (2003-2023)
# Versión 2.0 | Actualizado y depurado
# Autor: Gilberto González Pérez
# =======================================================================

pkgs <- c("tidyverse", "FactoMineR", "factoextra", "fpc", "mclust", "kernlab",
          "sf", "spdep", "tmap", "ggalluvial", "trelliscopejs", "agricolae","mice",
          "car", "MASS", "dunn.test", "pairwiseAdonis", "vegan", "ggcorrplot", "kableExtra",
          "gridExtra", "e1071", "psych", "corrplot", "igraph", "diagram", "reshape2")
suppressPackageStartupMessages(for (p in pkgs) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
})

set.seed(56) 

# -----------------------------------------------------------------------
# 1. CARGA Y PREPARACIÓN DE DATOS
# -----------------------------------------------------------------------

data <- bin 

vars_interes <- c("iact","cdig", "ite", "intal", "Qs_pacd","marpacd", "marppvs",
                  "eficap", "markup","automa", "ecos", "efene", "mbi","prod_cap", "prod_pacd",
                  "prod_ppvs","ibl_pot", "compacd", "comppvs", "Qs_ppvs", "Qs_pot")


data <- data %>% filter(!is.na(NOMGEO), !is.na(AE)) %>% na.omit()

# Años disponibles (actualizado a 2023)
cat("Años en la base:", sort(unique(data$tcode)), "\n")
cat("Observaciones totales:", nrow(data), "\n")

library(rpivotTable)

tgen<- rpivotTable(data, rows="NOMGEO", col="AE", aggregatorName="Average", 
                   vals="compacd")
tgen

stats <- data %>%
  dplyr::select(all_of(vars_interes)) %>%
  summarise(across(everything(), list(
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    iqr = ~IQR(., na.rm = TRUE),
    skew = ~skewness(., na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), 
               names_to = c("variable", "stat"), 
               names_pattern = "(.*)_(mean|median|sd|iqr|skew)") %>%
  pivot_wider(names_from = stat, values_from = value)

print(stats)

data %>%
  dplyr::select(all_of(vars_interes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  scale_y_log10() +   # ayuda a visualizar colas largas
  labs(title = "Distribución de variables de capacidades",
       subtitle = "Escala logarítmica en Y para resaltar outliers",
       x = "Variable", y = "Valor (log10)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data %>%
  dplyr::select(all_of(vars_interes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 40, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(data = . %>% group_by(variable) %>% summarise(mean = mean(value, na.rm = TRUE)),
             aes(xintercept = mean), color = "red", linetype = "dashed", size = 1) +
  geom_vline(data = . %>% group_by(variable) %>% summarise(median = median(value, na.rm = TRUE)),
             aes(xintercept = median), color = "blue", linetype = "solid", size = 1) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Histogramas con media (rojo) y mediana (azul)",
       subtitle = "La separación indica asimetría y posible influencia de outliers") +
  theme_minimal()

stats_diff <- stats %>%
  mutate(
    mean_med_ratio = mean / median,
    sd_iqr_ratio = sd / iqr
  )

ggplot(stats_diff, aes(x = reorder(variable, mean_med_ratio), y = mean_med_ratio)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Ratio media/mediana",
       subtitle = "Valores > 1 indican asimetría positiva (cola derecha)",
       x = "Variable", y = "Media / Mediana") +
  theme_minimal()

ggplot(stats_diff, aes(x = reorder(variable, sd_iqr_ratio), y = sd_iqr_ratio)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Ratio desviación estándar / IQR",
       subtitle = "Valores altos sugieren outliers que inflan la varianza",
       x = "Variable", y = "SD / IQR") +
  theme_minimal()

# -----------------------------------------------------------------------
# 2. ESTANDARIZACIÓN ROBUSTA
# -----------------------------------------------------------------------
# Uso de escalado robusto: mediana y RIC para manejar outliers
# y alta heterogeneidad sectorial.

robust_scale <- function(x) {
  iqr <- IQR(x, na.rm = TRUE)
  if (iqr == 0) return(rep(0, length(x)))           # BUG FIX: evitar NaN por IQR = 0
  (x - median(x, na.rm = TRUE)) / iqr
}

# Aplicar
data_scaled <- data %>%
  dplyr::select(all_of(vars_interes)) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), robust_scale))

# -----------------------------------------------------------------------
# 3. ÍNDICES COMPUESTOS POR PCA (ICP, ICI, ICS)
# -----------------------------------------------------------------------
# Se ponderan los tres primeros componentes principales según su eigenvalor.

compute_pca_index <- function(data_scaled = data_scaled, variables, label) {
  subset_data <- data_scaled[, variables]
  
  # Verificar varianza > 0 en todas las variables
  var_check <- sapply(subset_data, function(x) var(x, na.rm = TRUE))
  if (any(var_check < 1e-8)) {
    warning("Variables con varianza casi cero eliminadas en ", label, ": ",
            paste(names(var_check)[var_check < 1e-8], collapse = ", "))
    subset_data <- subset_data[, var_check >= 1e-8, drop = FALSE]
  }
  
  pca <- PCA(subset_data, graph = FALSE)
  
  eig_vals <- pca$eig[1:3, "eigenvalue"]
  w <- eig_vals / sum(eig_vals)
  
  index <- as.numeric(w[1] * pca$ind$coord[, 1] + w[2] * pca$ind$coord[, 2]+
                        w[3] * pca$ind$coord[, 3])
  
  cat("\n--- PCA:", label, "---\n")
  print(get_eigenvalue(pca)[1:3, ])
  cat("Ponderadores dim1/dim2/dim3:", round(w, 4), "\n")
  
  list(index = index, pca = pca, weights = w)
}

# =======================================================================
# VALIDACIÓN COMPLETA DEL EJERCICIO DE PCA
# Índices ICP, ICI, ICS — Capacidades Tecnológicas México
# =======================================================================
# Cubre:
#   1. Supuestos previos (KMO, Bartlett, correlaciones)
#   2. Adecuación del modelo PCA
#   3. Selección de componentes (eigenvalores, varianza explicada)
#   4. Calidad de representación (comunalidades, cos2)
#   5. Contribución de variables
#   6. Ortogonalidad de componentes
#   7. Estabilidad bootstrap
#   8. Validez convergente/discriminante entre índices
#   9. Reporte consolidado
# =======================================================================

# -----------------------------------------------------------------------
# CONFIGURACIÓN INICIAL
# -----------------------------------------------------------------------
# dir.create("validacion_pca", showWarnings = FALSE)

# Función auxiliar para guardar ggplot
guardar <- function(p, nombre, w = 8, h = 5) {
  ggsave(paste0("validacion_pca/", nombre, ".png"), p,
         width = w, height = h, dpi = 150)
  invisible(p)
}

# Definición de variables por índice
vars_icp <- c("automa", "iact", "markup")
vars_ici <- c("ecos",   "efene", "mbi")
vars_ics <- c("cdig",   "intal",  "ite")

indices <- list(
  ICP = vars_icp,
  ICI = vars_ici,
  ICS = vars_ics
)

# -----------------------------------------------------------------------
# FUNCIÓN MAESTRA DE VALIDACIÓN POR ÍNDICE (OPTIMIZADA)
# -----------------------------------------------------------------------

validar_pca <- function(data_scaled = data_scaled, vars, label, n_comp = 2) {
  # Función auxiliar para guardar gráficos
  guardar <- function(plot, nombre, width = 7, height = 5, dpi = 300) {
    ggplot2::ggsave(filename = paste0(nombre, ".png"), plot = plot,
                    width = width, height = height, dpi = dpi)
  }
  
  # Validaciones iniciales
  if (length(vars) < 2) {
    stop("Se necesitan al menos 2 variables para realizar PCA.")
  }
  
  cat("\n", strrep("=", 60), "\n")
  cat("  VALIDACIÓN PCA —", label, "\n")
  cat(strrep("=", 60), "\n\n")
  
  # Subset de columnas
  df <- data_scaled[, vars, drop = FALSE]
  
  # Convertir todas las columnas a numérico, tratando factores, caracteres y listas
  df <- as.data.frame(lapply(df, function(x) {
    # Si es factor, convertimos a character primero
    if (is.factor(x)) x <- as.character(x)
    # Intento de convertir a numérico; si falla, devolvemos NA
    tryCatch(as.numeric(x), error = function(e) rep(NA_real_, length(x)))
  }))
  
  # Eliminar filas con algún NA (valores no convertibles)
  df <- df[stats::complete.cases(df), , drop = FALSE]
  
  if (nrow(df) == 0) {
    stop("No hay observaciones completas después de convertir las variables a numérico.")
  }
  
  n  <- nrow(df)
  p  <- ncol(df)
  mat <- as.matrix(df)
  
  # ── 0. FIABILIDAD BÁSICA ──────────────────────────────────────────────
  cat("── 0. Muestra y fiabilidad ─────────────────────────────\n")
  cat("   n =", n, " | p =", p, " | ratio n/p =", round(n/p, 1), "\n")
  if (n/p < 5)  warning("  ⚠  Ratio n/p < 5: muestra pequeña para PCA.")
  if (n < 50)   warning("  ⚠  n < 50: interpretación con cautela.")
  
  # Alpha de Cronbach
  alpha_res <- tryCatch(
    suppressWarnings(psych::alpha(df, check.keys = TRUE)$total$raw_alpha),
    error = function(e) NA
  )
  cat("   Alpha de Cronbach:", round(alpha_res, 3))
  if (!is.na(alpha_res)) {
    if (alpha_res >= 0.70) cat("  ✔  Aceptable (≥0.70)\n")
    else if (alpha_res >= 0.60) cat("  ⚠  Marginal (0.60-0.70)\n")
    else cat("  ✗  Bajo (<0.60): revisar composición del índice\n")
  } else cat("\n")
  
  # ── 1. MATRIZ DE CORRELACIONES ────────────────────────────────────────
  cat("\n── 1. Matriz de correlaciones ──────────────────────────\n")
  R <- cor(mat, use = "complete.obs")
  det_R <- det(R)
  cat("   Determinante |R|:", formatC(det_R, format = "e", digits = 4))
  if (det_R < 0.00001) cat("  ✔  Multicolinealidad suficiente para PCA\n")
  else if (det_R < 0.01) cat("  ✔  Correlaciones adecuadas\n")
  else cat("  ⚠  Correlaciones débiles: PCA puede no ser informativo\n")
  
  p_corr <- suppressWarnings(
    ggcorrplot::ggcorrplot(R, hc.order = TRUE, type = "lower",
                           lab = TRUE, lab_size = 3,
                           colors = c("#C00000", "white", "#1F4E79"),
                           title = paste("Matriz de correlaciones —", label)) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))
  )
  guardar(p_corr, paste0("01_correlaciones_", label), width = 7, height = 7)
  
  # ── 2. PRUEBA DE BARTLETT ────────────────────────────────────────────
  cat("\n── 2. Prueba de esfericidad de Bartlett ────────────────\n")
  bartlett <- tryCatch(psych::cortest.bartlett(R, n = n), error = function(e) NULL)
  if (!is.null(bartlett)) {
    cat("   Chi-cuadrado:", round(bartlett$chisq, 2),
        "| gl:", bartlett$df,
        "| p-valor:", format(bartlett$p.value, scientific = TRUE), "\n")
    if (bartlett$p.value < 0.05)
      cat("   ✔  Rechaza H0 (R = I): las variables están correlacionadas.\n")
    else
      cat("   ✗  No rechaza H0: variables independientes. PCA no recomendado.\n")
  } else cat("   ⚠  No se pudo calcular Bartlett.\n")
  
  # ── 3. KMO ────────────────────────────────────────────────────────────
  cat("\n── 3. Índice KMO (Kaiser-Meyer-Olkin) ──────────────────\n")
  kmo_res <- tryCatch(psych::KMO(mat), error = function(e) NULL)
  if (!is.null(kmo_res)) {
    kmo_global <- kmo_res$MSA
    cat("   KMO global:", round(kmo_global, 3))
    kmo_label <- dplyr::case_when(
      kmo_global >= 0.90 ~ "  ✔✔ Excelente",
      kmo_global >= 0.80 ~ "  ✔  Muy bueno",
      kmo_global >= 0.70 ~ "  ✔  Bueno",
      kmo_global >= 0.60 ~ "  ⚠  Mediocre",
      kmo_global >= 0.50 ~ "  ⚠  Miserable",
      TRUE               ~ "  ✗  Inaceptable"
    )
    cat(kmo_label, "\n\n")
    
    kmo_vars <- data.frame(
      Variable = names(kmo_res$MSAi),
      KMO      = round(kmo_res$MSAi, 3)
    ) %>% dplyr::mutate(Evaluacion = dplyr::case_when(
      KMO >= 0.80 ~ "Muy bueno",
      KMO >= 0.70 ~ "Bueno",
      KMO >= 0.60 ~ "Mediocre",
      KMO >= 0.50 ~ "Miserable",
      TRUE        ~ "Eliminar"
    ))
    print(kmo_vars)
    
    vars_problema <- kmo_vars$Variable[kmo_vars$KMO < 0.50]
    if (length(vars_problema) > 0)
      cat("\n   ⚠  Considerar eliminar:", paste(vars_problema, collapse = ", "), "\n")
  }
  
  # ── 4. PCA ────────────────────────────────────────────────────────────
  cat("\n── 4. Ejecución del PCA ────────────────────────────────\n")
  pca <- FactoMineR::PCA(df, graph = FALSE)
  # Asegurar que n_comp no exceda el número de componentes disponibles
  n_comp <- min(n_comp, ncol(pca$var$coord))
  
  # ── 5. EIGENVALORES Y VARIANZA EXPLICADA ──────────────────────────────
  cat("\n── 5. Eigenvalores y varianza explicada ────────────────\n")
  eig_mat <- pca$eig
  print(round(eig_mat, 3))
  
  n_kaiser <- sum(eig_mat[, 1] > 1)  # primera columna = eigenvalue
  cat("\n   Componentes con λ > 1 (criterio Kaiser):", n_kaiser, "\n")
  
  # Varianza acumulada para los primeros n_comp componentes
  var_acum <- if (nrow(eig_mat) >= n_comp) eig_mat[n_comp, 3] else NA
  if (!is.na(var_acum)) {
    cat("   Varianza explicada por", n_comp, "componentes:", round(var_acum, 1), "%")
    if (var_acum >= 60) cat("  ✔\n") else cat("  ⚠  Por debajo del 60% recomendado\n")
  } else {
    cat("   No hay suficientes componentes.\n")
  }
  
  # Gráfico de sedimentación
  p_scree <- factoextra::fviz_eig(pca, addlabels = TRUE, ylim = c(0, 100),
                                  barfill = "#2E75B6", barcolor = "white",
                                  linecolor = "#C00000") +
    ggplot2::geom_hline(yintercept = 100/p, linetype = "dashed",
                        color = "gray50", size = 0.8) +   # cambiado linewidth -> size
    ggplot2::annotate("text", x = p - 0.5, y = 100/p + 1.5,
                      label = paste0("1/p = ", round(100/p, 1), "%"),
                      size = 3, color = "gray40") +
    ggplot2::labs(title = paste("Gráfico de sedimentación —", label),
                  subtitle = if (!is.na(var_acum)) paste0("Varianza acumulada (", n_comp, " comp.) = ", round(var_acum, 1), "%") else NULL) +
    ggplot2::theme_minimal(base_size = 11)
  guardar(p_scree, paste0("02_sedimentacion_", label))
  
  # ── 6. CARGAS FACTORIALES ─────────────────────────────────────────────
  cat("\n── 6. Cargas factoriales (coordenadas en componentes) ──\n")
  coords <- as.data.frame(pca$var$coord[, 1:n_comp, drop = FALSE])
  colnames(coords) <- paste0("Dim.", 1:n_comp)
  coords$Variable <- rownames(coords)
  coords <- dplyr::select(coords, Variable, dplyr::everything())
  print(coords %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))))
  
  # Variables con carga baja en ambos componentes
  max_carga <- apply(abs(pca$var$coord[, 1:n_comp, drop = FALSE]), 1, max)
  vars_baja_carga <- names(max_carga)[max_carga < 0.40]
  if (length(vars_baja_carga) > 0)
    cat("\n   ⚠  Carga < 0.40 en todos los componentes seleccionados:",
        paste(vars_baja_carga, collapse = ", "), "\n")
  else
    cat("\n   ✔  Todas las variables tienen carga ≥ 0.40 en al menos un componente\n")
  
  # ── 7. COMUNALIDADES ──────────────────────────────────────────────────
  cat("\n── 7. Comunalidades (cos² acumulado en", n_comp, "comp.) ─\n")
  cos2_acum <- rowSums(pca$var$cos2[, 1:n_comp, drop = FALSE])
  df_com <- data.frame(Variable = names(cos2_acum),
                       Comunalidad = round(cos2_acum, 3)) %>%
    dplyr::mutate(Evaluacion = dplyr::case_when(
      Comunalidad >= 0.70 ~ "✔  Bien representada",
      Comunalidad >= 0.50 ~ "⚠  Representación media",
      TRUE                ~ "✗  Mal representada"
    ))
  print(df_com)
  
  vars_mal_rep <- df_com$Variable[cos2_acum < 0.50]
  if (length(vars_mal_rep) > 0)
    cat("\n   ⚠  Mal representadas (cos² <0.50):",
        paste(vars_mal_rep, collapse = ", "), "\n")
  
  # ── 8. CONTRIBUCIONES ────────────────────────────────────────────────
  cat("\n── 8. Contribución de variables a cada componente ──────\n")
  umbral <- 100 / p
  cat("   Umbral de contribución esperada (100/p):", round(umbral, 1), "%\n\n")
  contrib <- as.data.frame(pca$var$contrib[, 1:n_comp, drop = FALSE])
  colnames(contrib) <- paste0("Contrib_Dim", 1:n_comp)
  contrib$Variable <- rownames(contrib)
  print(contrib %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2))))
  
  # Gráficos de contribuciones
  p_contrib1 <- factoextra::fviz_contrib(pca, choice = "var", axes = 1, top = p,
                                         fill = "#2E75B6", color = "white") +
    ggplot2::labs(title = paste(label, "— Contribuciones Dim.1")) +
    ggplot2::theme_minimal(base_size = 10)
  p_contrib2 <- factoextra::fviz_contrib(pca, choice = "var", axes = 2, top = p,
                                         fill = "#C55A11", color = "white") +
    ggplot2::labs(title = paste(label, "— Contribuciones Dim.2")) +
    ggplot2::theme_minimal(base_size = 10)
  guardar(p_contrib1, paste0("03a_contribuciones_Dim1_", label))
  guardar(p_contrib2, paste0("03b_contribuciones_Dim2_", label))
  
  # ── 9. BIPLOT ─────────────────────────────────────────────────────────
  cat("\n── 9. Biplot ────────────────────────────────────────────\n")
  p_biplot <- factoextra::fviz_pca_biplot(
    pca,
    col.var    = "#C00000",
    col.ind    = "cos2",
    gradient.cols = c("#AAAAAA", "#2E75B6", "#1F4E79"),
    repel      = TRUE,
    label      = "var",
    alpha.ind  = 0.4,
    title      = paste("Biplot PCA —", label)
  ) + ggplot2::theme_minimal(base_size = 10)
  guardar(p_biplot, paste0("04_biplot_", label), width = 8, height = 7)
  
  # ── 10. ORTOGONALIDAD DE COMPONENTES (por construcción es exacta) ────
  cat("\n── 10. Ortogonalidad de componentes ────────────────────\n")
  cat("   Por construcción, los componentes son ortogonales (correlación ~ 0).\n")
  
  # ── 11. PONDERADORES Y CONSTRUCCIÓN DEL ÍNDICE ────────────────────────
  cat("\n── 11. Ponderadores y construcción del índice ──────────\n")
  eig_vals <- pca$eig[1:n_comp, 1]
  w <- eig_vals / sum(eig_vals)
  cat("   Eigenvalores usados:", paste(round(eig_vals, 3), collapse = " | "), "\n")
  cat("   Ponderadores (w1, w2):", paste(round(w, 4), collapse = " | "), "\n")
  cat("   Suma ponderadores:", round(sum(w), 6), "  ✔\n")
  
  index <- as.numeric(w[1] * pca$ind$coord[, 1] +
                        if (n_comp >= 2) w[2] * pca$ind$coord[, 2] else 0)
  
  # ── 12. DISTRIBUCIÓN DEL ÍNDICE ───────────────────────────────────────
  cat("\n── 12. Distribución del índice ─────────────────────────\n")
  cat("   Min:", round(min(index), 3), "| Mediana:", round(median(index), 3),
      "| Max:", round(max(index), 3), "| SD:", round(sd(index), 3), "\n")
  
  # Test de normalidad (Shapiro con submuestra si n > 5000)
  n_test <- min(n, 4999)
  set.seed(42)
  muestra <- sample(index, n_test)
  sw <- shapiro.test(muestra)
  cat("   Shapiro-Wilk (n=", n_test, "): W =", round(sw$statistic, 4),
      "| p =", format(sw$p.value, scientific = TRUE))
  if (sw$p.value > 0.05) cat("  ✔  No rechaza normalidad\n")
  else cat("  ⚠  Rechaza normalidad — usar interpretación no paramétrica\n")
  
  p_hist <- ggplot2::ggplot(data.frame(idx = index), ggplot2::aes(idx)) +
    ggplot2::geom_histogram(ggplot2::aes(y = after_stat(density)), bins = 40,
                            fill = "#2E75B6", color = "white", alpha = 0.8) +
    ggplot2::geom_density(color = "#C00000", size = 1) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::labs(title = paste("Distribución del índice", label),
                  x = label, y = "Densidad") +
    ggplot2::theme_minimal(base_size = 11)
  guardar(p_hist, paste0("05_distribucion_", label))
  
  # ── 13. ESTABILIDAD BOOTSTRAP (optimizado con prcomp) ─────────────────
  cat("\n── 13. Estabilidad bootstrap de cargas (B = 200) ───────\n")
  set.seed(56)
  B <- 200
  # Usamos prcomp para mayor velocidad
  pca_orig <- prcomp(df, scale. = TRUE)  # estandarizado igual que PCA()
  carga_orig <- pca_orig$rotation[, 1]
  
  cargas_boot <- matrix(NA, nrow = B, ncol = p,
                        dimnames = list(NULL, vars))
  
  for (b in seq_len(B)) {
    idx_b <- sample(n, n, replace = TRUE)
    boot_sample <- df[idx_b, , drop = FALSE]
    pca_b <- tryCatch(prcomp(boot_sample, scale. = TRUE), error = function(e) NULL)
    if (!is.null(pca_b)) {
      carga_b <- pca_b$rotation[, 1]
      # Alinear signo
      if (cor(carga_orig, carga_b) < 0) carga_b <- -carga_b
      cargas_boot[b, ] <- carga_b
    }
  }
  
  cargas_boot <- na.omit(cargas_boot)
  boot_summary <- data.frame(
    Variable  = vars,
    Carga_PCA = round(pca$var$coord[, 1], 3),
    Media_Boot = round(colMeans(cargas_boot), 3),
    SD_Boot    = round(apply(cargas_boot, 2, sd), 3),
    IC_Bajo    = round(apply(cargas_boot, 2, quantile, 0.025, na.rm = TRUE), 3),
    IC_Alto    = round(apply(cargas_boot, 2, quantile, 0.975, na.rm = TRUE), 3)
  ) %>%
    dplyr::mutate(Estable = IC_Bajo * IC_Alto > 0)
  
  print(boot_summary)
  inestables <- boot_summary$Variable[!boot_summary$Estable]
  if (length(inestables) > 0)
    cat("\n   ⚠  Cargas inestables (IC cruza cero):",
        paste(inestables, collapse = ", "), "\n")
  else
    cat("\n   ✔  Todas las cargas son estables (IC no cruza cero)\n")
  
  p_boot <- ggplot2::ggplot(boot_summary, ggplot2::aes(x = reorder(Variable, Carga_PCA),
                                                       y = Carga_PCA, color = Estable)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = IC_Bajo, ymax = IC_Alto), width = 0.3) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::scale_color_manual(values = c("TRUE" = "#375623", "FALSE" = "#C00000"),
                                labels = c("TRUE" = "Estable", "FALSE" = "Inestable")) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = paste("Estabilidad bootstrap de cargas Dim.1 —", label),
                  subtitle = "Intervalos de confianza al 95% (B = 200)",
                  x = "", y = "Carga factorial", color = "") +
    ggplot2::theme_minimal(base_size = 11)
  guardar(p_boot, paste0("06_bootstrap_", label))
  
  # ── RESUMEN ───────────────────────────────────────────────────────────
  cat("\n── RESUMEN ─────────────────────────────────────────────\n")
  puntos_ok <- c(
    !is.null(bartlett) && bartlett$p.value < 0.05,
    !is.null(kmo_res) && kmo_global >= 0.60,
    !is.na(var_acum) && var_acum >= 60,
    all(cos2_acum >= 0.50, na.rm = TRUE),
    all(boot_summary$Estable, na.rm = TRUE)
  )
  criterios <- c("Bartlett p<0.05", "KMO≥0.60",
                 "Varianza≥60%", "Comunalidades≥0.50", "Bootstrap estable")
  for (i in seq_along(criterios))
    cat("  ", if (puntos_ok[i]) "✔" else "✗", criterios[i], "\n")
  
  cat("\n  Criterios superados:", sum(puntos_ok), "/", length(puntos_ok), "\n")
  if (sum(puntos_ok) == length(puntos_ok))
    cat("  → PCA VÁLIDO Y ROBUSTO ✔✔\n")
  else if (sum(puntos_ok) >= 4)
    cat("  → PCA ACEPTABLE con reservas menores ✔\n")
  else
    cat("  → REVISAR EL PCA: múltiples criterios fallidos ✗\n")
  
  # Resultado invisible
  invisible(list(
    label       = label,
    pca         = pca,
    index       = index,
    kmo         = if (!is.null(kmo_res)) kmo_global else NA,
    bartlett    = bartlett,
    varianza    = var_acum,
    comunalidades = cos2_acum,
    ponderadores  = w,
    boot_summary  = boot_summary,
    criterios_ok  = puntos_ok
  ))
}

# -----------------------------------------------------------------------
# EJECUTAR VALIDACIÓN PARA LOS TRES ÍNDICES
# -----------------------------------------------------------------------
# Asegurar que data_std existe en el entorno (debes haberla creado antes)

resultados_val <- lapply(names(indices), function(lbl) {
  validar_pca(data_scaled = data_scaled , vars = indices[[lbl]], label = lbl, n_comp = 2)
})
names(resultados_val) <- names(indices)

# 1. Extraer índices en un solo data frame ---------------------------------

ids <- if (!is.null(rownames(data_scaled))) rownames(data_scaled) else 1:nrow(data_scaled)

indices_df <- data.frame(
  id = ids,
  ICP = resultados_val$ICP$index,
  ICI = resultados_val$ICI$index,
  ICS = resultados_val$ICS$index
)

write.csv(indices_df, "indices_pca.csv", row.names = FALSE)

# 2. Guardar cargas (loadings) de cada componente -------------------------
for (lbl in names(resultados_val)) {
  # Cargas factoriales (coordenadas de las variables)
  loadings <- as.data.frame(resultados_val[[lbl]]$pca$var$coord)
  loadings$variable <- rownames(loadings)
  write.csv(loadings, paste0("validacion_pca/cargas_", lbl, ".csv"), row.names = FALSE)
  
  # Comunalidades (cos² acumulado)
  comunalidades <- data.frame(
    variable = names(resultados_val[[lbl]]$comunalidades),
    comunalidad = resultados_val[[lbl]]$comunalidades
  )
  write.csv(comunalidades, paste0("validacion_pca/comunalidades_", lbl, ".csv"), row.names = FALSE)
  
  # Resumen de bootstrap
  boot <- resultados_val[[lbl]]$boot_summary
  write.csv(boot, paste0("validacion_pca/bootstrap_", lbl, ".csv"), row.names = FALSE)
}

# 3. (Opcional) Guardar resumen de criterios y varianza explicada ---------
resumen_criterios <- data.frame(
  indice = names(resultados_val),
  kmo = sapply(resultados_val, function(x) x$kmo),
  varianza_acum = sapply(resultados_val, function(x) x$varianza),
  bartlett_p = sapply(resultados_val, function(x) if(!is.null(x$bartlett)) x$bartlett$p.value else NA),
  criterios_ok = sapply(resultados_val, function(x) sum(x$criterios_ok))
)

write.csv(resumen_criterios, "resumen_criterios_robpca.csv", row.names = FALSE)

# -----------------------------------------------------------------------
# 4. PREPARACIÓN DE BASE PARA CLUSTERING
# -----------------------------------------------------------------------

res_icp  <- compute_pca_index(data_scaled, vars_icp, "ICP")
data$ICP <- res_icp$index
data_scaled$ICP <- res_icp$index

res_ici  <- compute_pca_index(data_scaled, vars_ici, "ICI")
data$ICI <- res_ici$index
data_scaled$ICI <- res_ici$index

res_ics  <- compute_pca_index(data_scaled, vars_ics, "ICS")
data$ICS <- res_ics$index
data_scaled$ICS <- res_ics$index

data_scaled <- data_scaled %>%
  mutate(
    ICP = robust_scale(ICP),
    ICI = robust_scale(ICI),
    ICS = robust_scale(ICS)
  )


data_scaled$NOMGEO <- data$NOMGEO
data_scaled$AE <- data$AE
data_scaled$ID <- data$ID
data_scaled$tcode <- data$tcode

write.csv(data_scaled, "data_scaled.csv", row.names = FALSE)

# Multicolinealidad

modelo <- lm(ICS ~ ICI+ICP+compacd+comppvs+
               marpacd+marppvs+prod_pacd+Qs_pot+
               prod_ppvs, data = data_scaled)
vif(modelo)

# Variables de interés para clustering
vi <- c("ICI", "ICP", "ICS", "compacd", "comppvs",
        "marpacd", "marppvs", "prod_pacd",
         "prod_ppvs", "Qs_pot")
        
# Verificar que todas existen
missing_vi <- vi[!vi %in% names(data)]
if (length(missing_vi) > 0) stop("Faltan variables: ", paste(missing_vi, collapse = ", "))



### Base de datos para clustering

caa <- dplyr::select(data_scaled, all_of(vi))
caa$NOMGEO <- data$NOMGEO
caa$AE <- data$AE
caa$ID <- data$ID
caa$tcode <- data$tcode
caa_split <- split(caa, caa$tcode)

cat("\nAños disponibles para clustering:", names(caa_split), "\n")
cat("Observaciones por año:\n")
sapply(caa_split, nrow) %>% print()


# -----------------------------------------------------------------------
# 5. DETERMINACIÓN DEL NÚMERO ÓPTIMO DE CLUSTERS
# -----------------------------------------------------------------------

#dir.create("resultados_clusters", showWarnings = FALSE)

# Función unificada para WSS + Silueta
evaluar_k_optimo <- function(df_ano, vars, k_max = 10, seed = 56) {
  
  ano <- unique(df_ano$tcode)
  datos <- as.matrix(df_ano[, vars])
  rownames(datos) <- NULL
  
  if (nrow(datos) < 3) { warning("Año ", ano, ": < 3 filas."); return(NULL) }
  if (!all(is.finite(datos))) { warning("Año ", ano, ": valores no finitos."); return(NULL) }
  
  k_max_real <- min(k_max, nrow(datos) - 1)
  
  # --- WSS (método del codo) ---
  set.seed(seed)
  wss <- sapply(1:k_max_real, function(k) {
    tryCatch(kmeans(datos, centers = k, nstart = 25, iter.max = 100)$tot.withinss,
             error = function(e) NA_real_)
  })
  
  # --- Silueta manual ---
  if (k_max_real >= 2) {
    dist_mat <- as.matrix(dist(datos))
    sil_vals <- sapply(2:k_max_real, function(k) {
      set.seed(seed)
      km <- tryCatch(kmeans(datos, centers = k, nstart = 25, iter.max = 100),
                     error = function(e) return(NULL))
      if (is.null(km)) return(NA_real_)
      
      clusters <- km$cluster
      n <- length(clusters)
      s_total <- vapply(seq_len(n), function(i) {
        same <- which(clusters == clusters[i])
        if (length(same) == 1) return(0)
        a_i <- mean(dist_mat[i, same[same != i]])
        other_cls <- unique(clusters[clusters != clusters[i]])
        b_i <- min(sapply(other_cls, function(c) mean(dist_mat[i, clusters == c])))
        (b_i - a_i) / max(a_i, b_i)
      }, numeric(1))
      mean(s_total, na.rm = TRUE)
    })
    df_sil <- data.frame(k = 2:k_max_real, sil = sil_vals)
  } else {
    df_sil <- data.frame(k = integer(0), sil = numeric(0))
  }
  
  # Gráficos
  df_wss <- data.frame(k = 1:k_max_real, wss = wss)
  
  p_wss <- ggplot(df_wss, aes(k, wss)) +
    geom_line() + geom_point() +
    geom_vline(xintercept = 4, linetype = "dashed", color = "red") +
    labs(title = paste("Método del Codo —", ano), x = "k", y = "WSS") +
    scale_x_continuous(breaks = 1:k_max_real) + theme_minimal()
  
  ggsave(paste0("resultados_clusters/wss_", ano, ".pdf"), p_wss, width = 6, height = 4)
  
  if (nrow(df_sil) > 0) {
    p_sil <- ggplot(df_sil %>% filter(!is.na(sil)), aes(k, sil)) +
      geom_line() + geom_point() +
      labs(title = paste("Silueta —", ano), x = "k", y = "Ancho silueta") +
      scale_x_continuous(breaks = df_sil$k) + theme_minimal()
    ggsave(paste0("resultados_clusters/silhouette_", ano, ".pdf"), p_sil, width = 6, height = 4)
  }
  
  list(ano = ano, wss = df_wss, silhouette = df_sil)
}

k_results <- lapply(caa_split, evaluar_k_optimo, vars = vi)

k_results

# -----------------------------------------------------------------------
# 6. ESTABILIDAD BOOTSTRAP (JACCARD)
# -----------------------------------------------------------------------

evaluar_estabilidad <- function(df_year, vars, k_range = 2:8, B = 100,
                                method = "ward.D2", seed = 56) {
  ano <- unique(df_year$tcode)
  datos <- as.data.frame(df_year[, vars])
  
  if (nrow(datos) < 2) { warning("Año ", ano, ": < 2 filas."); return(NULL) }
  if (!all(sapply(datos, is.numeric))) stop("Variables no numéricas en año ", ano)
  if (any(!is.finite(as.matrix(datos)))) stop("Valores no finitos en año ", ano)
  
  datos_mat <- as.matrix(datos)
  rownames(datos_mat) <- colnames(datos_mat) <- NULL
  
  resultados <- setNames(
    lapply(k_range, function(k) {
      message("  Año ", ano, " | k = ", k)
      set.seed(seed)                                   # BUG FIX: usar parámetro seed
      tryCatch(
        clusterboot(datos_mat, B = B, clustermethod = hclustCBI,
                    method = method, k = k),
        error = function(e) { message("  Error: ", e$message); NULL }
      )
    }),
    as.character(k_range)
  )
  
  jaccard_df <- purrr::map_dfr(resultados, function(r) {
    tibble(media_jaccard = if (!is.null(r)) mean(r$bootmean, na.rm = TRUE) else NA_real_)
  }, .id = "k") %>% mutate(k = as.integer(k))
  
  list(jaccard = jaccard_df, resultados_completos = resultados)
}

estabilidad_anios <- lapply(caa_split, function(df) {
  tryCatch(evaluar_estabilidad(df, vars = vi, B = 100, method = "ward.D2"),
           error = function(e) { cat("Error:", e$message, "\n"); NULL })
})

jaccard_all <- purrr::map_dfr(estabilidad_anios, ~ .x$jaccard, .id = "anio")

ggplot(jaccard_all, aes(k, media_jaccard, color = anio, group = anio)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 0.75, linetype = "dashed", color = "red") +
  labs(title = "Estabilidad Bootstrap por año (Jaccard)",
       x = "k", y = "Media Jaccard") + theme_minimal()


# -----------------------------------------------------------------------
# 7. ANÁLISIS DE SENSIBILIDAD (ARI)
# -----------------------------------------------------------------------

sensibilidad_k <- function(data, vars, k_ref, k_range = 2:8,
                           metodo_hclust = "ward.D2", semilla = 123) {
  if (!requireNamespace("mclust", quietly = TRUE)) install.packages("mclust")
  
  datos <- data %>% dplyr::select(all_of(vars)) %>% scale() %>% as.data.frame()
  dist_ref <- dist(datos)
  clusters_ref <- cutree(hclust(dist_ref, method = metodo_hclust), k = k_ref)
  
  set.seed(semilla)
  resultados <- purrr::map_dfr(k_range[k_range != k_ref], function(k) {
    clusters_k <- cutree(hclust(dist_ref, method = metodo_hclust), k = k)
    tibble(k = k, ari = mclust::adjustedRandIndex(clusters_ref, clusters_k))
  })
  
  grafico <- ggplot(resultados, aes(k, ari)) +
    geom_line() + geom_point() +
    geom_hline(yintercept = 0.7, linetype = "dashed", color = "red", alpha = 0.5) +
    labs(title = paste("Sensibilidad al número de clusters (k_ref =", k_ref, ")"),
         x = "k", y = "ARI vs k_ref") + theme_minimal()
  
  list(comparaciones = resultados, grafico = grafico)
}

sensibilidad_anios <- lapply(caa_split, function(df) {
  tryCatch(sensibilidad_k(df, vars = vi, k_ref = 4, k_range = 2:8),
           error = function(e) { cat("Error:", e$message, "\n"); NULL })
})

comparaciones_todas <- bind_rows(lapply(names(sensibilidad_anios), function(anio) {
  if (is.null(sensibilidad_anios[[anio]])) return(NULL)
  sensibilidad_anios[[anio]]$comparaciones %>% mutate(anio = anio, .before = 1)
}))

comparaciones_todas

# Asegurar que k es numérico y anio es factor
comparaciones_todas <- comparaciones_todas %>%
  mutate(k = as.numeric(k),
         anio = as.factor(anio))

# Gráfico de líneas
p_lineas <- ggplot(comparaciones_todas, aes(x = k, y = ari, color = anio, group = anio)) +
  geom_line() + geom_point() +
  labs(title = "Estabilidad del clustering (ARI) por año") +
  theme_minimal()

# Mapa de calor
p_heatmap <- ggplot(comparaciones_todas, aes(x = factor(k), y = anio, fill = ari)) +
  geom_tile() + geom_text(aes(label = round(ari, 2)), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Heatmap de ARI", x = "k", y = "Año")

# Mostrar gráficos
print(p_lineas)
print(p_heatmap)

# -----------------------------------------------------------------------
# 8. CLUSTERING JERÁRQUICO (WARD.D2)
# -----------------------------------------------------------------------

hclust_anual <- function(df, vars, k = 4,
                         dist_method = "euclidean",
                         hclust_method = "ward.D2",    # ward.D2 es más estable que ward.D
                         cluster_colname = "cluster_ward") {
  ano <- unique(df$tcode)
  datos_matrix <- as.matrix(df[, vars])
  rownames(datos_matrix) <- colnames(datos_matrix) <- NULL
  
  if (nrow(datos_matrix) < 2 || !all(is.finite(datos_matrix))) {
    warning("Año ", ano, ": datos insuficientes o no finitos."); return(NULL)
  }
  
  dist_obj   <- dist(datos_matrix, method = dist_method)
  hc         <- hclust(dist_obj, method = hclust_method)
  clusters   <- cutree(hc, k = k)
  df[[cluster_colname]] <- as.factor(clusters)
  
  list(df_actualizado = df, hclust_obj = hc,
       dist_matrix = dist_obj, clusters = clusters, k = k)
}

resultados_hclust <- lapply(caa_split, function(df) {
  tryCatch(hclust_anual(df, vars = vi, k = 4, cluster_colname = "cluster_ward"),
           error = function(e) { cat("Error:", e$message, "\n"); NULL })
})

# Silueta por año
silhouette_results <- purrr::map2(resultados_hclust, names(resultados_hclust), function(res, anio) {
  if (is.null(res)) return(NULL)
  sil     <- silhouette(res$clusters, res$dist_matrix)
  avg_w   <- summary(sil)$avg.width
  cat("Año", anio, "| Silueta promedio:", round(avg_w, 4), "\n")
  list(year = anio, avg_width = avg_w, k = res$k, plot = fviz_silhouette(sil) + theme_minimal())
})


# -----------------------------------------------------------------------
# 9. CLUSTERING AVANZADO: GMM + ESPECTRAL
# -----------------------------------------------------------------------

# Gaussian Mixture Models (GMM)
# Dado que DBSCAN mostró un grupo denso y varios satélites, 
# GMM podría modelar mejor esas formas alargadas o irregulares.

# Paquetes necesarios
library(mclust)       # para GMM
library(kernlab)       # para specc (clustering espectral)

advanced_clustering <- function(data, 
                                vars, 
                                cluster_col_base = "cluster_ward", # nombre de la columna base (opcional)
                                year_label = NULL,
                                G_gmm = 4,               # número de clusters para GMM (si se fija)
                                k_spec = 4,               # número de clusters para espectral
                                eps_dbscan = 0.65,         # eps para DBSCAN (ajustar según datos)
                                minPts_dbscan = 7,
                                run_dbscan = TRUE) {
  
  if (is.null(year_label) && "tcode" %in% colnames(data)) {
    year_label <- unique(data$tcode)[1]
  }
  
  # Seleccionar y preparar datos numéricos
  datos_matrix <- data %>% dplyr::select(all_of(vars)) %>% as.matrix()
  rownames(datos_matrix) <- NULL
  colnames(datos_matrix) <- NULL
  
  # 1. GMM
  cat("   - Ejecutando GMM automático...\n")
  gmm_auto <- tryCatch(Mclust(datos_matrix), error = function(e) NULL)
  
  cat("   - Ejecutando GMM con G =", G_gmm, "...\n")
  gmm_fijo <- tryCatch(Mclust(datos_matrix, G = G_gmm), error = function(e) NULL)
  
  # 2. Clustering espectral
  cat("   - Ejecutando clustering espectral con k =", k_spec, "...\n")
  spec <- tryCatch(specc(datos_matrix, centers = k_spec), error = function(e) NULL)
  
  # 3. DBSCAN (opcional)
  db <- NULL
  if (run_dbscan) {
    cat("   - Ejecutando DBSCAN con eps =", eps_dbscan, ", minPts =", minPts_dbscan, "...\n")
    db <- tryCatch(dbscan::dbscan(datos_matrix, eps = eps_dbscan, minPts = minPts_dbscan), 
                   error = function(e) NULL)
  }
  
  # Asignar clusters a los data frames
  # GMM fijo
  data$cluster_gmm <- if (!is.null(gmm_fijo)) as.factor(gmm_fijo$classification) else NA
  # Espectral
  data$cluster_spec <- if (!is.null(spec)) as.factor(as.integer(spec)) else NA
  # DBSCAN  (BUG FIX: el bloque if estaba incompleto; se añade la rama else)
  data$cluster_db <- if (!is.null(db)) as.factor(db$cluster) else NA
  
  # 4. Cálculo de siluetas
  dist_matrix <- dist(datos_matrix)
  sil_gmm <- sil_spec <- sil_db <- NULL
  mean_sil_gmm <- mean_sil_spec <- mean_sil_db <- NA
  
  if (!is.null(gmm_fijo) && nlevels(data$cluster_gmm) > 1) {
    sil_gmm <- silhouette(as.integer(data$cluster_gmm), dist_matrix)
    mean_sil_gmm <- mean(sil_gmm[, 3])
  }
  
  if (!is.null(spec) && nlevels(data$cluster_spec) > 1) {
    sil_spec <- silhouette(as.integer(data$cluster_spec), dist_matrix)
    mean_sil_spec <- mean(sil_spec[, 3])
  }
  
  if (!is.null(db) && sum(db$cluster != 0) > 1) {
    # Excluir ruido para silueta
    idx <- db$cluster != 0
    if (sum(idx) > 1) {
      dist_sin_ruido <- dist(datos_matrix[idx, ])
      sil_db <- silhouette(db$cluster[idx], dist_sin_ruido)
      mean_sil_db <- mean(sil_db[, 3])
    }
  }
  
  # 5. Comparación GMM vs Espectral
  tabla_contingencia <- NULL
  ari <- NA
  if (!is.null(gmm_fijo) && !is.null(spec)) {
    tabla_contingencia <- table(data$cluster_gmm, data$cluster_spec)
    ari <- tryCatch(mclust::adjustedRandIndex(data$cluster_gmm, data$cluster_spec), 
                    error = function(e) NA)
  }
  
  # Devolver resultados
  list(
    anio = year_label,
    data_actualizado = data,
    gmm_auto = gmm_auto,
    gmm_fijo = gmm_fijo,
    spectral = spec,
    dbscan = db,
    siluetas = list(
      gmm = sil_gmm,
      spectral = sil_spec,
      dbscan = sil_db
    ),
    medias_sil = list(
      gmm = mean_sil_gmm,
      spectral = mean_sil_spec,
      dbscan = mean_sil_db
    ),
    comparacion = list(
      tabla_contingencia = tabla_contingencia,
      ari = ari
    )
  )
}

# Lista para almacenar resultados avanzados
resultados_avanzados <- list()

for (anio in names(resultados_hclust)) {
  cat("\n========== Procesando clustering avanzado para año:", anio, "==========\n")
  res <- resultados_hclust[[anio]]
  if (is.null(res)) next
  
  df_actualizado <- res$df_actualizado
  
  test <- tryCatch(
    advanced_clustering(
      data = df_actualizado,
      vars = vi,
      year_label = anio,
      G_gmm = 4,
      k_spec = 4,
      eps_dbscan = 0.65,
      minPts_dbscan = 7,
      run_dbscan = TRUE
    ),
    error = function(e) {
      cat("❌ Error en año", anio, ":", e$message, "\n")
      NULL
    }
  )
  resultados_avanzados[[anio]] <- test
}

# Tabla de medias de silueta por método y año
sil_summary <- bind_rows(lapply(resultados_avanzados, function(x) {
  if (is.null(x)) return(NULL)
  data.frame(
    anio = x$anio,
    GMM = x$medias_sil$gmm,
    Espectral = x$medias_sil$spectral,
    DBSCAN = x$medias_sil$dbscan,
    stringsAsFactors = FALSE
  )
}))

print(sil_summary)

# ARI por año
ari_summary <- bind_rows(lapply(resultados_avanzados, function(x) {
  if (is.null(x)) return(NULL)
  data.frame(
    anio = x$anio,
    ARI_GMM_vs_Espectral = x$comparacion$ari,
    stringsAsFactors = FALSE
  )
}))

print(ari_summary)

# Guardar resultados
write.csv(sil_summary, "siluetas_avanzadas.csv", row.names = FALSE)
write.csv(ari_summary, "ari_comparacion.csv", row.names = FALSE)
saveRDS(resultados_avanzados, "resultados_avanzados.rds")


# 10. ESTADÍSTICAS DESCRIPTIVAS POR CLUSTER
# -----------------------------------------

summarize_clusters <- function(data, 
                               vars, 
                               cluster_col = "cluster_gmm", 
                               year_label = NULL) {
  
  # Verificar que la columna de clusters existe
  if (!cluster_col %in% colnames(data)) {
    stop("La columna '", cluster_col, "' no existe en los datos.")
  }
  
  # Calcular resúmenes por cluster
  summary_df <- data %>%
    group_by(!!sym(cluster_col)) %>%
    summarise(
      n = n(),
      across(all_of(vars),
             list(media = ~ mean(.x, na.rm = TRUE),
                  mediana = ~ median(.x, na.rm = TRUE),
                  de = ~ sd(.x, na.rm = TRUE),
                  q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
                  q3 = ~ quantile(.x, 0.75, na.rm = TRUE)),
             .names = "{.col}_{.fn}")
    ) %>%
    ungroup()
  
  # Convertir a formato largo y luego ancho por estadístico
  summary_long <- summary_df %>%
    pivot_longer(
      cols = -c(!!sym(cluster_col), n),
      names_to = c("variable", "estadistico"),
      names_pattern = "(.+)_([^_]+)$",
      values_to = "valor"
    ) %>%
    pivot_wider(
      names_from = estadistico,
      values_from = valor
    ) %>%
    # Renombrar columna de cluster para claridad
    rename(cluster = !!sym(cluster_col))
  
  # Agregar año si se proporciona
  if (!is.null(year_label)) {
    summary_long <- summary_long %>% mutate(anio = year_label, .before = 1)
  }
  
  return(summary_long)
}

# Lista de resúmenes por año
lista_resumenes <- map2(resultados_avanzados, names(resultados_avanzados), function(res, anio) {
  if (is.null(res)) return(NULL)
  
  # Verificar que la columna cluster_spec existe
  if (!"cluster_gmm" %in% colnames(res$data_actualizado)) {
    warning("La columna 'cluster_spec' no existe en el año ", anio)
    return(NULL)
  }
  
  summarize_clusters(
    data = res$data_actualizado,   # data frame completo
    vars = vi,
    cluster_col = "cluster_gmm",
    year_label = anio
  )
})

# Unir todos en un solo data frame
resumen_total <- bind_rows(lista_resumenes)

# Filtrar año 2003 y formatear
resumen_2003 <- resumen_total %>% filter(anio == "2003")

# Redondear y mostrar con kable
library(kableExtra)

resumen_2003 %>%
  mutate(across(c(media, mediana, de, q1, q3), ~ round(., 2))) %>%
  kable(
    col.names = c("Año", "Cluster", "N", "Variable", "Media", "Mediana", "DE", "Q1", "Q3"),
    align = "c",
    caption = "Resumen por cluster - Año 2003"
  ) %>%
  kable_styling("striped", full_width = FALSE) %>%
  collapse_rows(columns = 1:3, valign = "top")

# Guardar resumen consolidado
write_csv(resumen_total, "resumen_clusters_todos_anos.csv")

# Guardar un archivo por año (opcional)
# walk2(lista_resumenes, names(lista_resumenes), function(df, anio) {
#  if (!is.null(df)) {
#    write_csv(df, paste0("resumen_clusters_", anio, ".csv"))
#  }
#})

# 7. GRÁFICOS DE CAJA POR CLUSTER 
# -------------------------------

plot_cluster_boxplots <- function(data, 
                                  vars, 
                                  cluster_col = "cluster_gmm", 
                                  year_label = NULL) {
  
  # Verificar que la columna de clusters existe
  if (!cluster_col %in% colnames(data)) {
    stop("La columna '", cluster_col, "' no existe en los datos.")
  }
  
  # Si no se proporciona año, intentar extraer de la columna 'tcode' (si existe)
  if (is.null(year_label)) {
    if ("tcode" %in% colnames(data)) {
      year_label <- unique(data$tcode)
      if (length(year_label) > 1) {
        warning("Múltiples años en los datos, usando el primero.")
        year_label <- year_label[1]
      }
    } else {
      year_label <- ""
    }
  }
  
  # Convertir a formato largo
  data_long <- data %>%
    dplyr::select(all_of(c(cluster_col, vars))) %>%
    tidyr::pivot_longer(cols = all_of(vars), 
                        names_to = "variable", 
                        values_to = "valor")
  
  # Crear gráfico
  p <- ggplot(data_long, aes(x = .data[[cluster_col]], y = valor, fill = .data[[cluster_col]])) +
    geom_boxplot() +
    facet_wrap(~ variable, scales = "free_y") +
    labs(title = ifelse(year_label != "", 
                        paste("Distribución de variables por agrupación -", year_label),
                        "Distribución de variables por agrupación"),
         x = "Cluster", y = "Valor") +
    theme_minimal() +
    theme(legend.position = "none")
  
  return(p)
}

# Lista de gráficos (uno por año)
boxplot_list <- map2(resultados_avanzados, names(resultados_avanzados), function(res, anio) {
  if (is.null(res)) return(NULL)   # saltar años que fallaron
  plot_cluster_boxplots(
    data = res$data_actualizado,
    vars = vi,
    cluster_col = "cluster_gmm",
    year_label = anio
  )
})

# Asignar nombres
names(boxplot_list) <- names(resultados_avanzados)

walk2(boxplot_list, names(boxplot_list), function(plot, anio) {
  if (!is.null(plot)) {
    ggsave(filename = paste0("boxplot_clusters_", anio, ".png"), 
           plot = plot, width = 12, height = 8, dpi = 150)
  }
})

# Verificación rápida
print(boxplot_list[["2003"]])
print(boxplot_list[["2008"]])
print(boxplot_list[["2013"]])
print(boxplot_list[["2018"]])
print(boxplot_list[["2023"]])

# 11. ANÁLISIS DE VARIANZA (ANOVA) Y POST-HOC
# -------------------------------------------

anova_clusters <- function(data, 
                           vars, 
                           cluster_col = "cluster_gmm", 
                           year_label = NULL) {
  
  # Verificar columna de clusters
  if (!cluster_col %in% colnames(data)) {
    stop("La columna '", cluster_col, "' no existe en los datos.")
  }
  
  # Preparar datos en formato largo
  datos_anova <- data %>%
    mutate(cluster = as.factor(.data[[cluster_col]])) %>%
    dplyr::select(cluster, all_of(vars)) %>%
    tidyr::pivot_longer(-cluster, names_to = "variable", values_to = "valor") %>%
    mutate(valor = as.numeric(valor)) %>%
    tidyr::drop_na(valor) %>%
    group_by(variable, cluster) %>%
    filter(n() >= 2) %>%                # cada cluster debe tener al menos 2 obs para ANOVA
    ungroup()
  
  # Aplicar ANOVA por variable
  anova_results <- datos_anova %>%
    group_by(variable) %>%
    group_modify(~ {
      # Verificar que haya al menos 2 clusters
      if (n_distinct(.x$cluster) < 2) {
        return(tibble(
          term = "cluster", df = NA, sumsq = NA, meansq = NA, 
          statistic = NA, p.value = NA, SS_total = NA, eta_sq = NA
        ))
      }
      
      # Intentar modelo lineal
      tryCatch({
        modelo <- lm(valor ~ cluster, data = .x)
        anova_modelo <- anova(modelo)
        # Suma de cuadrados total (SS_total = var(valor) * (n-1))
        SS_total <- var(.x$valor) * (nrow(.x) - 1)
        
        tibble(
          term = "cluster",
          df = anova_modelo$Df[1],
          sumsq = anova_modelo$`Sum Sq`[1],
          meansq = anova_modelo$`Mean Sq`[1],
          statistic = anova_modelo$`F value`[1],
          p.value = anova_modelo$`Pr(>F)`[1],
          SS_total = SS_total,
          eta_sq = sumsq / SS_total
        )
      }, error = function(e) {
        tibble(
          term = "cluster", df = NA, sumsq = NA, meansq = NA,
          statistic = NA, p.value = NA, SS_total = NA, eta_sq = NA
        )
      })
    }) %>%
    ungroup() %>%
    # Calcular eta_sq (ya se calculó dentro, pero lo aseguramos)
    mutate(
      eta_sq = ifelse(is.na(eta_sq), sumsq / SS_total, eta_sq),
      efecto = case_when(
        eta_sq < 0.06 ~ "pequeño",
        eta_sq < 0.14 ~ "mediano",
        TRUE ~ "grande"
      )
    ) %>%
    dplyr::select(variable, df, sumsq, meansq, statistic, p.value, eta_sq, efecto)
  
  # Agregar columna de año si se proporciona
  if (!is.null(year_label)) {
    anova_results <- anova_results %>% mutate(anio = year_label, .before = 1)
  }
  
  return(anova_results)
}

# Lista de resultados ANOVA por año
anova_list <- map2(resultados_avanzados, names(resultados_avanzados), function(res, anio) {
  if (is.null(res)) return(NULL)
  anova_clusters(
    data = res$data_actualizado,
    vars = vi,
    cluster_col = "cluster_gmm",
    year_label = anio
  )
})
names(anova_list) <- names(resultados_avanzados)

anova_total <- bind_rows(anova_list)

# Ver tabla completa
library(kableExtra)

anova_total %>%
  mutate(across(c(df, sumsq, meansq, statistic, p.value, eta_sq), ~ round(., 4))) %>%
  kable(caption = "Resultados de ANOVA por variable y año",
        col.names = c("Año", "Variable", "DF", "Sum Sq", "Mean Sq", "F", "p-valor", "Eta^2", "Efecto")) %>%
  kable_styling("striped", full_width = FALSE)

write_csv(anova_total, "anova_clusters_todos_anos.csv")


# Pruebas post-hoc de Tukey para cada variable

anova_tukey_final <- function(data, 
                              vars, 
                              cluster_col = "cluster_gmm", 
                              year_label = NULL,
                              p_threshold = 0.05) {
  
  if (!cluster_col %in% colnames(data)) {
    stop("La columna '", cluster_col, "' no existe en los datos.")
  }
  
  if (is.null(year_label) && "tcode" %in% colnames(data)) {
    year_label <- unique(data$tcode)[1]
  }
  
  # Listas para acumular resultados
  anova_list <- list()
  tukey_list <- list()
  
  for (var in vars) {
    cat("Procesando variable:", var, "año:", year_label, "\n")
    
    # Extraer vectores
    cluster_vec <- data[[cluster_col]]
    valor_vec <- data[[var]]
    
    # Eliminar NA
    keep <- !is.na(cluster_vec) & !is.na(valor_vec)
    cluster <- cluster_vec[keep]
    valor <- valor_vec[keep]
    
    if (length(valor) < 3 || length(unique(cluster)) < 2) {
      cat("  -> Datos insuficientes, omitiendo.\n")
      next
    }
    
    if (!is.factor(cluster)) cluster <- as.factor(cluster)
    cluster <- droplevels(cluster)
    
    tryCatch({
      # ANOVA con lm
      modelo <- lm(valor ~ cluster)
      anova_res <- anova(modelo)
      
      anova_row <- data.frame(
        variable = var,
        term = "cluster",
        df = anova_res$Df[1],
        sumsq = anova_res$`Sum Sq`[1],
        meansq = anova_res$`Mean Sq`[1],
        statistic = anova_res$`F value`[1],
        p.value = anova_res$`Pr(>F)`[1],
        stringsAsFactors = FALSE
      )
      
      ss_total <- sum(anova_res$`Sum Sq`)
      anova_row$eta_sq <- anova_row$sumsq / ss_total
      anova_row$efecto <- cut(anova_row$eta_sq, 
                              breaks = c(0, 0.06, 0.14, Inf),
                              labels = c("pequeño", "mediano", "grande"),
                              right = FALSE)
      
      anova_list[[var]] <- anova_row
      
      # Tukey HSD
      aov_model <- aov(valor ~ cluster)
      tukey <- TukeyHSD(aov_model, "cluster")
      
      comparaciones <- as.data.frame(tukey$cluster)
      comparaciones$comparacion <- rownames(comparaciones)
      comparaciones$variable <- var
      comparaciones <- comparaciones[, c("variable", "comparacion", 
                                         "diff", "lwr", "upr", "p adj")]
      colnames(comparaciones) <- c("variable", "comparacion", 
                                   "difference", "lower", "upper", "pvalue")
      rownames(comparaciones) <- NULL
      
      tukey_list[[var]] <- comparaciones
      
    }, error = function(e) {
      cat("  ❌ Error en variable", var, ":", e$message, "\n")
    })
  }
  
  # Combinar resultados
  anova_all <- if (length(anova_list) > 0) do.call(rbind, anova_list) else data.frame()
  tukey_all <- if (length(tukey_list) > 0) do.call(rbind, tukey_list) else data.frame()
  
  # Agregar año si existe
  if (!is.null(year_label) && nrow(anova_all) > 0) {
    anova_all <- cbind(anio = year_label, anova_all)
  }
  if (!is.null(year_label) && nrow(tukey_all) > 0) {
    tukey_all <- cbind(anio = year_label, tukey_all)
  }
  
  # Filtrar Tukey
  if (p_threshold > 0 && nrow(tukey_all) > 0) {
    tukey_all <- tukey_all[tukey_all$pvalue < p_threshold, , drop = FALSE]
  }
  
  list(anova = anova_all, tukey = tukey_all)
}

anova_tukey_results <- map2(resultados_avanzados, names(resultados_avanzados), function(res, anio) {
  if (is.null(res)) return(NULL)
  anova_tukey_final(
    data = res$data_actualizado,
    vars = vi,
    cluster_col = "cluster_gmm",
    year_label = anio,
    p_threshold = 0.05
  )
})

# Unir ANOVA de todos los años
anova_all_years <- bind_rows(lapply(anova_tukey_results, function(x) x$anova), .id = "anio")
tukey_all_years <- bind_rows(lapply(anova_tukey_results, function(x) x$tukey), .id = "anio")

write.csv(anova_all_years, "anova_todos_anos.csv", row.names = FALSE)
write.csv(tukey_all_years, "tukey_todos_anos.csv", row.names = FALSE)

# 9. VALIDACIÓN DE SUPUESTOS
# ----------------------------

library(biotools)
library(clue)
library(cluster)
library(clValid)

diagnosticos_clusters <- function(data, 
                                  vars, 
                                  cluster_col = "cluster_gmm", 
                                  year_label = NULL) {
  
  # Verificar columna de clusters
  if (!cluster_col %in% colnames(data)) {
    stop("La columna '", cluster_col, "' no existe en los datos.")
  }
  
  # Extraer año si no se proporciona
  if (is.null(year_label) && "tcode" %in% colnames(data)) {
    year_label <- unique(data$tcode)[1]
  }
  
  # Asegurar que cluster sea factor
  data[[cluster_col]] <- as.factor(data[[cluster_col]])
  
  # 1. Pruebas de normalidad (Shapiro-Wilk por grupo y variable)
  grupos <- levels(data[[cluster_col]])
  resultados_norm <- data.frame()
  
  for (v in vars) {
    for (g in grupos) {
      # Extraer valores del grupo
      valores <- data[data[[cluster_col]] == g, v]
      valores <- valores[!is.na(valores)]
      
      # Shapiro.test requiere al menos 3 observaciones
      if (length(valores) >= 3 && length(valores) <= 5000) {
        p_val <- tryCatch(shapiro.test(valores)$p.value, error = function(e) NA)
      } else {
        p_val <- NA
      }
      
      resultados_norm <- rbind(resultados_norm, data.frame(
        cluster = g,
        variable = v,
        shapiro_p = p_val,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Añadir año
  if (!is.null(year_label)) {
    resultados_norm <- cbind(anio = year_label, resultados_norm)
  }
  
  # 2. Prueba de homogeneidad de varianzas (Levene)
  resultados_levene <- data.frame()
  
  for (v in vars) {
    # Construir fórmula con backticks para nombres de variables
    formula <- as.formula(paste("`", v, "` ~ `", cluster_col, "`", sep = ""))
    
    test <- tryCatch(
      car::leveneTest(formula, data = data),
      error = function(e) NULL
    )
    
    if (!is.null(test) && nrow(test) > 1) {
      # La salida de leveneTest tiene dos filas: grupo y residuales
      F_value <- test$`F value`[1]
      p_value <- test$`Pr(>F)`[1]
      homocedastico <- p_value > 0.05
    } else {
      F_value <- NA
      p_value <- NA
      homocedastico <- NA
    }
    
    resultados_levene <- rbind(resultados_levene, data.frame(
      variable = v,
      F_value = F_value,
      p_value = p_value,
      homocedastico = homocedastico,
      stringsAsFactors = FALSE
    ))
  }
  
  # Añadir año
  if (!is.null(year_label)) {
    resultados_levene <- cbind(anio = year_label, resultados_levene)
  }
  
  list(
    normalidad = resultados_norm,
    levene = resultados_levene
  )
}

# Lista para almacenar resultados
diagnosticos_anios <- list()

for (anio in names(resultados_avanzados)) {
  cat("\n========== Procesando diagnóstico para año:", anio, "==========\n")
  res <- resultados_avanzados[[anio]]
  if (is.null(res)) next
  
  diag <- tryCatch(
    diagnosticos_clusters(
      data = res$data_actualizado,
      vars = vi,
      cluster_col = "cluster_gmm",
      year_label = anio
    ),
    error = function(e) {
      cat("Error en año", anio, ":", e$message, "\n")
      NULL
    }
  )
  diagnosticos_anios[[anio]] <- diag
}

# Normalidad
normalidad_all <- bind_rows(lapply(diagnosticos_anios, function(x) x$normalidad), .id = "anio")
# Levene
levene_all <- bind_rows(lapply(diagnosticos_anios, function(x) x$levene), .id = "anio")

write.csv(normalidad_all, "normalidad_por_cluster.csv", row.names = FALSE)
write.csv(levene_all, "levene_homogeneidad.csv", row.names = FALSE)

# Normalidad
kable(normalidad_all, digits = 4, caption = "Pruebas de normalidad (Shapiro-Wilk) por cluster y variable") %>%
  kable_styling("striped")

# Levene
kable(levene_all, digits = 4, caption = "Prueba de Levene para homogeneidad de varianzas") %>%
  kable_styling("striped")

library(fpc)      # para índices alternativos

diagnosticos_clusters_robusto <- function(data, vars, cluster_col = "cluster_gmm",
                                          year_label = NULL, n_bootstrap = 50) {
  
  # Función auxiliar para añadir año sin errores
  add_year <- function(df, yr) {
    if (is.null(yr)) return(df)
    if (nrow(df) == 0) return(data.frame(anio = character(0), df, stringsAsFactors = FALSE))
    cbind(data.frame(anio = rep(yr, nrow(df)), stringsAsFactors = FALSE), df)
  }
  
  if (!cluster_col %in% colnames(data)) stop("Falta la columna de clústeres.")
  if (length(vars) == 0) stop("No se especificaron variables.")
  if (is.null(year_label) && "tcode" %in% colnames(data)) year_label <- unique(data$tcode)[1]
  
  data[[cluster_col]] <- as.factor(data[[cluster_col]])
  grupos <- levels(data[[cluster_col]])
  k <- length(grupos)
  p <- length(vars)
  
  if (k < 2) {
    warning("Se requieren al menos 2 grupos.")
    return(NULL)
  }
  
  # Verificación crítica: tamaño mínimo por grupo
  tamaños <- table(data[[cluster_col]])
  if (any(tamaños <= p + 2)) {
    warning("Algún grupo tiene muy pocas observaciones (<= p+2). Algunos índices podrían no calcularse.")
  }
  
  # 1. Normalidad multivariada (Mardia) --------------------------------------------
  norm_multi <- data.frame()
  for (g in grupos) {
    dat_g <- na.omit(data[data[[cluster_col]] == g, vars, drop = FALSE])
    s <- p_s <- kurt <- p_k <- NA
    if (nrow(dat_g) > p + 2) {
      mard <- tryCatch(psych::mardia(dat_g, plot = FALSE), error = function(e) NULL)
      if (!is.null(mard)) {
        if (length(mard$skewness) == 1) s <- mard$skewness
        if (length(mard$p.skew) == 1)   p_s <- mard$p.skew
        if (length(mard$kurtosis) == 1) kurt <- mard$kurtosis
        if (length(mard$p.kurt) == 1)   p_k <- mard$p.kurt
      }
    }
    norm_multi <- rbind(norm_multi, data.frame(
      cluster = g, skewness = s, p_skew = p_s,
      kurtosis = kurt, p_kurt = p_k, stringsAsFactors = FALSE
    ))
  }
  norm_multi <- add_year(norm_multi, year_label)
  
  # 2. Box's M --------------------------------------------------------------------
  box_result <- tryCatch({
    bt <- biotools::boxM(data[, vars], grouping = data[[cluster_col]])
    add_year(data.frame(statistic = bt$statistic, p_value = bt$p.value), year_label)
  }, error = function(e) add_year(data.frame(statistic = NA, p_value = NA), year_label))
  
    
  # 3. Estabilidad bootstrap (Jaccard) y separación (silueta, Dunn alternativo) ----
  estabilidad <- NULL
  if (n_bootstrap > 0) {
    # Distancia para silueta / índices
    dist_orig <- tryCatch(dist(data[, vars]), error = function(e) NULL)
    
    if (!is.null(dist_orig)) {
      sil_orig <- tryCatch(
        as.numeric(summary(silhouette(as.integer(data[[cluster_col]]), dist_orig))$avg.width),
        error = function(e) NA
      )
      # Usamos cluster.stats de fpc (más robusto que dunn a veces)
      dunn_orig <- tryCatch({
        cs <- fpc::cluster.stats(d = dist_orig, clustering = as.integer(data[[cluster_col]]),
                                 silhouette = FALSE, G2 = FALSE, G3 = FALSE)
        cs$dunn2   # índice de Dunn generalizado (más estable)
      }, error = function(e) NA)
    } else {
      sil_orig <- NA
      dunn_orig <- NA
    }
    
    # Bootstrap
    jaccard_boot <- numeric(n_bootstrap)
    for (b in seq_len(n_bootstrap)) {
      idx <- sample(nrow(data), replace = TRUE)
      boot_data <- data[idx, ]
      fit_b <- tryCatch(
        Mclust(boot_data[, vars], G = k, modelNames = best_model, verbose = FALSE),
        error = function(e) NULL
      )
      if (!is.null(fit_b) && length(unique(fit_b$classification)) == k) {
        orig_cl <- as.integer(data[[cluster_col]][idx])
        boot_cl <- fit_b$classification
        jacc_mat <- matrix(0, k, k)
        for (i in 1:k) {
          for (j in 1:k) {
            inter <- sum(orig_cl == i & boot_cl == j)
            union <- sum(orig_cl == i | boot_cl == j)
            jacc_mat[i, j] <- if (union > 0) inter / union else 0
          }
        }
        asignacion <- clue::solve_LSAP(1 - jacc_mat)
        jacc_boot_vals <- jacc_mat[cbind(1:k, asignacion)]
        jaccard_boot[b] <- mean(jacc_boot_vals)
      } else {
        jaccard_boot[b] <- NA
      }
    }
    jaccard_boot <- jaccard_boot[!is.na(jaccard_boot)]
    estabilidad <- list(
      jaccard_medio       = if (length(jaccard_boot) > 0) mean(jaccard_boot) else NA,
      jaccard_bootstrap   = jaccard_boot,
      silhouette_original = sil_orig,
      dunn_original       = dunn_orig
    )
  }
  
  list(
    normalidad_multi = norm_multi,
    box_m            = box_result,
    estabilidad      = estabilidad
  )
}

diagnosticos_anios_g <- list()

for (anio in names(resultados_avanzados)) {
  cat("\n==========", anio, "==========\n")
  res <- resultados_avanzados[[anio]]
  if (is.null(res)) next
  
  diag <- tryCatch(
    diagnosticos_clusters_robusto(
      data          = res$data_actualizado,
      vars          = vi,
      cluster_col   = "cluster_gmm",
      year_label    = anio,
      n_bootstrap   = 50
    ),
    error = function(e) {
      cat("Error en", anio, ":", e$message, "\n")
      NULL
    }
  )
  diagnosticos_anios_g[[anio]] <- diag
}

norm_multi_all   <- bind_rows(lapply(diagnosticos_anios_g, `[[`, "normalidad_multi"), .id = "anio")
box_m_all        <- bind_rows(lapply(diagnosticos_anios_g, `[[`, "box_m"), .id = "anio")

# Estabilidad (tabla resumen)
estabilidad_tab <- do.call(rbind, lapply(names(diagnosticos_anios_g), function(a) {
  e <- diagnosticos_anios_g[[a]]$estabilidad
  if (!is.null(e)) {
    data.frame(anio = a, jaccard = e$jaccard_medio,
               silueta = e$silhouette_original, dunn = e$dunn_original)
  }
}))

# ------------------------------------------------------------
# 1. Combinar todos los años en data.frames únicos
# ------------------------------------------------------------


# Normalidad multivariada (Mardia)
norm_multi_all <- bind_rows(lapply(diagnosticos_anios_g, `[[`, "normalidad_multi"), .id = "anio")

# Box's M
box_m_all <- bind_rows(lapply(diagnosticos_anios_g, `[[`, "box_m"), .id = "anio")

# Estabilidad (resumen tabular)
estabilidad_tab <- do.call(rbind, lapply(names(diagnosticos_anios_g), function(a) {
  e <- diagnosticos_anios_g[[a]]$estabilidad
  if (!is.null(e)) {
    data.frame(anio = a,
               jaccard_medio = e$jaccard_medio,
               silhouette    = e$silhouette_original,
               dunn          = e$dunn_original,
               stringsAsFactors = FALSE)
  } else {
    NULL
  }
}))

# ------------------------------------------------------------
# 2. Guardar como CSV
# ------------------------------------------------------------
write.csv(norm_multi_all,  "normalidad_multivariada.csv", row.names = FALSE)
write.csv(box_m_all,       "box_m_homogeneidad.csv",      row.names = FALSE)
write.csv(estabilidad_tab, "estabilidad_clusters.csv",     row.names = FALSE)

# ------------------------------------------------------------
# 3. Mostrar tablas con kable (formato HTML o LaTeX)
# ------------------------------------------------------------
# Normalidad multivariada
kable(norm_multi_all, digits = 4,
      caption = "Normalidad multivariada (Mardia) por conglomerado") %>%
  kable_styling("striped", full_width = FALSE)

# Box's M
kable(box_m_all, digits = 4,
      caption = "Prueba de homogeneidad de covarianzas (Box's M)") %>%
  kable_styling("striped", full_width = FALSE)

# Estabilidad y separación
kable(estabilidad_tab, digits = 4,
      caption = "Estabilidad bootstrap e índices de separación") %>%
  kable_styling("striped", full_width = FALSE)

# Función para calcular entropía normalizada a partir de la matriz z
entropia_normalizada <- function(z) {
  k <- ncol(z)
  n <- nrow(z)
  z[z < 1e-10] <- 1e-10   # evitar log(0)
  entropia_total <- -sum(z * log(z))
  1 - entropia_total / (n * log(k))
}

# Función para obtener la entropía por grupo
entropia_por_grupo <- function(z, grupos) {
  k <- ncol(z)
  res <- data.frame()
  for (g in unique(grupos)) {
    idx <- which(grupos == g)
    if (length(idx) > 0) {
      z_g <- z[idx, , drop = FALSE]
      z_g[z_g < 1e-10] <- 1e-10
      entropia_g <- -sum(z_g * log(z_g))
      E_g <- 1 - entropia_g / (length(idx) * log(k))
      res <- rbind(res, data.frame(cluster = g, entropia = E_g))
    }
  }
  res
}

# Bucle por años
entropia_anios <- data.frame()
entropia_detalle <- data.frame()

for (anio in names(resultados_avanzados)) {
  res <- resultados_avanzados[[anio]]
  
  # Determinar qué modelo GMM usar (prioridad: gmm_fijo, luego gmm_auto)
  modelo <- NULL
  if ("gmm_fijo" %in% names(res)) {
    modelo <- res$gmm_fijo
  } else if ("gmm_auto" %in% names(res)) {
    modelo <- res$gmm_auto
  }
  
  if (is.null(modelo)) {
    cat("Año", anio, ": No se encontró modelo GMM.\n")
    next
  }
  
  z <- modelo$z
  if (is.null(z)) {
    cat("Año", anio, ": Matriz z no disponible.\n")
    next
  }
  
  # Entropía global
  E_global <- entropia_normalizada(z)
  entropia_anios <- rbind(entropia_anios, data.frame(anio = anio, entropia = E_global))
  
  # Entropía por grupo (necesita la asignación dura)
  grupos <- modelo$classification
  detalle <- entropia_por_grupo(z, grupos)
  detalle$anio <- anio
  entropia_detalle <- rbind(entropia_detalle, detalle)
  
  cat("Año", anio, ": Entropía normalizada =", round(E_global, 4), "\n")
}

# Mostrar tablas

kable(entropia_anios, digits = 4, 
      caption = "Entropía normalizada de clasificación por año (GMM)") %>%
  kable_styling("striped")

kable(entropia_detalle, digits = 4, 
      caption = "Entropía por conglomerado y año") %>%
  kable_styling("striped")

# Guardar entropía global por año
write.csv(entropia_anios, "entropia_clasificacion_global.csv", row.names = FALSE)

# Guardar entropía detallada por grupo y año
write.csv(entropia_detalle, "entropia_clasificacion_por_grupo.csv", row.names = FALSE)


# 10. MANOVA y PERMANOVA global
# ----------

# Función para instalar/verificar pairwiseAdonis si es necesario
ensure_pairwiseAdonis <- function() {
  if (!requireNamespace("pairwiseAdonis", quietly = TRUE)) {
    message("Instalando pairwiseAdonis desde GitHub...")
    if (!requireNamespace("devtools", quietly = TRUE)) {
      install.packages("devtools")
    }
    devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
  }
  library(pairwiseAdonis)
}

library(pairwiseAdonis)

# Función multivariate_tests 
multivariate_tests <- function(data, 
                               vars, 
                               cluster_col = "cluster_gmm", 
                               year_label = NULL,
                               permutations = 999,
                               run_pairwise = TRUE) {
  
  if (!cluster_col %in% colnames(data)) {
    stop("La columna '", cluster_col, "' no existe en los datos.")
  }
  
  if (is.null(year_label) && "tcode" %in% colnames(data)) {
    year_label <- unique(data$tcode)[1]
  }
  
  data[[cluster_col]] <- as.factor(data[[cluster_col]])
  n_clusters <- nlevels(data[[cluster_col]])
  if (n_clusters < 2) {
    warning("Año ", year_label, " tiene menos de 2 clusters. Se omiten pruebas multivariadas.")
    return(list(manova = NULL, permanova_global = NULL, permanova_pares = NULL))
  }
  
  # 1. MANOVA
  manova_formula <- as.formula(paste("cbind(", paste(vars, collapse = ","), ") ~ `", cluster_col, "`", sep = ""))
  manova_model <- tryCatch(
    manova(manova_formula, data = data),
    error = function(e) {
      warning("Error en MANOVA para año ", year_label, ": ", e$message)
      return(NULL)
    }
  )
  
  manova_summary <- NULL
  if (!is.null(manova_model)) {
    manova_summary <- tryCatch(
      summary(manova_model, test = "Wilks"),
      error = function(e) {
        warning("Error en summary de MANOVA para año ", year_label, ": ", e$message)
        NULL
      }
    )
  }
  
  # 2. PERMANOVA global
  datos_matrix <- data %>% dplyr::select(all_of(vars)) %>% as.matrix()
  dist_obj <- dist(datos_matrix)
  
  permanova_global <- tryCatch(
    adonis2(dist_obj ~ data[[cluster_col]], permutations = permutations),
    error = function(e) {
      warning("Error en PERMANOVA global para año ", year_label, ": ", e$message)
      return(NULL)
    }
  )
  
  # 3. PERMANOVA por pares (solo si global es significativo)
  permanova_pares <- NULL
  if (run_pairwise && !is.null(permanova_global)) {
    p_global <- permanova_global[1, "Pr(>F)"]
    if (!is.na(p_global) && p_global < 0.05) {
      ensure_pairwiseAdonis()
      # Construir fórmula correcta para pairwise.adonis2
      formula_pairs <- as.formula(paste("dist_obj ~", cluster_col))
      permanova_pares <- tryCatch(
        pairwise.adonis2(formula_pairs, data = data, permutations = permutations),
        error = function(e) {
          warning("Error en pairwise.adonis2 para año ", year_label, ": ", e$message)
          NULL
        }
      )
    } else {
      message("PERMANOVA global no significativo (p = ", round(p_global, 4), ") para año ", year_label, ". No se realizan comparaciones por pares.")
    }
  }
  
  list(
    anio = year_label,
    manova = manova_summary,
    permanova_global = permanova_global,
    permanova_pares = permanova_pares
  )
}

# Aplicar a todos los años con bucle
resultados_multivariados <- list()

for (anio in names(resultados_avanzados)) {
  cat("\n========== Procesando pruebas multivariadas para año:", anio, "==========\n")
  res <- resultados_avanzados[[anio]]
  if (is.null(res)) next
  
  df_actualizado <- res$data_actualizado
  
  test <- tryCatch(
    multivariate_tests(
      data = df_actualizado,
      vars = vi,
      cluster_col = "cluster_gmm",
      year_label = anio,
      permutations = 999,
      run_pairwise = TRUE
    ),
    error = function(e) {
      cat("❌ Error en año", anio, ":", e$message, "\n")
      NULL
    }
  )
  resultados_multivariados[[anio]] <- test
}

# Extraer tablas de PERMANOVA global
permanova_global_all <- bind_rows(lapply(resultados_multivariados, function(x) {
  if (!is.null(x$permanova_global)) {
    as.data.frame(x$permanova_global) %>% 
      tibble::rownames_to_column("termino") %>%
      mutate(anio = x$anio, .before = 1)
  }
}))

permanova_global_all

# Guardar
write.csv(permanova_global_all, "permanova_global_todos_anos.csv", row.names = FALSE)

# Guardar resultados completos en un archivo RDS
saveRDS(resultados_multivariados, "resultados_multivariados.rds")



# Prueba de Discriminación: Linear Discriminant Analysis (LDA)

evaluar_lda <- function(data, 
                        cluster_col, 
                        predictors, 
                        year_label = NULL,
                        cv = TRUE) {
  
  # Verificar columnas
  if (!cluster_col %in% colnames(data)) {
    warning("Año ", year_label, ": columna '", cluster_col, "' no encontrada. Se omite.")
    return(NULL)
  }
  missing_pred <- predictors[!predictors %in% colnames(data)]
  if (length(missing_pred) > 0) {
    warning("Año ", year_label, ": predictores faltantes: ", 
            paste(missing_pred, collapse = ", "), ". Se omite.")
    return(NULL)
  }
  
  # Preparar datos: asegurar cluster como factor y eliminar NAs
  data_clean <- data %>%
    mutate(cluster = as.factor(.data[[cluster_col]])) %>%
    dplyr::select(cluster, all_of(predictors)) %>%
    tidyr::drop_na()
  
  # Validaciones
  if (nrow(data_clean) == 0) {
    warning("Año ", year_label, ": sin datos después de eliminar NAs.")
    return(NULL)
  }
  n_clusters <- nlevels(data_clean$cluster)
  if (n_clusters < 2) {
    warning("Año ", year_label, ": menos de 2 clusters.")
    return(NULL)
  }
  cluster_counts <- table(data_clean$cluster)
  if (any(cluster_counts < 2)) {
    warning("Año ", year_label, ": algún cluster tiene menos de 2 observaciones.")
    return(NULL)
  }
  
  # Fórmula
  formula <- as.formula(paste("cluster ~", paste(predictors, collapse = " + ")))
  
  # Modelo LDA sin CV (opcional, para coeficientes)
  lda_model <- tryCatch(
    lda(formula, data = data_clean),
    error = function(e) {
      warning("Año ", year_label, ": error en LDA: ", e$message)
      NULL
    }
  )
  if (is.null(lda_model)) return(NULL)
  
  # Validación cruzada
  if (cv) {
    lda_cv <- tryCatch(
      lda(formula, data = data_clean, CV = TRUE),
      error = function(e) {
        warning("Año ", year_label, ": error en LDA CV: ", e$message)
        NULL
      }
    )
    if (is.null(lda_cv)) return(NULL)
    
    conf_matrix <- table(Predicho = lda_cv$class, Real = data_clean$cluster)
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  } else {
    lda_cv <- NULL
    conf_matrix <- NULL
    accuracy <- NA
  }
  
  list(
    anio = year_label,
    cluster_col = cluster_col,
    accuracy = accuracy,
    conf_matrix = conf_matrix,
    lda_model = lda_model,
    lda_cv = lda_cv,
    n_obs = nrow(data_clean),
    n_clusters = n_clusters
  )
}

# Tipos de cluster a evaluar
tipos_cluster <- c("cluster_gmm", "cluster_spec")

# Lista para almacenar resultados
resultados_lda <- list()

for (anio in names(resultados_avanzados)) {
  cat("\n========== Procesando año:", anio, "==========\n")
  res_anio <- resultados_avanzados[[anio]]
  df_anio <- res_anio$data_actualizado   # data frame del año
  
  for (clust in tipos_cluster) {
    if (!clust %in% colnames(df_anio)) {
      cat("  -", clust, "no encontrado, se omite.\n")
      next
    }
    
    cat("  - Evaluando", clust, "...\n")
    res_lda <- evaluar_lda(
      data = df_anio,
      cluster_col = clust,
      predictors = vi,
      year_label = paste(anio, clust, sep = "_"),
      cv = TRUE
    )
    
    if (!is.null(res_lda)) {
      resultados_lda[[paste(anio, clust, sep = "_")]] <- res_lda
      cat("    Accuracy:", round(res_lda$accuracy, 4), "\n")
    }
  }
}

# Criterio: Accuracy > 0.75 indica clusters bien diferenciados

importancia_variables_lda <- function(lda_model, data = NULL, metodo = "ambos") {
  
  if (!inherits(lda_model, "lda")) {
    stop("El objeto debe ser de clase 'lda'")
  }
  
  # Coeficientes escalados (scaling)
  coefs <- as.data.frame(lda_model$scaling)
  coefs$variable <- rownames(coefs)
  
  # Importancia basada en coeficientes: media de valores absolutos por variable
  imp_coef <- data.frame(
    variable = coefs$variable,
    importancia_coef = rowMeans(abs(coefs[, -ncol(coefs), drop = FALSE]))
  )
  
  resultado <- imp_coef
  
  # Si se solicita loadings y se proporcionan datos, calcular correlaciones
  if (metodo %in% c("loadings", "ambos") && !is.null(data)) {
    # Predecir puntuaciones discriminantes
    pred <- predict(lda_model, data)
    scores <- pred$x  # matriz de puntuaciones
    
    # Calcular correlación entre cada variable y cada función discriminante
    vars_num <- data[, rownames(lda_model$scaling), drop = FALSE]
    correlaciones <- cor(vars_num, scores, use = "complete.obs")
    
    # Importancia como media de valores absolutos de las correlaciones
    imp_load <- data.frame(
      variable = rownames(correlaciones),
      importancia_load = rowMeans(abs(correlaciones))
    )
    
    resultado <- merge(imp_coef, imp_load, by = "variable", all = TRUE)
  }
  
  # Ordenar por la primera columna de importancia (descendente)
  orden_col <- grep("importancia", names(resultado))[1]
  resultado <- resultado[order(resultado[[orden_col]], decreasing = TRUE), ]
  rownames(resultado) <- NULL
  
  return(resultado)
}

# Ejemplo para un modelo específico

calcular_importancia_lda_anios <- function(anios, metodo = "ambos", output_dir = ".") {
  # Verificar que los objetos globales existen
  if (!exists("resultados_lda") || !exists("resultados_avanzados")) {
    stop("No se encontraron los objetos 'resultados_lda' o 'resultados_avanzados' en el entorno global.")
  }
  
  resultados <- list()
  
  for (anio in anios) {
    cat("\n", strrep("-", 50), "\n")
    cat("Procesando año:", anio, "\n")
    cat(strrep("-", 50), "\n")
    
    # Construir nombres de acceso
    nombre_lda <- paste0(anio, "_cluster_gmm")
    nombre_data <- as.character(anio)
    
    # Verificar existencia en las listas
    if (is.null(resultados_lda[[nombre_lda]])) {
      warning("No se encontró '", nombre_lda, "' en resultados_lda. Se omite el año.")
      next
    }
    if (is.null(resultados_avanzados[[nombre_data]])) {
      warning("No se encontró '", nombre_data, "' en resultados_avanzados. Se omite el año.")
      next
    }
    
    lda_model <- resultados_lda[[nombre_lda]]$lda_model
    data <- resultados_avanzados[[nombre_data]]$data_actualizado
    
    # Calcular importancia
    imp <- importancia_variables_lda(
      lda_model = lda_model,
      data = data,
      metodo = metodo
    )
    
    # Mostrar resultado
    print(imp)
    
    # Guardar CSV
    archivo <- file.path(output_dir, paste0("importancia_", anio, "_gmm.csv"))
    write.csv(imp, archivo, row.names = FALSE)
    cat("Archivo guardado:", archivo, "\n")
    
    resultados[[as.character(anio)]] <- imp
  }
  
  invisible(resultados)
}

anios <- c(2003, 2008, 2013, 2018, 2023)

# Ejecutar
resultados_importancia <- calcular_importancia_lda_anios(anios)

### pruebas de Kruskal-Wallis y Friedman

test_kw_friedman <- function(resultados_avanzados,
                             vars,
                             cluster_cols = c("cluster_gmm", "cluster_spec"),
                             anos = NULL,
                             id_col = "ID") {  # necesario para Friedman
  
  # Si no se especifican años, tomar todos
  if (is.null(anos)) anos <- names(resultados_avanzados)
  
  # Verificar que los años existen
  anos_existentes <- intersect(anos, names(resultados_avanzados))
  if (length(anos_existentes) == 0) stop("Ninguno de los años especificados existe en resultados_avanzados.")
  
  # ----- Kruskal-Wallis por año y variable -----
  resultados_kw <- list()
  
  for (anio in anos_existentes) {
    cat("\n📅 Año:", anio, "\n")
    df_anio <- resultados_avanzados[[anio]]$data_actualizado
    
    for (clust in cluster_cols) {
      if (!clust %in% colnames(df_anio)) {
        cat("  ⚠️ Cluster", clust, "no encontrado en año", anio, "- se omite.\n")
        next
      }
      
      cat("  🔹 Cluster:", clust, "\n")
      
      for (var in vars) {
        if (!var %in% colnames(df_anio)) {
          cat("    ⚠️ Variable", var, "no encontrada - se omite.\n")
          next
        }
        
        # Preparar datos
        datos_test <- df_anio %>%
          dplyr::select(cluster = !!sym(clust), valor = !!sym(var)) %>%
          tidyr::drop_na()
        
        # Verificar condiciones
        if (nrow(datos_test) < 3) {
          cat("    ⚠️", var, ": menos de 3 obs, se omite.\n")
          next
        }
        if (length(unique(datos_test$cluster)) < 2) {
          cat("    ⚠️", var, ": menos de 2 clusters, se omite.\n")
          next
        }
        
        # Kruskal-Wallis
        kw_test <- tryCatch(
          kruskal.test(valor ~ cluster, data = datos_test),
          error = function(e) NULL
        )
        
        if (!is.null(kw_test)) {
          resultados_kw[[paste(anio, clust, var, sep = "_")]] <- data.frame(
            anio = anio,
            cluster = clust,
            variable = var,
            estadistico = kw_test$statistic,
            p_valor = kw_test$p.value,
            df = kw_test$parameter,
            n_total = nrow(datos_test),
            n_clusters = length(unique(datos_test$cluster)),
            stringsAsFactors = FALSE
          )
          cat("    ✅", var, "- p =", format(kw_test$p.value, digits = 4), "\n")
        } else {
          cat("    ❌", var, "- error en prueba\n")
        }
      }
    }
  }
  
  # Consolidar KW
  df_kw <- bind_rows(resultados_kw)
  
  # ----- Friedman (requiere datos balanceados: mismo ID en todos los años) -----
  resultados_friedman <- list()
  
  # Para Friedman, necesitamos datos con la misma unidad a través del tiempo
  # Primero, verificamos que exista la columna ID en todos los años
  id_disponible <- all(sapply(anos_existentes, function(anio) {
    id_col %in% colnames(resultados_avanzados[[anio]]$data_actualizado)
  }))
  
  if (id_disponible) {
    cat("\n📊 Realizando prueba de Friedman (comparación temporal por ID y cluster)...\n")
    
    # Unir datos de todos los años para cada ID
    datos_largos <- map_dfr(anos_existentes, function(anio) {
      df_anio <- resultados_avanzados[[anio]]$data_actualizado
      df_anio %>%
        dplyr::select(!!sym(id_col), all_of(vars), all_of(cluster_cols)) %>%
        mutate(anio = anio)
    })
    
    for (clust in cluster_cols) {
      if (!clust %in% colnames(datos_largos)) next
      
      cat("\n  🔹 Cluster:", clust, "\n")
      
      for (var in vars) {
        if (!var %in% colnames(datos_largos)) next
        
        # Preparar datos en formato ancho: una columna por año
        datos_ancho <- datos_largos %>%
          filter(!is.na(!!sym(clust))) %>%
          dplyr::select(!!sym(id_col), anio, !!sym(clust), valor = !!sym(var)) %>%
          tidyr::drop_na() %>%
          tidyr::pivot_wider(names_from = anio, values_from = valor, values_fill = NA)
        
        # Filtrar IDs que tengan datos en todos los años
        # BUG FIX: excluir columnas que no son anios (id_col y clust)
        anos_presentes <- setdiff(colnames(datos_ancho), c(id_col, clust))
        if (length(anos_presentes) < 2) {
          cat("    ⚠️", var, ": menos de 2 años, se omite.\n")
          next
        }
        
        # Filtrar IDs que tengan datos en todos los años
        datos_completos <- datos_ancho %>%
          filter(complete.cases(.))
        
        if (nrow(datos_completos) < 3) {
          cat("    ⚠️", var, ": menos de 3 IDs completos, se omite.\n")
          next
        }
        
        # Matriz para Friedman: filas = IDs, columnas = años
        matriz_fried <- as.matrix(datos_completos[, -1])
        
        fried_test <- tryCatch(
          friedman.test(matriz_fried),
          error = function(e) NULL
        )
        
        if (!is.null(fried_test)) {
          resultados_friedman[[paste(clust, var, sep = "_")]] <- data.frame(
            cluster = clust,
            variable = var,
            estadistico = fried_test$statistic,
            p_valor = fried_test$p.value,
            df = fried_test$parameter,
            n_ids = nrow(datos_completos),
            n_anos = length(anos_presentes),
            stringsAsFactors = FALSE
          )
          cat("    ✅", var, "- p =", format(fried_test$p.value, digits = 4), "\n")
        } else {
          cat("    ❌", var, "- error en prueba\n")
        }
      }
    }
  } else {
    cat("\n⚠️ No se pudo realizar Friedman: falta la columna '", id_col, "' en algunos años.\n")
  }
  
  df_friedman <- bind_rows(resultados_friedman)
  
  list(
    kruskal_wallis = df_kw,
    friedman = df_friedman
  )
}

# Ejecutar pruebas
resultados_pruebas <- test_kw_friedman(
  resultados_avanzados = resultados_avanzados,
  vars = vi,
  cluster_cols = c("cluster_gmm", "cluster_spec"),
  anos = names(resultados_avanzados), 
  id_col = "ID"  
)

# Ver resultados
print(resultados_pruebas$kruskal_wallis)
print(resultados_pruebas$friedman)

# Guardar en CSV
write.csv(resultados_pruebas$kruskal_wallis, "kruskal_wallis_resultados.csv", row.names = FALSE)
if (nrow(resultados_pruebas$friedman) > 0) {
  write.csv(resultados_pruebas$friedman, "friedman_resultados.csv", row.names = FALSE)
}

### Analísis post-hoc

comparaciones_pares <- function(data, 
                                cluster_col, 
                                vars, 
                                p_adjust_method = "bonferroni",
                                test_type = c("dunn", "wilcox")) {
  
  # Verificar paquetes necesarios
  if (!requireNamespace("dunn.test", quietly = TRUE)) {
    install.packages("dunn.test")
  }
  library(dunn.test)
  
  # Verificar columna de clusters
  if (!cluster_col %in% colnames(data)) {
    stop("La columna '", cluster_col, "' no existe en los datos.")
  }
  
  # Asegurar que cluster sea factor
  data[[cluster_col]] <- as.factor(data[[cluster_col]])
  clusters_unicos <- levels(data[[cluster_col]])
  n_clusters <- length(clusters_unicos)
  
  if (n_clusters < 2) {
    stop("Se necesitan al menos 2 clusters para comparaciones por pares.")
  }
  
  # Elegir tipo de prueba automáticamente si no se especifica
  if (missing(test_type)) {
    test_type <- ifelse(n_clusters > 2, "dunn", "wilcox")
  } else {
    test_type <- match.arg(test_type)
  }
  
  # Resultados
  resultados <- list()
  
  for (var in vars) {
    if (!var %in% colnames(data)) {
      warning("La variable '", var, "' no existe en los datos. Se omite.")
      next
    }
    
    # Filtrar datos completos
    datos_temp <- data[!is.na(data[[var]]) & !is.na(data[[cluster_col]]), ]
    if (nrow(datos_temp) == 0) {
      warning("No hay datos completos para la variable ", var)
      next
    }
    
    # Convertir cluster a factor (por si acaso)
    datos_temp[[cluster_col]] <- as.factor(datos_temp[[cluster_col]])
    
    # Verificar que todos los clusters tengan al menos 2 observaciones
    tab <- table(datos_temp[[cluster_col]])
    if (any(tab < 2)) {
      warning("La variable ", var, " tiene clusters con menos de 2 observaciones. Se omiten esos clusters.")
      # Eliminar clusters con menos de 2 obs
      clusters_validos <- names(tab)[tab >= 2]
      datos_temp <- datos_temp[datos_temp[[cluster_col]] %in% clusters_validos, ]
      datos_temp[[cluster_col]] <- droplevels(datos_temp[[cluster_col]])
    }
    
    if (nlevels(datos_temp[[cluster_col]]) < 2) next
    
    # Realizar prueba según el tipo
    if (test_type == "dunn") {
      # Prueba de Dunn (requiere al menos 2 grupos, funciona con >2)
      res_dunn <- dunn.test::dunn.test(
        x = datos_temp[[var]],
        g = datos_temp[[cluster_col]],
        method = p_adjust_method,
        kw = FALSE,        # No mostrar resultado de Kruskal-Wallis
        label = TRUE,
        wrap = FALSE,
        table = FALSE,
        list = FALSE,
        altp = TRUE        # Usar p-valores alternativos (recomendado)
      )
      
      # Extraer resultados en un dataframe
      comparaciones <- data.frame(
        comparacion = res_dunn$comparisons,
        estadistico = res_dunn$Z,
        p_valor = res_dunn$altP,          # p-valor alternativo (más preciso)
        p_ajustado = res_dunn$altP.adjust, # p-valor ajustado
        stringsAsFactors = FALSE
      )
      
    } else { # test_type == "wilcox"
      # Mann-Whitney para dos grupos (si hay más de 2, se harán todas las combinaciones)
      if (nlevels(datos_temp[[cluster_col]]) == 2) {
        # Solo dos grupos, una comparación
        grupos <- levels(datos_temp[[cluster_col]])
        wt <- wilcox.test(
          x = datos_temp[[var]][datos_temp[[cluster_col]] == grupos[1]],
          y = datos_temp[[var]][datos_temp[[cluster_col]] == grupos[2]],
          exact = FALSE,
          correct = TRUE
        )
        comparaciones <- data.frame(
          comparacion = paste(grupos[1], "-", grupos[2]),
          estadistico = wt$statistic,
          p_valor = wt$p.value,
          p_ajustado = wt$p.value,  # solo una comparación, no se ajusta
          stringsAsFactors = FALSE
        )
      } else {
        # Múltiples comparaciones con wilcox.test y ajuste manual
        combinaciones <- combn(levels(datos_temp[[cluster_col]]), 2, simplify = FALSE)
        comparaciones_list <- lapply(combinaciones, function(par) {
          x <- datos_temp[[var]][datos_temp[[cluster_col]] == par[1]]
          y <- datos_temp[[var]][datos_temp[[cluster_col]] == par[2]]
          wt <- wilcox.test(x, y, exact = FALSE, correct = TRUE)
          data.frame(
            comparacion = paste(par[1], "-", par[2]),
            estadistico = wt$statistic,
            p_valor = wt$p.value,
            stringsAsFactors = FALSE
          )
        })
        comparaciones <- do.call(rbind, comparaciones_list)
        # Ajustar p-valores
        comparaciones$p_ajustado <- p.adjust(comparaciones$p_valor, method = p_adjust_method)
      }
    }
    
    # Añadir columna de significancia
    comparaciones$significativo <- ifelse(comparaciones$p_ajustado < 0.05, "Sí", "No")
    
    # Ordenar por p-valor ajustado
    comparaciones <- comparaciones[order(comparaciones$p_ajustado), ]
    
    # Guardar en la lista
    resultados[[var]] <- comparaciones
  }
  
  return(resultados)
}

# Definir tipos de cluster a evaluar
tipos_cluster <- c("cluster_gmm", "cluster_spec")

# Lista para almacenar resultados
resultados_pares <- list()

for (anio in names(resultados_avanzados)) {
  cat("\n========== Procesando año:", anio, "==========\n")
  
  # Extraer data frame actualizado
  df_anio <- resultados_avanzados[[anio]]$data_actualizado
  
  for (clust in tipos_cluster) {
    if (!clust %in% colnames(df_anio)) {
      cat("  -", clust, "no encontrado, se omite.\n")
      next
    }
    
    cat("  - Comparaciones por pares para", clust, "...\n")
    
    # Ejecutar función
    res_pares <- tryCatch(
      comparaciones_pares(
        data = df_anio,
        cluster_col = clust,
        vars = vi,
        p_adjust_method = "bonferroni",
        test_type = "dunn"   # o "wilcox"; se autoelige si se omite
      ),
      error = function(e) {
        cat("    Error:", e$message, "\n")
        NULL
      }
    )
    
    if (!is.null(res_pares)) {
      resultados_pares[[paste(anio, clust, sep = "_")]] <- res_pares
      
      # Mostrar un resumen rápido
      for (var in names(res_pares)) {
        n_sig <- sum(res_pares[[var]]$significativo == "Sí")
        cat("      Variable", var, ":", n_sig, "comparaciones significativas\n")
      }
    }
  }
}

# Función para aplanar resultados

resultados_totales <- bind_rows(lapply(names(resultados_pares), function(nombre) {
  lista_var <- resultados_pares[[nombre]]
  bind_rows(lapply(names(lista_var), function(var) {
    lista_var[[var]] %>% mutate(combinacion = nombre, variable = var, .before = 1)
  }))
}))

# Guardar
write.csv(resultados_totales, "comparaciones_pares_todas.csv", row.names = FALSE)


# ================================================================
#### *** Aglomeraciones sectoriales por entidad ***

# Funciones para determinar k óptimo por entidad

# Función GMM con diagnóstico
gmm_opt <- function(data, max_k = 7, entidad_id = "desconocida") {
  cat("GMM para entidad:", entidad_id, " - dim(data):", paste(dim(data), collapse="x"), "\n")
  if (is.null(data) || nrow(data) < 2) {
    cat("  -> datos insuficientes, retorno k=1\n")
    return(list(k_opt = 1, clusters = rep(1, nrow(data))))
  }
  # Eliminar columnas constantes
  cols_var <- apply(data, 2, function(x) length(unique(x)) > 1)
  if (sum(cols_var) == 0) {
    cat("  -> todas las columnas constantes, retorno k=1\n")
    return(list(k_opt = 1, clusters = rep(1, nrow(data))))
  }
  data <- data[, cols_var, drop = FALSE]
  cat("  -> después de eliminar constantes, dim:", paste(dim(data), collapse="x"), "\n")
  
  n <- nrow(data)
  max_k <- min(max_k, n - 1)
  if (max_k < 2) {
    cat("  -> max_k < 2, retorno k=1\n")
    return(list(k_opt = 1, clusters = rep(1, n)))
  }
  
  set.seed(123)
  gmm <- tryCatch(
    Mclust(data, G = 1:max_k, verbose = FALSE),
    error = function(e) {
      cat("  -> error en Mclust:", e$message, "\n")
      NULL
    }
  )
  if (is.null(gmm) || is.null(gmm$G)) {
    cat("  -> GMM no devolvió resultado, retorno k=1\n")
    return(list(k_opt = 1, clusters = rep(1, n)))
  }
  cat("  -> GMM seleccionó k =", gmm$G, "\n")
  list(k_opt = gmm$G, clusters = gmm$classification)
}

# Función espectral con diagnóstico
spectral_opt <- function(data, max_k = 7, entidad_id = "desconocida") {
  cat("Espectral para entidad:", entidad_id, " - dim(data):", paste(dim(data), collapse="x"), "\n")
  if (is.null(data) || nrow(data) < 2) {
    cat("  -> datos insuficientes, retorno k=1\n")
    return(list(k_opt = 1, clusters = rep(1, nrow(data))))
  }
  cols_var <- apply(data, 2, function(x) length(unique(x)) > 1)
  if (sum(cols_var) == 0) {
    cat("  -> todas las columnas constantes, retorno k=1\n")
    return(list(k_opt = 1, clusters = rep(1, nrow(data))))
  }
  data <- data[, cols_var, drop = FALSE]
  cat("  -> después de eliminar constantes, dim:", paste(dim(data), collapse="x"), "\n")
  
  n <- nrow(data)
  max_k <- min(max_k, n - 1)
  if (max_k < 2) {
    cat("  -> max_k < 2, retorno k=1\n")
    return(list(k_opt = 1, clusters = rep(1, n)))
  }
  
  # Si el número de columnas es 1, specc puede fallar; entonces usar k-means simple como alternativa
  if (ncol(data) == 1) {
    cat("  -> solo una variable, usando k-means en su lugar\n")
    set.seed(56)
    km <- kmeans(data, centers = 2) # solo para tener algo
    # Evaluar silueta para varios k con k-means
    sil_vals <- sapply(2:max_k, function(k) {
      set.seed(56)
      km <- kmeans(data, centers = k, nstart = 10)
      sil <- silhouette(km$cluster, dist(data))
      mean(sil[, 3], na.rm = TRUE)
    })
    k_opt <- which.max(sil_vals) + 1
    set.seed(56)
    km_final <- kmeans(data, centers = k_opt, nstart = 10)
    return(list(k_opt = k_opt, clusters = km_final$cluster))
  }
  
  dist_mat <- dist(data)
  sil_vals <- sapply(2:max_k, function(k) {
    set.seed(123)
    spec <- tryCatch(specc(as.matrix(data), centers = k), error = function(e) {
      cat("    error en specc para k=", k, ":", e$message, "\n")
      NULL
    })
    if (is.null(spec)) return(NA)
    sil <- tryCatch(silhouette(as.integer(spec), dist_mat), error = function(e) NA)
    if (inherits(sil, "silhouette")) {
      mean(sil[, 3], na.rm = TRUE)
    } else {
      NA
    }
  })
  cat("  -> sil_vals:", paste(round(sil_vals,3), collapse=", "), "\n")
  if (all(is.na(sil_vals))) {
    cat("  -> todos NA, retorno k=1\n")
    return(list(k_opt = 1, clusters = rep(1, n)))
  }
  k_opt <- which.max(sil_vals) + 1
  cat("  -> k_opt elegido:", k_opt, "\n")
  set.seed(123)
  spec_final <- specc(as.matrix(data), centers = k_opt)
  list(k_opt = k_opt, clusters = as.integer(spec_final))
}


cluster_by_entity <- function(df_ano, 
                              vars, 
                              entity_col = "entidad", 
                              row_id_col = "row_id",
                              min_obs = 3,
                              gmm_fun = gmm_opt,
                              spectral_fun = spectral_opt) {
  
  # Verificar columnas necesarias
  if (!entity_col %in% colnames(df_ano)) {
    stop("La columna '", entity_col, "' no existe en los datos.")
  }
  
  # Añadir row_id si no existe
  if (!row_id_col %in% colnames(df_ano)) {
    df_ano[[row_id_col]] <- 1:nrow(df_ano)
  }
  
  # Seleccionar columnas de interés
  datos_base <- df_ano %>%
    dplyr::select(row_id = !!sym(row_id_col), 
                  entidad = !!sym(entity_col), 
                  all_of(vars))
  
  # Dividir por entidad, filtrando las que tienen al menos min_obs observaciones
  datos_por_entidad <- datos_base %>%
    group_by(entidad) %>%
    filter(n() >= min_obs) %>%
    group_split()
  
  cat("\n📦 Total de entidades a procesar:", length(datos_por_entidad), "\n")
  
  resultados_lista <- list()
  
  for (i in seq_along(datos_por_entidad)) {
    df_ent <- datos_por_entidad[[i]]
    ent <- as.character(unique(df_ent$entidad))
    n_filas <- nrow(df_ent)
    
    cat("\n========================================\n")
    cat("🔹 Procesando entidad", i, ":", ent, "\n")
    cat("   Número de filas:", n_filas, "\n")
    
    # Extraer matriz de datos (solo variables numéricas)
    data_mat <- as.matrix(df_ent[, vars, drop = FALSE])
    
    # GMM
    cat("   --- GMM ---\n")
    gmm_res <- tryCatch(
      gmm_fun(data_mat, entidad_id = ent),
      error = function(e) {
        cat("   ❌ Error en GMM:", e$message, "\n")
        list(k_opt = 1, clusters = rep(1, n_filas))
      }
    )
    # Verificar integridad de la respuesta
    if (is.null(gmm_res$clusters) || length(gmm_res$clusters) != n_filas) {
      cat("   ⚠️  GMM devolvió clusters inválidos, usando k=1\n")
      gmm_res <- list(k_opt = 1, clusters = rep(1, n_filas))
    }
    
    # Espectral
    cat("   --- Espectral ---\n")
    spec_res <- tryCatch(
      spectral_fun(data_mat, entidad_id = ent),
      error = function(e) {
        cat("   ❌ Error en Espectral:", e$message, "\n")
        list(k_opt = 1, clusters = rep(1, n_filas))
      }
    )
    if (is.null(spec_res$clusters) || length(spec_res$clusters) != n_filas) {
      cat("   ⚠️  Espectral devolvió clusters inválidos, usando k=1\n")
      spec_res <- list(k_opt = 1, clusters = rep(1, n_filas))
    }
    
    # Añadir resultados al dataframe de la entidad
    df_result <- df_ent %>%
      mutate(
        cluster_gmm = gmm_res$clusters,
        cluster_spectral = spec_res$clusters,
        k_gmm = gmm_res$k_opt,
        k_spectral = spec_res$k_opt
      )
    
    resultados_lista[[i]] <- df_result
  }
  
  # Combinar todos los resultados
  resultados_final <- bind_rows(resultados_lista)
  
  # Unir con el data frame original (incluye entidades que no cumplían min_obs)
  df_final <- df_ano %>%
    left_join(
      resultados_final %>% dplyr::select(!!sym(row_id_col), cluster_gmm, cluster_spectral, k_gmm, k_spectral),
      by = row_id_col   # BUG FIX: se usa el valor de la variable, no la cadena literal 'row_id_col'
    ) %>%
    # Para las entidades no procesadas, asignar NA o valor por defecto
    mutate(
      cluster_gmm = ifelse(is.na(cluster_gmm), 0, cluster_gmm),
      cluster_spectral = ifelse(is.na(cluster_spectral), 0, cluster_spectral),
      k_gmm = ifelse(is.na(k_gmm), 1, k_gmm),
      k_spectral = ifelse(is.na(k_spectral), 1, k_spectral)
    )
  
  return(df_final)
}


resultados_por_entidad_anios <- list()

for (anio in names(caa_split)) {
  cat("\n========== Procesando año:", anio, "==========\n")
  df_anio <- caa_split[[anio]]
  
  # Añadir row_id si no existe
  if (!"row_id" %in% colnames(df_anio)) {
    df_anio$row_id <- 1:nrow(df_anio)
  }
  
  res_anio <- tryCatch(
    cluster_by_entity(
      df_ano = df_anio,
      vars = vi,
      entity_col = "NOMGEO",
      row_id_col = "row_id",
      min_obs = 3,
      gmm_fun = gmm_opt,
      spectral_fun = spectral_opt
    ),
    error = function(e) {
      cat("❌ Error en año", anio, ":", e$message, "\n")
      NULL
    }
  )
  
  resultados_por_entidad_anios[[anio]] <- res_anio
}

# Guardar cada año por separado
for (anio in names(resultados_por_entidad_anios)) {
  if (!is.null(resultados_por_entidad_anios[[anio]])) {
    write.csv(resultados_por_entidad_anios[[anio]], 
              paste0("caa", anio, "_con_clusters.csv"), 
              row.names = FALSE)
  }
}

### 12. Analizar transición
# ----------------------------------------------------------------------------
# 1. PROCESAMIENTO POR AÑO (clustering avanzado)
# ----------------------------------------------------------------------------

resultados_avanzados_t <- list()

for (anio in names(caa_split)) {
  cat("\n========== Procesando año:", anio, "==========\n")
  df_anio <- caa_split[[anio]]
  res <- advanced_clustering(
    data = df_anio,
    vars = vi,
    cluster_col_base = "cluster_ward",  
    year_label = anio,
    G_gmm = 4,
    k_spec = 4,
    eps_dbscan = 0.65,
    minPts_dbscan = 7,
    run_dbscan = T 
  )
  resultados_avanzados_t[[anio]] <- res
}

# ----------------------------------------------------------------------------
# 2. UNIFICAR DATOS DE TODOS LOS AÑOS EN UN SOLO DATA FRAME
# ----------------------------------------------------------------------------

# Extraemos los data frames actualizados y agregamos columna tcode (año numérico)
df_unificado <- map_dfr(names(resultados_avanzados_t), function(anio) {
  res <- resultados_avanzados_t[[anio]]
  if (is.null(res)) return(NULL)
  res$data_actualizado %>%
    mutate(tcode = as.integer(anio))
})

# Verificamos estructura
glimpse(df_unificado)

# Creamos el identificador único por unidad espacial
df_unificado <- df_unificado %>%
  mutate(ID = paste(NOMGEO, AE, sep = "_"))

# ----------------------------------------------------------------------------
# 3. DEFINIR PERIODOS DE TRANSICIÓN
# ----------------------------------------------------------------------------

pares_anios <- list(
  c(2003, 2008), c(2008, 2013), c(2013, 2018), c(2018, 2023), c(2003, 2023)
)
names(pares_anios) <- c("2003_2008", "2008_2013", "2013_2018", "2018_2023", "2003_2023")

# ----------------------------------------------------------------------------
# 4. FUNCIÓN PARA CREAR MATRIZ DE TRANSICIÓN
# ----------------------------------------------------------------------------

crear_matriz_transicion <- function(datos, 
                                    anio_inicio, 
                                    anio_fin, 
                                    cluster_col = "cluster_gmm",
                                    id_col = "ID",
                                    time_col = "tcode") {
  
  # Verificar columnas necesarias
  columnas_necesarias <- c(id_col, time_col, cluster_col)
  faltantes <- setdiff(columnas_necesarias, colnames(datos))
  if (length(faltantes) > 0) {
    stop("Faltan columnas: ", paste(faltantes, collapse = ", "))
  }
  
  # --- Verificar duplicados (con base R) ---
  # Filtrar filas de los años de interés
  idx <- which(datos[[time_col]] %in% c(anio_inicio, anio_fin))
  if (length(idx) == 0) {
    stop("No hay datos para los años especificados.")
  }
  
  # Tabla de frecuencias ID x año
  frec <- table(datos[[id_col]][idx], datos[[time_col]][idx])
  if (any(frec > 1)) {
    stop("Hay IDs con múltiples filas en un mismo año. Revisa los datos.")
  }
  
  # --- Extraer clusters de inicio y fin ---
  # Año inicio
  idx_inicio <- which(datos[[time_col]] == anio_inicio)
  inicio <- data.frame(
    id = datos[[id_col]][idx_inicio],
    cluster_inicio = datos[[cluster_col]][idx_inicio],
    stringsAsFactors = FALSE
  )
  names(inicio)[1] <- id_col
  
  # Año fin
  idx_fin <- which(datos[[time_col]] == anio_fin)
  fin <- data.frame(
    id = datos[[id_col]][idx_fin],
    cluster_fin = datos[[cluster_col]][idx_fin],
    stringsAsFactors = FALSE
  )
  names(fin)[1] <- id_col
  
  # --- Unir por ID y contar transiciones ---
  # Unión (inner join)
  transiciones <- merge(inicio, fin, by = id_col, all = FALSE)
  
  # Si no hay transiciones, crear data frame vacío con estructura adecuada
  if (nrow(transiciones) == 0) {
    transiciones <- data.frame(cluster_inicio = integer(), 
                               cluster_fin = integer(), 
                               n = integer())
  } else {
    # Contar combinaciones
    transiciones <- aggregate(list(n = rep(1, nrow(transiciones))), 
                              by = list(cluster_inicio = transiciones$cluster_inicio, 
                                        cluster_fin = transiciones$cluster_fin), 
                              FUN = length)
  }
  
  # --- Completar todas las combinaciones posibles con cero ---
  clusters <- sort(unique(c(transiciones$cluster_inicio, transiciones$cluster_fin)))
  
  # Crear cuadrícula completa
  grid <- expand.grid(cluster_inicio = clusters, cluster_fin = clusters, 
                      stringsAsFactors = FALSE)
  
  # Fusionar con las frecuencias observadas
  matriz_completa <- merge(grid, transiciones, 
                           by = c("cluster_inicio", "cluster_fin"), 
                           all.x = TRUE)
  matriz_completa$n[is.na(matriz_completa$n)] <- 0
  
  # Convertir a matriz usando xtabs (base R)
  matriz_completa <- xtabs(n ~ cluster_inicio + cluster_fin, data = matriz_completa)
  matriz_completa <- as.matrix(matriz_completa)
  
  # Ordenar filas y columnas
  matriz_completa <- matriz_completa[order(rownames(matriz_completa)), 
                                     order(colnames(matriz_completa))]
  
  return(matriz_completa)
}

# ----------------------------------------------------------------------------
# 5. CALCULAR MATRICES DE TRANSICIÓN PARA CADA PERIODO
# ----------------------------------------------------------------------------

# Listas para almacenar resultados
matrices_transicion <- list()
matrices_norm <- list()
estabilidades <- data.frame(Periodo = character(), Estabilidad = numeric())
tests_markov <- list()

for (nombre in names(pares_anios)) {
  anios <- pares_anios[[nombre]]
  cat("\n========== Transición", nombre, "==========\n")
  
  # Calcular matriz de transición (absoluta)
  M <- crear_matriz_transicion(
    datos = df_unificado,
    anio_inicio = anios[1],
    anio_fin = anios[2],
    cluster_col = "cluster_gmm",   
    id_col = "ID",
    time_col = "tcode"
  )
  matrices_transicion[[nombre]] <- M
  
  # Matriz normalizada por filas
  M_norm <- prop.table(M, margin = 1)
  matrices_norm[[nombre]] <- M_norm
  
  # Estabilidad (% de casos en diagonal)
  estab <- sum(diag(M)) / sum(M) * 100
  estabilidades <- rbind(estabilidades, data.frame(Periodo = nombre, Estabilidad = round(estab, 2)))
  
  # Test de independencia (simulado por si hay celdas con pocos casos)
  test <- chisq.test(M, simulate.p.value = TRUE, B = 10000)
  tests_markov[[nombre]] <- test
  
  # Mostrar resultados
  print(round(M_norm, 3))
  cat("Estabilidad:", round(estab, 2), "%\n")
  cat("p-value:", format(test$p.value, scientific = TRUE), "\n")
  
  # Guardar matrices en CSV
  write_csv(as.data.frame(M), file = paste0("transicion_", nombre, ".csv"))
  write_csv(as.data.frame(M_norm), file = paste0("transicion_norm_", nombre, ".csv"))
}

# Mostrar y guardar tabla de estabilidades
print(estabilidades)
write_csv(estabilidades, "estabilidades_por_periodo_markov.csv")

# Heatmaps

plot_transition_heatmap <- function(matriz_prob, titulo, subtitulo = NULL) {
  melted <- melt(matriz_prob)
  colnames(melted) <- c("Desde", "Hacia", "Probabilidad")
  
  ggplot(melted, aes(Hacia, Desde, fill = Probabilidad)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", Probabilidad)), 
              color = "black", size = 3) +
    scale_fill_gradient2(low = "white", high = "red", 
                         midpoint = 0.5, limits = c(0, 1)) +
    labs(title = titulo, subtitle = subtitulo,
         x = "Cluster destino", y = "Cluster origen") +
    theme_minimal()
}

for (nombre in names(matrices_norm)) {
  p <- plot_transition_heatmap(
    matrices_norm[[nombre]],
    titulo = paste("Probabilidades de transición", gsub("_", "-", nombre)),
    subtitulo = paste("p-value =", format(tests_markov[[nombre]]$p.value, scientific = TRUE))
  )
  print(p)
  ggsave(paste0("heatmap_", nombre, ".png"), p, width = 8, height = 6)
}

# Extraer persistencia (diagonal) para cada periodo y cluster

ls(pattern = "matrices")

matrices_normalizadas <- matrices_norm

persistencia_por_cluster <- function(matrices_norm, nombres_clusters = NULL) {
  # Verificar que la lista no esté vacía
  if (length(matrices_norm) == 0) stop("La lista de matrices está vacía.")
  
  # Obtener clusters de la primera matriz (asumiendo que todas tienen los mismos)
  clusters <- rownames(matrices_norm[[1]])
  if (is.null(clusters)) {
    # Si no hay nombres de fila, asignar números 1..n
    clusters <- seq_len(nrow(matrices_norm[[1]]))
    warning("Las matrices no tienen nombres de fila. Se asignan números del 1 al ", length(clusters))
  }
  
  if (is.null(nombres_clusters)) nombres_clusters <- clusters
  
  df_pers <- data.frame(cluster = nombres_clusters)
  for (nom in names(matrices_norm)) {
    M <- matrices_norm[[nom]]
    # Extraer diagonal (persistencia)
    diag_val <- diag(M)
    if (length(diag_val) != length(clusters)) {
      warning("La matriz ", nom, " tiene dimensiones inconsistentes. Se omitirá.")
      next
    }
    df_pers[[paste0("persistencia_", nom)]] <- diag_val
  }
  
  # Calcular promedio (excluyendo columnas no numéricas, como cluster)
  df_pers$promedio <- rowMeans(df_pers[, sapply(df_pers, is.numeric)], na.rm = TRUE)
  df_pers
}


# Asegurar que matrices_normalizadas existe
if (!exists("matrices_normalizadas")) {
  if (exists("matrices_transicion")) {
    matrices_normalizadas <- map(matrices_transicion, ~ prop.table(.x, margin = 1))
  } else {
    stop("No se encontraron matrices de transición. Revisa el análisis previo.")
  }
}

# Calcular persistencia
persistencia <- persistencia_por_cluster(matrices_normalizadas)
print(persistencia)

# Guardar resultados
write.csv(persistencia, "persistencia_por_cluster.csv", row.names = FALSE)

# Diagrama alluvial (requiere trayectorias completas)

trayectorias <- df_unificado %>%
  filter(tcode %in% c(2003, 2008, 2013, 2018, 2023)) %>%
  arrange(ID, tcode) %>%
  group_by(ID) %>%
  summarise(
    cluster_2003 = .data$cluster_gmm[tcode == 2003][1],
    cluster_2008 = .data$cluster_gmm[tcode == 2008][1],
    cluster_2013 = .data$cluster_gmm[tcode == 2013][1],
    cluster_2018 = .data$cluster_gmm[tcode == 2018][1],
    cluster_2023 = .data$cluster_gmm[tcode == 2023][1],
    .groups = "drop"
  ) %>%
  filter(complete.cases(.))

trayectorias_count <- trayectorias %>%
  group_by(cluster_2003, cluster_2008, cluster_2013, cluster_2018, cluster_2023) %>%
  summarise(frecuencia = n(), .groups = "drop") %>%
  arrange(desc(frecuencia))

# Top 350 trayectorias
top_tray <- trayectorias_count %>% slice_head(n = 350)

ggplot(top_tray,
       aes(axis1 = cluster_2003, axis2 = cluster_2008, axis3 = cluster_2013,
           axis4 = cluster_2018, axis5 = cluster_2023, y = frecuencia)) +
  geom_alluvium(aes(fill = factor(cluster_2003)), width = 1/12) +
  geom_stratum(width = 1/12, fill = "lightgray", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("2003", "2008", "2013", "2018", "2023"), expand = c(0.05, 0.05)) +
  labs(title = "Transiciones de aglomeraciones GMM 2003-2023",
       x = "Período", y = "Número de sector/entidades", fill = "Cluster 2003") +
  theme_minimal()
ggsave("alluvial_espectral.png", width = 14, height = 8)

# Análisis de la transición

analizar_cadena_markov <- function(mat_transicion, 
                                   desde = NULL, 
                                   periodo = "",
                                   plot = TRUE,
                                   return_all = FALSE) {
  
  # Si la matriz es de frecuencias, normalizar a probabilidades (por filas)
  if (all(mat_transicion == floor(mat_transicion)) && any(mat_transicion > 0)) {
    P <- prop.table(mat_transicion, margin = 1)
  } else {
    # Asumir que ya está normalizada (pero verificar que las filas sumen 1)
    if (any(abs(rowSums(mat_transicion) - 1) > 1e-6)) {
      warning("Las filas no suman 1. Se normalizarán.")
      P <- prop.table(mat_transicion, margin = 1)
    } else {
      P <- mat_transicion
    }
  }
  
  # Obtener etiquetas de estados
  if (is.null(desde)) {
    if (!is.null(rownames(P))) {
      estados <- rownames(P)
    } else if (!is.null(colnames(P))) {
      estados <- colnames(P)
    } else {
      estados <- as.character(1:nrow(P))
    }
  } else {
    estados <- desde
  }
  rownames(P) <- colnames(P) <- estados
  
  # Verificar que la cadena sea ergódica (para estacionaria)
  # Primero, construir grafo dirigido
  g <- graph_from_adjacency_matrix(P, mode = "directed", weighted = TRUE, diag = TRUE)
  
  # Verificar si es irreducible (fuertemente conexo)
  is_irreducible <- is.connected(g, mode = "strong")
  
  estacionaria <- NULL
  tiempos_retorno <- NULL
  
  if (is_irreducible) {
    # Calcular distribución estacionaria
    # Usar eigen para la matriz transpuesta (izquierda)
    eig <- eigen(t(P))
    # El autovector asociado al autovalor 1
    idx <- which(abs(eig$values - 1) < 1e-8)
    if (length(idx) > 0) {
      est <- Re(eig$vectors[, idx[1]])
      est <- est / sum(est)  # normalizar
      names(est) <- estados
      estacionaria <- est
      
      # Tiempo medio de retorno: 1 / estacionaria
      tiempos_retorno <- 1 / estacionaria
      names(tiempos_retorno) <- estados
    }
  } else {
    cat("La cadena no es irreducible. No se puede calcular distribución estacionaria única.\n")
  }
  
  # Imprimir resultados
  cat("\n========================================\n")
  cat("Cadena de Markov - Periodo:", periodo, "\n")
  cat("========================================\n")
  cat("Matriz de probabilidades de transición P:\n")
  print(round(P, 4))
  
  if (!is.null(estacionaria)) {
    cat("\nDistribución estacionaria:\n")
    print(round(estacionaria, 4))
    cat("\nTiempo medio de retorno (en pasos):\n")
    print(round(tiempos_retorno, 2))
  }
  
  # Grafo
  if (plot) {
    # Usar diagram::plotmat para un gráfico simple
    par(mar = c(1,1,1,1))
    plotmat(P, 
            pos = length(estados),  # número de nodos
            name = estados,
            box.size = 0.1,
            cex.txt = 0.8,
            self.cex = 0.5,
            self.shiftx = c(0.1, -0.1),
            main = paste("Cadena de Markov -", periodo))
  }
  
  if (return_all) {
    return(list(
      P = P,
      estacionaria = estacionaria,
      tiempos_retorno = tiempos_retorno,
      grafo = g,
      irreducible = is_irreducible
    ))
  } else {
    invisible(list(P = P, estacionaria = estacionaria))
  }
}

library(diagram)
library(igraph)

for (nom in names(matrices_norm)) {
  cat("\nProcesando:", nom)
  
  # 1. Obtener todos los resultados
  res <- analizar_cadena_markov(
    mat_transicion = matrices_norm[[nom]], 
    periodo = nom, 
    plot = FALSE,          # Evitamos que dibuje en pantalla ahora
    return_all = TRUE
  )
  
  # 2. Guardar resultados numéricos en CSV
  write.csv(res$P, file = paste0("resultados_clusters/matriz_", nom, ".csv"))
  
  if (!is.null(res$estacionaria)) {
    df_est <- data.frame(estado = names(res$estacionaria), prob = res$estacionaria)
    write.csv(df_est, file = paste0("resultados_clusters/estacionaria_", nom, ".csv"), 
              row.names = FALSE)
    
    df_ret <- data.frame(estado = names(res$tiempos_retorno), tiempo = res$tiempos_retorno)
    write.csv(df_ret, file = paste0("resultados_clusters/tiempos_retorno_", nom, ".csv"), 
              row.names = FALSE)
  }
  
  # 3. Guardar el objeto completo como RDS (opcional)
  saveRDS(res, file = paste0("resultados_clusters/resultados_", nom, ".rds"))
  
  # 4. Guardar el gráfico en un archivo PNG
  png(filename = paste0("resultados_clusters/grafo_", nom, ".png"),
      width = 800, height = 600, res = 100)
  
  # Volvemos a dibujar el gráfico usando la misma función, pero con plot = TRUE
  # y sin volver a calcular todo (podríamos usar directamente plotmat, pero así es más simple)
  analizar_cadena_markov(
    mat_transicion = matrices_norm[[nom]], 
    periodo = nom, 
    plot = TRUE,
    return_all = FALSE    # no necesitamos la salida
  )
  
  dev.off()  # cierra el dispositivo PNG
}

# Interpretación de resultados
# Distribución estacionaria: proporción de tiempo a largo plazo que la cadena pasa en cada estado (cluster).
# Tiempo medio de retorno: número promedio de pasos para regresar a un estado, comenzando desde ese mismo estado.
# Si la cadena no es irreducible, significa que hay estados "absorbentes" o grupos de estados que no se comunican; en ese caso, no hay una distribución estacionaria única.


# =======================
####### 13. Análisis espacial

# Leer shapefile de entidades 
mx <- st_read("C:/Users/gezum/Desktop/Entidades_Federativas/Entidades_Federativas.shp", quiet = TRUE)

# Corrección de encoding 
encodings <- readr::guess_encoding("C:/Users/gezum/Desktop/Entidades_Federativas/Entidades_Federativas.dbf")
text_columns <- sapply(mx, is.character)
for (col in names(mx)[text_columns]) {
  mx[[col]] <- iconv(mx[[col]], from = "windows-1252", to = "UTF-8")
}

analisis_espacial_anual <- function(df_anio, 
                                    shape_entidades, 
                                    vars, 
                                    year_label = NULL,
                                    k_vecinos = 4) {
  
  if (is.null(year_label) && "tcode" %in% colnames(df_anio)) {
    year_label <- unique(df_anio$tcode)[1]
  }
  
  cat("\n========== Análisis espacial para año:", year_label, "==========\n")
  
  # 1. Unir con shapefile
  df_sf <- df_anio %>%
    inner_join(shape_entidades, by = "NOMGEO") %>%
    st_as_sf()
  
  # 2. Agregar por entidad (promedio de variables)
  datos_entidad <- df_sf %>%
    st_drop_geometry() %>%
    group_by(NOMGEO) %>%
    summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  # 3. Obtener geometría única por entidad (disolver)
  entidades_sf <- df_sf %>%
    group_by(NOMGEO) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    ungroup()
  
  # Unir datos agregados con geometrías
  entidades_sf <- left_join(entidades_sf, datos_entidad, by = "NOMGEO")
  
  # Verificar número de entidades
  cat("   Número de entidades procesadas:", nrow(entidades_sf), "\n")
  
  # 4. Crear matriz de vecindad
  coords <- st_centroid(entidades_sf) %>% st_coordinates()
  nb <- knn2nb(knearneigh(coords, k = k_vecinos))
  listw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # 5. Calcular Moran global para cada variable
  resultados_global <- data.frame(
    Variable = character(),
    Moran_I = numeric(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (var in vars) {
    valores <- entidades_sf[[var]]
    if (all(is.na(valores))) next
    
    moran <- tryCatch(
      moran.test(valores, listw = listw, zero.policy = TRUE),
      error = function(e) NULL
    )
    
    if (!is.null(moran)) {
      resultados_global <- rbind(
        resultados_global,
        data.frame(
          Variable = var,
          Moran_I = moran$estimate[1],
          p_value = moran$p.value
        )
      )
      cat("   ✓", var, "- Moran I:", round(moran$estimate[1], 4), 
          "- p-value:", round(moran$p.value, 4), "\n")
    } else {
      cat("   ✗ Error en", var, "\n")
    }
  }
  
  # 6. Calcular Moran local para cada variable y añadirlo al sf
  for (var in vars) {
    valores <- entidades_sf[[var]]
    if (all(is.na(valores))) next
    
    local <- tryCatch(
      localmoran(valores, listw = listw, zero.policy = TRUE),
      error = function(e) NULL
    )
    
    if (!is.null(local)) {
      col_name <- paste0("local_", var)
      entidades_sf[[col_name]] <- local[, "Ii"]
      entidades_sf[[paste0("p_", var)]] <- local[, "Pr(z != E(Ii))"]
    }
  }
  
  # 7. Añadir año
  entidades_sf$anio <- year_label
  
  list(
    anio = year_label,
    sf_entidades = entidades_sf,
    moran_global = resultados_global,
    listw = listw,
    nb = nb
  )
} 

# Lista de años (nombres de caa_split)
anios <- names(caa_split)

# Resultados espaciales por año
resultados_espaciales <- list()

for (anio in anios) {
  df_anio <- caa_split[[anio]]
  res <- analisis_espacial_anual(
    df_anio = df_anio,
    shape_entidades = mx,
    vars = vi,           # tus variables de interés
    year_label = anio,
    k_vecinos = 4
  )
  resultados_espaciales[[anio]] <- res
}

# Unir todas las tablas de Moran global
moran_global_todos <- bind_rows(lapply(resultados_espaciales, function(x) {
  x$moran_global %>% mutate(anio = x$anio, .before = 1)
}))

# Mostrar con kable
moran_global_todos %>%
  mutate(across(c(Moran_I, p_value), ~ round(., 4))) %>%
  kable(caption = "Índice de Moran global por año y variable") %>%
  kable_styling("striped")

# Guardar
write.csv(moran_global_todos, "moran_global_todos_anos.csv", row.names = FALSE)
    
# Ver nombres de columnas en el objeto espacial del año 2003
names(resultados_espaciales[["2003"]]$sf_entidades)


# 1. Variable a graficar (debe coincidir con tus nombres: "local_ICS", "local_ICP", etc.)
var_mapa <- "local_prod_ppvs" 

# 2. Consolidar los resultados espaciales en una sola tabla sf
mapa_paneles <- bind_rows(lapply(resultados_espaciales, function(x) {
  # Extraemos el sf y nos aseguramos de que el año sea una columna
  df_sf <- x$sf_entidades
  df_sf$Anio <- as.character(x$anio)
  return(df_sf)
}))

# 3. Crear el panel con ggplot2 
panel_continuo <- ggplot(mapa_paneles) +
  # Dibujar los polígonos
  geom_sf(aes(fill = !!sym(var_mapa)), color = "grey30", size = 0.1) +
  # Escala de color divergente: Púrpura (bajo) -> Blanco (0) -> Rojo (alto)
  scale_fill_gradient2(
    low = "#7C3AED",   # Púrpura intenso (como en tu imagen)
    mid = "white", 
    high = "#EF4444",  # Rojo intenso
    midpoint = 0,
    name = var_mapa
  ) +
  # Dividir en paneles por año
  facet_wrap(~Anio, ncol = 3) +
  # Tema minimalista y limpio
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major = element_line(color = "grey95", linetype = "dashed"),
    axis.text = element_text(size = 8, color = "grey50"),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = paste("Análisis de autocorrelación espacial local:", var_mapa),
    subtitle = "Evolución temporal 2003-2023 (Valores del estadístico Ii de Moran local)",
    x = NULL, y = NULL
  )

# 4. Guardar en alta calidad
ggsave(paste0("Panel_Evolucion_", var_mapa, ".png"), 
       panel_continuo, width = 14, height = 10, dpi = 300)

# Mostrar el resultado
print(panel_continuo)


# Proporción de variables significativas por año
moran_global_todos %>%
  group_by(anio) %>%
  summarise(
    total_vars = n(),
    significativas = sum(p_value < 0.05, na.rm = TRUE),
    prop_signif = significativas / total_vars
  ) %>%
  mutate(prop_signif = round(prop_signif * 100, 1)) %>%
  kable(caption = "Proporción de variables con autocorrelación significativa (p<0.05)")

# Guardar sf de cada año con los valores locales
for (anio in names(resultados_espaciales)) {
  st_write(resultados_espaciales[[anio]]$sf_entidades,
           paste0("entidades_", anio, "_moran_local.shp"), 
           delete_layer = TRUE)
}

# Visualización 
# Inicializar lista vacía
lista_dfs <- list()

# Iterar sobre los nombres de resultados_avanzados
for (anio in names(resultados_avanzados)) {
  x <- resultados_avanzados[[anio]]
  if (is.null(x) || is.null(x$data_actualizado)) {
    warning("Año ", anio, " no tiene data_actualizado. Se omite.")
    next
  }
  df <- x$data_actualizado
  if (!all(c("AE", "NOMGEO") %in% colnames(df))) {
    warning("Año ", anio, " no tiene columnas AE o NOMGEO. Se omite.")
    next
  }
  df$tcode <- anio
  df_sel <- df %>% dplyr::select(NOMGEO, AE, tcode, cluster_gmm, cluster_spec)
  lista_dfs[[anio]] <- df_sel
}

# Unir todos los data frames
jdf <- bind_rows(lista_dfs)

# --- UNIR CON GEOMETRÍAS ---
# Asegurar que mx tiene columna NOMGEO y es sf
mx_sf <- mx  # ya es sf

# Unir (left_join desde jdf hacia mx)
jdf_sf <- jdf %>%
  left_join(mx_sf %>% dplyr::select(NOMGEO, geometry), by = "NOMGEO") %>%
  st_as_sf()

# Verificar geometrías vacías
if (any(st_is_empty(jdf_sf))) {
  warning("Algunas filas no tienen geometría. Se eliminarán.")
  jdf_sf <- jdf_sf[!st_is_empty(jdf_sf), ]
}

cluster_elegido <- "cluster_gmm"

# Preprocesar geometríaslibrary(rmapshaper)

jdf_sf_clean <- jdf_sf %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 0.001)  

p1 <- ggplot(jdf_sf_clean) +
  geom_sf(aes(fill = as.factor(.data[[cluster_elegido]])), color = NA, size = 0) +
  scale_fill_viridis_d("Cluster") +
  facet_grid(AE ~ tcode) +
  theme_minimal() +
  labs(title = "Figura 1. Evolución de aglomeraciones (GMM) por sector y quinquenio") +
  theme(axis.text = element_blank(),
        strip.text.y = element_text(angle = 0, size = 8),
        strip.text.x = element_text(size = 10))
p1

# Guardar en disco (evita renderizado en pantalla)
ggsave("evolucion_clusters.png", p1, width = 12, height = 8, dpi = 300)

# --- Gráfico 2: Barras apiladas proporcionales por entidad ---
prop_entidad <- jdf %>%
  group_by(NOMGEO, tcode, .data[[cluster_elegido]]) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(NOMGEO, tcode) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p2 <- ggplot(prop_entidad, aes(x = as.factor(tcode), y = prop, 
                               fill = as.factor(.data[[cluster_elegido]]))) +
  geom_bar(stat = "identity", position = "fill", width = 0.8) +
  facet_wrap(~ NOMGEO, ncol = 4, nrow = 8) +
  labs(title = "",
       x = "Quinquenio", y = "Proporción") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d("Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        panel.spacing = unit(0.5, "lines"))

print(p2)
ggsave("barras_proporcion_entidad.png", p2, width = 12, height = 18)

# install.packages("patchwork")  # si aún no lo tienes
library(patchwork)

# 1. Versión sin título ni caption de p1
p1_mod <- p1 +
  labs(title = NULL, caption = NULL) +
  theme(plot.margin = margin(10, 15, 10, 10))

# 2. Quitar cualquier caption residual que pudiera tener p2 (va en blanco)
p2_mod <- p2 +
  labs(caption = NULL) +
  theme(plot.margin = margin(10, 15, 10, 10))

# 3. Combinar los gráficos en una sola columna (p1 arriba, p2 abajo)
figura_completa <- (p1_mod + p2_mod) +
  plot_annotation(
    title    = "Figura 1. Evolución de aglomeraciones (GMM) por sector-quinquenio y proporciones de aglomeración por Estado",
    caption  = "Fuente: Elaboración propia con información de los Censos Económicos 2004, 2009, 2014, 2019, 2024 de INEGI",
    theme    = theme(
      plot.title    = element_text(size = 14, face = "bold", hjust = 1),
      plot.caption  = element_text(size = 8, face = "italic", hjust = 1, vjust = 0),
      plot.margin   = margin(7, 10, 7, 7)
    )
  )

# 4. Mostrar el resultado
print(figura_completa)
ggsave("figura1.png", figura_completa, width = 10, height = 12, dpi = 300)

library(trelliscopejs)

caa_trellis <- jdf %>%
  mutate(panel_id = paste(NOMGEO, AE, sep = " - ")) %>%
  dplyr::count(panel_id, tcode, .data[[cluster_elegido]]) %>%
  group_by(panel_id, tcode) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(caa_trellis, aes(x = tcode, y = prop, fill = .data[[cluster_elegido]])) +
  geom_col() +
  facet_trelliscope(~ panel_id, 
                    ncol = 4, 
                    nrow = 8,
                    scales = "free_y",
                    path = "trelliscope_display") +
  labs(title = "Evolución Detallada por Entidad-AE",
       x = "Quinquenio", y = "Proporción") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d() +
  theme_minimal()

head(jdf_sf)

jdf_tabla <- st_drop_geometry(jdf_sf)

jdf_tabla %>%
  mutate(elemento = paste(NOMGEO, AE, sep = " - ")) %>%
  group_by(tcode, cluster_gmm) %>%
  summarise(
    elementos = paste(sort(unique(elemento)), collapse = ", "),
    .groups = "drop"
  )

library(knitr)
library(kableExtra)

resumen_gmm_con_ae <- jdf_tabla %>%
  group_by(tcode, cluster_gmm) %>%
  summarise(
    regiones = paste(sort(unique(NOMGEO)), collapse = ", "),
    actividades = paste(sort(unique(AE)), collapse = ", "),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(tcode, as.numeric(as.character(cluster_gmm)))

# Tabla con dos columnas: regiones y actividades
resumen_gmm_con_ae %>%
  kable(caption = "Clústeres GMM por periodo") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
  column_spec(3, width = "15em") %>%   # ancho para regiones
  column_spec(4, width = "10em")       # ancho para actividades

# Contar elementos por tcode y cluster_gmm (si no lo tienes)
conteos <- jdf_tabla %>%
  group_by(tcode, cluster_gmm) %>%
  summarise(n = n(), .groups = "drop")

ggplot(conteos, aes(x = tcode, y = n, fill = cluster_gmm)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Periodo", y = "Número de elementos", fill = "Clúster") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")


ggplot(resumen_gmm_con_ae, aes(x = tcode, y = AE, fill = cluster_gmm)) +
  geom_tile(color = "white", linewidth = 0.2) +
  facet_wrap(~ NOMGEO, scales = "free_y", ncol = 4, nrow = 8) +   # una faceta por región
  labs(x = "Periodo", y = "Actividad económica", fill = "Clúster") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 4),   # tamaño de etiquetas de AE
        strip.text = element_text(face = "bold")) +
  scale_fill_viridis_d()

library(dplyr)
library(tidyr)

# Asegurar que tcode es numérico o factor ordenado
jdf_tabla <- jdf_tabla %>%
  mutate(tcode_num = as.numeric(as.character(tcode)))

flujos <- jdf_tabla %>%
  arrange(NOMGEO, AE, tcode_num) %>%
  group_by(NOMGEO, AE) %>%
  mutate(
    cluster_prev = lag(cluster_gmm),
    periodo_prev = lag(tcode_num)
  ) %>%
  ungroup() %>%
  filter(!is.na(cluster_prev)) %>%
  dplyr::count(cluster_prev, cluster_gmm, periodo_prev, name = "freq") %>%  # dplyr::count
  rename(cluster_origen = cluster_prev, cluster_destino = cluster_gmm, periodo = periodo_prev)

flujos_agregados <- flujos %>%
  group_by(cluster_origen, cluster_destino) %>%
  summarise(freq = sum(freq), .groups = "drop")

library(tidygraph)
library(ggraph)

# Nodos: todos los clusters únicos
nodos <- data.frame(
  name = unique(c(flujos_agregados$cluster_origen, 
                  flujos_agregados$cluster_destino))
)

# Aristas
edges <- flujos_agregados %>%
  rename(from = cluster_origen, to = cluster_destino) %>%
  mutate(from = as.character(from), to = as.character(to))

# Grafo
grafo <- tbl_graph(nodes = nodos, edges = edges, directed = TRUE)

ggraph(grafo, layout = "fr") +   # "fr" = Fruchterman-Reingold
  geom_edge_link(aes(edge_width = freq), 
                 arrow = arrow(length = unit(3, 'mm')), 
                 end_cap = circle(3, 'mm')) +
  geom_node_point(aes(color = name), size = 8) +
  geom_node_text(aes(label = name), vjust = 1.5, size = 4) +
  theme_graph() +
  labs(title = "Flujos entre clusters (todos los periodos)", 
       edge_width = "Frecuencia", color = "Clúster")



# Lista de periodos únicos
periodos <- unique(flujos$periodo)

# Función para generar gráfico de un periodo
grafico_por_periodo <- function(periodo) {
  flujos_per <- flujos %>% filter(periodo == !!periodo)
  
  # Si no hay flujos para ese periodo, devuelve NULL
  if(nrow(flujos_per) == 0) return(NULL)
  
  edges <- flujos_per %>%
    rename(from = cluster_origen, to = cluster_destino) %>%
    mutate(from = as.character(from), to = as.character(to))
  
  nodos <- data.frame(name = unique(c(edges$from, edges$to)))
  
  grafo <- tbl_graph(nodes = nodos, edges = edges, directed = TRUE)
  
  ggraph(grafo, layout = "fr") +
    geom_edge_link(aes(edge_width = freq), 
                   arrow = arrow(length = unit(2, 'mm')), 
                   end_cap = circle(2, 'mm')) +
    geom_node_point(aes(color = name), size = 5) +
    geom_node_text(aes(label = name), vjust = 1.2, size = 3) +
    theme_graph() +
    labs(title = paste("Periodo", periodo), 
         edge_width = "Frecuencia", color = "Clúster")
}

# Generar gráficos y combinarlos
graficos <- lapply(periodos, grafico_por_periodo)
graficos <- graficos[!sapply(graficos, is.null)]

# Combinar con patchwork
wrap_plots(graficos, ncol = 2)

library(visNetwork)
library(igraph)

# Crear grafo con igraph
g <- graph_from_data_frame(d = edges, directed = TRUE, vertices = nodos)

# Convertir a visNetwork
visIgraph(g, layout = "layout_with_fr") %>%
  visEdges(arrows = "to") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

library(gganimate)

jdf_sf_simple <- jdf_sf %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 0.002)

p_anim <- ggplot(jdf_sf_simple) +
  geom_sf(aes(fill = as.factor(.data[[cluster_elegido]])), color = NA) +
  scale_fill_viridis_d("Cluster") +
  facet_wrap(~ AE) +
  transition_states(tcode, transition_length = 1, state_length = 1) +
  labs(title = "Quinquenio: {closest_state}")

anim_save("evolucion_clusters.gif", p_anim, fps = 2, width = 800, height = 600)


# Obtener último periodo
ultimo <- max(jdf_sf$tcode)
jdf_ultimo <- jdf_sf %>% dplyr::filter(tcode == ultimo)

ggplot(jdf_ultimo) +
  geom_sf(aes(fill = as.factor(.data[[cluster_elegido]])), color = NA) +
  facet_wrap(~ AE) +
  scale_fill_viridis_d("Cluster") +
  theme_minimal()
