# =======================================================================
# ANÁLISIS DE CONGLOMERADOS SECTOR-ENTIDAD: CAPACIDADES TECNOLÓGICAS
# Y DERRAMAS DE CONOCIMIENTO EN MÉXICO (2003-2023)
# Versión 2.0 | Actualizado y depurado
# Autor: Gilberto González Pérez
# =======================================================================

# -----------------------------------------------------------------------
# 0. PAQUETES
# -----------------------------------------------------------------------

packages <- c(
  # Manipulación y visualización
  "tidyverse", "knitr", "reshape2", "scales", "viridis",
  "ggpubr", "ggcorrplot", "gghighlight", "fmsb", "gridExtra",
  "kableExtra", "ggalluvial", "ggstream","fGarch",
  # Estadística descriptiva
  "psych", "broom", "car", "corrr", "janitor", "skimr", "naniar",
  # PCA y clustering
  "FactoMineR", "factoextra", "NbClust", "cluster", "mclust",
  "kernlab", "dbscan", "fpc", "agricolae", "dunn.test",
  # Multivariado y espacial
  "vegan", "MASS", "sf", "spdep", "tmap",
  # Otros
  "igraph", "treemapify", "highcharter", "trelliscopejs"
)

# Instalar los que falten y cargar todos
installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

suppressPackageStartupMessages(
  invisible(lapply(packages, library, character.only = TRUE))
)

# pairwiseAdonis desde GitHub si no está instalado
if (!requireNamespace("pairwiseAdonis", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
}
library(pairwiseAdonis)


# -----------------------------------------------------------------------
# 1. CARGA Y PREPARACIÓN DE DATOS
# -----------------------------------------------------------------------

data <- bin   

data <- data %>% filter(!is.na(NOMGEO), !is.na(AE)) %>% na.omit()

# Años disponibles (actualizado a 2023)
cat("Años en la base:", sort(unique(data$tcode)), "\n")
cat("Observaciones totales:", nrow(data), "\n")

library(rpivotTable)
tgen<- rpivotTable(data, rows="NOMGEO", col="AE", aggregatorName="Average", 
                   vals="compacd")
tgen

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

data_std <- data %>%
  mutate(across(automa:ptf & where(is.numeric), robust_scale))

# -----------------------------------------------------------------------
# 3. ÍNDICES COMPUESTOS POR PCA (ICP, ICI, ICS)
# -----------------------------------------------------------------------
# Se ponderan los dos primeros componentes principales según su eigenvalor.

compute_pca_index <- function(data_std, variables, label) {
  subset_data <- data_std[, variables]
  
  # Verificar varianza > 0 en todas las variables
  var_check <- sapply(subset_data, function(x) var(x, na.rm = TRUE))
  if (any(var_check < 1e-8)) {
    warning("Variables con varianza casi cero eliminadas en ", label, ": ",
            paste(names(var_check)[var_check < 1e-8], collapse = ", "))
    subset_data <- subset_data[, var_check >= 1e-8, drop = FALSE]
  }
  
  pca <- PCA(subset_data, graph = FALSE)
  
  eig_vals <- pca$eig[1:2, "eigenvalue"]
  w <- eig_vals / sum(eig_vals)
  
  index <- as.numeric(w[1] * pca$ind$coord[, 1] + w[2] * pca$ind$coord[, 2])
  
  cat("\n--- PCA:", label, "---\n")
  print(get_eigenvalue(pca)[1:3, ])
  cat("Ponderadores dim1/dim2:", round(w, 4), "\n")
  
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
# CONFIGURACIÓN GLOBAL
# -----------------------------------------------------------------------

dir.create("validacion_pca", showWarnings = FALSE)

# Función auxiliar: guardar ggplot
guardar <- function(p, nombre, w = 8, h = 5) {
  ggsave(paste0("validacion_pca/", nombre, ".png"), p,
         width = w, height = h, dpi = 150)
  invisible(p)
}

# Definición de variables por índice
vars_icp <- c("automa", "ecpacd", "ecppvs", "eficap", "ite")
vars_ici <- c("ecos",   "sact",   "efene",  "mbi",   "roa")
vars_ics <- c("cdig",   "intal",  "iact",   "ibl_pacd", "ibl_ppvs")

indices <- list(
  ICP = vars_icp,
  ICI = vars_ici,
  ICS = vars_ics
)

# -----------------------------------------------------------------------
# FUNCIÓN MAESTRA DE VALIDACIÓN POR ÍNDICE
# -----------------------------------------------------------------------

validar_pca <- function(data_std, vars, label, n_comp = 2) {
  
  cat("\n", strrep("=", 60), "\n")
  cat("  VALIDACIÓN PCA —", label, "\n")
  cat(strrep("=", 60), "\n\n")
  
  # Subset limpio
  df <- data_std[, vars] %>%
    mutate(across(everything(), as.numeric)) %>%
    filter(complete.cases(.))
  
  n  <- nrow(df)
  p  <- ncol(df)
  mat <- as.matrix(df)
  
  # ── 0. FIABILIDAD BÁSICA ──────────────────────────────────────────────
  cat("── 0. Muestra y fiabilidad ─────────────────────────────\n")
  cat("   n =", n, " | p =", p, " | ratio n/p =", round(n/p, 1), "\n")
  if (n/p < 5)  warning("  ⚠  Ratio n/p < 5: muestra pequeña para PCA.")
  if (n < 50)   warning("  ⚠  n < 50: interpretación con cautela.")
  
  # Alpha de Cronbach (cohesión interna de las variables del índice)
  alpha_res <- tryCatch(psych::alpha(df)$total$raw_alpha, error = function(e) NA)
  cat("   Alpha de Cronbach:", round(alpha_res, 3))
  if (!is.na(alpha_res)) {
    if (alpha_res >= 0.70) cat("  ✔  Aceptable (≥0.70)\n")
    else if (alpha_res >= 0.60) cat("  ⚠  Marginal (0.60-0.70)\n")
    else cat("  ✗  Bajo (<0.60): revisar composición del índice\n")
  } else cat("\n")
  
  
  # ── 1. SUPUESTO: CORRELACIONES ENTRE VARIABLES ────────────────────────
  cat("\n── 1. Matriz de correlaciones ──────────────────────────\n")
  R <- cor(mat, use = "complete.obs")
  
  # Determinante: cercano a 0 indica multicolinealidad alta (bueno para PCA)
  det_R <- det(R)
  cat("   Determinante |R|:", formatC(det_R, format = "e", digits = 4))
  if (det_R < 0.00001) cat("  ✔  Multicolinealidad suficiente para PCA\n")
  else if (det_R < 0.01) cat("  ✔  Correlaciones adecuadas\n")
  else cat("  ⚠  Correlaciones débiles: PCA puede no ser informativo\n")
  
  # Visualización
  p_corr <- ggcorrplot(R, hc.order = TRUE, type = "lower",
                       lab = TRUE, lab_size = 3,
                       colors = c("#C00000", "white", "#1F4E79"),
                       title = paste("Matriz de correlaciones —", label)) +
    theme(plot.title = element_text(face = "bold", size = 12))
  guardar(p_corr, paste0("01_correlaciones_", label))
  
  
  # ── 2. SUPUESTO: PRUEBA DE BARTLETT ──────────────────────────────────
  cat("\n── 2. Prueba de esfericidad de Bartlett ────────────────\n")
  bartlett <- tryCatch({
    psych::cortest.bartlett(R, n = n)
  }, error = function(e) NULL)
  
  if (!is.null(bartlett)) {
    cat("   Chi-cuadrado:", round(bartlett$chisq, 2),
        "| gl:", bartlett$df,
        "| p-valor:", format(bartlett$p.value, scientific = TRUE), "\n")
    if (bartlett$p.value < 0.05)
      cat("   ✔  Rechaza H0 (R = I): las variables están correlacionadas. PCA es apropiado.\n")
    else
      cat("   ✗  No rechaza H0: variables independientes. PCA no recomendado.\n")
  } else cat("   ⚠  No se pudo calcular Bartlett.\n")
  
  
  # ── 3. SUPUESTO: KMO ─────────────────────────────────────────────────
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
    ) %>% mutate(Evaluacion = dplyr::case_when(
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
  pca <- PCA(df, graph = FALSE)
  
  
  # ── 5. EIGENVALORES Y VARIANZA EXPLICADA ──────────────────────────────
  cat("\n── 5. Eigenvalores y varianza explicada ────────────────\n")
  # pca$eig es una matriz: columnas "eigenvalue", "percentage of variance",
  # "cumulative percentage of variance". Usar [ ] — nunca $ en matrices.
  eig_mat  <- pca$eig
  print(round(eig_mat, 3))
  
  n_kaiser <- sum(eig_mat[, "eigenvalue"] > 1)
  cat("\n   Componentes con λ > 1 (criterio Kaiser):", n_kaiser, "\n")
  
  var_acum <- eig_mat[n_comp, "cumulative percentage of variance"]
  cat("   Varianza explicada por", n_comp, "componentes:", round(var_acum, 1), "%")
  if (var_acum >= 60) cat("  ✔\n") else cat("  ⚠  Por debajo del 60% recomendado\n")
  
  # Gráfico de sedimentación
  p_scree <- fviz_eig(pca, addlabels = TRUE, ylim = c(0, 100),
                      barfill = "#2E75B6", barcolor = "white",
                      linecolor = "#C00000") +
    geom_hline(yintercept = 100/p, linetype = "dashed",
               color = "gray50", linewidth = 0.8) +
    annotate("text", x = p - 0.5, y = 100/p + 1.5,
             label = paste0("1/p = ", round(100/p, 1), "%"),
             size = 3, color = "gray40") +
    labs(title = paste("Gráfico de sedimentación —", label),
         subtitle = paste0("Varianza acumulada (", n_comp, " comp.) = ",
                           round(var_acum, 1), "%")) +
    theme_minimal(base_size = 11)
  guardar(p_scree, paste0("02_sedimentacion_", label))
  
  
  # ── 6. CARGAS FACTORIALES (COORDENADAS) ───────────────────────────────
  cat("\n── 6. Cargas factoriales (coordenadas en componentes) ──\n")
  coords <- as.data.frame(pca$var$coord[, 1:n_comp])
  colnames(coords) <- paste0("Dim.", 1:n_comp)
  coords$Variable <- rownames(coords)
  coords <- dplyr::select(coords, Variable, everything())
  coords_print <- coords %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
  print(coords_print)
  
  # Variables con carga baja en ambos componentes (posible candidata a eliminar)
  max_carga <- apply(abs(pca$var$coord[, 1:n_comp]), 1, max)
  vars_baja_carga <- names(max_carga)[max_carga < 0.40]
  if (length(vars_baja_carga) > 0)
    cat("\n   ⚠  Carga < 0.40 en todos los componentes seleccionados:",
        paste(vars_baja_carga, collapse = ", "), "\n")
  else
    cat("\n   ✔  Todas las variables tienen carga ≥ 0.40 en al menos un componente\n")
  
  
  # ── 7. COMUNALIDADES ──────────────────────────────────────────────────
  cat("\n── 7. Comunalidades (cos² acumulado en", n_comp, "comp.) ─\n")
  cos2_acum <- rowSums(pca$var$cos2[, 1:n_comp])
  df_com <- data.frame(Variable = names(cos2_acum),
                       Comunalidad = round(cos2_acum, 3)) %>%
    mutate(Evaluacion = dplyr::case_when(
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
  contrib <- as.data.frame(pca$var$contrib[, 1:n_comp])
  colnames(contrib) <- paste0("Contrib_Dim", 1:n_comp)
  contrib$Variable <- rownames(contrib)
  umbral <- 100 / p   # umbral uniforme
  cat("   Umbral de contribución esperada (100/p):", round(umbral, 1), "%\n\n")
  contrib_print <- contrib %>% mutate(across(where(is.numeric), ~ round(.x, 2)))
  print(contrib_print)
  
  # Gráfico de contribuciones
  p_contrib1 <- fviz_contrib(pca, choice = "var", axes = 1, top = p,
                             fill = "#2E75B6", color = "white") +
    labs(title = paste(label, "— Contribuciones Dim.1")) +
    theme_minimal(base_size = 10)
  
  p_contrib2 <- fviz_contrib(pca, choice = "var", axes = 2, top = p,
                             fill = "#C55A11", color = "white") +
    labs(title = paste(label, "— Contribuciones Dim.2")) +
    theme_minimal(base_size = 10)
  
  guardar(p_contrib1, paste0("03a_contribuciones_Dim1_", label))
  guardar(p_contrib2, paste0("03b_contribuciones_Dim2_", label))
  
  
  # ── 9. BIPLOT ──────────────────────────────────────────────────────────
  cat("\n── 9. Biplot ────────────────────────────────────────────\n")
  p_biplot <- fviz_pca_biplot(
    pca,
    col.var    = "#C00000",
    col.ind    = "cos2",
    gradient.cols = c("#AAAAAA", "#2E75B6", "#1F4E79"),
    repel      = TRUE,
    label      = "var",
    alpha.ind  = 0.4,
    title      = paste("Biplot PCA —", label)
  ) + theme_minimal(base_size = 10)
  guardar(p_biplot, paste0("04_biplot_", label), w = 8, h = 7)
  
  
  # ── 10. ORTOGONALIDAD DE COMPONENTES ─────────────────────────────────
  cat("\n── 10. Ortogonalidad de componentes ────────────────────\n")
  scores      <- pca$ind$coord[, 1:n_comp]
  cor_scores  <- cor(scores)
  off_diag    <- cor_scores[upper.tri(cor_scores)]
  max_corr    <- max(abs(off_diag))
  cat("   Correlación máxima entre componentes:", round(max_corr, 4))
  if (max_corr < 0.01)
    cat("  ✔  Perfectamente ortogonales (esperado en PCA)\n")
  else
    cat("  ⚠  Componentes no completamente ortogonales\n")
  
  
  # ── 11. PONDERADORES Y CONSTRUCCIÓN DEL ÍNDICE ────────────────────────
  cat("\n── 11. Ponderadores y construcción del índice ──────────\n")
  eig_vals <- pca$eig[1:n_comp, "eigenvalue"]
  w        <- eig_vals / sum(eig_vals)
  cat("   Eigenvalores usados:", paste(round(eig_vals, 3), collapse = " | "), "\n")
  cat("   Ponderadores (w1, w2):", paste(round(w, 4), collapse = " | "), "\n")
  cat("   Suma ponderadores:", round(sum(w), 6), "  ✔\n")
  
  index <- as.numeric(w[1] * pca$ind$coord[, 1] + w[2] * pca$ind$coord[, 2])
  
  
  # ── 12. DISTRIBUCIÓN DEL ÍNDICE ────────────────────────────────────────
  cat("\n── 12. Distribución del índice ─────────────────────────\n")
  cat("   Min:", round(min(index), 3), "| Mediana:", round(median(index), 3),
      "| Max:", round(max(index), 3), "| SD:", round(sd(index), 3), "\n")
  
  # Test de normalidad (Shapiro en muestra aleatoria si n > 5000)
  n_test  <- min(n, 4999)
  set.seed(42)
  muestra <- sample(index, n_test)
  sw      <- shapiro.test(muestra)
  cat("   Shapiro-Wilk (n=", n_test, "): W =", round(sw$statistic, 4),
      "| p =", format(sw$p.value, scientific = TRUE))
  if (sw$p.value > 0.05) cat("  ✔  No rechaza normalidad\n")
  else cat("  ⚠  Rechaza normalidad — usar interpretación no paramétrica\n")
  
  p_hist <- ggplot(data.frame(idx = index), aes(idx)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40,
                   fill = "#2E75B6", color = "white", alpha = 0.8) +
    geom_density(color = "#C00000", linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    labs(title = paste("Distribución del índice", label),
         x = label, y = "Densidad") +
    theme_minimal(base_size = 11)
  guardar(p_hist, paste0("05_distribucion_", label))
  
  
  # ── 13. ESTABILIDAD BOOTSTRAP ─────────────────────────────────────────
  cat("\n── 13. Estabilidad bootstrap de cargas (B = 200) ───────\n")
  set.seed(56)
  B       <- 200
  n_boot  <- min(n, nrow(mat))
  cargas_boot <- matrix(NA, nrow = B, ncol = p,
                        dimnames = list(NULL, vars))
  
  for (b in seq_len(B)) {
    idx_b  <- sample(seq_len(n_boot), n_boot, replace = TRUE)
    pca_b  <- tryCatch(
      PCA(as.data.frame(mat[idx_b, ]), graph = FALSE),
      error = function(e) NULL
    )
    if (!is.null(pca_b)) {
      # Alinear signo: correlacionar con primera carga original
      carga_orig <- pca$var$coord[, 1]
      carga_b    <- pca_b$var$coord[, 1]
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
    IC_Bajo    = round(apply(cargas_boot, 2, quantile, 0.025), 3),
    IC_Alto    = round(apply(cargas_boot, 2, quantile, 0.975), 3)
  ) %>%
    mutate(Estable = IC_Bajo * IC_Alto > 0)   # IC no cruza cero → estable
  
  print(boot_summary)
  inestables <- boot_summary$Variable[!boot_summary$Estable]
  if (length(inestables) > 0)
    cat("\n   ⚠  Cargas inestables (IC cruza cero):",
        paste(inestables, collapse = ", "), "\n")
  else
    cat("\n   ✔  Todas las cargas son estables (IC no cruza cero)\n")
  
  # Gráfico de intervalos bootstrap
  p_boot <- ggplot(boot_summary, aes(x = reorder(Variable, Carga_PCA),
                                     y = Carga_PCA, color = Estable)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = IC_Bajo, ymax = IC_Alto), width = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c("TRUE" = "#375623", "FALSE" = "#C00000"),
                       labels = c("TRUE" = "Estable", "FALSE" = "Inestable")) +
    coord_flip() +
    labs(title = paste("Estabilidad bootstrap de cargas Dim.1 —", label),
         subtitle = "Intervalos de confianza al 95% (B = 200)",
         x = "", y = "Carga factorial", color = "") +
    theme_minimal(base_size = 11)
  guardar(p_boot, paste0("06_bootstrap_", label))
  
  
  # ── RESULTADO FINAL ────────────────────────────────────────────────────
  cat("\n── RESUMEN ─────────────────────────────────────────────\n")
  puntos_ok <- c(
    bartlett$p.value < 0.05,
    !is.na(kmo_global) && kmo_global >= 0.60,
    var_acum >= 60,
    all(cos2_acum >= 0.50),
    all(boot_summary$Estable)
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
  
  # Devolver objetos útiles
  invisible(list(
    label       = label,
    pca         = pca,
    index       = index,
    kmo         = kmo_global,
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

resultados_val <- lapply(names(indices), function(lbl) {
  validar_pca(data_std, vars = indices[[lbl]], label = lbl, n_comp = 2)
})
names(resultados_val) <- names(indices)

# =======================================================================
# DIAGNÓSTICO Y MEJORA DE LOS ÍNDICES ICP / ICI / ICS
# =======================================================================
# Organización:
#   NIVEL 1 — Correcciones inmediatas (Alpha, variables problemáticas)
#   NIVEL 2 — Reestructuración de los índices (análisis paralelo, rotación)
#   NIVEL 3 — Alternativas al PCA exploratorio
# =======================================================================

dir.create("mejora_pca", showWarnings = FALSE)

guardar <- function(p, nombre, w = 8, h = 5)
  ggsave(paste0("mejora_pca/", nombre, ".png"), p, width = w, height = h, dpi = 150)

# -----------------------------------------------------------------------
# DEFINICIÓN ORIGINAL
# -----------------------------------------------------------------------
vars_icp_orig <- c("automa", "ecpacd", "ecppvs", "eficap", "ite")
vars_ici_orig <- c("ecos",   "sact",   "efene",  "mbi",   "roa")
vars_ics_orig <- c("cdig",   "intal",  "iact",   "ibl_pacd", "ibl_ppvs")

todas_vars <- c(vars_icp_orig, vars_ici_orig, vars_ics_orig)


# =======================================================================
# NIVEL 1 — CORRECCIONES INMEDIATAS
# =======================================================================

cat("\n", strrep("═", 60), "\n")
cat("  NIVEL 1 — CORRECCIONES INMEDIATAS\n")
cat(strrep("═", 60), "\n")


# ── 1A. Alpha de Cronbach CORRECTO (check.keys = TRUE) ────────────────

cat("\n── 1A. Alpha de Cronbach con check.keys = TRUE ─────────\n")

alpha_correcto <- function(df_vars, label) {
  df <- data_std[, df_vars]
  res <- psych::alpha(df, check.keys = TRUE)
  a   <- res$total$raw_alpha
  cat(sprintf("  %-5s → Alpha = %.3f", label, a))
  if      (a >= 0.70) cat("  ✔  Aceptable\n")
  else if (a >= 0.60) cat("  ⚠  Marginal\n")
  else                cat("  ✗  Bajo — las variables no forman una escala coherente\n")
  invisible(res)
}

alpha_correcto(vars_icp_orig, "ICP")
alpha_correcto(vars_ici_orig, "ICI")
alpha_correcto(vars_ics_orig, "ICS")

# ── 1B. Correlaciones internas detalladas ─────────────────────────────
# Permite ver exactamente qué pares de variables "tiran" en sentidos distintos.

cat("\n── 1B. Diagnóstico de correlaciones internas ───────────\n")

diagnostico_correlaciones <- function(df_vars, label) {
  df  <- data_std[, df_vars]
  R   <- cor(df, use = "complete.obs")
  det <- det(R)
  
  # Pares con correlación negativa fuerte (|r| > 0.20 y negativa)
  pares_neg <- which(R < -0.20 & lower.tri(R), arr.ind = TRUE)
  
  cat("\n  Índice:", label,
      "| Determinante:", round(det, 4),
      "| Correlación media:", round(mean(abs(R[lower.tri(R)])), 3), "\n")
  
  if (nrow(pares_neg) > 0) {
    cat("  ⚠  Pares con correlación negativa (|r|>0.20):\n")
    for (i in seq_len(nrow(pares_neg))) {
      r1 <- rownames(R)[pares_neg[i, 1]]
      r2 <- colnames(R)[pares_neg[i, 2]]
      cat(sprintf("     %s ↔ %s : r = %.3f\n", r1, r2, R[r1, r2]))
    }
  } else {
    cat("  ✔  Sin correlaciones negativas fuertes\n")
  }
  
  p <- ggcorrplot(R, hc.order = TRUE, type = "lower", lab = TRUE,
                  lab_size = 3.5,
                  colors = c("#C00000", "white", "#1F4E79"),
                  title = paste("Correlaciones internas —", label))
  guardar(p, paste0("1b_corr_", label))
}

diagnostico_correlaciones(vars_icp_orig, "ICP")
diagnostico_correlaciones(vars_ici_orig, "ICI")
diagnostico_correlaciones(vars_ics_orig, "ICS")


# ── 1C. Eliminar variables KMO < 0.50 y re-evaluar ───────────────────
# Basado en los resultados de la validación:
#   ICP: eliminar ecpacd, eficap (KMO < 0.50)
#   ICI: eliminar roa (KMO < 0.50)
#   ICS: eliminar cdig, intal, ibl_pacd, ibl_ppvs (KMO < 0.50)
#        OJO: eliminar ibl_pacd e ibl_ppvs deja ICS con muy pocas vars
#        → solo eliminar intal (KMO = 0.269, la más baja)

cat("\n── 1C. KMO post-eliminación de variables problemáticas ─\n")

vars_icp_v2 <- c("automa", "ecppvs", "ite")          # eliminados: ecpacd, eficap
vars_ici_v2 <- c("ecos", "sact", "efene", "mbi")     # eliminado: roa
vars_ics_v2 <- c("cdig", "iact", "ibl_pacd", "ibl_ppvs")  # eliminado: intal

re_kmo <- function(df_vars, label) {
  df  <- data_std[, df_vars]
  kmo <- psych::KMO(as.matrix(df))
  cat(sprintf("  %-8s → KMO global: %.3f  (variables: %s)\n",
              label, kmo$MSA, paste(df_vars, collapse = ", ")))
  kmo
}

cat("\n  Versiones reducidas:\n")
re_kmo(vars_icp_v2, "ICP_v2")
re_kmo(vars_ici_v2, "ICI_v2")
re_kmo(vars_ics_v2, "ICS_v2")


# =======================================================================
# NIVEL 2 — REESTRUCTURACIÓN DE LOS ÍNDICES
# =======================================================================

cat("\n\n", strrep("═", 60), "\n")
cat("  NIVEL 2 — REESTRUCTURACIÓN\n")
cat(strrep("═", 60), "\n")


# ── 2A. Análisis paralelo (determinar número real de factores) ─────────
# El análisis paralelo compara los eigenvalores observados con los
# esperados bajo datos aleatorios. Es más fiable que el criterio Kaiser.

cat("\n── 2A. Análisis paralelo por índice ────────────────────\n")

paralelo <- function(df_vars, label) {
  df  <- data_std[, df_vars]
  cat("\n  Análisis paralelo —", label, "\n")
  # fa.parallel: usa tanto PCA como FA para determinar n de factores/componentes
  ap <- psych::fa.parallel(df, fa = "pc", plot = FALSE, n.iter = 200,
                           sim = TRUE, error.bars = FALSE,
                           fm = "ml", main = paste("Paralelo", label))
  cat("  → Número sugerido de componentes PCA:", ap$ncomp, "\n")
  cat("  → Número sugerido de factores  FA:  ", ap$nfact, "\n")
  invisible(ap)
}

ap_icp <- paralelo(vars_icp_orig, "ICP")
ap_ici <- paralelo(vars_ici_orig, "ICI")
ap_ics <- paralelo(vars_ics_orig, "ICS")


# ── 2B. PCA con rotación Varimax ──────────────────────────────────────
# Cuando hay más de un componente, Varimax mejora la interpretabilidad
# al maximizar la varianza de las cargas al cuadrado → cargas más claras.
# Usar cuando el análisis paralelo sugiere ≥ 2 componentes.

cat("\n── 2B. PCA con rotación Varimax ────────────────────────\n")

pca_varimax <- function(df_vars, label, n_comp = 2) {
  df  <- data_std[, df_vars]
  mat <- as.matrix(df)
  
  # PCA base
  pca <- prcomp(mat, scale. = TRUE)
  
  # Rotación Varimax sobre los primeros n_comp componentes
  rot <- varimax(pca$rotation[, 1:n_comp])
  
  cargas_rot <- as.data.frame(rot$loadings[, 1:n_comp])
  colnames(cargas_rot) <- paste0("Factor", 1:n_comp)
  cargas_rot$Variable <- rownames(cargas_rot)
  
  cat("\n  Cargas rotadas (Varimax) —", label, "\n")
  print(cargas_rot %>%
          mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
          dplyr::select(Variable, everything()))
  
  # Varianza explicada por cada factor rotado
  var_exp <- colSums(rot$loadings^2) / nrow(rot$loadings) * 100
  cat("  Varianza por factor rotado:", paste(round(var_exp, 1), collapse = "% | "), "%\n")
  
  invisible(list(pca = pca, rot = rot, cargas = cargas_rot))
}

rot_icp <- pca_varimax(vars_icp_orig, "ICP", n_comp = 2)
rot_ici <- pca_varimax(vars_ici_orig, "ICI", n_comp = 2)
rot_ics <- pca_varimax(vars_ics_orig, "ICS", n_comp = 2)


# ── 2C. PCA global sobre las 15 variables ────────────────────────────
# Si las variables originales no se agrupan limpiamente en 3 índices,
# el PCA global revela la estructura factorial real de los datos.
# Esto puede sugerir una reasignación de variables a índices.

cat("\n── 2C. PCA global — estructura factorial real ──────────\n")

df_global <- data_std[, todas_vars]
pca_global <- PCA(df_global, graph = FALSE)

cat("\n  Eigenvalores globales:\n")
print(round(pca_global$eig[pca_global$eig[, "eigenvalue"] > 1, ], 3))

cat("\n  Análisis paralelo global:\n")
ap_global <- psych::fa.parallel(df_global, fa = "pc", plot = FALSE,
                                n.iter = 200, sim = TRUE)
cat("  → Componentes sugeridos:", ap_global$ncomp, "\n")
cat("  → Factores sugeridos:   ", ap_global$nfact, "\n")

# Cargas en los primeros 3-4 componentes globales
n_glob <- min(ap_global$ncomp, 4)
cargas_glob <- as.data.frame(pca_global$var$coord[, 1:n_glob])
colnames(cargas_glob) <- paste0("G", 1:n_glob)
cargas_glob$Variable <- rownames(cargas_glob)
cargas_glob$Indice_orig <- case_when(
  cargas_glob$Variable %in% vars_icp_orig ~ "ICP",
  cargas_glob$Variable %in% vars_ici_orig ~ "ICI",
  TRUE ~ "ICS"
)

cat("\n  Cargas globales (primeros", n_glob, "componentes):\n")
print(cargas_glob %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
        dplyr::select(Indice_orig, Variable, everything()) %>%
        arrange(Indice_orig))

# Heatmap de cargas globales
cargas_long <- cargas_glob %>%
  pivot_longer(cols = starts_with("G"),
               names_to = "Componente", values_to = "Carga")

p_cargas_glob <- ggplot(cargas_long,
                        aes(x = Componente, y = Variable, fill = Carga)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Carga, 2)), size = 3) +
  scale_fill_gradient2(low = "#C00000", mid = "white", high = "#1F4E79",
                       midpoint = 0, limits = c(-1, 1)) +
  facet_grid(Indice_orig ~ ., scales = "free_y", space = "free") +
  labs(title = "Cargas en PCA global (15 variables)",
       subtitle = "Agrupación original resaltada por faceta",
       x = "Componente global", y = "") +
  theme_minimal(base_size = 10) +
  theme(strip.text.y = element_text(angle = 0, face = "bold"))

p_cargas_glob

guardar(p_cargas_glob, "2c_cargas_pca_global", w = 7, h = 8)

cat("\n  → Si variables de índices distintos cargan en el mismo componente,\n")
cat("    la agrupación teórica no coincide con la estructura empírica.\n")
cat("    Ver: mejora_pca/2c_cargas_pca_global.png\n")


# =======================================================================
# NIVEL 3 — ALTERNATIVAS AL PCA EXPLORATORIO
# =======================================================================

cat("\n\n", strrep("═", 60), "\n")
cat("  NIVEL 3 — ALTERNATIVAS AL PCA EXPLORATORIO\n")
cat(strrep("═", 60), "\n")


# ── 3A. Índice por media ponderada por correlación (sin PCA) ──────────
# Cuando las variables no comparten un factor latente fuerte, una
# alternativa robusta es ponderar cada variable por su correlación
# promedio con las demás dentro del índice.
# Esta ponderación tiene justificación teórica (mayor coherencia interna
# → mayor peso) y no depende de supuestos del PCA.

cat("\n── 3A. Índice ponderado por correlación media ──────────\n")

indice_pond_corr <- function(df_vars, label) {
  df   <- data_std[, df_vars]
  R    <- cor(df, use = "complete.obs")
  
  # Correlación media de cada variable con las demás (sin contar consigo misma)
  cor_media <- (colSums(abs(R)) - 1) / (ncol(R) - 1)
  w_corr    <- cor_media / sum(cor_media)
  
  cat(sprintf("\n  Ponderadores por correlación media — %s:\n", label))
  df_w <- data.frame(Variable = names(w_corr),
                     Cor_media = round(cor_media, 3),
                     Peso = round(w_corr, 3))
  print(df_w)
  
  index <- as.numeric(as.matrix(df) %*% w_corr)
  cat("  → Índice: Min =", round(min(index), 2),
      "| Mediana =", round(median(index), 2),
      "| Max =", round(max(index), 2), "\n")
  
  list(index = index, pesos = w_corr, df_pesos = df_w)
}

ind_icp_corr <- indice_pond_corr(vars_icp_orig, "ICP")
ind_ici_corr <- indice_pond_corr(vars_ici_orig, "ICI")
ind_ics_corr <- indice_pond_corr(vars_ics_orig, "ICS")


# ── 3B. Análisis Factorial Confirmatorio (AFC) ────────────────────────
# Si la agrupación de variables en ICP/ICI/ICS tiene respaldo teórico
# sólido, el AFC prueba si el modelo de medida es consistente con datos.
# Requiere el paquete lavaan.

cat("\n── 3B. Análisis Factorial Confirmatorio (AFC) ──────────\n")

if (!requireNamespace("lavaan", quietly = TRUE)) install.packages("lavaan")
library(lavaan)

# Modelo teórico: cada índice es un factor latente
modelo_cfa <- "
  ICP =~ automa + ecpacd + ecppvs + eficap + ite
  ICI =~ ecos   + sact   + efene  + mbi    + roa
  ICS =~ cdig   + intal  + iact   + ibl_pacd + ibl_ppvs
"

df_cfa <- as.data.frame(data_std[, todas_vars])

fit_cfa <- tryCatch(
  lavaan::cfa(modelo_cfa, data = df_cfa,
              estimator = "MLR",      # robusto a no normalidad
              std.lv = TRUE),
  error = function(e) { cat("  ✗ AFC no convergió:", e$message, "\n"); NULL }
)

if (!is.null(fit_cfa)) {
  fit_idx <- lavaan::fitMeasures(fit_cfa,
                                 c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
  
  cat("\n  Índices de ajuste del AFC:\n")
  cat(sprintf("  Chi²(df=%g) = %.1f  p = %.4f\n",
              fit_idx["df"], fit_idx["chisq"], fit_idx["pvalue"]))
  cat(sprintf("  CFI = %.3f  (≥0.95 = buen ajuste)\n",  fit_idx["cfi"]))
  cat(sprintf("  TLI = %.3f  (≥0.95 = buen ajuste)\n",  fit_idx["tli"]))
  cat(sprintf("  RMSEA = %.3f  (≤0.06 = buen ajuste)\n", fit_idx["rmsea"]))
  cat(sprintf("  SRMR  = %.3f  (≤0.08 = buen ajuste)\n", fit_idx["srmr"]))
  
  if (fit_idx["cfi"] >= 0.95 && fit_idx["rmsea"] <= 0.06) {
    cat("\n  ✔  El modelo AFC ajusta bien — la agrupación teórica es válida.\n")
    cat("     Recomendación: usar las cargas del AFC como ponderadores.\n")
  } else {
    cat("\n  ✗  El modelo AFC NO ajusta bien.\n")
    cat("     Implicación: la agrupación teórica no está respaldada por los datos.\n")
    cat("     → Solicitar índices de modificación para reasignar variables.\n")
    
    # Índices de modificación (sugerencias de reasignación)
    mi <- lavaan::modindices(fit_cfa, sort = TRUE, maximum.number = 10)
    cat("\n  Top 10 índices de modificación (reasignaciones sugeridas):\n")
    print(mi[mi$op == "=~", c("lhs", "op", "rhs", "mi", "epc")])
  }
  
  # Cargas estandarizadas del AFC
  cat("\n  Cargas estandarizadas del AFC:\n")
  cargas_afc <- lavaan::standardizedSolution(fit_cfa) %>%
    filter(op == "=~") %>%
    dplyr::select(Factor = lhs, Variable = rhs,
                  Carga = est.std, SE = se, pvalue)
  print(cargas_afc %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
}


# ── 3C. Comparación de índices: PCA vs Correlación vs AFC ─────────────

cat("\n── 3C. Correlación entre métodos de construcción ───────\n")

# PCA 
robust_scale <- function(x) {
  iqr <- IQR(x, na.rm = TRUE)
  if (iqr == 0) return(rep(0, length(x)))
  (x - median(x, na.rm = TRUE)) / iqr
}

# Función PCA estándar
pca_index <- function(df_vars) {
  df  <- data_std[, df_vars]
  pca <- PCA(df, graph = FALSE)
  w   <- pca$eig[1:2, "eigenvalue"] / sum(pca$eig[1:2, "eigenvalue"])
  as.numeric(w[1] * pca$ind$coord[, 1] + w[2] * pca$ind$coord[, 2])
}

icp_pca  <- pca_index(vars_icp_orig)
ici_pca  <- pca_index(vars_ici_orig)
ics_pca  <- pca_index(vars_ics_orig)

icp_corr <- ind_icp_corr$index
ici_corr <- ind_ici_corr$index
ics_corr <- ind_ics_corr$index

comparacion <- data.frame(
  Indice  = c("ICP", "ICI", "ICS"),
  r_PCA_Corr = c(
    round(cor(icp_pca, icp_corr), 3),
    round(cor(ici_pca, ici_corr), 3),
    round(cor(ics_pca, ics_corr), 3)
  )
)

cat("\n  Correlación entre índice por PCA y por ponderación de correlación:\n")
print(comparacion)
cat("\n  Si r > 0.90: los dos métodos son equivalentes → usar el más simple.\n")
cat("  Si r < 0.70: los métodos divergen → revisar cuál tiene mejor\n")
cat("               respaldo teórico y estadístico.\n")


# -----------------------------------------------------------------------
# 4. PREPARACIÓN DE BASE PARA CLUSTERING
# -----------------------------------------------------------------------
# Reestructuración de variables por índice

vars_icp <- c("automa", "iact", "ibl_pot")
vars_ici <- c("ecos", "efene", "mbi" )
vars_ics <- c("cdig", "intal", "ite")

indices <- list(
  ICP = vars_icp,
  ICI = vars_ici,
  ICS = vars_ics
)

res_icp  <- compute_pca_index(data_std, vars_icp, "ICP")
data_std$ICP <- res_icp$index

res_ici  <- compute_pca_index(data_std, vars_ici, "ICI")
data_std$ICI <- res_ici$index

res_ics  <- compute_pca_index(data_std, vars_ics, "ICS")
data_std$ICS <- res_ics$index


resultados_val <- lapply(names(indices), function(lbl) {
  validar_pca(data_std, vars = indices[[lbl]], label = lbl, n_comp = 2)
})
names(resultados_val) <- names(indices)


# Variables de interés para clustering
vi <- c("ptf", "marpot","compot",
        "ICP", "ICI", "ICS", "Qs_pot")

# Verificar que todas existen
missing_vi <- vi[!vi %in% names(data_std)]
if (length(missing_vi) > 0) stop("Faltan variables: ", paste(missing_vi, collapse = ", "))

caa <- dplyr::select(data_std, NOMGEO, tcode, AE, ID, all_of(vi))
caa_split <- split(caa, caa$tcode)

cat("\nAños disponibles para clustering:", names(caa_split), "\n")
cat("Observaciones por año:\n")
sapply(caa_split, nrow) %>% print()


# -----------------------------------------------------------------------
# 5. DETERMINACIÓN DEL NÚMERO ÓPTIMO DE CLUSTERS
# -----------------------------------------------------------------------

dir.create("resultados_clusters", showWarnings = FALSE)

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

# -----------------------------------------------------------------------
# 8. CLUSTERING JERÁRQUICO (WARD.D2)
# -----------------------------------------------------------------------

hclust_anual <- function(df, vars, k = 6,
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
  tryCatch(hclust_anual(df, vars = vi, k = 6, cluster_colname = "cluster_ward"),
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
                                G_gmm = 6,               # número de clusters para GMM (si se fija)
                                k_spec = 6,               # número de clusters para espectral
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
  # DBSCAN
  data$cluster_db <- if (!is.null(db)) {
    # Obtener vector de asignación de clusters
    clusters <- db$cluster}
  
  
  
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
      G_gmm = 6,
      k_spec = 6,
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

# Para el año 2003
df_2003_av <- resultados_avanzados[["2003"]]$data_actualizado
df_2003_av %>%
  pivot_longer(cols = all_of(vi), names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = cluster_spec, y = valor, fill = cluster_spec)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Distribución de variables por cluster espectral - 2003",
       x = "Cluster espectral", y = "Valor") +
  theme_minimal() +
  theme(legend.position = "none")

# 10. ESTADÍSTICAS DESCRIPTIVAS POR CLUSTER
# -----------------------------------------

summarize_clusters <- function(data, 
                               vars, 
                               cluster_col = "cluster_spec", 
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
  if (!"cluster_spec" %in% colnames(res$data_actualizado)) {
    warning("La columna 'cluster_spec' no existe en el año ", anio)
    return(NULL)
  }
  
  summarize_clusters(
    data = res$data_actualizado,   # data frame completo
    vars = vi,
    cluster_col = "cluster_spec",
    year_label = anio
  )
})

# Unir todos en un solo data frame
resumen_total <- bind_rows(lista_resumenes)

# Filtrar año 2003 y formatear
resumen_2003 <- resumen_total %>% filter(anio == "2003")

# Redondear y mostrar con kable
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
                                  cluster_col = "cluster_spc", 
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
                        paste("Distribución de variables por cluster -", year_label),
                        "Distribución de variables por cluster"),
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
    cluster_col = "cluster_spec",
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

# 11. ANÁLISIS DE VARIANZA (ANOVA) Y POST-HOC
# -------------------------------------------

anova_clusters <- function(data, 
                           vars, 
                           cluster_col = "cluster_ward", 
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
    cluster_col = "cluster_spec",
    year_label = anio
  )
})
names(anova_list) <- names(resultados_avanzados)

anova_total <- bind_rows(anova_list)

# Ver tabla completa
anova_total %>%
  mutate(across(c(df, sumsq, meansq, statistic, p.value, eta_sq), ~ round(., 4))) %>%
  kable(caption = "Resultados de ANOVA por variable y año",
        col.names = c("Año", "Variable", "DF", "Sum Sq", "Mean Sq", "F", "p-valor", "Eta^2", "Efecto")) %>%
  kable_styling("striped", full_width = FALSE)

write_csv(anova_total, "anova_clusters_todos_anos.csv")


# Pruebas post-hoc de Tukey para cada variable

library(agricolae)   # para HSD.test

anova_tukey_final <- function(data, 
                              vars, 
                              cluster_col = "cluster_spec", 
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

library(car)   # para leveneTest

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

# Asegurar que los paquetes están instalados
if (!require("vegan")) install.packages("vegan")
if (!require("devtools")) install.packages("devtools")
if (!require("pairwiseAdonis")) {
  devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
}

library(vegan)
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

library(MASS)

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
imp03 <- importancia_variables_lda(
  lda_model = resultados_lda[["2003_cluster_spec"]]$lda_model,
  data = resultados_avanzados[["2003"]]$data_actualizado,
  metodo = "ambos"
)
print(imp03)
write.csv(imp03, "importancia_2003_gmm.csv", row.names = FALSE)

imp08 <- importancia_variables_lda(
  lda_model = resultados_lda[["2008_cluster_gmm"]]$lda_model,
  data = resultados_avanzados[["2008"]]$data_actualizado,
  metodo = "ambos"
)
print(imp08)
write.csv(imp08, "importancia_2008_gmm.csv", row.names = FALSE)

imp13 <- importancia_variables_lda(
  lda_model = resultados_lda[["2013_cluster_gmm"]]$lda_model,
  data = resultados_avanzados[["2013"]]$data_actualizado,
  metodo = "ambos"
)
print(imp13)
write.csv(imp13, "importancia_2013_gmm.csv", row.names = FALSE)

imp18 <- importancia_variables_lda(
  lda_model = resultados_lda[["2018_cluster_gmm"]]$lda_model,
  data = resultados_avanzados[["2018"]]$data_actualizado,
  metodo = "ambos"
)
print(imp18)
write.csv(imp18, "importancia_2018_gmm.csv", row.names = FALSE)

imp23 <- importancia_variables_lda(
  lda_model = resultados_lda[["2023_cluster_gmm"]]$lda_model,
  data = resultados_avanzados[["2023"]]$data_actualizado,
  metodo = "ambos"
)
print(imp23)
write.csv(imp23, "importancia_2023_gmm.csv", row.names = FALSE)


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
        
        # Verificar que haya al menos 2 años y que cada ID tenga datos en todos los años
        anos_presentes <- colnames(datos_ancho)[-1]  # excluir id_col
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
      resultados_final %>% dplyr::select(row_id, cluster_gmm, cluster_spectral, k_gmm, k_spectral),
      by = row_id_col
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

resultados_avanzados <- list()

for (anio in names(caa_split)) {
  cat("\n========== Procesando año:", anio, "==========\n")
  df_anio <- caa_split[[anio]]
  res <- advanced_clustering(
    data = df_anio,
    vars = vi,
    cluster_col_base = "cluster_ward",  
    year_label = anio,
    G_gmm = 6,
    k_spec = 6,
    eps_dbscan = 0.65,
    minPts_dbscan = 7,
    run_dbscan = FALSE 
  )
  resultados_avanzados[[anio]] <- res
}

df_unificado <- bind_rows(lapply(names(resultados_avanzados), function(anio) {
  res <- resultados_avanzados[[anio]]
  if (is.null(res)) return(NULL)
  # Extraer el data frame actualizado
  df_anio <- res$data_actualizado
  # Añadir columna de año
  df_anio %>%
    mutate(tcode = as.numeric(anio))
}))

# Verificar estructura
glimpse(df_unificado)

df_unificado <- df_unificado %>%
  mutate(ID = paste(NOMGEO, AE, sep = "_"))

# Definir pares de años (ajusta según tus años)
pares_anios <- list(
  c(2003, 2008), c(2008, 2013), c(2013, 2018), c(2018, 2023), c(2003, 2023)
)
names(pares_anios) <- c("2003_2008", "2008_2013", "2013_2018", "2018_2023", "2003_2023")

# Listas para almacenar resultados
matrices_transicion <- list()
matrices_norm <- list()
estabilidades <- data.frame(Periodo = character(), Estabilidad = numeric())
tests_markov <- list()

crear_matriz_transicion <- function(datos, 
                                    anio_inicio, 
                                    anio_fin, 
                                    cluster_col = "cluster_spec",
                                    id_col = "ID",
                                    time_col = "tcode") {
  
  # Verificar columnas necesarias
  if (!all(c(id_col, time_col, cluster_col) %in% colnames(datos))) {
    stop("Faltan columnas necesarias: ", 
         paste(setdiff(c(id_col, time_col, cluster_col), colnames(datos)), collapse = ", "))
  }
  
  # Filtrar años
  datos_filt <- datos %>% filter(!!sym(time_col) %in% c(anio_inicio, anio_fin))
  
  # Para cada ID, extraer cluster en año inicio y fin (tomar el primer valor si hay duplicados)
  transicion <- datos_filt %>%
    group_by(!!sym(id_col)) %>%
    summarise(
      cluster_inicio = .data[[cluster_col]][.data[[time_col]] == anio_inicio][1],
      cluster_fin    = .data[[cluster_col]][.data[[time_col]] == anio_fin][1],
      .groups = "drop"
    ) %>%
    filter(!is.na(cluster_inicio), !is.na(cluster_fin)) %>%
    count(cluster_inicio, cluster_fin)
  
  # Obtener todos los clusters presentes
  clusters <- sort(unique(c(transicion$cluster_inicio, transicion$cluster_fin)))
  
  # Crear cuadrícula completa
  grid <- expand.grid(cluster_inicio = clusters, cluster_fin = clusters, stringsAsFactors = FALSE)
  
  # Unir y rellenar ceros
  matriz_completa <- grid %>%
    left_join(transicion, by = c("cluster_inicio", "cluster_fin")) %>%
    mutate(n = replace_na(n, 0)) %>%
    pivot_wider(names_from = cluster_fin, values_from = n, values_fill = 0) %>%
    column_to_rownames("cluster_inicio") %>%
    as.matrix()
  
  # Ordenar filas y columnas
  matriz_completa <- matriz_completa[order(rownames(matriz_completa)), order(colnames(matriz_completa))]
  
  return(matriz_completa)
}

for (nombre in names(pares_anios)) {
  anios <- pares_anios[[nombre]]
  cat("\n========== Transición", nombre, "==========\n")
  
  M <- crear_matriz_transicion(
    datos = df_unificado,
    anio_inicio = anios[1],
    anio_fin = anios[2],
    cluster_col = "cluster_spec",   
    id_col = "ID",
    time_col = "tcode"
  )
  matrices_transicion[[nombre]] <- M
  
  M_norm <- prop.table(M, margin = 1)
  matrices_norm[[nombre]] <- M_norm
  
  estab <- sum(diag(M)) / sum(M) * 100
  estabilidades <- rbind(estabilidades, data.frame(Periodo = nombre, Estabilidad = round(estab, 2)))
  
  test <- chisq.test(M, simulate.p.value = TRUE, B = 10000)
  tests_markov[[nombre]] <- test
  
  # Mostrar resultados
  print(round(M_norm, 3))
  cat("Estabilidad:", round(estab, 2), "%\n")
  cat("p-value:", format(test$p.value, scientific = TRUE), "\n")
  
  # Guardar matrices en CSV
  write.csv(as.data.frame(M), paste0("transicion_", nombre, ".csv"), row.names = TRUE)
  write.csv(as.data.frame(M_norm), paste0("transicion_norm_", nombre, ".csv"), row.names = TRUE)
}

# Ver tabla de estabilidades
print(estabilidades)
write.csv(estabilidades, "estabilidades_por_periodo.csv", row.names = FALSE)

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
library(ggalluvial)

trayectorias <- df_unificado %>%
  filter(tcode %in% c(2003, 2008, 2013, 2018, 2023)) %>%
  arrange(ID, tcode) %>%
  group_by(ID) %>%
  summarise(
    cluster_2003 = .data$cluster_spec[tcode == 2003][1],
    cluster_2008 = .data$cluster_spec[tcode == 2008][1],
    cluster_2013 = .data$cluster_spec[tcode == 2013][1],
    cluster_2018 = .data$cluster_spec[tcode == 2018][1],
    cluster_2023 = .data$cluster_spec[tcode == 2023][1],
    .groups = "drop"
  ) %>%
  filter(complete.cases(.))

trayectorias_count <- trayectorias %>%
  group_by(cluster_2003, cluster_2008, cluster_2013, cluster_2018, cluster_2023) %>%
  summarise(frecuencia = n(), .groups = "drop") %>%
  arrange(desc(frecuencia))

# Top 50 trayectorias
top_tray <- trayectorias_count %>% slice_head(n = 50)

ggplot(top_tray,
       aes(axis1 = cluster_2003, axis2 = cluster_2008, axis3 = cluster_2013,
           axis4 = cluster_2018, axis5 = cluster_2023, y = frecuencia)) +
  geom_alluvium(aes(fill = factor(cluster_2003)), width = 1/12) +
  geom_stratum(width = 1/12, fill = "lightgray", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("2003", "2008", "2013", "2018", "2023"), expand = c(0.05, 0.05)) +
  labs(title = "Transiciones de aglomeraciones espectrales 2003-2023",
       x = "Período", y = "Número de entidades", fill = "Cluster 2003") +
  theme_minimal()
ggsave("alluvial_espectral.png", width = 14, height = 8)

# Análisis de la transición

analizar_cadena_markov <- function(mat_transicion, 
                                   desde = NULL, 
                                   periodo = "",
                                   plot = TRUE,
                                   return_all = FALSE) {
  
  # Verificar paquetes
  if (!requireNamespace("igraph", quietly = TRUE)) install.packages("igraph")
  if (!requireNamespace("diagram", quietly = TRUE)) install.packages("diagram")
  library(igraph)
  library(diagram)
  
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

for (nom in names(matrices_norm)) {
  cat("\n")
  analizar_cadena_markov(mat_transicion = matrices_norm[[nom]], 
                         periodo = nom, 
                         plot = TRUE)
}

# Interpretación de resultados
# Distribución estacionaria: proporción de tiempo a largo plazo que la cadena pasa en cada estado (cluster).
# Tiempo medio de retorno: número promedio de pasos para regresar a un estado, comenzando desde ese mismo estado.
# Si la cadena no es irreducible, significa que hay estados "absorbentes" o grupos de estados que no se comunican; en ese caso, no hay una distribución estacionaria única.


# =======================
####### 13. Análisis espacial

library(sf)
library(spdep)
library(tmap)

# Leer shapefile de entidades 
mx <- st_read("C:/Users/gezum/Desktop/Entidades_Federativas/Entidades_Federativas.shp", quiet = TRUE)

# Corrección de encoding 
encodings <- guess_encoding("C:/Users/gezum/Desktop/Entidades_Federativas/Entidades_Federativas.dbf")
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
    
# Ejemplo: año 2003, variable "icp" 

# Ver nombres de columnas en el objeto espacial del año 2003
names(resultados_espaciales[["2003"]]$sf_entidades)

# Ver resultados de Moran global para el año 2003
print(resultados_espaciales[["2003"]]$moran_global)

anio_ejemplo <- "2003"
var_ejemplo <- "ICP"  

sf_ent <- resultados_espaciales[[anio_ejemplo]]$sf_entidades
col_local <- paste0("local_", var_ejemplo)
sum(is.na(sf_ent$local_ICP))

# Verificar si hay geometrías inválidas
invalid <- st_is_valid(sf_ent)
table(invalid)

# Si hay inválidas, intentar repararlas
if(any(!invalid)) {
  sf_ent <- st_make_valid(sf_ent)
}

ggplot(sf_ent) +
  geom_sf(aes(fill = local_ICP)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal()

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
  # Verificar que no sea NULL y tenga la estructura esperada
  if (is.null(x) || is.null(x$data_actualizado)) {
    warning("Año ", anio, " no tiene data_actualizado. Se omite.")
    next
  }
  df <- x$data_actualizado
  # Verificar columnas necesarias
  if (!all(c("AE", "NOMGEO") %in% colnames(df))) {
    warning("Año ", anio, " no tiene columnas AE o NOMGEO. Se omite.")
    next
  }
  df$tcode <- anio  # o x$anio si está definido
  # Seleccionar columnas de interés
  df_sel <- df %>% dplyr::select(NOMGEO, AE, tcode, cluster_gmm, cluster_spec)
  lista_dfs[[anio]] <- df_sel
}

# Unir todos los data frames
jdf <- bind_rows(lista_dfs)

library(viridis)
library(scales)

# --- Construcción del data frame unificado con bucle for ---
lista_dfs <- list()
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
  df$tcode <- anio  # asumiendo que anio es el nombre del año
  df_sel <- df %>% dplyr::select(NOMGEO, AE, tcode, cluster_gmm, cluster_spec)
  lista_dfs[[anio]] <- df_sel
}

# Unir todos los años
jdf <- bind_rows(lista_dfs)

# --- Preparar shapefile ---
# Asegurar que mx está limpio y con nombres normalizados
mx$NOMGEO <- trimws(mx$NOMGEO)
jdf$NOMGEO <- trimws(jdf$NOMGEO)

# Unir con geometrías
jdf_sf <- jdf %>%
  left_join(mx, by = "NOMGEO") %>%
  st_as_sf()

# Verificar geometrías vacías
if (any(st_is_empty(jdf_sf))) {
  warning("Algunas filas no tienen geometría. Revisa la unión.")
}

# --- Gráfico 1: Mapa facetado por sector y año ---
cluster_elegido <- "cluster_spec"  

p1 <- ggplot(jdf_sf) +
  geom_sf(aes(fill = as.factor(.data[[cluster_elegido]])), color = NA) +
  scale_fill_viridis_d("Cluster") +
  facet_grid(AE ~ tcode) +
  theme_minimal() +
  labs(title = "Evolución de aglomeraciones espectrales por sector y quinquenio") +
  theme(axis.text = element_blank(),
        strip.text.y = element_text(angle = 0, size = 8),
        strip.text.x = element_text(size = 10))

print(p1)
ggsave("mapa_clusters_sector_anio.png", p1, width = 15, height = 20, limitsize = FALSE)

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
  labs(title = "Distribución proporcional de conglomerados por Entidad",
       x = "Quinquenio", y = "Proporción") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d("Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        panel.spacing = unit(0.5, "lines"))

print(p2)
ggsave("barras_proporcion_entidad.png", p2, width = 12, height = 18)


# Reconstruir jdf_sf si es necesario (basado en resultados_avanzados)
if (!exists("jdf_sf")) {
  lista_dfs <- map(resultados_avanzados, function(x) {
    df <- x$data_actualizado
    if (!"AE" %in% colnames(df)) stop("Falta columna AE")
    if (!"NOMGEO" %in% colnames(df)) stop("Falta columna NOMGEO")
    df$tcode <- x$anio
    df %>% dplyr::select(NOMGEO, AE, tcode, cluster_gmm, cluster_spec)
  })
  jdf <- bind_rows(lista_dfs)
  
  # Limpiar nombres y unir con shapefile
  mx$NOMGEO <- trimws(mx$NOMGEO)
  jdf$NOMGEO <- trimws(jdf$NOMGEO)
  jdf_sf <- jdf %>%
    left_join(mx, by = "NOMGEO") %>%
    st_as_sf()
}

# Elegir columna de clusters (espectral por defecto)
cluster_col <- "cluster_spec"

library(ggstream)

# 1. Preparar datos: contar observaciones por entidad, año y cluster
stream_data <- jdf_sf %>%
  st_drop_geometry() %>%
  group_by(NOMGEO, tcode, .data[[cluster_col]]) %>%
  summarise(n = n(), .groups = "drop")

# 2. Crear el gráfico con geom_stream
p_stream <- ggplot(stream_data, aes(x = tcode, y = n, fill = .data[[cluster_col]])) +
  geom_stream(bw = 0.8) +
  facet_wrap(~ NOMGEO, ncol = 4) +
  labs(title = "Distribución de Clusters por Entidad (stream graph)",
       x = "Quinquenio", y = "Número de observaciones") +
  scale_fill_viridis_d() +
  theme_minimal()

print(p_stream)
ggsave("stream_clusters_entidad.png", p_stream, width = 15, height = 20, limitsize = FALSE)

# Gráfico de barras proporcionales por entidad (sin AE para no saturar)
p_barras <- jdf_sf %>%
  group_by(NOMGEO, tcode, .data[[cluster_col]]) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(NOMGEO, tcode) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = as.factor(tcode), y = prop, fill = .data[[cluster_col]])) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ NOMGEO, ncol = 4) +
  labs(title = "Proporción de aglomeraciones por entidad y quinquenio",
       x = "Quinquenio", y = "Proporción") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_barras)
ggsave("barras_proporcion_entidad_clean.png", width = 14, height = 18)

library(trelliscopejs)

caa_trellis <- jdf %>%
  mutate(panel_id = paste(NOMGEO, AE, sep = " - ")) %>%
  count(panel_id, tcode, .data[[cluster_col]]) %>%
  group_by(panel_id, tcode) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(caa_trellis, aes(x = tcode, y = prop, fill = .data[[cluster_col]])) +
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
