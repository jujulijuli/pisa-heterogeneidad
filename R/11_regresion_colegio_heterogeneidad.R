# =============================================================================
# 11_regresion_colegio_heterogeneidad.R
#
# Pieza 2 (de 2) para responder la pregunta de Julián (2026-09-26): a nivel
# de COLEGIO (no de CCAA -- mucha más potencia, miles de colegios en vez de
# ~17), ¿cuánto explican la composición migrante/lingüística y la
# heterogeneidad de HISEI de la dispersión de resultados DENTRO del colegio
# (heterogeneidad) y de su media (nivel)?
#
# Método: lm() ponderado por nº de alumnos, con errores estándar robustos
# agrupados por CCAA (mismo `cluster_robust_ci()` -- sandwich CR1 -- ya
# usado en pisa-espana-ccaa/R/06_exposicion_digital.R, adaptado aquí
# agrupando por CCAA en vez de por colegio porque la unidad de análisis YA
# es el colegio). Deliberadamente NO es un modelo jerárquico bayesiano
# (INLA) -- esto es una primera pasada exploratoria y rápida; el
# tratamiento jerárquico riguroso, si hace falta, es extender
# pisa-espana-ccaa/R/09_modelo_covariables_colegio.R (pieza 3, aparte).
#
# AVISO: con ~17-19 CCAA como clusters, el ajuste de grados de libertad del
# error robusto es aproximado (la teoría asintótica de sandwich CR1 asume
# muchos más clusters) -- los IC deben leerse como orientativos, no exactos.
# =============================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
})

prep <- readRDS("data/student_esp_prep.rds") |>
  mutate(score_global = rowMeans(cbind(math, read, science), na.rm = TRUE))

weighted_sd <- function(x, w) {
  m <- weighted.mean(x, w)
  sqrt(sum(w * (x - m)^2) / sum(w))
}

UMBRAL_MIN_ALUMNOS_DENTRO <- 10

# --- 1. Dispersión y media de score_global por colegio-año ------------------

colegio_score_global <- prep |>
  filter(!is.na(score_global), !is.na(stu_wgt)) |>
  group_by(year, ccaa, global_school_id, public_private) |>
  filter(n() >= UMBRAL_MIN_ALUMNOS_DENTRO) |>
  summarise(n_alumnos = n(), w_total = sum(stu_wgt),
            media_colegio = weighted.mean(score_global, stu_wgt),
            sd_dentro = weighted_sd(score_global, stu_wgt), .groups = "drop")

# --- 2. Unir composición de HISEI y migración/lengua -------------------------

composicion_hisei <- readRDS("data/tbl_colegio_composicion_socioeconomica.rds") |>
  select(year, global_school_id, hisei_sd, diversidad_estatus)

composicion_migracion <- readRDS("data/tbl_colegio_migracion_lengua.rds") |>
  select(year, global_school_id, pct_migrante, pct_otra_lengua)

colegio_completo <- colegio_score_global |>
  inner_join(composicion_hisei, by = c("year", "global_school_id")) |>
  inner_join(composicion_migracion, by = c("year", "global_school_id")) |>
  filter(!is.na(public_private))  # necesario como covariable de control

message("Colegios-año con las 3 piezas de datos completas: ", nrow(colegio_completo),
        " (de ", nrow(colegio_score_global), " con dispersión de resultados calculada)")

saveRDS(colegio_completo, "data/tbl_colegio_completo_heterogeneidad.rds")

# --- 3. Error estándar robusto agrupado por CCAA (sandwich CR1) ------------
# Mismo patrón que cluster_robust_ci() en pisa-espana-ccaa/R/06_exposicion_digital.R,
# agrupando aquí por CCAA (la unidad de análisis ya es el colegio, no el alumno).

cluster_robust_ci <- function(fit, cluster) {
  X <- model.matrix(fit)
  stopifnot(nrow(X) == length(cluster))
  w <- weights(fit)
  score <- (residuals(fit) * w) * X
  meat <- rowsum(score, cluster)
  bread <- solve(t(X * w) %*% X)
  n_clust <- length(unique(cluster))
  k <- ncol(X)
  n <- nrow(X)
  adj <- (n_clust / (n_clust - 1)) * ((n - 1) / (n - k))
  vcov_cr <- adj * bread %*% (t(meat) %*% meat) %*% bread
  se <- sqrt(diag(vcov_cr))
  tibble(term = names(coef(fit)), estimate = coef(fit), se_cluster = se,
         ci_low = estimate - 1.96 * se, ci_high = estimate + 1.96 * se,
         n_clusters = n_clust)
}

# --- 4. Modelos anidados: heterogeneidad (SD dentro) y media ---------------
# Base (año + titularidad, sin composición) vs. completo (+ composición) --
# el incremento de R2 es "cuánto explica la composición, más allá de año y
# titularidad" (misma lógica de comparación anidada que el VPC-ablation de
# 09_modelo_covariables_colegio.R en pisa-espana-ccaa, aquí con R2 de lm en
# vez de VPC de un modelo jerárquico).

ajustar_par <- function(data, y_var) {
  f_base <- as.formula(paste(y_var, "~ factor(year) + public_private"))
  f_completo <- as.formula(paste(y_var, "~ factor(year) + public_private + pct_migrante + pct_otra_lengua + hisei_sd"))
  fit_base <- lm(f_base, data = data, weights = n_alumnos)
  fit_completo <- lm(f_completo, data = data, weights = n_alumnos)
  list(
    base = fit_base, completo = fit_completo,
    r2_base = summary(fit_base)$r.squared,
    r2_completo = summary(fit_completo)$r.squared,
    coefs = cluster_robust_ci(fit_completo, data$ccaa) |>
      filter(term %in% c("pct_migrante", "pct_otra_lengua", "hisei_sd"))
  )
}

modelo_sd <- ajustar_par(colegio_completo, "sd_dentro")
modelo_media <- ajustar_par(colegio_completo, "media_colegio")

# --- 5. Resultados -------------------------------------------------------

resultados <- bind_rows(
  modelo_sd$coefs |> mutate(y = "sd_dentro (heterogeneidad)",
                             r2_base = modelo_sd$r2_base, r2_completo = modelo_sd$r2_completo),
  modelo_media$coefs |> mutate(y = "media_colegio (nivel)",
                                r2_base = modelo_media$r2_base, r2_completo = modelo_media$r2_completo)
) |>
  select(y, term, estimate, ci_low, ci_high, r2_base, r2_completo, n_clusters)

saveRDS(resultados, "data/tbl_regresion_colegio_heterogeneidad.rds")
saveRDS(list(modelo_sd = modelo_sd, modelo_media = modelo_media), "data/modelos_colegio_heterogeneidad.rds")

message("\n=== N colegios-año en el modelo: ", nrow(colegio_completo), " (", n_distinct(colegio_completo$ccaa), " CCAA como clusters) ===")

message("\n=== Heterogeneidad (SD dentro del colegio) ~ composición ===")
message("R2 solo año+titularidad: ", round(modelo_sd$r2_base, 4),
        " | R2 + composición: ", round(modelo_sd$r2_completo, 4),
        " | incremento: ", round(modelo_sd$r2_completo - modelo_sd$r2_base, 4))
print(modelo_sd$coefs |> mutate(across(c(estimate, ci_low, ci_high), ~round(.x, 3))), n = Inf)

message("\n=== Media del colegio ~ composición ===")
message("R2 solo año+titularidad: ", round(modelo_media$r2_base, 4),
        " | R2 + composición: ", round(modelo_media$r2_completo, 4),
        " | incremento: ", round(modelo_media$r2_completo - modelo_media$r2_base, 4))
print(modelo_media$coefs |> mutate(across(c(estimate, ci_low, ci_high), ~round(.x, 3))), n = Inf)

message("\nListo. Tabla guardada en data/tbl_regresion_colegio_heterogeneidad.rds, ",
        "modelos completos en data/modelos_colegio_heterogeneidad.rds.")
