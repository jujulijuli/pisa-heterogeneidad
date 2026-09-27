# 02_dispersion_ccaa_colegio.R
#
# Dispersión (no solo medias) de resultados PISA a nivel de CCAA y de
# colegio, en toda la serie 2009-2025. Métricas: SD ponderada y rango
# P90-P10 ponderado (ver README.md, decisión 2026-09-26). Se desglosa
# también por titularidad (público/privado).
#
# AVISO DE COBERTURA: public_private tiene un ~76% de valores ausentes en
# 2009 (ver README.md) -- los desgloses por sector para 2009 deben
# interpretarse con cautela extrema (base efectiva ~24% de la muestra de
# esa edición). El script imprime esta cobertura al ejecutarse.
#
# Convención de ponderación: igual que en pisa-espana-ccaa (stu_wgt +
# aproximación de Kish para el error estándar de la media -- ver
# 08_tendencias_ccaa.R en el proyecto hermano), sin pesos replicados
# BRR-Fay (el proyecto hermano tampoco los usa; intsvy queda como
# referencia de validación, no como parte del pipeline).

suppressMessages({
  library(dplyr)
  library(tidyr)
})

student_long <- readRDS("data/student_esp_long.rds")

# --- Aviso de cobertura de public_private -----------------------------------

cobertura_pubpriv <- student_long |>
  distinct(year, student_id, public_private) |>
  group_by(year) |>
  summarise(pct_na = round(mean(is.na(public_private)) * 100, 1), .groups = "drop")
message("Cobertura de public_private por edición (% missing):")
print(cobertura_pubpriv)
if (any(cobertura_pubpriv$pct_na > 20)) {
  message("AVISO: al menos una edición (previsiblemente 2009) tiene una proporción ",
          "muy alta de public_private ausente -- ver README.md. Los desgloses por ",
          "titularidad para esa edición deben interpretarse con cautela extrema.")
}

# --- Funciones de ponderación ------------------------------------------------

weighted_var <- function(x, w) {
  m <- weighted.mean(x, w)
  sum(w * (x - m)^2) / sum(w)
}
weighted_sd <- function(x, w) sqrt(weighted_var(x, w))

# Cuantil ponderado por interpolación lineal sobre la función de distribución
# acumulada ponderada (sin dependencias externas: ni Hmisc ni survey).
weighted_quantile <- function(x, w, probs) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  o <- order(x)
  x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  vapply(probs, function(p) {
    idx <- which(cw >= p)[1]
    if (is.na(idx)) return(x[length(x)])
    if (idx == 1) return(x[1])
    x0 <- x[idx - 1]; x1 <- x[idx]
    cw0 <- cw[idx - 1]; cw1 <- cw[idx]
    if (cw1 == cw0) return(x1)
    x0 + (p - cw0) / (cw1 - cw0) * (x1 - x0)
  }, numeric(1))
}

dispersion_stats <- function(df, score_col = "score", w_col = "stu_wgt") {
  x <- df[[score_col]]; w <- df[[w_col]]
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  if (length(x) < 10) {
    return(tibble(n = length(x), media = NA_real_, sd = NA_real_,
                  p10 = NA_real_, p90 = NA_real_, p90_p10 = NA_real_))
  }
  q <- weighted_quantile(x, w, c(0.10, 0.90))
  tibble(n = length(x), media = weighted.mean(x, w), sd = weighted_sd(x, w),
         p10 = q[1], p90 = q[2], p90_p10 = q[2] - q[1])
}

# --- 1. Dispersión a nivel de CCAA (nacional + por sector) ------------------

disp_ccaa <- student_long |>
  filter(!is.na(score), !is.na(stu_wgt)) |>
  group_by(year, domain, ccaa) |>
  group_modify(~ dispersion_stats(.x)) |>
  ungroup()

disp_ccaa_sector <- student_long |>
  filter(!is.na(score), !is.na(stu_wgt), !is.na(public_private)) |>
  group_by(year, domain, ccaa, public_private) |>
  group_modify(~ dispersion_stats(.x)) |>
  ungroup()

disp_nacional <- student_long |>
  filter(!is.na(score), !is.na(stu_wgt)) |>
  group_by(year, domain) |>
  group_modify(~ dispersion_stats(.x)) |>
  ungroup()

disp_nacional_sector <- student_long |>
  filter(!is.na(score), !is.na(stu_wgt), !is.na(public_private)) |>
  group_by(year, domain, public_private) |>
  group_modify(~ dispersion_stats(.x)) |>
  ungroup()

# --- 2. Dispersión a nivel de colegio: entre-colegios y dentro-colegio ------

UMBRAL_MIN_ALUMNOS <- 5
UMBRAL_MIN_ALUMNOS_DENTRO <- 10

school_means <- student_long |>
  filter(!is.na(score), !is.na(stu_wgt)) |>
  group_by(year, domain, ccaa, global_school_id, public_private) |>
  summarise(n_alumnos = n(), w_total = sum(stu_wgt),
            media_colegio = weighted.mean(score, stu_wgt), .groups = "drop")

school_means_ok <- school_means |> filter(n_alumnos >= UMBRAL_MIN_ALUMNOS)

disp_entre_colegios_nacional <- school_means_ok |>
  group_by(year, domain) |>
  summarise(
    n_colegios = n(),
    sd_entre = weighted_sd(media_colegio, w_total),
    p10 = weighted_quantile(media_colegio, w_total, 0.10),
    p90 = weighted_quantile(media_colegio, w_total, 0.90),
    p90_p10 = p90 - p10,
    .groups = "drop"
  )

disp_entre_colegios_ccaa <- school_means_ok |>
  group_by(year, domain, ccaa) |>
  filter(n() >= 10) |>
  summarise(
    n_colegios = n(),
    sd_entre = weighted_sd(media_colegio, w_total),
    p90_p10 = weighted_quantile(media_colegio, w_total, 0.90) -
              weighted_quantile(media_colegio, w_total, 0.10),
    .groups = "drop"
  )

disp_entre_colegios_sector <- school_means_ok |>
  filter(!is.na(public_private)) |>
  group_by(year, domain, public_private) |>
  summarise(
    n_colegios = n(),
    sd_entre = weighted_sd(media_colegio, w_total),
    p90_p10 = weighted_quantile(media_colegio, w_total, 0.90) -
              weighted_quantile(media_colegio, w_total, 0.10),
    .groups = "drop"
  )

within_school <- student_long |>
  filter(!is.na(score), !is.na(stu_wgt)) |>
  group_by(year, domain, ccaa, global_school_id, public_private) |>
  filter(n() >= UMBRAL_MIN_ALUMNOS_DENTRO) |>
  summarise(n_alumnos = n(), sd_dentro = weighted_sd(score, stu_wgt), .groups = "drop")

disp_dentro_colegios_nacional <- within_school |>
  group_by(year, domain) |>
  summarise(n_colegios = n(),
            sd_dentro_media = weighted.mean(sd_dentro, n_alumnos), .groups = "drop")

disp_dentro_colegios_sector <- within_school |>
  filter(!is.na(public_private)) |>
  group_by(year, domain, public_private) |>
  summarise(n_colegios = n(),
            sd_dentro_media = weighted.mean(sd_dentro, n_alumnos), .groups = "drop")

# --- 3. Descomposición de varianza: ¿qué parte es entre-colegios? -----------
# Ley de la varianza total (aproximación descriptiva, ponderada por stu_wgt
# a nivel de alumno y por peso total del colegio a nivel de colegio; no
# sustituye al VPC de los modelos MAIHDA/INLA de pisa-espana-ccaa, es un
# complemento puramente descriptivo).

descomposicion_varianza <- student_long |>
  filter(!is.na(score), !is.na(stu_wgt), !is.na(global_school_id)) |>
  group_by(year, domain) |>
  group_modify(function(df, ...) {
    total_var <- weighted_var(df$score, df$stu_wgt)
    sch <- df |>
      group_by(global_school_id) |>
      summarise(n = n(), w_total = sum(stu_wgt), m = weighted.mean(score, stu_wgt),
                v = weighted_var(score, stu_wgt), .groups = "drop") |>
      filter(n >= UMBRAL_MIN_ALUMNOS_DENTRO)
    var_dentro <- weighted.mean(sch$v, sch$w_total)
    var_entre <- weighted_var(sch$m, sch$w_total)
    tibble(var_total = total_var, var_dentro_media = var_dentro,
           var_entre_colegios = var_entre,
           pct_entre_colegios = var_entre / (var_dentro + var_entre) * 100)
  }) |>
  ungroup()

# --- Guardar resultados ------------------------------------------------------

if (!dir.exists("data")) dir.create("data")
saveRDS(disp_ccaa, "data/tbl_dispersion_ccaa.rds")
saveRDS(disp_ccaa_sector, "data/tbl_dispersion_ccaa_sector.rds")
saveRDS(disp_nacional, "data/tbl_dispersion_nacional.rds")
saveRDS(disp_nacional_sector, "data/tbl_dispersion_nacional_sector.rds")
saveRDS(disp_entre_colegios_nacional, "data/tbl_dispersion_entre_colegios_nacional.rds")
saveRDS(disp_entre_colegios_ccaa, "data/tbl_dispersion_entre_colegios_ccaa.rds")
saveRDS(disp_entre_colegios_sector, "data/tbl_dispersion_entre_colegios_sector.rds")
saveRDS(disp_dentro_colegios_nacional, "data/tbl_dispersion_dentro_colegios_nacional.rds")
saveRDS(disp_dentro_colegios_sector, "data/tbl_dispersion_dentro_colegios_sector.rds")
saveRDS(descomposicion_varianza, "data/tbl_descomposicion_varianza.rds")

message("\nListo. Tablas de dispersión guardadas en data/.")
message("\n== Dispersión nacional por año/dominio ==")
print(disp_nacional, n = Inf)
message("\n== Descomposición de varianza (% entre-colegios) ==")
print(descomposicion_varianza, n = Inf)
message("\n== Dispersión entre-colegios, nacional ==")
print(disp_entre_colegios_nacional, n = Inf)
message("\n== Dispersión dentro-colegio (media), nacional ==")
print(disp_dentro_colegios_nacional, n = Inf)
