# =============================================================================
# 07_migracion_lengua.R
#
# Migración ampliada, matizada por lengua de origen (punto 3 del alcance,
# ver README.md). Usa data/student_esp_lengua.rds (06_extraer_lengua_hogar.R).
#
# Dos preguntas:
#  1. ¿Cuánto ha cambiado, edición a edición, la composición lingüística del
#     alumnado migrante (1ª y 2ª generación)? -- % que habla en casa una
#     lengua distinta del español/cooficial, por año. Es un proxy de
#     heterogeneidad de origen (y, en 2ª generación, de asimilación
#     lingüística intergeneracional: nacidos en España de padres migrantes
#     que ya no hablan en casa la lengua de origen).
#  2. Dentro de cada generación migrante, ¿la lengua de casa añade matiz a
#     la brecha de puntuación más allá del estatus migratorio ya conocido?
#     -- comparar 1ª/2ª generación que habla español/cooficial en casa
#     frente a 1ª/2ª generación que habla otra lengua.
#
# Ponderación: stu_wgt + SE de Kish (weighted_mean_ci/weighted_prop_ci),
# misma convención que 05_segregacion_heterogeneidad.R y
# 08_tendencias_ccaa.R en pisa-espana-ccaa -- NO replicate weights/BRR-Fay.
# =============================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
})

student <- readRDS("data/student_esp_lengua.rds") |>
  mutate(score_global = rowMeans(cbind(math, read, science), na.rm = TRUE))

weighted_mean_ci <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  n_eff <- (sum(w))^2 / sum(w^2)
  m <- weighted.mean(x, w)
  v <- sum(w * (x - m)^2) / sum(w)
  se <- sqrt(v / n_eff)
  tibble(media = m, ci_low = m - 1.96 * se, ci_high = m + 1.96 * se, n = length(x))
}

weighted_prop_ci <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  n_eff <- (sum(w))^2 / sum(w^2)
  p <- weighted.mean(x, w)
  se <- sqrt(p * (1 - p) / n_eff)
  tibble(prop = p, ci_low = pmax(0, p - 1.96 * se), ci_high = pmin(1, p + 1.96 * se), n = length(x))
}

# --- 1. Composición lingüística del alumnado migrante, por año -------------
# Solo 1ª y 2ª generación (los nativos hablan casi universalmente
# español/cooficial en casa -- no es la comparación interesante aquí).

lengua_composicion_year <- student |>
  filter(immig %in% c("primera_gen", "segunda_gen"),
         !is.na(lengua_hogar_3cat), !is.na(stu_wgt)) |>
  mutate(otra_lengua = lengua_hogar_3cat == "otra_lengua") |>
  group_by(year, immig) |>
  group_modify(~ weighted_prop_ci(.x$otra_lengua, .x$stu_wgt)) |>
  ungroup()

saveRDS(lengua_composicion_year, "data/tbl_lengua_composicion_year.rds")

message("=== % que habla en casa una lengua distinta de español/cooficial, por año y generación ===")
print(lengua_composicion_year |> mutate(across(c(prop, ci_low, ci_high), ~round(.x * 100, 1))), n = Inf)

# --- 2. Cobertura de celdas year x immig x lengua ---------------------------
# Antes de fiarse de la brecha de puntuación por año, hay que ver si hay
# tamaño de celda suficiente -- avisa en vez de asumir.

celdas <- student |>
  filter(immig %in% c("primera_gen", "segunda_gen"), !is.na(lengua_hogar_3cat)) |>
  count(year, immig, lengua_hogar_3cat, name = "n") |>
  complete(year, immig, lengua_hogar_3cat, fill = list(n = 0))

saveRDS(celdas, "data/tbl_celdas_lengua_immig.rds")
message("\n=== Tamaño de celda (year x immig x lengua_hogar_3cat) -- celdas <30 marcadas ===")
print(celdas |> mutate(aviso = ifelse(n < 30, "<<< N BAJO", "")), n = Inf)

UMBRAL_MIN_CELDA <- 30

# --- 3. Brecha de puntuación, POOLED 2009-2025 -------------------------------
# Colapsando español+cooficial en un solo grupo de referencia ("lengua de
# casa ya coincide con el entorno educativo") frente a "otra_lengua", cruzado
# con generación migrante. Pooled (no por año) para tener tamaño de celda
# adecuado; el desglose por año es el bloque 4, con su propio aviso de N.

brecha_lengua_pooled <- student |>
  filter(!is.na(immig), !is.na(lengua_hogar_3cat), !is.na(stu_wgt), !is.na(score_global)) |>
  mutate(
    lengua_2cat = ifelse(lengua_hogar_3cat == "otra_lengua", "otra_lengua", "espanol_cooficial"),
    grupo = paste(immig, lengua_2cat, sep = " / ")
  ) |>
  group_by(grupo, immig, lengua_2cat) |>
  group_modify(~ weighted_mean_ci(.x$score_global, .x$stu_wgt)) |>
  ungroup()

# BUG real, encontrado revisando el gráfico 08 contra la ejecución de Julián
# (2026-09-26): `brecha_lengua_pooled` tiene DOS filas para "nativo" (una por
# lengua_2cat), así que `pull(media)` devolvía un vector de longitud 2, y
# `media - ref_nativo` más abajo lo reciclaba fila a fila -- las filas de
# "otra_lengua" acababan restando la media de "nativo / otra_lengua" (451) en
# vez de la referencia única de nativos, e inflaba artificialmente lo bien
# que le iba a 2ª generación/otra_lengua (brecha ~0 cuando en realidad debía
# rondar -37). La referencia "vs. nativos" tiene que ser UN solo número: la
# media de TODOS los nativos, sin desglosar por lengua de casa.
ref_nativo <- student |>
  filter(immig == "nativo", !is.na(stu_wgt), !is.na(score_global)) |>
  summarise(m = weighted.mean(score_global, stu_wgt)) |>
  pull(m)

brecha_lengua_pooled <- brecha_lengua_pooled |>
  mutate(brecha_vs_nativo = media - ref_nativo) |>
  arrange(immig, lengua_2cat)

saveRDS(brecha_lengua_pooled, "data/tbl_brecha_lengua_pooled.rds")

message("\n=== Puntuación media (pooled 2009-2025) por generación x lengua de casa, y brecha vs. nativos ===")
print(brecha_lengua_pooled |>
        mutate(across(c(media, ci_low, ci_high, brecha_vs_nativo), ~round(.x, 1))),
      n = Inf)

# --- 4. Brecha de puntuación por año (supplementario, N permitiendo) -------

brecha_lengua_year <- student |>
  filter(immig %in% c("primera_gen", "segunda_gen"),
         !is.na(lengua_hogar_3cat), !is.na(stu_wgt), !is.na(score_global)) |>
  mutate(lengua_2cat = ifelse(lengua_hogar_3cat == "otra_lengua", "otra_lengua", "espanol_cooficial")) |>
  group_by(year, immig, lengua_2cat) |>
  group_modify(~ weighted_mean_ci(.x$score_global, .x$stu_wgt)) |>
  ungroup() |>
  mutate(n_bajo = n < UMBRAL_MIN_CELDA)

saveRDS(brecha_lengua_year, "data/tbl_brecha_lengua_year.rds")

message("\n=== Puntuación por año x generación x lengua de casa (celdas con N<", UMBRAL_MIN_CELDA, " marcadas) ===")
print(brecha_lengua_year |> mutate(media = round(media, 1)), n = Inf)

n_bajo_pct <- scales::percent(mean(brecha_lengua_year$n_bajo), accuracy = 1)
message(
  "\n", n_bajo_pct, " de las celdas year x immig x lengua tienen N<", UMBRAL_MIN_CELDA,
  " -- la lectura por año debe tratarse como orientativa en esas celdas; ",
  "la comparación pooled (bloque 3) es la más fiable de las dos."
)

message("\nListo. Tablas guardadas en data/.")
