# =============================================================================
# 09_composicion_colegio_migracion_lengua.R
#
# Pieza de datos que faltaba para responder la pregunta de Julián (2026-09-26)
# "¿en qué medida se explica la evolución de heterogeneidad/media por
# migración u otros elementos de composición socioeconómica?": hasta ahora
# la composición migrante/lingüística (06/07) solo estaba agregada a nivel
# nacional x año x generación -- aquí se agrega a nivel de COLEGIO y de CCAA
# (año x unidad), que es el nivel que hace falta para cruzarla con la
# heterogeneidad de HISEI (04) y la dispersión de resultados (02).
#
# Dos indicadores de composición migrante/lingüística, ambos ponderados por
# stu_wgt:
#  - pct_migrante: % de TODO el alumnado que es 1ª o 2ª generación.
#  - pct_otra_lengua: % de TODO el alumnado que habla en casa una lengua
#    distinta de español/cooficial -- esto es lo que genera heterogeneidad
#    de facto en el aula (mezcla lingüística), a diferencia de
#    pct_otra_lengua_migrantes (más abajo), que es un indicador de
#    asimilación DENTRO del alumnado migrante, no de heterogeneidad del
#    colegio en su conjunto.
#  - pct_otra_lengua_migrantes: % de otra lengua SOLO entre migrantes (1ª+2ª
#    gen) -- NA si el colegio/CCAA-año no tiene alumnado migrante con dato.
# =============================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
})

student <- readRDS("data/student_esp_lengua.rds")

weighted_pct <- function(indicador, w) {
  ok <- !is.na(indicador) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(indicador[ok], w[ok]) * 100
}

resumen_composicion <- function(df) {
  df <- df |> filter(!is.na(stu_wgt))
  es_migrante <- df$immig %in% c("primera_gen", "segunda_gen")
  otra_lengua <- df$lengua_hogar_3cat == "otra_lengua"
  tibble(
    n_alumnos = nrow(df),
    pct_migrante = weighted_pct(es_migrante, df$stu_wgt),
    pct_otra_lengua = weighted_pct(otra_lengua, df$stu_wgt),
    pct_otra_lengua_migrantes = weighted_pct(otra_lengua[es_migrante], df$stu_wgt[es_migrante])
  )
}

# --- 1. Nivel colegio ---------------------------------------------------------

UMBRAL_MIN_ALUMNOS <- 10

colegio_migracion_lengua <- student |>
  filter(!is.na(global_school_id)) |>
  group_by(year, ccaa, global_school_id, public_private) |>
  filter(n() >= UMBRAL_MIN_ALUMNOS) |>
  group_modify(~ resumen_composicion(.x)) |>
  ungroup()

saveRDS(colegio_migracion_lengua, "data/tbl_colegio_migracion_lengua.rds")

# --- 2. Nivel CCAA (agregado directo de alumnos, no de colegios) ------------

ccaa_migracion_lengua <- student |>
  group_by(year, ccaa) |>
  group_modify(~ resumen_composicion(.x)) |>
  ungroup()

saveRDS(ccaa_migracion_lengua, "data/tbl_ccaa_migracion_lengua.rds")

message("Listo. Guardado data/tbl_colegio_migracion_lengua.rds y data/tbl_ccaa_migracion_lengua.rds.")
message("\n== Composición migrante/lingüística por CCAA -- último año disponible ==")
print(ccaa_migracion_lengua |> filter(year == max(year)) |> arrange(desc(pct_otra_lengua)), n = Inf)
