# 04_heterogeneidad_composicion_colegio.R
#
# Heterogeneidad de la composición SOCIOECONÓMICA del colegio: no el nivel
# medio de HISEI/estatus_laboral del centro (ya conocido), sino su
# DISPERSIÓN interna -- ¿los colegios mezclan alumnado de distinto origen
# socioeconómico, o son cada vez más homogéneos por dentro? Y a nivel
# agregado: ¿ha aumentado la segregación ENTRE colegios por nivel
# socioeconómico?
#
# Dos métricas por colegio y año (mismo espíritu que 02_dispersion_ccaa_colegio.R,
# aplicado a composición socioeconómica en vez de a resultados):
#  1. SD ponderada de HISEI dentro del colegio (continua, en la escala HISEI).
#  2. Índice de diversidad de estatus_laboral dentro del colegio (categórica,
#     3 niveles baja/media/alta): entropía de Shannon normalizada a [0,1]
#     -- 0 = todo el alumnado en una sola categoría (máxima homogeneidad),
#     1 = tercio/tercio/tercio (máxima heterogeneidad posible con 3 niveles).
#     Se normaliza siempre por log(3), no por las categorías presentes en
#     cada colegio, para que sea comparable entre colegios con distinto
#     número de categorías representadas.
#
# Además, descomposición de varianza de HISEI (entre-colegios vs.
# dentro-colegio), paralela a la de 02_dispersion_ccaa_colegio.R para
# resultados: mide si la segregación ENTRE colegios por nivel
# socioeconómico ha cambiado, por separado de la heterogeneidad DENTRO de
# cada colegio.
#
# Ponderación: stu_wgt, misma convención que el resto del proyecto.
#
# AVISO DE COBERTURA: igual que en 02_dispersion_ccaa_colegio.R,
# public_private tiene ~76% de missing en 2009 -- los desgloses por sector
# de esa edición deben leerse con cautela extrema (ver README.md).

suppressMessages({
  library(dplyr)
  library(tidyr)
})

prep <- readRDS("data/student_esp_prep.rds")

# hisei llega como haven_labelled dentro de student_esp_prep.rds (columna
# cruda conservada tal cual desde 02_prepare_variables.R; solo la derivada
# estatus_laboral se convierte a numérica allí antes de categorizar). Sin
# convertir aquí, weighted.mean()/aritmética sobre hisei falla con "is not
# permitted" -- mismo patrón zap_labels()+as.numeric() ya usado en todo el
# proyecto para columnas HISEI/ESCS.
prep <- prep |> mutate(hisei = as.numeric(haven::zap_labels(hisei)))

# --- Funciones de ponderación (idénticas al resto del proyecto) ------------

weighted_var <- function(x, w) {
  m <- weighted.mean(x, w)
  sum(w * (x - m)^2) / sum(w)
}
weighted_sd <- function(x, w) sqrt(weighted_var(x, w))

shannon_entropy_norm <- function(categoria, w) {
  ok <- !is.na(categoria) & !is.na(w)
  categoria <- categoria[ok]; w <- w[ok]
  if (length(categoria) == 0) return(NA_real_)
  props <- tapply(w, categoria, sum) / sum(w)
  props <- props[!is.na(props) & props > 0]
  h <- -sum(props * log(props))
  h / log(3)  # normalizado por el máximo teórico con 3 categorías (baja/media/alta)
}

UMBRAL_MIN_ALUMNOS <- 10

# --- Aviso de cobertura de public_private (igual que en 02_...R) -----------

cobertura_pubpriv <- prep |>
  distinct(year, student_id, public_private) |>
  group_by(year) |>
  summarise(pct_na = round(mean(is.na(public_private)) * 100, 1), .groups = "drop")
message("Cobertura de public_private por edición (% missing):")
print(cobertura_pubpriv)

# --- 1. Composición socioeconómica por colegio: SD de HISEI + diversidad ---

school_hisei_sd <- prep |>
  filter(!is.na(hisei), !is.na(stu_wgt)) |>
  group_by(year, ccaa, school_id, global_school_id, public_private) |>
  filter(n() >= UMBRAL_MIN_ALUMNOS) |>
  summarise(n_alumnos = n(),
            hisei_media = weighted.mean(hisei, stu_wgt),
            hisei_sd = weighted_sd(hisei, stu_wgt),
            .groups = "drop")

school_estatus_div <- prep |>
  filter(!is.na(estatus_laboral), !is.na(stu_wgt)) |>
  group_by(year, ccaa, school_id, global_school_id, public_private) |>
  filter(n() >= UMBRAL_MIN_ALUMNOS) |>
  summarise(n_alumnos = n(),
            pct_baja = sum(stu_wgt[estatus_laboral == "baja"]) / sum(stu_wgt) * 100,
            pct_media = sum(stu_wgt[estatus_laboral == "media"]) / sum(stu_wgt) * 100,
            pct_alta = sum(stu_wgt[estatus_laboral == "alta"]) / sum(stu_wgt) * 100,
            diversidad_estatus = shannon_entropy_norm(estatus_laboral, stu_wgt),
            .groups = "drop")

colegio_composicion <- school_hisei_sd |>
  inner_join(
    school_estatus_div |> select(year, global_school_id, pct_baja, pct_media, pct_alta, diversidad_estatus),
    by = c("year", "global_school_id")
  )

saveRDS(colegio_composicion, "data/tbl_colegio_composicion_socioeconomica.rds")

# --- 2. Agregados nacionales y por sector, por año --------------------------
# Media entre colegios de las dos métricas, ponderada por el peso total del
# colegio (mismo criterio que la agregación entre-colegios de 02_...R).

composicion_nacional <- colegio_composicion |>
  group_by(year) |>
  summarise(
    n_colegios = n(),
    hisei_sd_media = weighted.mean(hisei_sd, n_alumnos),
    diversidad_media = weighted.mean(diversidad_estatus, n_alumnos),
    .groups = "drop"
  )

composicion_sector <- colegio_composicion |>
  filter(!is.na(public_private)) |>
  group_by(year, public_private) |>
  summarise(
    n_colegios = n(),
    hisei_sd_media = weighted.mean(hisei_sd, n_alumnos),
    diversidad_media = weighted.mean(diversidad_estatus, n_alumnos),
    .groups = "drop"
  )

composicion_ccaa <- colegio_composicion |>
  group_by(year, ccaa) |>
  filter(n() >= 10) |>
  summarise(
    n_colegios = n(),
    hisei_sd_media = weighted.mean(hisei_sd, n_alumnos),
    diversidad_media = weighted.mean(diversidad_estatus, n_alumnos),
    .groups = "drop"
  )

saveRDS(composicion_nacional, "data/tbl_composicion_nacional.rds")
saveRDS(composicion_sector, "data/tbl_composicion_sector.rds")
saveRDS(composicion_ccaa, "data/tbl_composicion_ccaa.rds")

# --- 3. Composición nacional de referencia (techo teórico de diversidad) ---
# Si la población nacional ya está desequilibrada entre baja/media/alta,
# ningún colegio puede alcanzar diversidad = 1 aunque mezcle perfectamente
# a los alumnos disponibles -- esto da el contexto para interpretar el
# índice de diversidad por colegio.

composicion_nacional_referencia <- prep |>
  filter(!is.na(estatus_laboral), !is.na(stu_wgt)) |>
  group_by(year) |>
  summarise(
    pct_baja = sum(stu_wgt[estatus_laboral == "baja"]) / sum(stu_wgt) * 100,
    pct_media = sum(stu_wgt[estatus_laboral == "media"]) / sum(stu_wgt) * 100,
    pct_alta = sum(stu_wgt[estatus_laboral == "alta"]) / sum(stu_wgt) * 100,
    diversidad_techo = shannon_entropy_norm(estatus_laboral, stu_wgt),
    .groups = "drop"
  )
saveRDS(composicion_nacional_referencia, "data/tbl_composicion_nacional_referencia.rds")

# --- 4. Descomposición de varianza de HISEI: ¿segregación ENTRE colegios? --
# Ley de la varianza total, igual que para resultados en 02_...R: cuánta
# varianza de HISEI está entre colegios (segregación socioeconómica de la
# red escolar) frente a dentro de cada colegio (mezcla interna).

descomposicion_hisei <- prep |>
  filter(!is.na(hisei), !is.na(stu_wgt), !is.na(global_school_id)) |>
  group_by(year) |>
  group_modify(function(df, ...) {
    total_var <- weighted_var(df$hisei, df$stu_wgt)
    sch <- df |>
      group_by(global_school_id) |>
      summarise(n = n(), w_total = sum(stu_wgt), m = weighted.mean(hisei, stu_wgt),
                v = weighted_var(hisei, stu_wgt), .groups = "drop") |>
      filter(n >= UMBRAL_MIN_ALUMNOS)
    var_dentro <- weighted.mean(sch$v, sch$w_total)
    var_entre <- weighted_var(sch$m, sch$w_total)
    tibble(var_total = total_var, var_dentro_media = var_dentro,
           var_entre_colegios = var_entre,
           pct_entre_colegios = var_entre / (var_dentro + var_entre) * 100)
  }) |>
  ungroup()

saveRDS(descomposicion_hisei, "data/tbl_descomposicion_hisei.rds")

# --- 5. Media de HISEI por sector y brecha público-privado ------------------
#
# NOTA IMPORTANTE (2026-09-26, añadida tras revisar con Julián una aparente
# contradicción): la brecha público-privado de NIVEL (esta tabla) y la
# heterogeneidad INTERNA por sector (composicion_sector, más arriba) son dos
# cosas distintas que no tienen por qué moverse juntas -- que los privados
# se vuelvan más homogéneos por dentro (SD/diversidad interna más baja) NO
# implica que la segregación entre-colegios (descomposicion_hisei) suba,
# salvo que además sus medias se alejen de las de los públicos. Esta tabla
# es la que permite comprobarlo.
#
# AVISO DE FIABILIDAD 2009: el ~24% de estudiantes de 2009 con
# `public_private` informado da un reparto público/privado de ~50/50 --
# muy distinto del ~67/33 real (visible en 2012-2025, con cobertura
# >=95%). Ese subconjunto de 2009 NO es una muestra aleatoria (sesgada
# hacia privados) y no debe usarse como punto de partida de ninguna
# comparación de tendencia por sector -- usar 2012 como primer año fiable
# para cualquier lectura de brecha público-privado.

medias_sector_hisei <- prep |>
  filter(!is.na(hisei), !is.na(stu_wgt), !is.na(public_private)) |>
  group_by(year, public_private) |>
  summarise(media_hisei = weighted.mean(hisei, stu_wgt), n = n(), .groups = "drop") |>
  tidyr::pivot_wider(id_cols = year, names_from = public_private, values_from = media_hisei) |>
  mutate(brecha_privado_publico = privado - publico)

saveRDS(medias_sector_hisei, "data/tbl_brecha_hisei_sector.rds")

# --- Mensajes de verificación ------------------------------------------------

message("\nListo. Tablas de composición socioeconómica guardadas en data/.")
message("\n== Composición nacional de referencia (% baja/media/alta y techo de diversidad) ==")
print(composicion_nacional_referencia, n = Inf)
message("\n== Heterogeneidad de composición, nacional, por año ==")
print(composicion_nacional, n = Inf)
message("\n== Heterogeneidad de composición, por sector ==")
print(composicion_sector, n = Inf)
message("\n== Descomposición de varianza de HISEI (% entre-colegios) ==")
print(descomposicion_hisei, n = Inf)
message("\n== Brecha de nivel HISEI público-privado (usar 2012+ como fiable, ver nota) ==")
print(medias_sector_hisei, n = Inf)
