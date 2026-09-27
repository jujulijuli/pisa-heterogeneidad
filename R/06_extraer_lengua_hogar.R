# =============================================================================
# 06_extraer_lengua_hogar.R
#
# Punto 3 del alcance del proyecto ("migración ampliada"): extrae LANGN
# (lengua hablada en casa) de los ficheros CRUDOS -- no está en
# student_esp_prep.rds/student_esp_pooled_extended.rds, que vienen ya
# construidos de pisa-espana-ccaa y no la incluyen -- y la une al dataset
# base por (year, school_id, student_id).
#
# Decisión ya confirmada y documentada en README.md (2026-09-26): SOLO
# LANGN para esta primera fase (no COBN/región de origen, no años de
# residencia -- ver README para el porqué).
#
# Reutiliza patrones YA VALIDADOS contra datos reales en otros proyectos
# hermanos, no inventados aquí:
#  - `resolve_col()`/`resolve_col_optional()` insensible a mayúsculas para
#    el fichero histórico ancho: idéntico a R/01b_incorporate_historic_ccaa.R
#    de pisa-espana-ccaa (mismo fichero fuente).
#  - `collapse_lang_hogar()`: extensión de `collapse_lang()` de
#    pisa-bilingues-lengua/R/02_prepare_variables.R (incluye el fix ya
#    encontrado ahí: LANGN en 2025 trae el código ISO entre paréntesis,
#    "Spanish (spa)" en vez de "Spanish"). Esa función se validó contra el
#    fichero INTERNACIONAL de la OCDE (2018/2022/2025) con etiquetas en
#    inglés. AQUÍ el fichero de 2022 es el NACIONAL del INEE
#    (PISA2022_Estudiantes_Esp.sav), que podría traer las etiquetas en
#    ESPAÑOL -- no verificado todavía, así que se añaden también las
#    variantes en español y se imprime la tabla de frecuencias de la
#    etiqueta CRUDA por año (antes de colapsar) para poder comprobarlo a
#    simple vista al ejecutar esto. Si "Español"/"Euskera"/etc. aparecen
#    ahí sin colapsar (cayendo en "otra_lengua" con mucho volumen), avisa
#    y hay que ampliar los patrones.
#
# AVISO DE COBERTURA: LANGN está confirmado presente en las 5 ediciones
# (ver README.md, tabla de disponibilidad, verificado 2026-09-26 con
# `haven::read_sav(path, n_max = 0)`), así que no se espera ninguna
# edición completa sin dato -- pero la CALIDAD de la categorización sí
# depende de que las etiquetas de texto se reconozcan bien, de ahí los
# diagnósticos de abajo.
# =============================================================================

suppressMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
})

# --- 0. Rutas a los ficheros crudos (AJUSTAR si no coinciden) ---------------
path_historic <- "/Volumes/discojuli/PISA/pisa-espana-ccaa/data/ESP_PISA00_03_06_09_12_15.sav"
path_2022     <- "../pisa-espana-ccaa/data/PISA2022_Estudiantes_Esp.sav"
path_2025     <- "/Volumes/discojuli/PISA/dataintwww/CY09_MS_STU_PUF.sav"

for (p in c(path_historic, path_2022, path_2025)) {
  if (!file.exists(p)) stop("No encuentro: ", p, " -- ajusta la ruta arriba")
}

# --- 1. Función de categorización (español / cooficial / otra_lengua) ------
# Ver nota de cabecera: extiende collapse_lang() de pisa-bilingues-lengua
# con variantes en español, por si el fichero nacional de 2022 etiqueta en
# español en vez de en inglés.
idiomas_cooficiales_patterns <- c(
  "Basque", "Euskera", "Eusquera", "Euskara",
  "Catalan", "Catalán", "Catalan-Valencian-Balear",
  "Galician", "Gallego",
  "Valencian", "Valenciano",
  "Aranese", "Aranés", "Aranes"
)
espanol_patterns <- c("Spanish", "Castilian", "Español", "Espanol", "Castellano")

collapse_lang_hogar <- function(x) {
  # Quita el sufijo " (xxx)" de código ISO que trae LANGN en el fichero de
  # 2025 (p.ej. "Spanish (spa)") -- mismo fix ya encontrado en
  # pisa-bilingues-lengua, aplicado aquí de forma general por si aparece
  # en más de una edición.
  x_norm <- trimws(sub(" \\([a-zA-Z]+\\)$", "", x))
  # Etiquetas explícitas de no-respuesta -- encontrado en 2022 real
  # ("Missing" se colaba en "otra_lengua", inflando ese grupo con alumnado
  # sin dato en vez de alumnado que de verdad habla otra lengua en casa).
  # Comparación insensible a mayúsculas por si aparece "missing"/"MISSING"
  # en otra edición.
  patrones_missing <- c("Missing", "No Response", "Not Applicable", "N/A", "Invalid")
  es_missing <- toupper(x_norm) %in% toupper(patrones_missing)
  case_when(
    es_missing ~ NA_character_,
    x_norm %in% espanol_patterns ~ "espanol",
    x_norm %in% idiomas_cooficiales_patterns ~ "cooficial",
    !is.na(x_norm) & x_norm != "" ~ "otra_lengua",
    TRUE ~ NA_character_
  )
}

diagnostico_year <- function(df, yr) {
  message("\n-- Año ", yr, ": top 15 etiquetas crudas de lengua de casa --")
  tabla <- df |>
    filter(year == yr) |>
    count(lang_home_raw, lengua_hogar_3cat, sort = TRUE) |>
    slice_head(n = 15)
  print(tabla, n = 15)
  pct_na <- df |> filter(year == yr) |> summarise(p = mean(is.na(lengua_hogar_3cat))) |> pull(p)
  message("  % NA en lengua_hogar_3cat: ", scales::percent(pct_na, accuracy = 0.1))
}

# --- 2. Histórico (2009/2012/2015) ------------------------------------------
message("Leyendo fichero histórico (puede tardar, ~530MB, ~3.500 columnas)...")
raw_hist <- haven::read_sav(path_historic)

col_lookup_upper <- setNames(names(raw_hist), toupper(names(raw_hist)))
resolve_col <- function(prefixed_name) {
  actual <- col_lookup_upper[[toupper(prefixed_name)]]
  if (is.null(actual)) {
    stop("No encuentro la columna '", prefixed_name, "' en el fichero histórico")
  }
  actual
}
resolve_col_optional <- function(prefixed_name) {
  idx <- match(toupper(prefixed_name), names(col_lookup_upper))
  if (is.na(idx)) NA_character_ else unname(col_lookup_upper[idx])
}

# Mismos id/edición que R/01b_incorporate_historic_ccaa.R de pisa-espana-ccaa
# (ese script ya extrajo y validó estos mismos school_id/student_id -- aquí
# se reconstruyen igual para poder unir por (year, school_id, student_id)).
hist_specs <- list(
  list(prefix = "P09", year = 2009L, schoolid = "SCHOOLID_PISA", studid = "StIDStd",  cnt = NULL),
  list(prefix = "P12", year = 2012L, schoolid = "SCHOOLID_PISA", studid = "StIDStd",  cnt = NULL),
  list(prefix = "P15", year = 2015L, schoolid = "CNTSCHID",      studid = "CNTSTUID", cnt = "CNT")
)

extract_hist_lang <- function(spec) {
  col <- function(name) resolve_col(paste0(spec$prefix, "_", name))
  col_langn <- resolve_col_optional(paste0(spec$prefix, "_LANGN"))
  if (is.na(col_langn)) {
    warning("LANGN no encontrada para ", spec$year, " en el fichero histórico -- ",
            "contradice la comprobación de disponibilidad del README, revisar manualmente.")
    return(NULL)
  }
  row_ok <- if (!is.null(spec$cnt)) {
    as.character(raw_hist[[col(spec$cnt)]]) == "QES" & !is.na(raw_hist[[col(spec$cnt)]])
  } else {
    !is.na(raw_hist[[col(spec$studid)]])
  }
  tibble::tibble(
    year        = spec$year,
    school_id   = as.character(raw_hist[[col(spec$schoolid)]]),
    student_id  = as.character(raw_hist[[col(spec$studid)]]),
    lang_home_raw = as.character(haven::as_factor(raw_hist[[col_langn]]))
  ) |>
    filter(row_ok)
}

lengua_hist <- bind_rows(lapply(hist_specs, extract_hist_lang))
rm(raw_hist)  # fichero pesado, liberar memoria antes de leer los siguientes

# --- 3. 2022 (fichero nacional INEE) -----------------------------------------
message("Leyendo LANGN de 2022 (fichero nacional INEE)...")
raw_2022 <- haven::read_sav(path_2022, col_select = any_of(c("CNTSCHID", "CNTSTUID", "LANGN")))
stopifnot("LANGN no está en el fichero de 2022 -- contradice el README, revisar" = "LANGN" %in% names(raw_2022))
lengua_2022 <- tibble::tibble(
  year = 2022L,
  school_id = as.character(raw_2022$CNTSCHID),
  student_id = as.character(raw_2022$CNTSTUID),
  lang_home_raw = as.character(haven::as_factor(raw_2022$LANGN))
)
rm(raw_2022)

# --- 4. 2025 (fichero internacional OCDE, filtrado a España) ---------------
# Mismo filtro que R/01d_incorporate_pisa2025_ccaa.R de pisa-espana-ccaa
# (CNT == "ESP", el código, no la etiqueta "Spain" -- error real ya
# encontrado y corregido en pisa-bilingues-lengua, ver memoria del
# proyecto).
message("Leyendo LANGN de 2025 (fichero internacional OCDE, solo columnas necesarias)...")
raw_2025 <- haven::read_sav(path_2025, col_select = any_of(c("CNT", "CNTSCHID", "CNTSTUID", "LANGN")))
raw_2025 <- raw_2025 |> filter(as.character(CNT) == "ESP")
stopifnot("No hay filas ESP en el fichero de 2025 -- revisar filtro CNT" = nrow(raw_2025) > 0)
lengua_2025 <- tibble::tibble(
  year = 2025L,
  school_id = as.character(raw_2025$CNTSCHID),
  student_id = as.character(raw_2025$CNTSTUID),
  lang_home_raw = as.character(haven::as_factor(raw_2025$LANGN))
)
rm(raw_2025)

# --- 5. Combinar, categorizar, diagnosticar ----------------------------------
lengua_hogar <- bind_rows(lengua_hist, lengua_2022, lengua_2025) |>
  mutate(lengua_hogar_3cat = collapse_lang_hogar(lang_home_raw))

message("\n=== Diagnóstico: etiquetas crudas de lengua de casa por año ===")
message("(revisar que 'español'/'cooficial' de verdad capturan las etiquetas -- ",
        "si aparece 'Español'/'Euskera'/etc. sin colapsar, hay que ampliar los patrones)")
for (yr in sort(unique(lengua_hogar$year))) diagnostico_year(lengua_hogar, yr)

# --- 6. Unir con el dataset base (student_esp_prep.rds) ---------------------
prep <- readRDS("data/student_esp_prep.rds")

student_lengua <- prep |>
  left_join(
    lengua_hogar |> select(year, school_id, student_id, lang_home_raw, lengua_hogar_3cat),
    by = c("year", "school_id", "student_id")
  )

message("\n=== Cobertura del cruce (join) por año ===")
cobertura <- student_lengua |>
  group_by(year) |>
  summarise(
    n = n(),
    pct_con_dato = scales::percent(mean(!is.na(lengua_hogar_3cat)), accuracy = 0.1),
    .groups = "drop"
  )
print(cobertura, n = Inf)
message(
  "\nSi la cobertura de algún año es sorprendentemente baja (p.ej. <70% cuando ",
  "el README dice que LANGN cubre esa edición), lo más probable es un desajuste ",
  "de IDs (school_id/student_id) entre este script y student_esp_prep.rds, no ",
  "una ausencia real del dato -- avisar antes de seguir con el análisis."
)

saveRDS(student_lengua, "data/student_esp_lengua.rds")
saveRDS(lengua_hogar, "data/tbl_lengua_hogar_raw.rds")  # tabla intermedia, para inspección si algo falla

message("\nListo. Guardado data/student_esp_lengua.rds y data/tbl_lengua_hogar_raw.rds.")
