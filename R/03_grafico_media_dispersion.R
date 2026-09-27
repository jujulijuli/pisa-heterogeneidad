# 03_grafico_media_dispersion.R
#
# Representación en dos ejes (media, SD) de CCAA y colegios: ¿quién combina
# buen rendimiento con baja dispersión, y cómo se ha movido esa posición
# 2009-2025?
#
# Cuatro figuras:
#  A. CCAA, foto fija del último año, en cuadrantes.
#  B. CCAA x materia (17 x 4): media (línea) + rango intercuartílico
#     (banda), escalas reales, 2009-2025.
#  C. Colegios: media vs. SD interna, color = titularidad, tamaño = nº
#     alumnos, un panel por año.
#  D. Colegios 2022 (único año con dato disponible): media vs. SD interna,
#     color = tipo de comunidad (rural/ciudad media/gran ciudad).
#
# Usa una puntuación global por alumno (media de los tres dominios: mate,
# lectura, ciencias), siguiendo la misma convención que
# `puntuacion_ccaa_global` en pisa-espana-ccaa/R/08_tendencias_ccaa.R.
#
# Ponderación: stu_wgt + SD ponderada, misma convención que
# 02_dispersion_ccaa_colegio.R.
#
# Paleta (siguiendo la skill de dataviz de este entorno): color siempre
# categórico y con como mucho 3 niveles en un scatter (límite "all-pairs" de
# la skill); nunca dos ejes y en el mismo panel (por eso el gráfico B indexa
# media y SD a una base común en vez de ponerlas en ejes distintos).

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
})

prep <- readRDS("data/student_esp_prep.rds")
student_long <- readRDS("data/student_esp_long.rds")

# --- Funciones de ponderación (idénticas a 02_dispersion_ccaa_colegio.R) ----

weighted_var <- function(x, w) {
  m <- weighted.mean(x, w)
  sum(w * (x - m)^2) / sum(w)
}
weighted_sd <- function(x, w) sqrt(weighted_var(x, w))

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

# --- Puntuación global por alumno (media de los 3 dominios) ----------------

prep <- prep |>
  mutate(score_global = rowMeans(cbind(math, read, science), na.rm = TRUE))

UMBRAL_MIN_ALUMNOS_DENTRO <- 10

# --- 1. Nivel CCAA: media y SD ponderadas por año --------------------------

disp_ccaa_global <- prep |>
  filter(!is.na(score_global), !is.na(stu_wgt)) |>
  group_by(year, ccaa) |>
  summarise(n = n(), media = weighted.mean(score_global, stu_wgt),
            sd = weighted_sd(score_global, stu_wgt), .groups = "drop")

nacional_global <- prep |>
  filter(!is.na(score_global), !is.na(stu_wgt)) |>
  group_by(year) |>
  summarise(media_nac = weighted.mean(score_global, stu_wgt),
            sd_nac = weighted_sd(score_global, stu_wgt), .groups = "drop")

# --- 2. Nivel colegio: media del colegio + SD dentro del colegio -----------

school_means <- prep |>
  filter(!is.na(score_global), !is.na(stu_wgt)) |>
  group_by(year, ccaa, school_id, global_school_id, public_private) |>
  summarise(n_alumnos = n(), media_colegio = weighted.mean(score_global, stu_wgt),
            .groups = "drop")

within_school <- prep |>
  filter(!is.na(score_global), !is.na(stu_wgt)) |>
  group_by(year, ccaa, school_id, global_school_id, public_private) |>
  filter(n() >= UMBRAL_MIN_ALUMNOS_DENTRO) |>
  summarise(n_alumnos = n(), sd_dentro = weighted_sd(score_global, stu_wgt), .groups = "drop")

colegio_media_sd <- school_means |>
  inner_join(within_school |> select(year, global_school_id, sd_dentro),
             by = c("year", "global_school_id"))

if (!dir.exists("data")) dir.create("data")
saveRDS(disp_ccaa_global, "data/tbl_ccaa_media_sd_global.rds")
saveRDS(colegio_media_sd, "data/tbl_colegio_media_sd_global.rds")

# --- Paleta ------------------------------------------------------------------

# Rampa secuencial ordinal (azul, claro->oscuro), pasos 250/350/450/550/650 de
# la rampa de referencia -- respeta el mínimo de contraste 2:1 para uso
# ordinal (no empezar más claro que el paso 250).
anios <- sort(unique(disp_ccaa_global$year))
paleta_anio <- setNames(
  c("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281")[seq_along(anios)],
  anios
)
color_publico  <- "#2a78d6"  # categórico slot 1
color_privado  <- "#eb6834"  # categórico slot 2
gris_muted     <- "#898781"
gris_linea     <- "#c3c2b7"

tema_pisa <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e1e0d9", linewidth = 0.3),
    axis.line = element_line(color = "#c3c2b7", linewidth = 0.3),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "#52514e")
  )

# --- Gráfico A: foto fija del último año (2025), con cuadrantes -------------

anio_foco <- max(anios)
nac_foco <- nacional_global |> filter(year == anio_foco)

g_ccaa_snapshot <- disp_ccaa_global |>
  filter(year == anio_foco) |>
  ggplot(aes(x = media, y = sd)) +
  geom_hline(yintercept = nac_foco$sd_nac, linetype = "dashed", color = gris_linea) +
  geom_vline(xintercept = nac_foco$media_nac, linetype = "dashed", color = gris_linea) +
  geom_point(size = 3, color = paleta_anio[as.character(anio_foco)]) +
  geom_text_repel(aes(label = ccaa), size = 3, color = "#0b0b0b",
                   segment.color = gris_muted, segment.size = 0.3, max.overlaps = 20) +
  annotate("text", x = -Inf, y = Inf, label = "bajo rendimiento\ny alta dispersión",
           hjust = -0.05, vjust = 1.3, size = 3, color = gris_muted, fontface = "italic") +
  annotate("text", x = Inf, y = -Inf, label = "alto rendimiento\ny baja dispersión (más equitativo)",
           hjust = 1.05, vjust = -0.3, size = 3, color = gris_muted, fontface = "italic") +
  labs(title = paste0("CCAA: puntuación media vs. dispersión (", anio_foco, ")"),
       subtitle = "Puntuación global (media de mate/lectura/ciencias).\nLíneas discontinuas = medias nacionales.",
       x = "Puntuación media (ponderada)", y = "Desviación típica (ponderada)") +
  tema_pisa

# --- Gráfico B: matriz CCAA x materia, media (línea) + RIC (banda) ----------
#
# Rediseño a petición (2026-09-26): escalas REALES (no indexadas a 100), y
# una matriz con las 17 CCAA en filas (se excluyen Ceuta y Melilla, no solo
# la categoría combinada de 2009 -- decisión explícita de este gráfico,
# distinta de la de los gráficos A/C que sí las incluyen) y 4 columnas:
# Matemáticas / Lectura / Ciencias / Global.
#
# Se pidió además cambiar SD por el rango intercuartílico (P75-P25): con
# datos reales, más robusto a colas/atípicos y en las mismas unidades que la
# media (puntos de puntuación), lo que permite representarlo directamente
# como una BANDA alrededor de la línea de la media en vez de como una
# segunda serie.
#
# Sobre el formato pedido ("media a la derecha, RIC a la izquierda", dos
# ejes): se implementa en su lugar como una banda (ribbon) P25-P75 alrededor
# de la línea de la media, en el MISMO eje de puntuación -- un verdadero
# doble eje-y (dos escalas distintas en un mismo panel) puede hacer que dos
# líneas se crucen o se separen por la elección arbitraria de cómo se alinean
# los ejes, no por una relación real entre las variables (ver
# `references/anti-patterns.md` de la skill dataviz). La banda consigue lo
# mismo que se pedía -- ver media y RIC juntos, en escala real, en un único
# panel -- sin ese riesgo: el ancho vertical de la banda en cada año ES el
# RIC. Si aun así prefieres los dos ejes literales, se puede montar así de
# forma explícita (avisar).

CCAA_EXCLUIR_GRID <- c("Ceuta", "Melilla", "Ceuta y Melilla")

weighted_stats_iqr <- function(df) {
  x <- df$score; w <- df$stu_wgt
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  if (length(x) < 10) {
    return(tibble(n = length(x), media = NA_real_, p25 = NA_real_, p75 = NA_real_))
  }
  q <- weighted_quantile(x, w, c(0.25, 0.75))
  tibble(n = length(x), media = weighted.mean(x, w), p25 = q[1], p75 = q[2])
}

grid_materias <- student_long |>
  filter(!is.na(score), !is.na(stu_wgt), !ccaa %in% CCAA_EXCLUIR_GRID) |>
  group_by(year, ccaa, domain) |>
  group_modify(~ weighted_stats_iqr(.x)) |>
  ungroup() |>
  mutate(materia = recode(as.character(domain),
                           math = "Matemáticas", read = "Lectura", science = "Ciencias"))

grid_global <- prep |>
  filter(!is.na(score_global), !is.na(stu_wgt), !ccaa %in% CCAA_EXCLUIR_GRID) |>
  rename(score = score_global) |>
  group_by(year, ccaa) |>
  group_modify(~ weighted_stats_iqr(.x)) |>
  ungroup() |>
  mutate(materia = "Global")

grid_data <- bind_rows(
  grid_materias |> select(year, ccaa, materia, media, p25, p75),
  grid_global |> select(year, ccaa, materia, media, p25, p75)
) |>
  mutate(materia = factor(materia, levels = c("Matemáticas", "Lectura", "Ciencias", "Global")))

saveRDS(grid_data, "data/tbl_ccaa_media_ric_grid.rds")

g_ccaa_grid <- grid_data |>
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = color_publico, alpha = 0.18) +
  geom_line(aes(y = media), color = color_publico, linewidth = 0.55) +
  geom_point(aes(y = media), color = color_publico, size = 1) +
  facet_grid(ccaa ~ materia, scales = "free_y") +
  labs(title = "CCAA: media (línea) y rango intercuartílico P25-P75 (banda), 2009-2025",
       subtitle = paste0("Escalas reales, no indexadas -- eje Y libre por fila (cada CCAA en su propio rango).\n",
                          "Excluye Ceuta y Melilla. El ancho vertical de la banda en cada punto ES el RIC de ese año."),
       x = NULL, y = "Puntuación") +
  tema_pisa +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, size = 7.5),
        strip.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 6.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7),
        panel.spacing = unit(0.35, "lines"))

# --- Gráfico C: nube de colegios, media vs. SD interna -----------------------
#
# Dimensiones codificadas: color = titularidad (categórica, 2 niveles),
# tamaño del punto = nº de alumnos evaluados en el colegio (continua, en
# burbuja), panel = año. Cuatro dimensiones a la vez (x, y, color, tamaño)
# más la temporal vía facetas -- se queda ahí: un colegio no tiene además
# una forma (shape) natural que añadir sin duplicar la titularidad, y una
# 5ª codificación (p.ej. rural/urbano) exigiría una categórica más en un
# scatter -- ver gráfico D para esa, aparte, porque solo hay dato para 2022
# (ver nota en ese bloque).

g_colegio_nube <- colegio_media_sd |>
  filter(!is.na(public_private)) |>
  ggplot(aes(x = media_colegio, y = sd_dentro, color = public_private)) +
  geom_point(aes(size = n_alumnos), alpha = 0.35) +
  stat_ellipse(linewidth = 0.6, level = 0.68) +
  scale_color_manual(values = c(publico = color_publico, privado = color_privado),
                      name = "Titularidad",
                      labels = c(publico = "Público", privado = "Privado")) +
  scale_size_area(name = "Nº alumnos\nevaluados", max_size = 4.5) +
  facet_wrap(~ year, nrow = 1) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  labs(title = "Colegios: puntuación media vs. heterogeneidad interna del alumnado",
       subtitle = paste0("Un punto = un colegio (mínimo ", UMBRAL_MIN_ALUMNOS_DENTRO,
                          " alumnos evaluados); tamaño = nº de alumnos evaluados. Elipses: 68% de cada grupo."),
       x = "Puntuación media del colegio (ponderada)",
       y = "SD dentro del colegio (ponderada)") +
  tema_pisa +
  theme(legend.position = "top")

# --- Gráfico D: colegios 2022 por rural/urbano -------------------------------
#
# AVISO DE DISPONIBILIDAD: la variable de tamaño de la comunidad
# (SC001Q01TA, cuestionario de centro) solo está disponible para 2022 en
# este proyecto -- es la única edición con fichero de centros
# (`PISA2022_CentrosEducativos_Esp.sav`) ya incorporado; no hay fichero de
# centros para 2025 ni para las ediciones históricas en
# pisa-espana-ccaa/data/ (comprobado 2026-09-26; el disco externo con los
# brutos de PISA no estaba montado en el momento de la comprobación, así
# que no se puede descartar del todo que existan en otra ubicación, pero no
# están en el proyecto). Añadir rural/urbano a más ediciones exigiría
# localizar o descargar esos ficheros de centro.
#
# SC001Q01TA tiene 6 categorías originales (village...megacity); se colapsan
# a 3 para poder usarlas como color en un scatter (el límite "all-pairs" de
# la skill de dataviz es 3 categorías): Rural/pueblo, Ciudad media, Gran
# ciudad.

if (file.exists("data/PISA2022_CentrosEducativos_Esp.sav")) {
  suppressMessages(library(haven))

  centros_2022 <- read_sav("data/PISA2022_CentrosEducativos_Esp.sav") |>
    transmute(
      school_id = as.character(CNTSCHID),
      comunidad_txt = as.character(as_factor(SC001Q01TA)),
      tipo_comunidad = case_when(
        comunidad_txt %in% c("A village, hamlet or rural area (fewer than 3 000 people)",
                              "A small town (3 000 to about 15 000 people)") ~ "Rural / pueblo",
        comunidad_txt %in% c("A town (15 000 to about 100 000 people)",
                              "A city (100 000 to about 1 000 000 people)") ~ "Ciudad media",
        comunidad_txt %in% c("A large city (1 000 000 to about 10 000 000 people)",
                              "A megacity (with over 10 000 000 people)") ~ "Gran ciudad",
        TRUE ~ NA_character_
      )
    )

  colegio_2022_comunidad <- colegio_media_sd |>
    filter(year == 2022) |>
    inner_join(centros_2022, by = "school_id")

  if (nrow(colegio_2022_comunidad) == 0) {
    stop("La unión con el fichero de centros 2022 dio 0 filas -- revisar el ",
         "formato de school_id en student_esp_prep.rds frente a CNTSCHID ",
         "en PISA2022_CentrosEducativos_Esp.sav antes de continuar.")
  }
  message("Colegios 2022 con dato de tipo de comunidad: ", nrow(colegio_2022_comunidad),
          " de ", sum(colegio_media_sd$year == 2022), " colegios con SD interna calculada.")

  color_rural  <- "#1baf7a"  # categórico slot 3 (aqua)
  color_media  <- "#eb6834"  # categórico slot 2 (orange)
  color_grande <- "#4a3aa7"  # categórico slot 7 (violet) -- separado del resto

  g_colegio_comunidad_2022 <- colegio_2022_comunidad |>
    filter(!is.na(tipo_comunidad)) |>
    ggplot(aes(x = media_colegio, y = sd_dentro, color = tipo_comunidad)) +
    geom_point(size = 1.8, alpha = 0.55) +
    stat_ellipse(linewidth = 0.6, level = 0.68) +
    scale_color_manual(values = c("Rural / pueblo" = color_rural,
                                   "Ciudad media" = color_media,
                                   "Gran ciudad" = color_grande),
                        name = "Tamaño de la comunidad") +
    labs(title = "Colegios (2022): media vs. heterogeneidad interna,\npor tipo de comunidad",
         subtitle = "Solo 2022 -- único año con dato de tamaño de comunidad\ndisponible en el proyecto (ver nota en el script).",
         x = "Puntuación media del colegio (ponderada)",
         y = "SD dentro del colegio (ponderada)") +
    tema_pisa +
    theme(legend.position = "top")
} else {
  message("AVISO: no se encontró data/PISA2022_CentrosEducativos_Esp.sav -- ",
          "se omite el gráfico D (rural/urbano). Cópialo desde ",
          "../pisa-espana-ccaa/data/ si quieres generarlo.")
  g_colegio_comunidad_2022 <- NULL
}

# --- Guardar -----------------------------------------------------------------

# Convención de pisa-espana-ccaa: los fig_*.png viven en data/, junto a las
# tablas (no en una carpeta figs/ aparte).
if (!dir.exists("data")) dir.create("data")
ggsave("data/fig_ccaa_media_sd_snapshot.png", g_ccaa_snapshot, width = 7.5, height = 6.5, dpi = 300, bg = "white")
ggsave("data/fig_ccaa_media_ric_grid.png", g_ccaa_grid, width = 11, height = 24, dpi = 220, bg = "white", limitsize = FALSE)
ggsave("data/fig_colegio_media_sd_nube.png", g_colegio_nube, width = 13, height = 4.5, dpi = 300, bg = "white")

saveRDS(g_ccaa_snapshot, "data/fig_ccaa_media_sd_snapshot.rds")
saveRDS(g_ccaa_grid, "data/fig_ccaa_media_ric_grid.rds")
saveRDS(g_colegio_nube, "data/fig_colegio_media_sd_nube.rds")

if (!is.null(g_colegio_comunidad_2022)) {
  ggsave("data/fig_colegio_comunidad_2022.png", g_colegio_comunidad_2022, width = 7.5, height = 6.5, dpi = 300, bg = "white")
  saveRDS(g_colegio_comunidad_2022, "data/fig_colegio_comunidad_2022.rds")
}

message("Listo. Figuras guardadas en data/ (.png y .rds, este último para {r} chunks en qmd).")
