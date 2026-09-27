# =============================================================================
# 10_correlacion_ecologica_heterogeneidad.R
#
# Pieza 1 (de 2) para responder la pregunta de Julián (2026-09-26): ¿las
# CCAA donde MÁS ha subido la heterogeneidad de composición (HISEI del
# colegio, % de alumnado con otra lengua en casa) son las mismas donde MÁS
# ha subido la dispersión de resultados? Y lo mismo para la MEDIA.
#
# Es una correlación ECOLÓGICA (unidad de análisis = CCAA, no alumno) --
# mismo patrón y misma cautela ya usados en pisa-espana-ccaa/R/08_tendencias_ccaa.R
# (scatter cambio-segregación vs. cambio-puntuación, N=17-19, marcado
# explícitamente como ecológico, no implica nivel individual). La pieza 2
# (11_regresion_colegio_heterogeneidad.R) da el tratamiento con potencia
# real, a nivel de colegio.
#
# Ventana temporal: 2012->2025 (no 2009), aunque hisei_sd_media/pct_otra_lengua
# no dependen de public_private y en principio serían fiables desde 2009 --
# se mantiene 2012 como primer año por consistencia con el resto del
# proyecto (evita además la categoría combinada "Ceuta y Melilla" de 2009).
# Ceuta y Melilla se excluyen de esta correlación (mismo criterio que el
# grid de 03_grafico_media_dispersion.R): muestra muy pequeña, se comportan
# como outliers de alta varianza que distorsionan una correlación de ya
# solo ~17 puntos.
# =============================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
})

CCAA_EXCLUIR <- c("Ceuta", "Melilla", "Ceuta y Melilla")
ANIOS <- c(2012, 2025)

# --- 0. Dispersión y media de score_global por CCAA-año (fresco, domain-agnóstico) --

prep <- readRDS("data/student_esp_prep.rds") |>
  mutate(score_global = rowMeans(cbind(math, read, science), na.rm = TRUE))

weighted_sd <- function(x, w) {
  m <- weighted.mean(x, w)
  sqrt(sum(w * (x - m)^2) / sum(w))
}

ccaa_score_global <- prep |>
  filter(!is.na(score_global), !is.na(stu_wgt), year %in% ANIOS, !ccaa %in% CCAA_EXCLUIR) |>
  group_by(year, ccaa) |>
  summarise(media = weighted.mean(score_global, stu_wgt),
            sd = weighted_sd(score_global, stu_wgt), .groups = "drop")

# --- 1. Componer la tabla CCAA x año con las 5 variables de interés --------

composicion_ccaa <- readRDS("data/tbl_composicion_ccaa.rds") |>
  filter(year %in% ANIOS, !ccaa %in% CCAA_EXCLUIR) |>
  select(year, ccaa, hisei_sd_media, diversidad_media)

migracion_ccaa <- readRDS("data/tbl_ccaa_migracion_lengua.rds") |>
  filter(year %in% ANIOS, !ccaa %in% CCAA_EXCLUIR) |>
  select(year, ccaa, pct_migrante, pct_otra_lengua)

tabla_ccaa <- ccaa_score_global |>
  inner_join(composicion_ccaa, by = c("year", "ccaa")) |>
  inner_join(migracion_ccaa, by = c("year", "ccaa"))

n_ccaa_completas <- tabla_ccaa |> count(ccaa) |> filter(n == 2) |> nrow()
message("CCAA con dato completo en ambos años (", paste(ANIOS, collapse = " y "), "): ",
        n_ccaa_completas, " de ", n_distinct(tabla_ccaa$ccaa))
if (n_ccaa_completas < n_distinct(tabla_ccaa$ccaa)) {
  message("AVISO: alguna CCAA falta en uno de los dos años -- revisar antes de fiarse del cambio.")
}

# --- 2. Cambios 2012->2025 por CCAA ------------------------------------------

cambios_ccaa <- tabla_ccaa |>
  filter(ccaa %in% (tabla_ccaa |> count(ccaa) |> filter(n == 2) |> pull(ccaa))) |>
  pivot_wider(id_cols = ccaa,
              names_from = year,
              values_from = c(media, sd, hisei_sd_media, diversidad_media, pct_migrante, pct_otra_lengua)) |>
  mutate(
    delta_media = .data[[paste0("media_", ANIOS[2])]] - .data[[paste0("media_", ANIOS[1])]],
    delta_sd = .data[[paste0("sd_", ANIOS[2])]] - .data[[paste0("sd_", ANIOS[1])]],
    delta_hisei_sd = .data[[paste0("hisei_sd_media_", ANIOS[2])]] - .data[[paste0("hisei_sd_media_", ANIOS[1])]],
    delta_diversidad = .data[[paste0("diversidad_media_", ANIOS[2])]] - .data[[paste0("diversidad_media_", ANIOS[1])]],
    delta_pct_migrante = .data[[paste0("pct_migrante_", ANIOS[2])]] - .data[[paste0("pct_migrante_", ANIOS[1])]],
    delta_pct_otra_lengua = .data[[paste0("pct_otra_lengua_", ANIOS[2])]] - .data[[paste0("pct_otra_lengua_", ANIOS[1])]]
  )

saveRDS(cambios_ccaa, "data/tbl_cambios_ccaa_2012_2025.rds")

# --- 3. Correlaciones (Pearson, con IC) --------------------------------------
# Cada fila es una hipótesis de mecanismo distinta -- se listan todas juntas
# para poder comparar magnitudes, no para elegir la que "sale mejor".

pares <- tribble(
  ~x_var,               ~y_var,   ~x_label,                                    ~y_label,
  "delta_hisei_sd",      "delta_sd",    "Cambio en heterogeneidad de HISEI del colegio", "Cambio en dispersión de resultados (SD)",
  "delta_pct_otra_lengua","delta_sd",   "Cambio en % alumnado con otra lengua en casa",  "Cambio en dispersión de resultados (SD)",
  "delta_pct_migrante",   "delta_sd",   "Cambio en % alumnado migrante",                 "Cambio en dispersión de resultados (SD)",
  "delta_hisei_sd",      "delta_media", "Cambio en heterogeneidad de HISEI del colegio", "Cambio en la media de resultados",
  "delta_pct_otra_lengua","delta_media","Cambio en % alumnado con otra lengua en casa",  "Cambio en la media de resultados",
  "delta_pct_migrante",   "delta_media","Cambio en % alumnado migrante",                 "Cambio en la media de resultados"
)

correlaciones <- pares |>
  rowwise() |>
  mutate(
    ct = list(cor.test(cambios_ccaa[[x_var]], cambios_ccaa[[y_var]])),
    r = ct$estimate,
    ci_low = ct$conf.int[1],
    ci_high = ct$conf.int[2],
    p_value = ct$p.value,
    n = nrow(cambios_ccaa)
  ) |>
  ungroup() |>
  select(-ct)

saveRDS(correlaciones, "data/tbl_correlaciones_ecologicas_ccaa.rds")

message("\n=== Correlaciones ecológicas, cambio CCAA ", ANIOS[1], "->", ANIOS[2],
        " (N=", nrow(cambios_ccaa), " CCAA -- ecológico, no implica nivel individual) ===")
print(correlaciones |> mutate(across(c(r, ci_low, ci_high, p_value), ~round(.x, 3))) |>
        select(x_label, y_label, r, ci_low, ci_high, p_value, n), n = Inf)

# --- 4. Gráficos: scatter cambio-cambio, un panel por par -------------------
# Solo ~17 puntos -- etiqueta directa de CCAA en vez de leyenda (mismo
# criterio que el snapshot de 03_grafico_media_dispersion.R), sin color
# categórico (no hay una tercera dimensión que lo justifique aquí).

color_punto <- "#2a78d6"
gris_linea  <- "#c3c2b7"

tema_pisa <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e1e0d9", linewidth = 0.3),
    axis.line = element_line(color = "#c3c2b7", linewidth = 0.3),
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(color = "#52514e", size = 8.5)
  )

grafico_scatter <- function(x_var, y_var, x_label, y_label) {
  fila_r <- correlaciones |> filter(x_var == !!x_var, y_var == !!y_var)
  ggplot(cambios_ccaa, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_hline(yintercept = 0, color = gris_linea, linewidth = 0.3) +
    geom_vline(xintercept = 0, color = gris_linea, linewidth = 0.3) +
    geom_smooth(method = "lm", se = TRUE, color = color_punto, fill = color_punto,
                alpha = 0.10, linewidth = 0.7) +
    geom_point(color = color_punto, size = 2.3) +
    ggrepel::geom_text_repel(aes(label = ccaa), size = 2.7, color = "#52514e",
                              max.overlaps = 20, segment.size = 0.2) +
    labs(x = x_label, y = y_label,
         subtitle = sprintf("r = %.2f [%.2f, %.2f], N = %d CCAA (ecológico)",
                             fila_r$r, fila_r$ci_low, fila_r$ci_high, fila_r$n)) +
    tema_pisa
}

g1 <- grafico_scatter("delta_hisei_sd", "delta_sd",
                       "Cambio en heterogeneidad de HISEI del colegio (2012->2025)",
                       "Cambio en dispersión de resultados (SD, 2012->2025)")
g2 <- grafico_scatter("delta_pct_otra_lengua", "delta_sd",
                       "Cambio en % con otra lengua en casa (2012->2025, pp)",
                       "Cambio en dispersión de resultados (SD, 2012->2025)")
g3 <- grafico_scatter("delta_hisei_sd", "delta_media",
                       "Cambio en heterogeneidad de HISEI del colegio (2012->2025)",
                       "Cambio en la media de resultados (2012->2025)")
g4 <- grafico_scatter("delta_pct_otra_lengua", "delta_media",
                       "Cambio en % con otra lengua en casa (2012->2025, pp)",
                       "Cambio en la media de resultados (2012->2025)")

library(patchwork)
g_combinado <- (g1 | g2) / (g3 | g4) +
  plot_annotation(
    title = "¿Qué CCAA cambian juntas? Composición vs. heterogeneidad y media (2012-2025)",
    subtitle = paste0("Correlación ECOLÓGICA (unidad = CCAA, N~17): una asociación agregada no implica\n",
                       "el mecanismo a nivel individual -- la pieza con potencia real (nivel colegio) es el script 11."),
    theme = theme(plot.title = element_text(face = "bold", size = 13),
                  plot.subtitle = element_text(color = "#52514e", size = 8.5))
  )

if (!dir.exists("data")) dir.create("data")
ggsave("data/fig_correlacion_ecologica_ccaa.png", g_combinado, width = 12, height = 9.5, dpi = 300, bg = "white")
saveRDS(g_combinado, "data/fig_correlacion_ecologica_ccaa.rds")

message("\nListo. Figura guardada en data/fig_correlacion_ecologica_ccaa.png")
