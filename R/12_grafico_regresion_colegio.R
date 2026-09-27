# =============================================================================
# 12_grafico_regresion_colegio.R
#
# Visualiza data/tbl_regresion_colegio_heterogeneidad.rds (11_regresion_...R):
# coeficientes (con IC robusto agrupado por CCAA) de composición migrante/
# lingüística y heterogeneidad de HISEI sobre (A) la heterogeneidad dentro
# del colegio y (B) su media -- con el R2 incremental como subtítulo, para
# leer de un vistazo cuánto explican estas variables más allá de año y
# titularidad.
#
# Nota de escala: los tres coeficientes de cada panel NO son comparables
# entre sí en magnitud (pct_migrante/pct_otra_lengua están en puntos
# porcentuales 0-100, hisei_sd está en la escala HISEI ~0-30) -- el gráfico
# los pone en paneles separados por variable dependiente pero NO los ordena
# por "importancia" a partir del tamaño del coeficiente bruto; eso exigiría
# estandarizar, que aquí se evita a propósito para mantener las unidades
# interpretables (puntos PISA por punto porcentual / por punto de HISEI).
# =============================================================================

suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
})

resultados <- readRDS("data/tbl_regresion_colegio_heterogeneidad.rds")

etiquetas_term <- c(
  pct_migrante = "% alumnado migrante\n(por punto porcentual)",
  pct_otra_lengua = "% con otra lengua en casa\n(por punto porcentual)",
  hisei_sd = "Heterogeneidad de HISEI del colegio\n(por punto de SD)"
)

color_sd <- "#4a3aa7"
color_media <- "#1baf7a"
gris_linea <- "#c3c2b7"

tema_pisa <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e1e0d9", linewidth = 0.3),
    axis.line = element_line(color = "#c3c2b7", linewidth = 0.3),
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(color = "#52514e", size = 8.5)
  )

grafico_coefs <- function(y_filtro, color, titulo) {
  df <- resultados |> filter(y == y_filtro) |>
    mutate(term_label = factor(etiquetas_term[term], levels = rev(etiquetas_term)))
  r2_base <- unique(df$r2_base); r2_completo <- unique(df$r2_completo)
  ggplot(df, aes(x = estimate, y = term_label)) +
    geom_vline(xintercept = 0, color = gris_linea, linewidth = 0.4) +
    geom_errorbar(aes(xmin = ci_low, xmax = ci_high), color = color,
                  orientation = "y", width = 0.15, linewidth = 0.7) +
    geom_point(color = color, size = 3) +
    labs(title = titulo, x = "Coeficiente (puntos PISA por unidad, IC 95% robusto por CCAA)", y = NULL,
         subtitle = sprintf("R² año+titularidad: %.3f -> con composición: %.3f (+%.3f)",
                             r2_base, r2_completo, r2_completo - r2_base)) +
    tema_pisa
}

g_sd <- grafico_coefs("sd_dentro (heterogeneidad)", color_sd,
                       "A. Heterogeneidad dentro del colegio (SD)")
g_media <- grafico_coefs("media_colegio (nivel)", color_media,
                          "B. Media del colegio")

g_combinado <- g_sd / g_media +
  plot_annotation(
    title = "¿Qué explica la composición? Modelo a nivel de colegio (panel 2009-2025)",
    subtitle = paste0("N = ", format(nrow(readRDS("data/tbl_colegio_completo_heterogeneidad.rds")), big.mark = ".", decimal.mark = ","),
                       " colegios-año, 19 CCAA como clusters -- exploratorio (lm ponderado + SE robusto por CCAA),\n",
                       "no sustituye al tratamiento jerárquico bayesiano de pisa-espana-ccaa."),
    theme = theme(plot.title = element_text(face = "bold", size = 13),
                  plot.subtitle = element_text(color = "#52514e", size = 8.5))
  )

if (!dir.exists("data")) dir.create("data")
ggsave("data/fig_regresion_colegio_heterogeneidad.png", g_combinado, width = 9, height = 8.5, dpi = 300, bg = "white")
saveRDS(g_combinado, "data/fig_regresion_colegio_heterogeneidad.rds")

message("Listo. Figura guardada en data/fig_regresion_colegio_heterogeneidad.png")
