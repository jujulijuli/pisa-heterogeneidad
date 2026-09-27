# 05_grafico_composicion_sector.R
#
# Visualiza la "reconciliación" discutida el 2026-09-26: la heterogeneidad
# INTERNA por sector (público/privado) diverge con el tiempo, mientras que
# ni la brecha de NIVEL entre sectores ni la segregación ENTRE colegios
# suben de la misma forma. Cuatro paneles, misma rejilla temporal
# (2009-2025), para poder leer la historia completa de un vistazo:
#
#  A. SD interna de HISEI, por sector.
#  B. Diversidad de estatus_laboral (entropía normalizada), por sector.
#  C. Media de HISEI (nivel), por sector -- la brecha NO se amplía desde 2012.
#  D. % de varianza de HISEI que es entre-colegios (segregación) -- nacional,
#     no depende de public_private así que no lleva el aviso de 2009.
#
# AVISO 2009 (paneles A/B/C): el subconjunto de 2009 con public_private
# informado (~24% de la muestra) tiene un reparto público/privado de ~50/50,
# muy distinto del ~67/33 real visible desde 2012 -- no es una muestra
# aleatoria. Por eso el tramo 2009-2012 se dibuja punteado y el punto de
# 2009 hueco en A/B/C: la lectura fiable de la brecha por sector empieza en
# 2012, no en 2009 (ver nota en 04_heterogeneidad_composicion_colegio.R).
#
# Requiere haber ejecutado antes 04_heterogeneidad_composicion_colegio.R
# (usa sus tablas guardadas en data/).

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})

composicion_sector <- readRDS("data/tbl_composicion_sector.rds")
descomposicion_hisei <- readRDS("data/tbl_descomposicion_hisei.rds")

# Paleta y tema (misma convención que 02_.../03_...R)
color_publico  <- "#2a78d6"
color_privado  <- "#eb6834"
gris_linea     <- "#c3c2b7"

tema_pisa <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e1e0d9", linewidth = 0.3),
    axis.line = element_line(color = "#c3c2b7", linewidth = 0.3),
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(color = "#52514e", size = 8.5),
    legend.position = "top"
  )

# --- Helper: línea por sector con el tramo 2009-2012 punteado/hueco --------

linea_sector <- function(df, y_col, y_lab, titulo) {
  df <- df |> rename(y = all_of(y_col)) |> filter(!is.na(public_private)) |>
    mutate(public_private = factor(public_private, levels = c("publico", "privado")))
  puente <- df |> filter(year %in% c(2009, 2012))
  principal <- df |> filter(year >= 2012)
  punto_2009 <- df |> filter(year == 2009)

  ggplot(df, aes(x = year, y = y, color = public_private, group = public_private)) +
    geom_line(data = puente, linetype = "dotted", linewidth = 0.5, alpha = 0.6) +
    geom_line(data = principal, linewidth = 0.8) +
    geom_point(data = principal, size = 1.8) +
    geom_point(data = punto_2009, shape = 1, size = 2, stroke = 1) +
    scale_color_manual(values = c(publico = color_publico, privado = color_privado),
                        name = NULL, labels = c(publico = "Público", privado = "Privado")) +
    labs(title = titulo, subtitle = "2009 (círculo hueco, línea punteada): cobertura de sector no fiable ese año",
         x = NULL, y = y_lab) +
    tema_pisa
}

g_a <- linea_sector(composicion_sector, "hisei_sd_media", "SD de HISEI dentro del colegio",
                     "A. Heterogeneidad interna (SD de HISEI)")
g_b <- linea_sector(composicion_sector, "diversidad_media", "Índice de diversidad (0-1)",
                     "B. Heterogeneidad interna (diversidad de estatus)")

# --- Panel C: media de HISEI por sector (nivel) -----------------------------

medias_sector_long <- readRDS("data/tbl_brecha_hisei_sector.rds") |>
  select(year, publico, privado) |>
  pivot_longer(cols = c(publico, privado), names_to = "public_private", values_to = "y")

g_c <- linea_sector(medias_sector_long, "y", "Media de HISEI",
                     "C. Nivel medio de HISEI (la brecha no se amplía desde 2012)")

# --- Panel D: % de varianza entre-colegios (segregación), nacional ---------
# A diferencia de A/B/C, esta serie es nacional y no depende de
# public_private -- no hereda el problema de cobertura/fiabilidad de 2009
# (ver cabecera del script), así que NO lleva el tramo punteado ni el punto
# hueco de 2009: toda la serie se dibuja con el mismo estilo, para no sugerir
# visualmente una cautela que aquí no aplica.

g_d <- descomposicion_hisei |>
  ggplot(aes(x = year, y = pct_entre_colegios)) +
  geom_line(color = "#4a3aa7", linewidth = 0.8) +
  geom_point(color = "#4a3aa7", size = 2) +
  labs(title = "D. Segregación entre colegios (% varianza de HISEI)",
       subtitle = "Nacional -- no depende de public_private, sin problema de cobertura en 2009",
       x = NULL, y = "% de la varianza entre-colegios") +
  tema_pisa

# --- Combinar en una rejilla 2x2 con patchwork ------------------------------

g_combinado <- (g_a | g_b) / (g_c | g_d) +
  plot_annotation(
    title = "Heterogeneidad de composición socioeconómica: qué diverge y qué no (2009-2025)",
    subtitle = paste0("A y B divergen por sector (privados cada vez más homogéneos por dentro); ",
                       "C y D se mantienen relativamente planas desde 2012 --\n",
                       "la homogeneización interna de los privados no viene (todavía) acompañada ",
                       "de una brecha de nivel mayor ni de más segregación entre colegios."),
    theme = theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(color = "#52514e", size = 9.5))
  )

if (!dir.exists("data")) dir.create("data")
ggsave("data/fig_composicion_sector_reconciliacion.png", g_combinado,
       width = 12, height = 9.5, dpi = 300, bg = "white")
saveRDS(g_combinado, "data/fig_composicion_sector_reconciliacion.rds")

message("Listo. Figura combinada guardada en data/fig_composicion_sector_reconciliacion.png")
