# =============================================================================
# 08_grafico_lengua_migracion.R
#
# Visualiza las dos tablas de 07_migracion_lengua.R en una figura de 2
# paneles:
#  A. Tendencia 2009-2025 de la composición lingüística del alumnado
#     migrante (% que habla en casa una lengua distinta de español/cooficial),
#     separado por 1ª y 2ª generación.
#  B. Brecha de puntuación vs. nativos (pooled 2009-2025), por generación x
#     lengua de casa -- para ver si la lengua de casa añade matiz a la
#     brecha ya conocida por generación migrante.
# =============================================================================

suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
})

lengua_composicion_year <- readRDS("data/tbl_lengua_composicion_year.rds")
brecha_lengua_pooled    <- readRDS("data/tbl_brecha_lengua_pooled.rds")

# Paleta (misma convención que el resto del proyecto)
color_primera  <- "#2a78d6"   # slot 1, azul
color_segunda  <- "#eb6834"   # slot 2, naranja
color_espcoof  <- "#1baf7a"   # slot 3, aqua -- "ya habla español/cooficial en casa"
color_otra     <- "#4a3aa7"   # slot 7, violeta -- "otra lengua en casa"
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

# --- Panel A: tendencia de composición lingüística --------------------------

g_a <- lengua_composicion_year |>
  mutate(immig_label = recode(immig, primera_gen = "1ª generación", segunda_gen = "2ª generación"),
         immig_label = factor(immig_label, levels = c("1ª generación", "2ª generación"))) |>
  ggplot(aes(x = year, y = prop, color = immig_label, group = immig_label)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = immig_label), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = c("1ª generación" = color_primera, "2ª generación" = color_segunda), name = NULL) +
  scale_fill_manual(values = c("1ª generación" = color_primera, "2ª generación" = color_segunda), guide = "none") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(title = "A. Lengua distinta de español/cooficial en casa, alumnado migrante",
       subtitle = "% por generación migrante y año (banda = IC 95%)",
       x = NULL, y = "% con otra lengua en casa") +
  tema_pisa

# --- Panel B: brecha de puntuación vs. nativos, por generación x lengua ----

brecha_plot <- brecha_lengua_pooled |>
  filter(immig != "nativo") |>
  mutate(
    immig_label = recode(immig, primera_gen = "1ª generación", segunda_gen = "2ª generación"),
    lengua_label = recode(lengua_2cat, espanol_cooficial = "Ya habla español/cooficial en casa",
                           otra_lengua = "Habla otra lengua en casa"),
    grupo_y = paste(immig_label, lengua_label, sep = "\n"),
    grupo_y = factor(grupo_y, levels = rev(unique(grupo_y)))
  )

g_b <- brecha_plot |>
  ggplot(aes(x = brecha_vs_nativo, y = grupo_y, color = lengua_label)) +
  geom_vline(xintercept = 0, color = gris_linea, linewidth = 0.4) +
  geom_errorbar(aes(xmin = ci_low - (media - brecha_vs_nativo), xmax = ci_high - (media - brecha_vs_nativo)),
                orientation = "y", width = 0.15, linewidth = 0.6) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Ya habla español/cooficial en casa" = color_espcoof,
                                 "Habla otra lengua en casa" = color_otra), name = NULL) +
  labs(title = "B. Brecha de puntuación vs. nativos (pooled 2009-2025)",
       subtitle = "Por generación migrante x lengua de casa -- 0 = puntuación media de nativos",
       x = "Puntos PISA vs. nativos", y = NULL) +
  tema_pisa +
  theme(legend.position = "top", axis.text.y = element_text(size = 8.5))

# --- Combinar -----------------------------------------------------------------

g_combinado <- g_a / g_b +
  plot_annotation(
    title = "Migración matizada por lengua de origen",
    subtitle = paste0("La composición lingüística del alumnado migrante y si la lengua de casa ",
                       "añade matiz a la brecha ya conocida por generación."),
    theme = theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(color = "#52514e", size = 9.5))
  )

if (!dir.exists("data")) dir.create("data")
ggsave("data/fig_lengua_migracion.png", g_combinado, width = 9, height = 10, dpi = 300, bg = "white")
saveRDS(g_combinado, "data/fig_lengua_migracion.rds")

message("Listo. Figura guardada en data/fig_lengua_migracion.png")
