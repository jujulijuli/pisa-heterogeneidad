# pisa-heterogeneidad

Proyecto hermano de `pisa-espana-ccaa` (y por tanto primo de
`pisa-bilingues-lengua` / `pisa-maihda-trends`). Inicio: 2026-09-26.

## Alcance (acordado 2026-09-26)

Parte del descriptivo ya construido en `pisa-espana-ccaa` (serie 2009-2025:
P09/P12/P15 históricas + 2022 INEE + 2025 OCDE) y lo amplía en tres frentes,
los tres incluidos desde esta primera fase:

1. **Dispersión** de resultados (no solo medias) a nivel de CCAA y de
   colegio, en toda la serie.
2. **Migración ampliada**: matizada por lengua de origen y, para 1ª
   generación, por años de residencia.
3. **Heterogeneidad de la composición socioeconómica del colegio**: cómo ha
   evolucionado a nivel de centro (no solo el nivel medio de ESCS/HISEI del
   centro, sino su dispersión interna).

## Procedencia de los datos

`data/` se puebla ejecutando `R/01_import_from_ccaa.R`, que copia (no
re-deriva) los `.rds` ya construidos en `pisa-espana-ccaa/data/`:
`student_esp_pooled_extended.rds`, `student_esp_prep.rds`,
`student_esp_long.rds`. Si `pisa-espana-ccaa` incorpora una nueva edición o
cambia su pipeline, basta con volver a ejecutar ese script para refrescar la
copia local. No se duplican los `.sav` crudos ni los ficheros de modelo
(INLA) de `pisa-espana-ccaa`; este proyecto trabaja sobre el dataset ya
pooled/preparado.

Para las variables NUEVAS que `pisa-espana-ccaa` no extrae (lengua en casa,
país de nacimiento, edad de llegada), este proyecto lee directamente de los
mismos ficheros `.sav`/histórico y las une por (`year`, `school_id`,
`student_id`) al dataset base -- ver comprobación de disponibilidad abajo
antes de construir esa extracción.

## Comprobación de disponibilidad de datos: migración ampliada

Antes de diseñar el análisis de migración ampliada se comprobaron (2026-09-26,
metadatos únicamente, sin cargar los ficheros completos) las variables
relevantes en el fichero histórico ancho (`ESP_PISA00_03_06_09_12_15.sav`,
3470 columnas) y en los ficheros 2022/2025:

| Variable | 2009 | 2012 | 2015 | 2022 | 2025 | Uso previsto |
|---|---|---|---|---|---|---|
| `IMMIG` (nativo/2ª/1ª gen.) | Sí | Sí | Sí | Sí | Sí | Estatus migratorio base (ya se usa en `pisa-espana-ccaa`) |
| `COBN_S/M/F` (país de nacimiento alumno/madre/padre) | Sí | Sí | Sí | Sí | Sí (`COBN_S`, `COBN_P1/P2` en 2025 -- padres ya no distinguidos por sexo) | Región de origen (matiz adicional a IMMIG) |
| `LANGN` (lengua hablada en casa) | Sí | Sí | Sí | Sí | Sí | **Variable principal para "lengua de origen"** -- única con cobertura completa en toda la serie |
| `ST021Q01TA` (edad de llegada al país) | **No** | **No** | Sí | Sí | Sí | Años de residencia = edad actual − edad de llegada, **solo calculable para 1ª generación en 2015/2022/2025** |

**Limitación a documentar en el análisis**: los años de residencia de la
población de 1ª generación solo pueden calcularse para 2015, 2022 y 2025 (3
de las 5 ediciones); en 2009 y 2012 no existe la pregunta de edad de llegada
(`ST021Q01TA`) en el fichero histórico, así que ese componente del análisis
de migración tendrá necesariamente una serie temporal más corta que el resto
del proyecto. `LANGN` e `IMMIG`, en cambio, cubren las 5 ediciones sin
huecos.

`COBN_S` usa códigos de país de 3 dígitos (5 dígitos en P06, no usada aquí).
**Decisión (2026-09-26): no se usa para esta primera fase** -- se descarta la
recodificación a macrorregión (habría exigido construir una tabla de
correspondencia país→región inexistente por ahora); el matiz de origen se
limita a `LANGN`, que ya tiene cobertura completa.

**Decisión (2026-09-26) sobre años de residencia**: se excluye de esta
primera fase. `ST021Q01TA` solo existe en 2015/2022/2025; se deja fuera para
no fragmentar el análisis de migración en una sub-serie de 3 ediciones
frente a las 5 del resto del proyecto. Puede añadirse en una fase posterior
como análisis complementario explícitamente marcado como serie corta.

## Métricas de dispersión (decisión 2026-09-26)

Para resultados (a nivel de CCAA y de colegio) y para heterogeneidad de
composición socioeconómica del colegio, se usan dos métricas complementarias:

- **SD** (desviación típica): medida estándar, coherente con la
  descomposición de varianza (VPC) ya usada en los modelos MAIHDA de
  `pisa-espana-ccaa`.
- **P90-P10** (rango entre percentil 90 y percentil 10): más robusto a
  valores atípicos que el rango completo, y más interpretable en el texto
  ("diferencia entre el alumnado de rendimiento/composición alta y bajo")
  que la varianza.

## Migración ampliada: lengua de origen (scripts 06-08)

Implementa el punto 3 del alcance, con la decisión ya documentada arriba
(solo `LANGN`, sin años de residencia):

- `R/06_extraer_lengua_hogar.R`: extrae `LANGN` de los ficheros CRUDOS
  (histórico ancho 2009/2012/2015, `PISA2022_Estudiantes_Esp.sav`,
  `CY09_MS_STU_PUF.sav` filtrado a España para 2025 -- `student_esp_prep.rds`
  NO trae esta variable, hay que ir a la fuente). La categoriza en 3 grupos
  (`lengua_hogar_3cat`): español, cooficial (euskera/catalán/gallego/
  valenciano/aranés), otra lengua -- reutiliza `collapse_lang()` de
  `pisa-bilingues-lengua` (incluye el fix ya encontrado ahí para el código
  ISO entre paréntesis que trae LANGN en 2025), ampliada con variantes en
  español por si el fichero nacional de 2022 etiqueta distinto del
  internacional. Imprime la tabla de etiquetas crudas por año al ejecutarse
  -- revisar esa tabla antes de confiar en la categorización, no se ha
  podido verificar contra los ficheros reales desde aquí (demasiado
  pesados para el entorno cloud). Guarda `data/student_esp_lengua.rds`
  (dataset base + lengua de casa) y `data/tbl_lengua_hogar_raw.rds`
  (tabla intermedia, para depurar si el cruce falla).
- `R/07_migracion_lengua.R`: (a) tendencia 2009-2025 de qué % del alumnado
  de 1ª/2ª generación habla en casa una lengua distinta de español/
  cooficial; (b) brecha de puntuación vs. nativos, por generación x lengua
  de casa (pooled 2009-2025, con desglose por año supletorio y aviso de
  tamaño de celda <30). Pregunta que responde: ¿la lengua de casa añade
  matiz a la brecha ya conocida por generación migrante, o la generación
  ya lo explica todo?
- `R/08_grafico_lengua_migracion.R`: figura de 2 paneles (tendencia de
  composición lingüística + brecha por generación x lengua), guardada en
  `data/fig_lengua_migracion.png`.

Pendiente: que Julián ejecute 06 (necesita los ficheros crudos, no
disponibles desde el entorno cloud) y confirme la tabla de diagnóstico de
etiquetas antes de dar por buena la categorización; 07 y 08 dependen de su
salida (`data/student_esp_lengua.rds`).

## ¿Qué explica la evolución de heterogeneidad/media? (scripts 09-12)

Julián preguntó (2026-09-26): la heterogeneidad y la media han evolucionado
en las distintas CCAA/colegios -- ¿en qué medida lo explican los hallazgos
de migración/lengua (06-08), u otros elementos de composición
socioeconómica (HISEI, del proyecto `pisa-espana-ccaa`)? Se abordó en 3
piezas; aquí están las piezas 1 y 2 (la 3 -- extender el modelo INLA
jerárquico de `pisa-espana-ccaa/R/09_modelo_covariables_colegio.R` -- se
aborda aparte, en ese proyecto).

- `R/09_composicion_colegio_migracion_lengua.R`: agrega
  `data/student_esp_lengua.rds` (06) a nivel de COLEGIO y de CCAA (año x
  unidad) -- hasta ahora la composición migrante/lingüística solo estaba a
  nivel nacional x año x generación. Calcula `pct_migrante` (% de todo el
  alumnado 1ª/2ª gen.), `pct_otra_lengua` (% de todo el alumnado con otra
  lengua en casa -- el indicador real de heterogeneidad lingüística del
  aula) y `pct_otra_lengua_migrantes` (asimilación dentro de migrantes).
  Umbral de 10 alumnos por colegio-año. Guarda
  `data/tbl_colegio_migracion_lengua.rds` y
  `data/tbl_ccaa_migracion_lengua.rds`.
- `R/10_correlacion_ecologica_heterogeneidad.R` (Pieza 1): correlación
  ECOLÓGICA a nivel de CCAA (N~17, 2012->2025, excluyendo Ceuta/Melilla) --
  ¿las CCAA donde más sube la heterogeneidad de HISEI del colegio o el %
  de otra lengua son las mismas donde más sube la dispersión de
  resultados (o la media)? 6 correlaciones de Pearson (3 predictores x 2
  desenlaces) con IC, más un scatter de 4 paneles con etiquetas directas
  de CCAA. Explícitamente marcado como ecológico -- no implica mecanismo a
  nivel individual. Guarda `data/tbl_cambios_ccaa_2012_2025.rds`,
  `data/tbl_correlaciones_ecologicas_ccaa.rds`,
  `data/fig_correlacion_ecologica_ccaa.png`.
- `R/11_regresion_colegio_heterogeneidad.R` (Pieza 2, la de potencia
  real): panel a nivel de COLEGIO-año (miles de unidades en vez de ~17).
  Compara un modelo `lm()` ponderado base (año + titularidad) frente a uno
  con composición añadida (+ `pct_migrante` + `pct_otra_lengua` +
  `hisei_sd`), para dos desenlaces: heterogeneidad dentro del colegio (SD)
  y media del colegio. Reporta el incremento de R² y los coeficientes con
  error robusto agrupado por CCAA (sandwich CR1, adaptado de
  `cluster_robust_ci()` en `pisa-espana-ccaa/06_exposicion_digital.R`).
  Deliberadamente NO es jerárquico bayesiano -- es la primera pasada
  rápida; el tratamiento riguroso es la pieza 3 (INLA, aparte). Guarda
  `data/tbl_colegio_completo_heterogeneidad.rds`,
  `data/tbl_regresion_colegio_heterogeneidad.rds`,
  `data/modelos_colegio_heterogeneidad.rds`.
- `R/12_grafico_regresion_colegio.R`: gráfico tipo forest-plot de los 3
  coeficientes de la pieza 2, un panel por desenlace, con el R² base->
  completo como subtítulo. Guarda
  `data/fig_regresion_colegio_heterogeneidad.png`.

Orden de ejecución: 06 (ya ejecutado con datos reales) -> 09 -> 10 -> 11 ->
12. **Importante**: 09-12 solo se han probado con datos de lengua
FICTICIOS (generados al azar) para verificar que el pipeline corre sin
errores -- los números que salgan de esta prueba no significan nada. Hay
que ejecutarlos con los datos reales (tras 06) para que las cifras y
gráficos sean válidos.

Pieza 3 (extender el INLA jerárquico con estos mismos hallazgos) está en
`pisa-espana-ccaa/R/09_modelo_covariables_colegio.R`, no aquí -- ver el
bloque M6 añadido a ese script (2026-09-26). Ese bloque M6 depende de que
este proyecto (`06` + `09`) ya se haya ejecutado con datos REALES --
`tbl_colegio_migracion_lengua.rds` es justo el fichero que M6 necesita.

## Estado

En construcción. Estructura del libro Quarto (`_quarto.yml`) contiene por
ahora solo `index.qmd`; los capítulos de dispersión, migración ampliada y
heterogeneidad de composición se añadirán a medida que se construyan los
scripts en `R/`.
