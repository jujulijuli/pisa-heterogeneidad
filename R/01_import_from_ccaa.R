# 01_import_from_ccaa.R
#
# Copia (no re-deriva) el dataset ya pooled/preparado de pisa-espana-ccaa
# como punto de partida de este proyecto. Volver a ejecutar este script
# cuando pisa-espana-ccaa incorpore una nueva edición o cambie su pipeline.
#
# Fuente: ../pisa-espana-ccaa/data/  (proyecto hermano, misma carpeta Paper/PISA)

source_dir <- "../pisa-espana-ccaa/data"
dest_dir <- "data"

files_to_copy <- c(
  "student_esp_pooled_extended.rds", # dataset pooled 2009-2025 (crudo, con hisei/ocod de validación ya retirados)
  "student_esp_prep.rds",            # tras 02_prepare_variables.R: estrato MAIHDA, estatus_laboral, escs_q, etc.
  "student_esp_long.rds"             # formato largo (una fila por dominio: math/read/science)
)

if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

for (f in files_to_copy) {
  src <- file.path(source_dir, f)
  dst <- file.path(dest_dir, f)
  if (!file.exists(src)) {
    stop("No encuentro '", src, "' -- ¿está pisa-espana-ccaa en la ruta esperada ",
         "(hermano de este proyecto, en la misma carpeta Paper/PISA)?")
  }
  file.copy(src, dst, overwrite = TRUE)
  message("Copiado: ", f, " (", round(file.size(dst) / 1024^2, 1), " MB)")
}

message("\nListo. ", length(files_to_copy), " ficheros copiados a '", dest_dir, "/'.")
