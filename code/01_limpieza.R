source("code/00_config.R")

# --- 1.1 INPC ---
inpc <- read_excel(file.path(base_dir, "inpc_mensual.xls"))
names(inpc) <- tolower(names(inpc))
inpc_anual <- inpc %>%
  mutate(anio = as.numeric(substr(as.character(fecha), 1, 4))) %>%
  group_by(anio) %>%
  summarise(indice = mean(inpc, na.rm = TRUE), .groups = "drop") %>%
  filter(anio %in% anios) %>%
  mutate(factor_deflacion = indice / indice[anio == 2024])

# --- 1.2 Funciones ---
leer <- function(a, tabla) {
  ruta <- file.path(base_dir, a, paste0(tabla, ".csv"))
  d <- read.csv(ruta, stringsAsFactors = FALSE)
  names(d) <- tolower(names(d))
  if(tabla == "viviendas") d <- d %>% select(folioviv, tipo_viv)
  d
}

recodif_educ <- function(x) {
  factor(case_when(
    x %in% c(1,2)      ~ "Sin instrucción",
    x %in% c(3,4)      ~ "Primaria",
    x %in% c(5,6)      ~ "Secundaria",
    x %in% c(7,8)      ~ "Media superior",
    x %in% c(9,10,11)  ~ "Superior",
    TRUE               ~ NA_character_
  ), levels = c("Sin instrucción","Primaria","Secundaria", "Media superior","Superior"))
}

# --- 1.3 Apilar los cinco años ---
lista <- list()
for(a in anios) {
  cat("Procesando", a, "...\n")
  conc <- leer(a, "concentradohogar")
  gast <- leer(a, "gastoshogar")
  viv  <- leer(a, "viviendas")
  
  claves <- if(a <= 2022) "L027" else c("093110","093121","093123")
  
  gast_jardin <- gast %>%
    filter(clave %in% claves) %>%
    group_by(folioviv, foliohog) %>%
    summarise(gasto_jardin = sum(gasto_tri, na.rm = TRUE), .groups = "drop")
  
  lista[[as.character(a)]] <- conc %>%
    left_join(gast_jardin, by = c("folioviv","foliohog")) %>%
    left_join(viv, by = "folioviv") %>%
    mutate(
      gasto_jardin = ifelse(is.na(gasto_jardin), 0, gasto_jardin),
      compra       = ifelse(gasto_jardin > 0, 1, 0),
      anio         = a
    ) %>%
    left_join(inpc_anual, by = "anio") %>%
    mutate(
      gasto_jardin_real = gasto_jardin / factor_deflacion,
      ing_cor_real      = ing_cor / factor_deflacion,
      ln_gasto_real     = ifelse(compra == 1, log(gasto_jardin_real), NA),
      ln_ingreso_real   = log(ing_cor_real),
      tamano_hogar      = tot_integ,
      educacion_jefe    = recodif_educ(educa_jefe),
      tipo_vivienda     = ifelse(tipo_viv == 1, 1, 0),
      urbano            = ifelse(tam_loc == 1, 1, 0),
      mujer_jefe        = ifelse(sexo_jefe == 2, 1, 0),
      anios_educacion   = case_when(
        educa_jefe == 1 ~ 0,    educa_jefe == 2 ~ 1,
        educa_jefe == 3 ~ 3,    educa_jefe == 4 ~ 6,
        educa_jefe == 5 ~ 7.5,  educa_jefe == 6 ~ 9,
        educa_jefe == 7 ~ 10.5, educa_jefe == 8 ~ 12,
        educa_jefe == 9 ~ 14,   educa_jefe == 10 ~ 16,
        educa_jefe == 11 ~ 17.5
      )
    ) %>%
    filter(ing_cor > 0, !is.na(educacion_jefe), is.finite(ln_ingreso_real))
}

datos <- bind_rows(lista) %>%
  mutate(
    shock2020     = ifelse(anio == 2020, 1, 0),
    post_pandemia = ifelse(anio >= 2022, 1, 0),
    anio_2024     = ifelse(anio == 2024, 1, 0),
    upm_unica     = paste0(anio, "_", upm),
    estrato_unico = paste0(anio, "_", est_dis)
  )

cat("Total hogares:", nrow(datos), "\n")

# --- PARTE 2. DISEÑO MUESTRAL COMPLEJO ---
diseno        <- svydesign(ids = ~upm_unica, strata = ~estrato_unico, weights = ~factor, data = datos, nest = TRUE)
diseno_compra <- subset(diseno, compra == 1)

# Guardar para el siguiente script
saveRDS(datos, "data/processed/datos.rds")
saveRDS(diseno, "data/processed/diseno.rds")
saveRDS(diseno_compra, "data/processed/diseno_compra.rds")