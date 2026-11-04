source("code/00_config.R")
datos <- readRDS("data/processed/datos.rds")

# --- TABLA 2 ---
t2 <- datos %>%
  group_by(anio) %>%
  group_modify(~ {
    d <- svydesign(ids = ~upm_unica, strata = ~estrato_unico, weights = ~factor, data = .x, nest = TRUE)
    p <- coef(svymean(~compra, design = d, na.rm = TRUE))[1]
    data.frame(Hogares = nrow(.x), Compradores = sum(.x$compra), Pct_Compradores = p*100, Pct_Ceros = (1-p)*100)
  }) %>% ungroup()
write.csv(t2, "tablas_tesis/T2_participacion.csv", row.names = FALSE)

# --- TABLA 3 ---
t3 <- datos %>%
  filter(compra == 1) %>%
  mutate(grupo = case_when(
    anio %in% c(2016,2018) ~ "Pre (2016-2018)",
    anio == 2020            ~ "Shock (2020)",
    anio %in% c(2022,2024) ~ "Post (2022-2024)"
  )) %>%
  group_by(grupo) %>%
  group_modify(~ {
    d <- svydesign(ids = ~upm_unica, strata = ~estrato_unico, weights = ~factor, data = .x, nest = TRUE)
    data.frame(
      N = nrow(.x),
      Tamano = coef(svymean(~tamano_hogar, d, na.rm = TRUE))[1],
      Edad = coef(svymean(~edad_jefe, d, na.rm = TRUE))[1],
      Educ_anios = coef(svymean(~anios_educacion, d, na.rm = TRUE))[1],
      Pct_Jef_Fem = coef(svymean(~mujer_jefe, d, na.rm = TRUE))[1]*100,
      Pct_Urbano = coef(svymean(~urbano, d, na.rm = TRUE))[1]*100,
      Pct_Casa = coef(svymean(~tipo_vivienda, d, na.rm = TRUE))[1]*100,
      Ingreso_mediano = coef(svyquantile(~ing_cor_real, d, quantiles = 0.5, ci = FALSE))[1],
      Gasto_mediano = coef(svyquantile(~gasto_jardin_real, d, quantiles = 0.5, ci = FALSE))[1]
    )
  }) %>% ungroup()
write.csv(t3, "tablas_tesis/T3_perfil.csv", row.names = FALSE)

# --- TABLA 4 ---
t4 <- datos %>%
  group_by(anio) %>%
  group_modify(~ {
    d <- svydesign(ids = ~upm_unica, strata = ~estrato_unico, weights = ~factor, data = .x, nest = TRUE)
    if(sum(.x$compra) >= 30) {
      mod <- svyglm(ln_gasto_real ~ ln_ingreso_real + tamano_hogar + educacion_jefe, design = subset(d, compra == 1))
      data.frame(Elasticidad = coef(mod)["ln_ingreso_real"], EE = sqrt(diag(vcov(mod)))["ln_ingreso_real"])
    } else {
      data.frame(Elasticidad = NA, EE = NA)
    }
  }) %>% ungroup()
write.csv(t4, "tablas_tesis/T4_elast_anual.csv", row.names = FALSE)