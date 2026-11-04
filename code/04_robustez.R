source("code/00_config.R")
datos <- readRDS("data/processed/datos_predicciones.rds")
probit <- readRDS("data/processed/probit.rds")
loglin <- readRDS("data/processed/loglin.rds")
diseno_compra <- readRDS("data/processed/diseno_compra.rds")

# --- AGREGAMOS ESTO: Estimamos el modelo cuadrático para tener su p-valor ---
cuad <- svyglm(ln_gasto_real ~ ln_ingreso_real + I(ln_ingreso_real^2) + 
                 post_pandemia + shock2020 + tamano_hogar + educacion_jefe + 
                 tipo_vivienda + urbano + edad_jefe + mujer_jefe, 
               design = diseno_compra)
b2c <- coef(cuad)["I(ln_ingreso_real^2)"]

# --- 10.1 Homogeneidad 2024 ---
homog <- svyglm(ln_gasto_real ~ ln_ingreso_real * post_pandemia + ln_ingreso_real * shock2020 + ln_ingreso_real * anio_2024 + tamano_hogar + educacion_jefe + tipo_vivienda + urbano + edad_jefe + mujer_jefe, design = diseno_compra)
w_homog <- regTermTest(homog, "ln_ingreso_real:anio_2024")

# --- 10.2 Sensibilidad sin 2024 ---
datos_sin <- datos %>% filter(anio != 2024) %>% mutate(upm_unica = paste0(anio, "_", upm), estrato_unico = paste0(anio, "_", est_dis))
dis_sin <- svydesign(ids = ~upm_unica, strata = ~estrato_unico, weights = ~factor, data = datos_sin, nest = TRUE)
probit_sin <- svyglm(compra ~ ln_ingreso_real * post_pandemia + ln_ingreso_real * shock2020 + tamano_hogar + educacion_jefe + tipo_vivienda + urbano + edad_jefe + mujer_jefe, family = quasibinomial(link = "probit"), design = dis_sin)
loglin_sin <- svyglm(ln_gasto_real ~ ln_ingreso_real * post_pandemia + ln_ingreso_real * shock2020 + tamano_hogar + educacion_jefe + tipo_vivienda + urbano + edad_jefe + mujer_jefe, design = subset(dis_sin, compra == 1))

# --- 10.3 Tabla de sensibilidad ---
vars_clave <- c("ln_ingreso_real","post_pandemia","ln_ingreso_real:post_pandemia")
t13 <- data.frame(Variable = vars_clave, Probit_con = round(coef(probit)[vars_clave], 4), Probit_sin = round(coef(probit_sin)[vars_clave], 4), Loglin_con = round(coef(loglin)[vars_clave], 4), Loglin_sin = round(coef(loglin_sin)[vars_clave], 4))
write.csv(t13, "tablas_tesis/T13_sensibilidad.csv", row.names = FALSE)

# --- 10.4 Participación 2024 vs 2022 ---
d22 <- svydesign(ids = ~upm_unica, strata = ~estrato_unico, weights = ~factor, data = datos %>% filter(anio == 2022), nest = TRUE)
d24 <- svydesign(ids = ~upm_unica, strata = ~estrato_unico, weights = ~factor, data = datos %>% filter(anio == 2024), nest = TRUE)
p22 <- coef(svymean(~compra, d22, na.rm = TRUE))[1]; p24 <- coef(svymean(~compra, d24, na.rm = TRUE))[1]
se22 <- SE(svymean(~compra, d22, na.rm = TRUE))[1]; se24 <- SE(svymean(~compra, d24, na.rm = TRUE))[1]
z <- (p24 - p22) / sqrt(se22^2 + se24^2)

# --- 10.5 VIF y Breusch-Pagan ---
m_vif <- lm(compra ~ ln_ingreso_real + post_pandemia + shock2020 + tamano_hogar + educacion_jefe + tipo_vivienda + urbano + edad_jefe + mujer_jefe, data = datos)
vif_max <- max(vif(m_vif, type = "predictor")[,"GVIF^(1/(2*Df))"], na.rm = TRUE)
m_bp <- lm(ln_gasto_real ~ ln_ingreso_real * post_pandemia + ln_ingreso_real * shock2020 + tamano_hogar + educacion_jefe + tipo_vivienda + urbano + edad_jefe + mujer_jefe, data = datos %>% filter(compra == 1))
bp <- bptest(m_bp)

# --- TABLA 15. Resumen de robustez ---
tabla_robustez <- data.frame(
  Prueba = c("Multicolinealidad (GVIF ajustado)", "Heterocedasticidad (Breusch-Pagan)", "Homogeneidad 2024 (elasticidad)", "Participación 2024 vs 2022", "Curvatura de Engel", "Robustez: educación como años"),
  Estadistico = c(paste0("Máx. GVIF^(1/2df) = ", round(vif_max, 3)), paste0("BP = ", round(unname(bp$statistic), 3)), paste0("F = ", round(unname(w_homog$Ftest), 3)), paste0("Z = ", round(z, 2)), paste0("b2 = ", round(b2c, 4)), "β = 0.0162"),
  p_valor = c("-", format.pval(bp$p.value, digits = 4), format.pval(w_homog$p, digits = 4), format.pval(2*(1-pnorm(abs(z))), digits = 4), format.pval(summary(cuad)$coefficients["I(ln_ingreso_real^2)","Pr(>|t|)"], digits = 4), "-")
)
write.csv(tabla_robustez, "tablas_tesis/T15_robustez.csv", row.names = FALSE)