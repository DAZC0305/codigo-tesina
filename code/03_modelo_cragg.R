source("code/00_config.R")
datos <- readRDS("data/processed/datos.rds")
diseno <- readRDS("data/processed/diseno.rds")
diseno_compra <- readRDS("data/processed/diseno_compra.rds")

# --- PARTE 4: MODELO DE DOS PARTES ---
probit <- svyglm(compra ~ ln_ingreso_real * post_pandemia + ln_ingreso_real * shock2020 + tamano_hogar + educacion_jefe + tipo_vivienda + urbano + edad_jefe + mujer_jefe, family = quasibinomial(link = "probit"), design = diseno)
loglin <- svyglm(ln_gasto_real ~ ln_ingreso_real * post_pandemia + ln_ingreso_real * shock2020 + tamano_hogar + educacion_jefe + tipo_vivienda + urbano + edad_jefe + mujer_jefe, design = diseno_compra)

cat("\n===== PROBIT =====\n");  print(summary(probit))
cat("\n===== LOG-LINEAL =====\n"); print(summary(loglin))

write.csv(tidy(probit), "tablas_tesis/T5_probit.csv", row.names = FALSE)
write.csv(tidy(loglin), "tablas_tesis/T7_loglin.csv", row.names = FALSE)

# --- PARTE 5: EFECTOS MARGINALES ---
# Para el Probit (usó el diseño completo 'diseno')
marg_cont <- avg_slopes(probit, 
                        variables = c("ln_ingreso_real","tamano_hogar","edad_jefe","post_pandemia","shock2020","tipo_vivienda","urbano","mujer_jefe"), 
                        wts = weights(diseno))
write.csv(marg_cont, "tablas_tesis/T6_marginales.csv", row.names = FALSE)

marg_educ <- avg_comparisons(probit, variables = "educacion_jefe", wts = weights(diseno))
write.csv(marg_educ, "tablas_tesis/T6b_educacion.csv", row.names = FALSE)

# Para el Log-lineal (usó el subconjunto 'diseno_compra', solo compradores)
marg_educ_loglin <- avg_comparisons(loglin, variables = "educacion_jefe", wts = weights(diseno_compra))

# --- PARTE 6: ELASTICIDADES POR PERIODO ---
b1 <- coef(loglin)["ln_ingreso_real"]; b4 <- coef(loglin)["ln_ingreso_real:post_pandemia"]; b5 <- coef(loglin)["ln_ingreso_real:shock2020"]
V  <- vcov(loglin); var_b1 <- V["ln_ingreso_real", "ln_ingreso_real"]
var_b1_b4 <- var_b1 + V["ln_ingreso_real:post_pandemia", "ln_ingreso_real:post_pandemia"] + 2 * V["ln_ingreso_real", "ln_ingreso_real:post_pandemia"]
var_b1_b5 <- var_b1 + V["ln_ingreso_real:shock2020", "ln_ingreso_real:shock2020"] + 2 * V["ln_ingreso_real", "ln_ingreso_real:shock2020"]

t8 <- data.frame(
  Periodo = c("Base (2016-2018)", "Shock (2020)", "Post (2022-2024)"),
  Elasticidad = c(b1, b1 + b5, b1 + b4),
  EE = c(sqrt(var_b1), sqrt(var_b1_b5), sqrt(var_b1_b4))
) %>% mutate(IC_inf = Elasticidad - 1.96*EE, IC_sup = Elasticidad + 1.96*EE)
write.csv(t8, "tablas_tesis/T8_elasticidades.csv", row.names = FALSE)

# --- PARTE 7: CURVA DE ENGEL ---
cuad <- svyglm(ln_gasto_real ~ ln_ingreso_real + I(ln_ingreso_real^2) + post_pandemia + shock2020 + tamano_hogar + educacion_jefe + tipo_vivienda + urbano + edad_jefe + mujer_jefe, design = diseno_compra)
b1c <- coef(cuad)["ln_ingreso_real"]; b2c <- coef(cuad)["I(ln_ingreso_real^2)"]
cat("\nConvexidad: b2 =", round(b2c, 4), " (p =", round(summary(cuad)$coefficients["I(ln_ingreso_real^2)","Pr(>|t|)"], 4), ")\n")

q <- svyquantile(~ln_ingreso_real, design = diseno_compra, quantiles = seq(0.1, 0.9, 0.1), ci = FALSE)
dec <- c(as.numeric(coef(q))[1:9], coef(svymean(~ln_ingreso_real, design = diseno_compra, na.rm = TRUE))[1])
t9 <- data.frame(Decil = c("10%","20%","30%","40%","50%","60%","70%","80%","90%","Media"), Ingreso_trimestral = round(exp(dec), 0), Elasticidad = round(b1c + 2*b2c*dec, 3))
write.csv(t9, "tablas_tesis/T9_deciles.csv", row.names = FALSE)

# --- PARTE 8: ELASTICIDAD POR ESTRATO ---
t10 <- datos %>%
  filter(compra == 1, !is.na(est_socio)) %>%
  group_by(estrato = factor(est_socio, 1:4, labels = c("Bajo","Medio bajo","Medio alto","Alto"))) %>%
  group_modify(~ {
    d <- svydesign(ids = ~upm_unica, strata = ~estrato_unico, weights = ~factor, data = .x, nest = TRUE)
    m <- svyglm(ln_gasto_real ~ ln_ingreso_real + tamano_hogar + educacion_jefe, design = d)
    data.frame(Elasticidad = coef(m)["ln_ingreso_real"], EE = sqrt(diag(vcov(m)))["ln_ingreso_real"])
  }) %>% mutate(IC_inf = Elasticidad - 1.96*EE, IC_sup = Elasticidad + 1.96*EE) %>% ungroup()
write.csv(t10, "tablas_tesis/T10_estrato.csv", row.names = FALSE)

# --- PARTE 9: GASTO ESPERADO INCONDICIONAL ---
smearing <- weighted.mean(exp(residuals(loglin)), w = weights(diseno_compra), na.rm = TRUE)
cat("\nFactor de smearing ponderado:", round(smearing, 4), "\n")

datos$prob_c <- predict(probit, newdata = datos, type = "response", allow.new.levels = TRUE)
datos$gasto_c <- NA_real_
datos$gasto_c[datos$compra == 1] <- exp(predict(loglin, newdata = datos %>% filter(compra == 1), allow.new.levels = TRUE)) * smearing
datos$gasto_incond <- datos$prob_c * ifelse(is.na(datos$gasto_c), 0, datos$gasto_c)

diseno_full <- svydesign(ids = ~upm_unica, strata = ~estrato_unico, weights = ~factor, data = datos, nest = TRUE)
gasto_periodo <- svyby(~prob_c + gasto_c + gasto_incond, ~anio, diseno_full, svymean, na.rm = TRUE)
write.csv(gasto_periodo, "tablas_tesis/T12_gasto_incondicional.csv", row.names = FALSE)

# Guardamos todo para robustez y gráficos
saveRDS(probit, "data/processed/probit.rds")
saveRDS(loglin, "data/processed/loglin.rds")
saveRDS(datos, "data/processed/datos_predicciones.rds")