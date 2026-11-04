source("code/00_config.R")
datos <- readRDS("data/processed/datos_predicciones.rds")
probit <- readRDS("data/processed/probit.rds")
marg_cont <- read.csv("tablas_tesis/T6_marginales.csv")
marg_educ <- read.csv("tablas_tesis/T6b_educacion.csv")
t9 <- read.csv("tablas_tesis/T9_deciles.csv")
t10 <- read.csv("tablas_tesis/T10_estrato.csv")

# --- GRÁFICA 1 ---
datos_g1 <- marg_cont %>%
  mutate(variable = case_when(term == "post_pandemia" ~ "Post-pandemia", term == "shock2020" ~ "Shock 2020", term == "ln_ingreso_real" ~ "Ingreso (log)", term == "tamano_hogar" ~ "Tamaño del hogar", term == "urbano" ~ "Localidad urbana", term == "tipo_vivienda" ~ "Casa independiente", term == "mujer_jefe" ~ "Jefatura femenina", term == "edad_jefe" ~ "Edad del jefe", TRUE ~ term),
         efecto_pp = estimate * 100, inf = (estimate - 1.96*std.error) * 100, sup = (estimate + 1.96*std.error) * 100) %>% arrange(desc(abs(efecto_pp)))
g1 <- ggplot(datos_g1, aes(x = reorder(variable, efecto_pp), y = efecto_pp, fill = efecto_pp > 0)) + geom_col(alpha = 0.8, width = 0.7) + geom_errorbar(aes(ymin = inf, ymax = sup), width = 0.2) + geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") + coord_flip() + geom_text(aes(label = paste0(round(efecto_pp, 2), " pp")), hjust = ifelse(datos_g1$efecto_pp > 0, -0.2, 1.2), size = 4, fontface = "bold") + labs(title = "Factores asociados a la probabilidad de compra", subtitle = "Efectos marginales promedio (puntos porcentuales)", x = "", y = "Cambio en probabilidad (pp)") + scale_fill_manual(values = c("tomato", "steelblue")) + theme_minimal(base_size = 14) + theme(legend.position = "none")
ggsave("graficos_tesis/figura1_efectos_marginales.png", g1, width = 10, height = 7, dpi = 300)

# --- GRÁFICA 1b ---
datos_g1b <- marg_educ %>%
  mutate(nivel = str_replace(contrast, " - Sin instrucción", ""), nivel = factor(nivel, levels = c("Primaria","Secundaria", "Media superior","Superior")), efecto_pp = estimate * 100, inf = (estimate - 1.96*std.error) * 100, sup = (estimate + 1.96*std.error) * 100)
g1b <- ggplot(datos_g1b, aes(x = nivel, y = efecto_pp)) + geom_col(fill = "steelblue", alpha = 0.8, width = 0.6) + geom_errorbar(aes(ymin = inf, ymax = sup), width = 0.15) + geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") + geom_text(aes(label = paste0(round(efecto_pp, 2), " pp")), vjust = -0.7, size = 4, fontface = "bold") + labs(title = "Gradiente educativo en la probabilidad de compra", subtitle = "Comparaciones vs 'Sin instrucción'", x = "Nivel educativo del jefe", y = "Cambio en probabilidad (pp)") + theme_minimal(base_size = 14)
ggsave("graficos_tesis/figura1b_gradiente_educativo.png", g1b, width = 8, height = 5.5, dpi = 300)

# --- GRÁFICA 2 ---
curva <- rbind(
  data.frame(ln_ingreso_real = seq(8, 13, length.out = 100), post_pandemia = 0, shock2020 = 0, tamano_hogar = 3.4, educacion_jefe = factor("Superior", levels = levels(datos$educacion_jefe)), tipo_vivienda = 1, urbano = 0, edad_jefe = 52, mujer_jefe = 0, Periodo = "Pre (2016-2018)"),
  data.frame(ln_ingreso_real = seq(8, 13, length.out = 100), post_pandemia = 0, shock2020 = 1, tamano_hogar = 3.4, educacion_jefe = factor("Superior", levels = levels(datos$educacion_jefe)), tipo_vivienda = 1, urbano = 0, edad_jefe = 52, mujer_jefe = 0, Periodo = "Shock (2020)"),
  data.frame(ln_ingreso_real = seq(8, 13, length.out = 100), post_pandemia = 1, shock2020 = 0, tamano_hogar = 3.4, educacion_jefe = factor("Superior", levels = levels(datos$educacion_jefe)), tipo_vivienda = 1, urbano = 0, edad_jefe = 52, mujer_jefe = 0, Periodo = "Post (2022-2024)")
)
curva$prob <- predict(probit, newdata = curva, type = "response")
curva$Periodo <- factor(curva$Periodo, levels = c("Pre (2016-2018)", "Shock (2020)", "Post (2022-2024)"))
g2 <- ggplot(curva, aes(x = exp(ln_ingreso_real), y = prob, color = Periodo)) + geom_line(linewidth = 1.3) + scale_x_log10(labels = scales::dollar_format(prefix = "$"), breaks = c(10000, 30000, 50000, 100000, 200000)) + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + scale_color_manual(values = c("blue", "darkorange", "red")) + labs(title = "Probabilidad de compra en tres periodos", subtitle = "Predicciones para un perfil de referencia", x = "Ingreso real trimestral (escala logarítmica)", y = "Probabilidad de comprar", color = "") + theme_minimal(base_size = 14) + theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16), axis.text = element_text(size = 12))
ggsave("graficos_tesis/figura2_tres_periodos.png", g2, width = 10, height = 6, dpi = 300)

# --- GRÁFICA 3 ---
g3 <- ggplot(t9, aes(x = Ingreso_trimestral, y = Elasticidad)) + geom_line(color = "steelblue", linewidth = 1.5) + geom_point(size = 4, color = "darkblue") + geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) + scale_x_log10(labels = scales::dollar_format(prefix = "$")) + labs(title = "Elasticidad-ingreso a lo largo de la distribución", subtitle = "Especificación cuadrática en log-ingreso (compradores)", x = "Ingreso real trimestral (escala logarítmica)", y = "Elasticidad-ingreso (η)") + theme_minimal(base_size = 14)
ggsave("graficos_tesis/figura3_curva_engel.png", g3, width = 9, height = 5.5, dpi = 300)

# --- GRÁFICA 4 ---
# FORZAMOS EL ORDEN DE LOS FACTORES
t10$estrato <- factor(t10$estrato, levels = c("Bajo", "Medio bajo", "Medio alto", "Alto"))

g4 <- ggplot(t10, aes(x = estrato, y = Elasticidad, fill = estrato)) + geom_col(alpha = 0.8) + geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2) + geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) + geom_text(aes(label = round(Elasticidad, 3)), vjust = -0.7, size = 4.5, fontface = "bold") + labs(title = "Elasticidad-ingreso por estrato socioeconómico", subtitle = "Estimaciones condicionales para hogares compradores", x = "", y = "Elasticidad-ingreso") + scale_fill_brewer(palette = "Blues") + theme_minimal(base_size = 14) + theme(legend.position = "none")
ggsave("graficos_tesis/figura4_estrato.png", g4, width = 6, height = 5, dpi = 300)

cat("\n============================================================\n")
cat("PROCESO COMPLETADO.\n")
cat("Tablas en:      tablas_tesis/\n")
cat("Gráficos en:    graficos_tesis/\n")
cat("============================================================\n")