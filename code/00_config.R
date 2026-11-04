# ============================================================================
# CONFIGURACIÓN GLOBAL
# ============================================================================
rm(list = ls())
options(scipen = 999)

library(tidyverse)
library(survey)
library(broom)
library(readxl)
library(marginaleffects)
library(car)
library(lmtest)
library(knitr)

# Ruta relativa: R buscará la carpeta data/ENIGH desde donde esté parado.
base_dir <- file.path("data", "ENIGH")
anios <- c(2016, 2018, 2020, 2022, 2024)

if(!dir.exists("tablas_tesis"))   dir.create("tablas_tesis")
if(!dir.exists("graficos_tesis")) dir.create("graficos_tesis")
if(!dir.exists("data/processed")) dir.create("data/processed")

options(survey.lonely.psu = "adjust")