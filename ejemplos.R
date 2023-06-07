library(httr)
library(knitr)
library(jsonlite)
library(tidyverse)
library(dagitty)
library(kableExtra)
library(dagitty)
library(ggdag)
library(ggplot2)

-----------------------------
# Lee Miro
miro_datos <- datosMiro(servMiro = "miro", user = "miguel-token")
miro_datos

miro_valida <- miro_validar(papelitos = miro_datos$nodos, arcos = miro_datos$arcos)
t(miro_valida)

# DAG
dag_miro <- prepara_DAG(nodos = miro_datos$nodos, arcos = miro_datos$arcos)
dag_miro$gg_dag
var <- "carb_suelo"
ind_cond_carb <- cond_indepOnvar(dag_miro$indepCond, var)
ind_cond_carb

# Netica
docDNE <- red_DNE(datos_marcos = miro_datos$marcos,
papelitos =  miro_datos$nodos,
arcos = miro_datos$arcos)
cat(docDNE)
