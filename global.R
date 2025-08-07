# PACOTES ===================================================================================================================================================

# install.packages("pacman")
pacman::p_load(tidyverse, rio, geobr, ggthemes, paletteer, scales, shiny, 
               rsconnect, bslib, bsicons, Cairo, ggiraph, showtext, conflicted, rmapshaper)

showtext_auto()

font_add_google("Open Sans", "opensans")
theme_set(theme_hc(base_family = "opensans"))

options(scipen = 999, shiny.usecairo = TRUE)


# CHAMANDO SCRIPTS ==========================================================================================================================================

# Scripts do shiny:
Scripts <- c("ScriptDisque100v9.R", "cards.R", "conteudoPaginas.R")

Dados4 <- import("Dados Filtrados.rds")
export(Dados4, "Dados Filtrados.rdata")

# Importando:
map(Scripts, source)
