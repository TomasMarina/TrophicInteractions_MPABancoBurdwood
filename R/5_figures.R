## FIGURES: RED TRÓFICA AMP NAMUNCURÁ-BANCO BURDWOOD
# Autor: Tomás Ignacio Marina
# Fecha: 18/08/2022


# Paquetes ----

packages <- c("tidyverse", "ggplot2", "igraph", "multiweb")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


# Cargar datos ----

load("data/cleaned-data_ago22.rda")
load("data/foodweb-data_ago22.rda")


# Figuras ----

## Escala global ----


## Escala subgrupos ----


## Escala especie ----




