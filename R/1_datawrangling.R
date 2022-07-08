## DATA WRANGLING: RED TRÓFICA AMP NAMUNCURÁ-BANCO BURDWOOD
# Autor: Tomás Ignacio Marina
# Fecha: 06/07/2021


## Paquetes ----

packages <- c("tidyverse", "ggplot2", "naniar", "igraph", "multiweb",
              "NetIndices", "ggjoy")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


## Lista de Especies ----

sp_raw <- read.csv("data/ListaEspecies_AMPNBB_julv2_22.csv")
colnames(sp_raw)
sp_raw <- sp_raw %>% 
  add_count(FunctionalGroup, name = "Richness") %>% 
  mutate(FunctionalGroup = fct_reorder(FunctionalGroup, Richness, .desc = TRUE))


## Lista de Interacciones ----

int_raw <- read.csv("data/ListaInteracciones_AMPNBB_julv2_22.csv")
int_raw <- int_raw %>% 
  mutate(Prey = case_when(Prey %in% c("Coscinodiscus_sp", "Podosira_stelligera") ~ "Diatoms_centric",
                          Prey %in% c("Fragilariopsis_kerguelensis", "Navicula_sp", "Pseudonitzschia_sp", "Tabularia_fasciculata") ~ "Diatoms_pennate",
                          Prey %in% c("Paralia_sulcata", "Thalassionema_nitzschioides") ~ "Diatoms_benthic",
                          TRUE ~ Prey))
int_raw <- unique(int_raw[2:10])  # exclude repeated interactions
row.names.remove <- c(2077, 2083)  # exclude rows with Prey == "Demospongiae *"
int_raw <- int_raw[!(row.names(int_raw) %in% row.names.remove), ]

## Save data ----

save(sp_raw, int_raw, 
     file = "data/cleaned-data_julv2_22.rda")
