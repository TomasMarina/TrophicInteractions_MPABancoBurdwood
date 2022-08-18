## DATA WRANGLING: RED TRÓFICA AMP NAMUNCURÁ-BANCO BURDWOOD
# Autor: Tomás Ignacio Marina
# Fecha: 06/07/2021


## Paquetes ----

packages <- c("tidyverse")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


## Lista de Especies ----

sp_raw <- read.csv("data/ListaEspecies_AMPNBB_ago_22.csv")
colnames(sp_raw)
sp_raw <- sp_raw %>% 
  add_count(FunctionalGroup, name = "Richness") %>% 
  mutate(FunctionalGroup = fct_reorder(FunctionalGroup, Richness, .desc = TRUE))


## Lista de Interacciones ----

int_raw <- read.csv("data/ListaInteracciones_AMPNBB_ago_22.csv")
int_raw <- int_raw %>% 
  mutate(Prey = case_when(Prey %in% c("Coscinodiscus_sp", "Podosira_stelligera") ~ "Diatoms_centric",
                          Prey %in% c("Fragilariopsis_kerguelensis", "Navicula_sp", "Pseudonitzschia_sp", "Tabularia_fasciculata") ~ "Diatoms_pennate",
                          Prey %in% c("Paralia_sulcata", "Thalassionema_nitzschioides") ~ "Diatoms_benthic",
                          TRUE ~ Prey)) %>% 
  mutate(PreyGroup = case_when(Prey == "Phytoplankton *" ~ "Phytoplankton_Misc", 
                               Prey == "EpiphyticPhytoplankton *" ~ "Phytoplankton_Misc", TRUE ~ PreyGroup))  # low-resolved Phytoplankton
int_raw <- unique(int_raw[2:10])  # exclude repeated interactions
int_raw <- subset(int_raw, !(Prey %in% "Demospongiae *"))  # exclude rows with Prey == "Demospongiae *"


## Save data ----

save(sp_raw, int_raw, 
     file = "data/cleaned-data_agos22.rda")
