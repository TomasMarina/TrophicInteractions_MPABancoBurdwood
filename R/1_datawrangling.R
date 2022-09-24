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

sp_raw <- read.csv("data/ListaEspecies_AMPNBB_sep_22.csv")
colnames(sp_raw)
sp_raw <- sp_raw %>% 
  add_count(FunctionalGroup, name = "Richness") %>% 
  mutate(FunctionalGroup = fct_reorder(FunctionalGroup, Richness, .desc = TRUE))


## Lista de Interacciones ----

int_raw <- read.csv("data/ListaInteracciones_AMPNBB_sep_22.csv")

int_raw <- int_raw %>% 
  # collapse phytoplankton species
  mutate(Prey = case_when(Prey %in% c("Coscinodiscus_sp", "Podosira_stelligera") ~ "Diatoms_centric",
                          Prey %in% c("Fragilariopsis_kerguelensis", "Navicula_sp", "Pseudonitzschia_sp", "Tabularia_fasciculata") ~ "Diatoms_pennate",
                          Prey %in% c("Paralia_sulcata", "Thalassionema_nitzschioides") ~ "Diatoms_benthic",
                          Prey %in% c("Dinophysis_acuminata", "Gyrodinium_sp", "Gyrodinium_spirale", "Protoperidinium_sp") ~ "Dinoflagellates_heterosol",
                          Prey %in% c("Strombidium_conicum", "Strombidium_sp") ~ "Strombidium_spp", TRUE ~ Prey)) %>% 
  # low-resolved Phytoplankton
  mutate(PreyGroup = case_when(Prey == "Phytoplankton *" ~ "Phytoplankton_Misc", 
                               Prey == "EpiphyticPhytoplankton *" ~ "Phytoplankton_Misc", TRUE ~ PreyGroup)) %>% 
  # collapse Ascidians by Genus
  mutate(Prey = case_when(Prey %in% c("Aplidium_falklandicum","Aplidium_fuegiense","Aplidium_meridianum","Aplidium_globosum","Aplidium_polarsterni") ~ "Aplidium_spp",
                          Prey %in% c("Cnemidocarpa_verrucosa","Cnemidocarpa_nordenskjöldi","Cnemidocarpa_drygalskii") ~ "Cnemidocarpa_spp",
                          Prey %in% c("Molgula_malvinensis","Molgula_pulchra","Molgula_setigera") ~ "Molgula_spp",
                          Prey %in% c("Polyzoa_opuntia","Polyzoa_reticulata") ~ "Polyzoa_spp",
                          Prey %in% c("Pyura_paessleri","Pyura_pilosa") ~ "Pyura_spp", TRUE ~ Prey),
         Predator = case_when(Predator %in% c("Aplidium_falklandicum","Aplidium_fuegiense","Aplidium_meridianum","Aplidium_globosum","Aplidium_polarsterni") ~ "Aplidium_spp",
                              Predator %in% c("Cnemidocarpa_verrucosa","Cnemidocarpa_nordenskjöldi","Cnemidocarpa_drygalskii") ~ "Cnemidocarpa_spp",
                              Predator %in% c("Molgula_malvinensis","Molgula_pulchra","Molgula_setigera") ~ "Molgula_spp",
                              Predator %in% c("Polyzoa_opuntia","Polyzoa_reticulata") ~ "Polyzoa_spp",
                              Predator %in% c("Pyura_paessleri","Pyura_pilosa") ~ "Pyura_spp", TRUE ~ Predator)) %>% 
  # collapse Porifera by Class and Order
  mutate(Prey = case_when(Prey == "Haliclona_sp" ~ "Haplosclerida",
         Prey %in% c("Isodictya_sp", "Mycale_sp") ~ "Poecilosclerida",
         Prey == "Craniella_leptoderma" ~ "Tetractinellida", 
         Prey %in% c("Clathria_(Clathria)_toxipraedita", "Clathria_(Axosuberites)_nidificata", "Clathria_(Microciona)_antarctica") ~ "Clathria_spp",
         TRUE ~ Prey),
         Predator = case_when(Predator == "Haliclona_sp" ~ "Haplosclerida",
                              Predator %in% c("Isodictya_sp", "Mycale_sp") ~ "Poecilosclerida",
                              Predator == "Craniella_leptoderma" ~ "Tetractinellida", 
                              Predator %in% c("Clathria_(Clathria)_toxipraedita", "Clathria_(Axosuberites)_nidificata", "Clathria_(Microciona)_antarctica") ~ "Clathria_spp",
                              TRUE ~ Predator)) %>% 
  # collapse Polychaeta
  mutate(Prey = case_when(Prey %in% c("Pista_corrientis", "Pista_mirabilis", "Pista_sp") ~ "Pista_spp", TRUE ~ Prey),
         Predator = case_when(Predator %in% c("Pista_corrientis", "Pista_mirabilis", "Pista_sp") ~ "Pista_spp", TRUE ~ Predator)) %>% 
  # collapse Cnidaria
  mutate(Prey = case_when(Prey %in% c("Thouarella_antarctica", "Thouarella_chilensis", "Thouarella_sp", "Thouarella_variabilis", "Thouarella_viridis") ~ "Thouarella_spp", 
                          Prey %in% c("Obelia_longissima", "Obelia_sp") ~ "Obelia_spp", 
                          Prey %in% c("Flabellum_apertum", "Flabellum_curvatum", "Flabellum_areum", "Flabellum_thouarsi") ~ "Flabellum_spp", TRUE ~ Prey),
         Predator = case_when(Predator %in% c("Thouarella_antarctica", "Thouarella_chilensis", "Thouarella_sp", "Thouarella_variabilis", "Thouarella_viridis") ~ "Thouarella_spp", 
                              Predator %in% c("Obelia_longissima", "Obelia_sp") ~ "Obelia_spp", 
                              Predator %in% c("Flabellum_apertum", "Flabellum_curvatum", "Flabellum_areum", "Flabellum_thouarsi") ~ "Flabellum_spp", TRUE ~ Predator))


int_raw <- unique(int_raw[2:10])  # exclude repeated interactions

int_raw <- subset(int_raw, !(Prey %in% "Demospongiae *"))  # exclude rows with Prey == "Demospongiae *"


# Check ----

int_good_res <- int_raw %>% 
  filter(!str_detect(Prey, "\\*$")) %>% 
  filter(!str_detect(Predator, "\\*$")) %>% 
  dplyr::select(Prey, Predator, PreyGroup, PredGroup, PredStrategy, FoodSource) %>% 
  distinct(Prey, Predator, .keep_all = TRUE)

library(igraph)
g <- graph_from_edgelist(as.matrix(int_good_res[,1:2]), directed = TRUE)
sp_raw_fg <- sp_raw[,c("TrophicSpecies", "FunctionalGroup", "Zone")] %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  relocate(any_of(c("FunctionalGroup", "Zone", "TrophicSpecies"))) %>% rename(id = TrophicSpecies)
df_g <- igraph::as_data_frame(g, 'both')
df_g$vertices <- df_g$vertices %>% 
  left_join(sp_raw_fg, c('name' = 'id'))
g <- graph_from_data_frame(df_g$edges, directed = TRUE, vertices = df_g$vertices)

library(NetIndices)
adj_mat <- as_adjacency_matrix(g, sparse = TRUE)
tl <- round(TrophInd(as.matrix(adj_mat)), digits = 3)
V(g)$TL <- tl$TL
V(g)$Omn <- tl$OI

spp_name <- as.data.frame(V(g)$name)
spp_fg <- as.data.frame(V(g)$FunctionalGroup)
spp_tl <- as.data.frame(V(g)$TL)
spp_zone <- as.data.frame(V(g)$Zone)
spp_db <- bind_cols(spp_name, spp_fg, spp_tl, spp_zone)
colnames(spp_db) <- c("TrophicSpecies", "FunctionalGroup", "TL", "Zone")

spp_tl_1 <- spp_db %>% 
  filter(TL == 1, Zone != "Talud")
# It should only comprise "Bacteria", "	"Coccolithophorids", "Diatoms", "Dinoflagellates" (Azadinium_sp)
# "Non-living", "Phytoplankton_Misc", "Silicoflagellates" & "Zooplankton" (Eggs_Fish)


## Save data ----

save(sp_raw, int_raw, 
     file = "data/cleaned-data_sep22.rda")
