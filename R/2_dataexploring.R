## DATA EXPLORING: RED TRÓFICA AMP NAMUNCURÁ-BANCO BURDWOOD
# Autor: Tomás Ignacio Marina
# Fecha: 09/11/2021


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


## Cargar datos ----

load("data/cleaned-data_jul22.rda")


## Lista de Especies ----

# Resolución taxonómica de sp
sp_count <- sum(str_detect(sp_raw$TrophicSpecies, "_"))
sp_res <- sp_count/nrow(sp_raw)

# Grupos funcionales
unique(sp_raw$FunctionalGroup)

# Distribución de especies en GF
group_count <- sp_raw %>%
  count(FunctionalGroup) %>%
  mutate(FunctionalGroup = fct_reorder(FunctionalGroup, n, .desc = TRUE))
sum(group_count$n)

# Definir colores para cada GF
col_palette <- scales::hue_pal()(35)
FGColors <- setNames(col_palette, levels(sp_raw$FunctionalGroup))

(plot_grupos <- ggplot(group_count, aes(x = FunctionalGroup, y = n, fill = FunctionalGroup)) + 
    geom_bar(stat = "identity", width = .85) +
    scale_fill_manual(values = FGColors) +
    labs(x = "Grupos funcionales", y = "Especies tróficas") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(face = "bold", size = 14),
          axis.title.y = element_text(face = "bold", size = 14)))

# Zonas
unique(sp_raw$Zone)

# Distribución de especies en Z
zone_count <- sp_raw %>%
  count(Zone) %>%
  mutate(Zone = fct_reorder(Zone, n, .desc = TRUE))

(plot_zone <- ggplot(zone_count, aes(x = Zone, y = n, fill = Zone)) + 
    geom_bar(stat = "identity") +
    labs(x = "Zona", y = "Especies tróficas") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16)))

# Meseta
sp_mes <- sp_raw %>% 
  filter(Zone == "Meseta") %>% 
  count(FunctionalGroup) %>% 
  mutate(FunctionalGroup = fct_reorder(FunctionalGroup, n, .desc = TRUE))
sum(sp_mes$n)

(plot_mes <- ggplot(sp_mes, aes(x = FunctionalGroup, y = n, fill = FunctionalGroup)) + 
    geom_bar(stat = "identity", width = .85) +
    scale_fill_manual(values = FGColors) +
    labs(x = "Grupos funcionales", y = "Especies tróficas") +
    ggtitle("MESETA") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(face = "bold", size = 14),
          axis.title.y = element_text(face = "bold", size = 14),
          plot.title = element_text(size = 16)) +
    annotate("text",  x = Inf, y = Inf, label = paste("especies tróficas = ", sum(sp_mes$n), sep = ""), vjust=1, hjust=1))

# Ta/Me
sp_talmes <- sp_raw %>% 
  filter(Zone == "Ta/Me") %>% 
  count(FunctionalGroup) %>% 
  mutate(FunctionalGroup = fct_reorder(FunctionalGroup, n, .desc = TRUE))
sum(sp_talmes$n)

(plot_talmes <- ggplot(sp_talmes, aes(x = FunctionalGroup, y = n, fill = FunctionalGroup)) + 
    geom_bar(stat = "identity", width = .85) +
    scale_fill_manual(values = FGColors) +
    labs(x = "Grupos funcionales", y = "Especies tróficas") +
    ggtitle("TALUD/MESETA") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(face = "bold", size = 14),
          axis.title.y = element_text(face = "bold", size = 14),
          plot.title = element_text(size = 16)) +
    annotate("text",  x = Inf, y = Inf, label = paste("especies tróficas = ", sum(sp_talmes$n), sep = ""), vjust=1, hjust=1))

# Talud
sp_tal <- sp_raw %>% 
  filter(Zone == "Talud") %>% 
  count(FunctionalGroup) %>% 
  mutate(FunctionalGroup = fct_reorder(FunctionalGroup, n, .desc = TRUE))
sum(sp_tal$n)

(plot_tal <- ggplot(sp_tal, aes(x = FunctionalGroup, y = n, fill = FunctionalGroup)) + 
    geom_bar(stat = "identity", width = .85) +
    scale_fill_manual(values = FGColors) +
    labs(x = "Grupos funcionales", y = "Especies tróficas") +
    ggtitle("TALUD") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(face = "bold", size = 14),
          axis.title.y = element_text(face = "bold", size = 14),
          plot.title = element_text(size = 16)) +
    annotate("text",  x = Inf, y = Inf, label = paste("especies tróficas = ", sum(sp_tal$n), sep = ""), vjust=1, hjust=1))

# Datos biológicos faltantes
naniar::gg_miss_upset(sp_raw[,12:16], nsets = 5)

(plot_faltan <- vis_miss(sp_raw[,12:16]) +
  ylab("\nEspecies tróficas") +
  scale_fill_manual(labels = c("Presentes (9.2%)", "Faltantes (90.8%)"), values = c("green", "black")) +
  guides(fill=guide_legend()) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        plot.title = element_text(size = 16)))

gg_miss_var(sp_raw[,12:16], show_pct = TRUE) + ylim(0, 100)


## Lista de Interacciones ----

# Cantidad total de interacciones
tot_int <- nrow(int_raw)

# Cantidad de grupos
un.prey <- as.data.frame(unique(int_raw$PreyGroup))  # prey groups
un.pred <- as.data.frame(unique(int_raw$PredGroup))  # pred groups
colnames(un.prey) <- "Group"
colnames(un.pred) <- "Group"
unique_FG <- unique(bind_rows(un.pred, un.prey))  # 35 groups

# Definir colores para cada GF
col_palette <- scales::hue_pal()(nrow(unique_FG))
FGColors <- setNames(col_palette, levels(unique_FG$Group))

# Distribucion de spp en GF
un.prey.sp <- as.data.frame(unique(int_raw$Prey))  # prey spp
un.pred.sp <- as.data.frame(unique(int_raw$Predator))  # pred spp
colnames(un.prey.sp) <- "TrophicSpecies"
colnames(un.pred.sp) <- "TrophicSpecies"
unique_spp <- unique(bind_rows(un.pred.sp, un.prey.sp))  # 510 spp
unique_spp$TrophicSpecies <- sub(" .*", "", unique_spp$TrophicSpecies)  # neglect *: indicates low taxonomic resolution

sp_FG <- sp_raw[, c("TrophicSpecies", "FunctionalGroup")]

# Asignar GF a spp
int_sp <- unique_spp %>% 
  left_join(sp_FG)
group_sp <- int_sp %>%
  count(FunctionalGroup) %>%
  mutate(FunctionalGroup = fct_reorder(FunctionalGroup, n, .desc = TRUE))
sum(group_sp$n)

(plot_group <- ggplot(group_sp, aes(x = FunctionalGroup, y = n, fill = FunctionalGroup)) + 
    geom_bar(stat = "identity") +
    labs(x = "Grupos funcionales", y = "Especies tróficas") +
    theme(legend.position = "none",
          axis.title.y = element_text(face = "bold", size = 14),
          axis.title.x = element_text(face = "bold", size = 14),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 14)))

# Estrategia de alimentacion
strategy_count <- int_raw %>% 
  count(PredStrategy) %>% 
  mutate(PredStrategy = fct_reorder(PredStrategy, n, .desc = TRUE))

(plot_strategy <- ggplot(strategy_count, aes(x = PredStrategy, y = n, fill = PredStrategy)) + 
    geom_bar(stat = "identity") +
    labs(x = "Estrategia de alimentación", y = "Interacciones tróficas") +
    theme(legend.position = "none",
          axis.title.y = element_text(face = "bold", size = 14),
          axis.title.x = element_text(face = "bold", size = 14),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 14)))

int_hunt <- int_raw %>% 
  filter(PredStrategy == "Hunter") %>% 
  count(PredGroup) %>% 
  mutate(PredGroup = fct_reorder(PredGroup, n, .desc = TRUE))

int_susp <- int_raw %>% 
  filter(PredStrategy == "Suspension-feeder") %>% 
  count(PredGroup) %>% 
  mutate(PredGroup = fct_reorder(PredGroup, n, .desc = TRUE))

# Fuente de alimentacion
source_count <- int_raw %>% 
  count(FoodSource) %>% 
  mutate(FoodSource = fct_reorder(FoodSource, n, .desc = TRUE))

(plot_source <- ggplot(source_count, aes(x = FoodSource, y = n, fill = FoodSource)) + 
    geom_bar(stat = "identity") +
    labs(x = "Fuente de alimentación", y = "Interacciones tróficas") +
    theme(legend.position = "none",
          axis.title.y = element_text(face = "bold", size = 14),
          axis.title.x = element_text(face = "bold", size = 14),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 14)))

data_plot_het <- int_raw %>% 
  filter(FoodSource == "Heterotroph") %>% 
  mutate(Prey = sub(" .*", "", Prey)) %>% 
  mutate(Predator = sub(" .*", "", Predator)) %>% 
  dplyr::select(c(Prey, PreyGroup, Predator, PredGroup, FoodSource)) %>% 
  add_count(Predator, name = "NumPrey") %>% 
  add_count(PredGroup, name = "NumPredByGroup") %>% 
  filter(NumPrey > 2)

(plot_int_het <- ggplot(data_plot_het, aes(x = NumPrey, y = PredGroup, fill = PredGroup)) + 
    geom_joy(scale = 3.75) +
    scale_fill_manual(values = FGColors) +
    labs(x = "Número de interacciones", y = "Grupo funcional") +
    theme_joy(grid = FALSE) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16)))

int_het <- int_raw %>% 
  filter(FoodSource == "Heterotroph") %>% 
  count(PredGroup) %>% 
  mutate(PredGroup = fct_reorder(PredGroup, n, .desc = TRUE))
int_pp <- int_raw %>% 
  filter(FoodSource == "Primary producer") %>% 
  count(PredGroup) %>% 
  mutate(PredGroup = fct_reorder(PredGroup, n, .desc = TRUE))

# Interacciones baja resolucion
# Necesitan especificacion
int_need <- int_raw %>% filter_at(.vars = vars(Prey, Predator),
                                  .vars_predicate = any_vars(str_detect(., "\\*$")))

int_need_prey <- int_need %>%
  filter_at(.vars = vars(Prey),
            .vars_predicate = any_vars(str_detect(., "\\*$"))) %>% 
  count(PreyGroup) %>%
  mutate(PreyGroup = fct_reorder(PreyGroup, n, .desc = TRUE))
int_need_pred <- int_need %>%
  filter_at(.vars = vars(Predator),
            .vars_predicate = any_vars(str_detect(., "\\*$"))) %>% 
  count(PredGroup) %>%
  mutate(PredGroup = fct_reorder(PredGroup, n, .desc = TRUE))

(plot_need_prey <- ggplot(int_need_prey, aes(x = PreyGroup, y = n, fill = PreyGroup)) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values = FGColors) +
    labs(x = "Presas (grupo funcional)", y = "Número de interacciones") +
    theme(legend.position = "none",
          axis.title.y = element_text(face = "bold", size = 16),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) +
    annotate("text",  x = Inf, y = Inf, label = paste("presas que necesitan resolución = ", sum(int_need_prey$n), sep = ""), vjust=1, hjust=1))
num_int_prey <- sum(int_need_prey$n)

(plot_need_pred <- ggplot(int_need_pred, aes(x = PredGroup, y = n, fill = PredGroup)) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values = FGColors) +
    labs(x = "Depredadores (grupo funcional)", y = "Número de interacciones") +
    theme(legend.position = "none",
          axis.title.y = element_text(face = "bold", size = 16),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
    annotate("text",  x = Inf, y = Inf, label = paste("dep que necesitan resolución = ", sum(int_need_pred$n), sep = ""), vjust=1, hjust=1))
num_int_pred <- sum(int_need_pred$n)


## Listas por GF ----

sort(unique(int_raw$PredGroup))
sort(unique(int_raw$PreyGroup))

IntList_prey <- split(int_raw, f = int_raw$PreyGroup)
IntList_pred <- split(int_raw, f = int_raw$PredGroup)
# keys <- unique(c(names(IntList_prey), names(IntList_pred)))
# IntList_all <- setNames(mapply(c, IntList_prey[keys], IntList_pred[keys]), keys)

# Interaction lists
Amphipoda <- bind_rows(IntList_prey[["Amphipoda"]], IntList_pred[["Amphipoda"]])
Ascidiacea <- bind_rows(IntList_prey[["Ascidiacea"]], IntList_pred[["Ascidiacea"]])
Bacillariophyceae <- bind_rows(IntList_prey[["Bacillariophyceae"]], IntList_pred[["Bacillariophyceae"]])
Bacteria <- bind_rows(IntList_prey[["Bacteria"]], IntList_pred[["Bacteria"]])
Benthos_Misc <- bind_rows(IntList_prey[["Benthos_Misc"]], IntList_pred[["Benthos_Misc"]])
Bivalvia <- bind_rows(IntList_prey[["Bivalvia"]], IntList_pred[["Bivalvia"]])
Brachiopoda <- bind_rows(IntList_prey[["Brachiopoda"]], IntList_pred[["Brachiopoda"]])
Bryozoa <- bind_rows(IntList_prey[["Bryozoa"]], IntList_pred[["Bryozoa"]])
Cephalopoda <- bind_rows(IntList_prey[["Cephalopoda"]], IntList_pred[["Cephalopoda"]])
Ciliates <- bind_rows(IntList_prey[["Ciliates"]], IntList_pred[["Ciliates"]])
Cnidaria <- bind_rows(IntList_prey[["Cnidaria"]], IntList_pred[["Cnidaria"]])
Coccolithophorids <- bind_rows(IntList_prey[["Coccolithophorids"]], IntList_pred[["Coccolithophorids"]])
Cumacea <- bind_rows(IntList_prey[["Cumacea"]], IntList_pred[["Cumacea"]])
Decapoda <- bind_rows(IntList_prey[["Decapoda"]], IntList_pred[["Decapoda"]])
Dinoflagellates <- bind_rows(IntList_prey[["Dinoflagellates"]], IntList_pred[["Dinoflagellates"]])
Echinodermata <- bind_rows(IntList_prey[["Echinodermata"]], IntList_pred[["Echinodermata"]])
Fish_BenPel <- bind_rows(IntList_prey[["Fish_BenPel"]], IntList_pred[["Fish_BenPel"]])
Fish_Demersal <- bind_rows(IntList_prey[["Fish_Demersal"]], IntList_pred[["Fish_Demersal"]])
Fish_Pel <- bind_rows(IntList_prey[["Fish_Pel"]], IntList_pred[["Fish_Pel"]])
Foraminifera <- bind_rows(IntList_prey[["Foraminifera"]], IntList_pred[["Foraminifera"]])
Gastropoda <- bind_rows(IntList_prey[["Gastropoda"]], IntList_pred[["Gastropoda"]])
Isopoda <- bind_rows(IntList_prey[["Isopoda"]], IntList_pred[["Isopoda"]])
Non_living <- bind_rows(IntList_prey[["Non-living"]], IntList_pred[["Non-living"]])
Phytoplankton <- bind_rows(IntList_prey[["Phytoplankton"]], IntList_pred[["Phytoplankton"]])
Picophytoplankton <- bind_rows(IntList_prey[["Picophytoplankton"]], IntList_pred[["Picophytoplankton"]])
Polychaeta <- bind_rows(IntList_prey[["Polychaeta"]], IntList_pred[["Polychaeta"]])
Polyplacophora <- bind_rows(IntList_prey[["Polyplacophora"]], IntList_pred[["Polyplacophora"]])
Porifera <- bind_rows(IntList_prey[["Porifera"]], IntList_pred[["Porifera"]])
SeaBirds <- bind_rows(IntList_prey[["SeaBirds"]], IntList_pred[["SeaBirds"]])
Silicoflagellates <- bind_rows(IntList_prey[["Silicoflagellates"]], IntList_pred[["Silicoflagellates"]])
Zooplankton <- bind_rows(IntList_prey[["Zooplankton"]], IntList_pred[["Zooplankton"]])
MarineMammals_1 <- bind_rows(IntList_prey[["MarineMammals_1"]], IntList_pred[["MarineMammals_1"]])
MarineMammals_2 <- bind_rows(IntList_prey[["MarineMammals_2"]], IntList_pred[["MarineMammals_2"]])
MarineMammals_3 <- bind_rows(IntList_prey[["MarineMammals_3"]], IntList_pred[["MarineMammals_3"]])

Zooplankton_pred <- IntList_pred[["Zooplankton"]]
MarineMammals_all <- bind_rows(MarineMammals_1, MarineMammals_2, MarineMammals_3)
Phytoplankton_all <- bind_rows(Bacillariophyceae, Coccolithophorids, Dinoflagellates, Foraminifera,
                               Phytoplankton, Picophytoplankton, Silicoflagellates)
Larvae_fish <- int_raw %>% 
  subset(., Prey == "Larvae_Fish" | Predator == "Larvae_Fish")
Fish_all <- bind_rows(Fish_BenPel, Fish_Pel, Fish_Demersal)

# Save interactions lists
write.csv(Amphipoda[,-1], file = "interaction_lists/Amphipoda.csv")
write.csv(Ascidiacea[,-1], file = "interaction_lists/Ascidiacea.csv")
write.csv(Bivalvia[,-1], file = "interaction_lists/Bivalvia.csv")
write.csv(Brachiopoda[,-1], file = "interaction_lists/Brachiopoda.csv")
write.csv(Bryozoa[,-1], file = "interaction_lists/Bryozoa.csv")
write.csv(Cephalopoda[,-1], file = "interaction_lists/Cephalopoda.csv")
write.csv(Zooplankton_pred[,-1], file = "interaction_lists/Zooplankton_pred.csv")
write.csv(Cnidaria[,-1], file = "interaction_lists/Cnidaria.csv")
write.csv(Cumacea[,-1], file = "interaction_lists/Cumacea.csv")
write.csv(Larvae_fish[,-1], file = "interaction_lists/Larvae_fish.csv")
write.csv(Fish_all[,-1], file = "interaction_lists/Fish.csv")
write.csv(Gastropoda[,-1], file = "interaction_lists/Gastropoda.csv")
write.csv(Isopoda[,-1], file = "interaction_lists/Isopoda.csv")
write.csv(Polychaeta[,-1], file = "interaction_lists/Polychaeta.csv")
write.csv(Polyplacophora[,-1], file = "interaction_lists/Polyplacophora.csv")
write.csv(SeaBirds[,-1], file = "interaction_lists/SeaBirds.csv")
write.csv(MarineMammals_all[,-1], file = "interaction_lists/MarineMammals.csv")
write.csv(Phytoplankton_all[,-1], file = "interaction_lists/Phytoplankton.csv")
write.csv(Decapoda[,-1], file = "interaction_lists/Decapoda.csv")
write.csv(Echinodermata[,-1], file = "interaction_lists/Echinodermata.csv")
write.csv(Porifera[,-1], file = "interaction_lists/Porifera.csv")
