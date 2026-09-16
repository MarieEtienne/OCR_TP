################# SCRIPT ANALYSES DES COMMUNAUTES #####################

# Chargement des packages
require(vegan)
require(ggplot2)
require(tidyverse)
require(ggdendro)
require(ade4)
require(factoextra)
require(dplyr)
require(GGally)
require(reshape2)


# 1. Préparation des données ##################################################################################

## 1a. Chargement des données ####
data_mat = read.table(
  "Matrice_Odonate.csv",
  sep = ";",
  header = T,
  row.names = 1
)

head(data_mat)

# Pour l'exercice, nous repassons d'une matrice site~espèce à un jeu de données "de terrain".
dta_long <- data_mat %>%
  rownames_to_column("Site") %>%
  pivot_longer(cols = -Site,
               names_to = "Species",
               values_to = "abondance") %>%
  filter(abondance > 0) %>%
  dplyr::select(Site, Species)

head(dta_long)

# Mettre un tableau brute sous forme de matrice site~espèce

data_mat2 = dcast(dta_long, Site ~ Species, length) ## création de la matrice. Le nom des espèces apparaît en nom de colonne
rownames(data_mat2) = data_mat2$Site ## Mettre l'identifiant de la communauté en nom de ligne
data_mat2$Site = NULL ## Supprimer la ligne ID puisque chaque colonne doit représenter une espèce

str(data_mat2)
rownames(data_mat2)


# Jeu de données variables écologiques
data_eco = read.table(
  "Communauté_4km²_biogeo_ZH.csv",
  sep = ";",
  header = T,
  fileEncoding = "latin3"
)

head(data_eco)



## Avant de mesurer les indices, que comprenez-vous du jeu de données ? Quelle est la problématique derrière ce protocole ?
## Quelles sont les hypothèses ?

#
#
#
#
#
#
#
#
#
#
#
#



## Pour la suite, nous allons comparer les communautés végétales en fonction de l'âge de la forêt ?
### Quelle est la classe de la variable ? Quelles sont les différentes modalités ?

## Nous allons coupler cela avec des informations quantitative comme les indices d'Ellenberg.


# 2. Complétude des données #############################################################################

## AVant d'analyser plus en profondeur le jeu de données, nous pouvons évaluer sa complétude.
### Pour cela, nous n'avons pas accès à différentes tailles du quadrats ou à des dates d'inventaires.
### Cependant, avec le nombre d'espèces, nous pouvons nous pencher sur une première approche de complétude.
### Pour cela, il est difficile d'utiliser la méthode Braun-Blanquet, nous allons donc partir d'une matrice de présence/absence.

data_mat_compl = ifelse(data_mat > 0, 1, 0)


? vegan::specaccum()

spec <- specaccum(data_mat_compl, method = "random", permutations = 999) ## specaccum # La méthode "random" tire aléatoirement l'ordre des sites et le répère 999 fois.
plot(spec,
     main = "Courbe d'accumulation d'espèce en fonction du nombre de sites échantillonnés",
     xlab = "Nombre de sites",
     ylab = "Nombre d'espèces")

## Calculer l'indice de Chao pour tout le jeu de données

freq_sp = colSums(data_mat_compl)

? estaccumR()
?estimateR()
Chao1 = estimateR(freq_sp) ## Permet de mesurer la complétude à travers l'équation de Chao

Chao1

Completude  = Chao1[1] / Chao1[2] * 100
print(paste0(
  "La complétude du jeu de données total est de : ",
  round(Completude, 0),
  "%"
))

## Même chose pour chaque massif

data_eco$Plot <- data_eco$Site

for (type in unique(data_eco$Biogeo_max)) {
  data_eco_type = subset(data_eco, Biogeo_max == type)
  droplevels(data_eco_type)
  
  data_mat_type = data_mat[rownames(data_mat) %in% unique(data_eco_type$Plot), ]
  
  data_mat_type_compl = ifelse(data_mat_type > 0, 1, 0)
  
  spec <- specaccum(data_mat_type_compl) ## specaccum
  plot(spec,
       main = type,
       xlab = "Nombre de sites",
       ylab = "Nombre d'espèces")
  
  freq_sp = colSums(data_mat_type_compl)
  Chao1 = estimateR(freq_sp)
  
  Completude  = Chao1[1] / Chao1[2] * 100
  print(paste0(
    "La complétude pour le type ",
    type,
    " est de : ",
    round(Completude, 0),
    "%"
  ))
  
}


## Même chose pour l'âge des forêts




# 3. Indicateurs échelle alpha ####
# Pour rappel, les indicateurs à l'échelle alpha calculent des métriques à l'échelle de chaque communauté individuellement.


## 3a. Richesse spécifique ####

# Si la richesse spécifique est simple à calculer manuellement, si votre jeu de données comportent plusieurs centaines de communautés, il est plus simple d'utiliser une fonction automatique


# data_eco$Forest.continuity=sample(c("Recent", "Old"), 467, replace=T)
# data_eco$EIV_N=sample(c(10:20), 467, replace=T)

##### CALCUL #####

? vegan::specnumber()

richesse = specnumber(data_mat_non_vide) #specnumber() permet de calculer la richess spécifique de chaque communauté
richesse = as.data.frame(richesse)

richesse$Plot = rownames(richesse)

data_eco = merge(data_eco_non_vide, richesse, by = "Plot")

str(data_eco)

library(ggplot2)

ggplot(data_eco_non_vide) +
  aes( x = richesse)+
  geom_bar(fill = '#538f38')+
  ggtitle('Nombre de site par valeur de richesse spécifique')# Visualisation de la distribution de la richesse spécifique dans les communautés

#### Diagram de Venn ###############


# Espèces présentes dans chaque modalité
sp_Lit = data_mat[data_eco$Biogeo_max == "Littoral", ] |>
  colSums() |>
  (\(x) names(x[x > 0]))()

sp_Rel = data_mat[data_eco$Biogeo_max == "Relief", ] |>
  colSums() |>
  (\(x) names(x[x > 0]))()

sp_Pla = data_mat[data_eco$Biogeo_max == "Plaine", ] |>
  colSums() |>
  (\(x) names(x[x > 0]))()

sp_Mer = data_mat[data_eco$Biogeo_max == "Meridional", ] |>
  colSums() |>
  (\(x) names(x[x > 0]))()

venn_list = list(Littoral = sp_Lit, Relief = sp_Rel,Meridional = sp_Mer, Plaine = sp_Pla)

## Représentation en diagramme de Venn

library(ggvenn)
library(eulerr)

ggvenn(
  venn_list,
  fill_color = c("lightblue", "pink", "palegreen", "lightyellow"),
  stroke_size = 1,
  set_name_size = 5
) +
  labs(title = "Richesse spécifique selon la biogéographie")

plot(euler(venn_list), fills = c("lightblue", "pink", "palegreen", "lightyellow"), quantities = TRUE)

##### REPRESENTATION #####

## Représentation de la richesse en fonction d'une variable qualitative
data_eco %>% group_by(Biogeo_max) %>%  ggplot() +
  geom_histogram(aes(x = richesse, fill = Biogeo_max), col =
                   "black") + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  
  xlab("Richesse spécifique") + ylab("Nombre de relevés") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

data_eco %>% group_by(Biogeo_max) %>%  ggplot() +
  geom_histogram(aes(x = richesse, fill = Biogeo_max)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  
  xlab("Richesse spécifique") + ylab("Nombre de relevés") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36)) +
  facet_wrap( ~ Biogeo_max) # Permet de créer un graphe par modalité


data_eco %>%   ggplot() +
  geom_boxplot(aes(x = Biogeo_max , y = richesse, fill = Biogeo_max)) + ylim(0, 42) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  
  xlab("Age Forêt") + ylab("Richesse spécifique") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

## Représentation de la richesse spécifique en fonction d'une variable explicative quantitative : L'Indice d'Ellenberg Azote

data_eco %>%  ggplot() +
  geom_point(aes(x = EIV_N, y = richesse)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  
  ylab("Richesse spécifique") + xlab("Indice d'Ellenberg Azote") + ylim(0, 42) +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

### En ajoutant une première relation
data_eco %>%  ggplot() +
  geom_point(aes(x = EIV_N, y = richesse)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  geom_smooth(
    aes(x = EIV_N, y = richesse),
    col = "darkgreen",
    fill = "lightgreen",
    size = 3,
    method = "lm",
    linetype = "dashed"
  ) +
  ylab("Richesse spécifique") + xlab("Indice d'Ellenberg Azote") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

## En ajoutant la variable qualitative
data_eco %>% group_by %>%  ggplot() +
  geom_point(aes(x = EIV_N, y = richesse, fill = Forest.continuity)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  geom_smooth(
    aes(
      x = EIV_N,
      y = richesse,
      col = Forest.continuity,
      fill = Forest.continuity
    ),
    size = 3,
    method = "lm",
    linetype = "dashed"
  ) + ylim(0, 42) +
  ylab("Richesse spécifique") + xlab("Indice d'Ellenberg Azote") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))


# Avec la richesse spécifique, nous pouvons déjà approcher une certaine compréhension de la répartion des communautés.
# Ici que constatez-vous ?




## 3b. Abondance ####

##### CALCUL #####

abondance = rowSums(data_mat_non_vide %>% select(-Plot, -tot)) # Permet de mesurer le nombre d'individu dans chaque communauté. En utilisant colSums, il est possible d'avoir le nombre d'observation de chaque espèce
abondance = as.data.frame(abondance)
abondance$Plot = rownames(abondance)

data_eco = merge(data_eco_non_vide, abondance, by = "Plot")

hist(colSums(data_mat))

ggplot(data_eco_non_vide) +
  aes( x = abondance)+
  geom_bar(fill = '#538f38')+
  ggtitle("Nombre de sites par valeur d'abondance")# Visualisation de la distribution de la richesse spécifique dans les communautés


##### REPRESENTATION #####


## Pour faire un diagramme rang fréquence :

### Méthode automatique
# require(BiodiversityR)
# BiodiversityR::rankabunplot(data_mat)

### Méthode mannuelle
abund <- colSums(data_mat)
abund = as.data.frame(abund)
abund$Species = as.vector(colnames(data_mat))
abund = abund[order(-abund$abund), ]
abund$Rang <- 1:nrow(abund)

write.csv2(abund, file = "abondance.csv")

ggplot(abund, aes(Rang, abund, label = Species)) +
  geom_col(fill = "darkgreen") +
  xlab("Rang des espèces") + ylab("Abondance") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 10)) + theme(axis.title.y = element_text(size =
                                                                                      10)) + theme(axis.text.x = element_text(size = 10)) +
  theme(axis.text.y = element_text(size = 10)) +
  ggrepel::geom_label_repel(size = 3)



## 3c. Diversité spécifique ####

# La diversité spécifique mixe les deux informations précédentes : nombre d'espèce & équitabilité

### A. Indice de Shannon #######

###### CALCUL ######



## Mesurer manuellement
Espece = c()
Pi = c()
LOG_Pi = c()
Ind_sha_indiv = c()

# for (com in 1:nrow(data_mat)){

data_mat_indiv = data_mat_non_vide[19, ] # Ne récupérer qu'une seule communauté pour l'exemple



data_mat_indiv = droplevels(data_mat_indiv)## Permet d'enlever les modalités "fantômes"


summary(data_mat_indiv)

for (esp in (colnames(data_mat_indiv))) {
  data_mat_indiv_esp = data_mat_indiv[, esp]
  
  if (data_mat_indiv_esp == 0) {
    # Passer à la prochaine espèce si l'espèce n'est pas présente
    next
  }
  
  abond_relative = data_mat_indiv_esp / sum(data_mat_indiv) # Mesure de l'abondance relative de chaque espèce
  
  logar = log10(abond_relative)
  
  indice_comm = abond_relative * logar
  
  Espece = c(Espece, esp)
  Pi = c(Pi, abond_relative)
  LOG_Pi = c(LOG_Pi, logar)
  Ind_sha_indiv = c(Ind_sha_indiv, indice_comm)
  
  Shannon_unique_community = data.frame(
    Espece = Espece,
    Abondance_relative = Pi,
    Log_abondance = LOG_Pi,
    Shannon_espece = Ind_sha_indiv
  )
}

Shannon_unique_community

Shannon = -sum(Ind_sha_indiv)

Shannon

# data_eco$shannon[com,]=Shannon

# }


## Mesurer par une fonction

shannon = vegan::diversity(data_mat_non_vide %>% select(-tot, -Plot), index = "shannon") #diversity() permet de calculer différents indices pour la diversité spécifique

hist(shannon)

shannon = as.data.frame(shannon)
shannon$Plot = rownames(shannon)

data_eco = merge(data_eco_non_vide, shannon, by = "Plot")

ggplot(data_eco_non_vide) +
  aes(x = shannon)+
  geom_histogram(fill = '#538f38')+
  ggtitle("Histograme de Shannon")# Visualisation de la distribution de la richesse spécifique dans les communautés


##### REPRESENTATION #####

## Représentation de Shannon en fonction d'une variable qualitative
data_eco %>% group_by(Biogeo_max) %>%  ggplot() +
  geom_histogram(aes(x = shannon, fill = Biogeo_max), col = "black") + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  
  xlab("Indice de Shannon") + ylab("Nombre") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

data_eco %>% group_by(Biogeo_max) %>%  ggplot() +
  geom_histogram(aes(x = shannon, fill = Biogeo_max), col = "black") + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  
  xlab("Indice de Shannon") + ylab("Nombre") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36)) +
  facet_wrap( ~ Biogeo_max) # Permet de créer un graphe par modalité


data_eco %>%   ggplot() +
  geom_boxplot(aes(x = Biogeo_max , y = shannon, fill = Biogeo_max)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  
  xlab("Age Forêt") + ylab("Diversité de Shannon") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))


## Comment interpréter écologiquement ce résultat ?


## Représentation de la richesse spécifique en fonction d'une variable explicative quantitative

data_eco %>%  ggplot() +
  geom_point(aes(x = EIV_N, y = shannon)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  
  ylab("Indice de Shannon") + xlab("Indice d'Ellenberg Azote") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

### En ajoutant une première relation
data_eco %>%  ggplot() +
  geom_point(aes(x = EIV_N, y = shannon)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  geom_smooth(
    aes(x = EIV_N, y = shannon),
    col = "darkgreen",
    fill = "lightgreen",
    size = 3,
    method = "lm",
    linetype = "dashed"
  ) +
  ylab("Indice de Shannon") + xlab("Indice d'Ellenberg Azote") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

## En ajoutant la variable qualitative
data_eco %>% group_by %>%  ggplot() +
  geom_point(aes(x = EIV_N, y = shannon, fill = Biogeo_max)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  geom_smooth(
    aes(
      x = EIV_N,
      y = shannon,
      col = Forest.continuity,
      fill = Forest.continuity
    ),
    size = 3,
    method = "lm",
    linetype = "dashed"
  ) +
  ylab("Indice de Shannon") + xlab("Indice d'Ellenberg Azote") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

data_eco %>% group_by %>%  ggplot() +
  geom_point(aes(x = EIV_N, y = shannon, fill = Forest.continuity)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  geom_smooth(
    aes(
      x = EIV_N,
      y = shannon,
      col = Forest.continuity,
      fill = Forest.continuity
    ),
    size = 3,
    method = "lm",
    linetype = "dashed"
  ) +
  ylab("Indice de Shannon") + xlab("Indice d'Ellenberg Azote") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36)) +
  facet_wrap( ~ Forest.continuity)

# Ici que constatez-vous ?


### B. Indice de Simpson #######

###### CALCUL ######



## Mesurer par une fonction

simpson = vegan::diversity(data_mat_non_vide %>% select(-tot,-Plot), index = "simpson") #diversity() permet de calculer différents indices pour la diversité spécifique

hist(simpson)

simpson = as.data.frame(simpson)
simpson$Plot = rownames(simpson)

data_eco_non_vide = merge(data_eco_non_vide, simpson, by = "Plot")

ggplot(data_eco_non_vide %>% select(simpson)) +
  aes(x = simpson)+
  geom_histogram(fill = '#538f38', bins = 10)+
  ggtitle("Histograme de Simpson")

##### REPRESENTATION #####

## Représentation de simpson en fonction d'une variable qualitative

data_eco %>%   ggplot() +
  geom_boxplot(aes(x = Forest.continuity , y = simpson, fill = Forest.continuity)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  
  xlab("Age Forêt") + ylab("Diversité de Simpson") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))


data_eco %>% group_by(Forest.continuity) %>%  ggplot() +
  geom_histogram(aes(x = simpson, fill = Forest.continuity), col = "black") + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  
  xlab("Indice de simpson") + ylab("Nombre de relevés") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))


## Représentation de la richesse spécifique en fonction d'une variable explicative quantitative

data_eco %>%  ggplot() +
  geom_point(aes(x = EIV_N, y = simpson)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  
  ylab("Indice de simpson") + xlab("Indice d'Ellenberg Azote") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

### En ajoutant une première relation
data_eco %>%  ggplot() +
  geom_point(aes(x = EIV_N, y = simpson)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  geom_smooth(
    aes(x = EIV_N, y = simpson),
    col = "darkgreen",
    fill = "lightgreen",
    size = 3,
    method = "lm",
    linetype = "dashed"
  ) +
  ylab("Indice de simpson") + xlab("Indice d'Ellenberg Azote") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

## En ajoutant la variable qualitative
data_eco %>% group_by %>%  ggplot() +
  geom_point(aes(x = EIV_N, y = simpson, fill = Forest.continuity)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  geom_smooth(
    aes(
      x = EIV_N,
      y = simpson,
      col = Forest.continuity,
      fill = Forest.continuity
    ),
    size = 3,
    method = "lm",
    linetype = "dashed"
  ) +
  ylab("Indice de simpson") + xlab("Indice d'Ellenberg Azote") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))


# Ici que constatez-vous ?


### C. Comparaison Shannon vs. Simpson #######

print(paste0(
  "La corrélation entre les deux indices est de : ",
  round(cor(data_eco$shannon, data_eco$simpson), 2) * 100,
  "%"
))

data_eco %>%  ggplot() +
  geom_point(aes(x = shannon, y = simpson)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  geom_smooth(
    aes(x = shannon, y = simpson),
    col = "darkgreen",
    fill = "lightgreen",
    size = 3,
    method = "lm",
    linetype = "dashed"
  ) +
  ylab("Indice de Simpson") + xlab("Indice de Shannon") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

data_eco %>%  ggplot() +
  geom_point(aes(x = shannon, y = simpson, col = Forest.continuity)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  geom_smooth(
    aes(
      x = shannon,
      y = simpson,
      col = Forest.continuity,
      fill = Forest.continuity
    ),
    size = 3,
    method = "lm",
    linetype = "dashed"
  ) +
  ylab("Indice de Simpson") + xlab("Indice de Shannon") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

require(ggforce)
data_eco %>%  ggplot() +
  geom_point(aes(x = shannon, y = simpson, col = Forest.continuity)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  stat_ellipse(
    aes(
      x = shannon,
      y = simpson,
      col = Forest.continuity,
      fill = Forest.continuity
    ),
    size = 3,
    method = "lm",
    linetype = "dashed"
  ) +
  ylab("Indice de Simpson") + xlab("Indice de Shannon") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

## 3d. Equitabilité ####

# Pour rappel, l'équitabilité permet de mesurer l'homogénéité des individus et espèces au sein des communautés
# Un moyen de mesurer cette homogénéité est d'utilise l'indicateur de Piélou
# Aucune fonction automatique n'existe pour mesurer l'indice de Piélou, mais il est très simple à faire à la main

###### CALCUL ######
data_eco$pielou = 0

for (i in 1:nrow(data_eco)) {
  data_eco$pielou[i] = data_eco$shannon[i] / log(data_eco$richesse)[i] # Attention d'être dans la même "base" que celle utilisée par la fonction diversity() lors du calcul de shannon
}


hist(data_eco$pielou)

##### REPRESENTATION #####

## Représentation de pielou en fonction d'une variable qualitative
data_eco %>% group_by(Biogeo_max) %>%  ggplot() +
  geom_histogram(aes(x = pielou, fill = Biogeo_max), col = "black") + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  
  xlab("Indice de pielou") + ylab("Nombre de relevés") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

data_eco %>%   ggplot() +
  geom_boxplot(aes(x = Biogeo_max , y = pielou, fill = Biogeo_max)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  
  xlab("Age Forêt") + ylab("Indice de pielou") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))


## Représentation de la richesse spécifique en fonction d'une variable explicative quantitative

data_eco %>%  ggplot() +
  geom_point(aes(x = EIV_N, y = pielou)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  
  ylab("Indice de pielou") + xlab("Indice d'Ellenberg Azote") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

### En ajoutant une première relation
data_eco %>%  ggplot() +
  geom_point(aes(x = EIV_N, y = pielou)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  geom_smooth(
    aes(x = EIV_N, y = pielou),
    col = "darkgreen",
    fill = "lightgreen",
    size = 3,
    method = "lm",
    linetype = "dashed"
  ) +
  ylab("Indice de pielou") + xlab("Indice d'Ellenberg Azote") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

## En ajoutant la variable qualitative
data_eco %>% group_by %>%  ggplot() +
  geom_point(aes(x = EIV_N, y = pielou, fill = Forest.continuity)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  geom_smooth(
    aes(
      x = EIV_N,
      y = pielou,
      col = Forest.continuity,
      fill = Forest.continuity
    ),
    size = 3,
    method = "lm",
    linetype = "dashed"
  ) +
  ylab("Indice de pielou") + xlab("Indice d'Ellenberg Azote") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36))

data_eco %>% group_by %>%  ggplot() +
  geom_point(aes(x = EIV_N, y = pielou, fill = Biogeo_max)) + # Permet de visualiser la richesse spécifique en fonction d'une variable explicative
  geom_smooth(
    aes(
      x = EIV_N,
      y = pielou,
      col = Biogeo_max,
      fill = Biogeo_max
    ),
    size = 3,
    method = "lm",
    linetype = "dashed"
  ) +
  ylab("Indice de pielou") + xlab("Indice d'Ellenberg Azote") +
  theme(
    panel.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    panel.border = element_rect(size = 4, fill = NA)
  ) +
  theme(axis.title.x = element_text(size = 36)) + theme(axis.title.y = element_text(size =
                                                                                      36)) + theme(axis.text.x = element_text(size = 36)) +
  theme(axis.text.y = element_text(size = 36)) +
  facet_wrap( ~ Forest.continuity)

# Ici que constatez-vous ?







# 4. Indicateurs échelle beta #################################################################################

## 4.a Matrice de dissimilarité ####
### A Jaccard ####

data_mat$tot <- rowSums(data_mat, na.rm=TRUE)
data_mat$Plot <- abondance$Plot

data_mat_non_vide <- data_mat %>% 
  filter(tot != 0)


jaccard = vegdist(
  data_mat_non_vide %>% select(-Plot),
  method = "jaccard",
  binary = T,
  diag = F,
  upper = T
) ## Fonction du package vegan. binary = T permet de transformer la matrice site~espèce en présence/absence

mat_jaccard = as.matrix(jaccard) ## transformer l'objet 'dist' en matrice pour visualiser. Cela n'est pas nécessaire pour utiliser l'objet dans les analyses multivariées.

mat_jaccard_inf = lower.tri(mat_jaccard) # Ne prendre que le triangle inférieur de la matrice.

dta_jaccard = data.frame(
  # Permet de visualiser le résultat paire par paire
  siteA = rownames(mat_jaccard)[row(mat_jaccard)[mat_jaccard_inf]],
  siteB = colnames(mat_jaccard)[col(mat_jaccard)[mat_jaccard_inf]],
  distance = mat_jaccard[mat_jaccard_inf]
)

dta_jaccard


## Autre méthode pour calculer la matrice de dissimilarité :

require(betapart)

data_mat2 = data_mat
data_mat2[data_mat2 > 0] = 1 # Autre moyen pour transformer la matrice en présence/absence

jaccard2 = beta.pair(data_mat2, index.family = 'jaccard')

jaccard2 # Une liste à trois tableaux : beta.jac = dissimilarité de jaccard ; beta.jtu = partie "turnover" de la dissimilarité ; beta.jne = partie "nestedness" de la dissimilarité

require(reshape2)

dta_jaccard2 = melt(as.matrix(jaccard2$beta.jac), varnames = c("ID_row", "ID_col"))
dta_jaccard2


### B. Bray-Curtis ####
bray = vegdist(data_mat,
               method = "bray",
               diag = F,
               upper = T) ## Fonction du package vegan. binary = T permet de transformer la matrice site~espèce en présence/absence

mat_bray = as.matrix(bray) ## transformer l'objet 'dist' en matrice pour visualiser. Cela n'est pas nécessaire pour utiliser l'objet dans les analyses multivariées.

mat_bray_inf = lower.tri(mat_bray) # Ne prendre que le triangle inférieur de la matrice.

dta_bray = data.frame(
  # Permet de visualiser le résultat paire par paire
  siteA = rownames(mat_bray)[row(mat_bray)[mat_bray_inf]],
  siteB = colnames(mat_bray)[col(mat_bray)[mat_bray_inf]],
  distance = mat_bray[mat_bray_inf]
)

dta_bray


## Autre méthode pour calculer la matrice de dissimilarité :

require(betapart)


bray2 = beta.pair.abund(data_mat, index.family = 'bray')

bray2

require(reshape2)

dta_bray2 = melt(as.matrix(bray2$beta.bray), varnames = c("ID_row", "ID_col"))
dta_bray2

## 4.b Dendrogramme par Classification Ascendante Hierarchique (CAH) ####

arbre_jaccard = hclust(jaccard) ## Ici, nous avons besoin de l'objet 'dist' et non d'une matrice ou un data.frame

arbre_jaccard %>% ggdendrogram() + theme(
  text = element_text(size = 14),
  # Taille du texte global
  axis.text = element_text(size = 12),
  # Taille du texte des axes
  axis.title = element_text(size = 14)  # Taille des titres des axes
) +
  geom_segment(size = 5)

### Pour colorer les sites en fonction d'une variable facteur
library(dendextend)

cols = c("blue", "red", "forestgreen", "orange")



data_eco_non_vide <- data_eco %>% 
  filter(data_eco$Plot %in% data_mat_non_vide$Plot)

data_mat_non_vide <- data_mat_non_vide %>%
  filter(data_mat_non_vide$Plot %in% data_eco_non_vide$Plot)

groupe = factor(data_eco_non_vide$Biogeo_max)
lab_cols = cols[groupe]
groupe

dend <- as.dendrogram(arbre_jaccard)

labels_colors(dend) <- lab_cols[order.dendrogram(dend)]
labels_colors(dend)
# dend <- color_branches(dend, k = 2, col=c("blue","red"), groupLabels = unique(data_eco$Forest.continuity))
# plot
plot(dend)


# Passage en ggplot2
ddata = dendro_data(dend, type = "rectangle")

# couleurs des labels dans le bon ordre
label_colors = data.frame(label = labels(dend), col = labels_colors(dend))

# graphique
ggplot() +
  # branches
  geom_segment(
    data = segment(ddata),
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    linewidth = 0.8,
    color = "grey30"
  ) +
  
  # labels colorés
  geom_text(
    data = label(ddata) |>  merge(label_colors, by = "label"),
    aes(
      x = x,
      y = y - 0.02 * max(segment(ddata)$y),
      label = label,
      color = col
    ),
    angle = 45,
    hjust = 1,
    size = 2,
    show.legend = FALSE
  ) +
  
  scale_color_identity() +
  labs(title = "Dendrogramme Jaccard", x = "", y = "Distance") +
  
  theme_minimal(base_size = 14) +
  
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )




### Avant de se lancer dans des analyses multivariées, que constatez-vous à travers la représentation de la dissimilarité sur une dimmension

## 4.c Analyse multivariée : #####

### A. Analyse factorielle des correspondances (AFC) ####

###### CALCUL ######

require(ade4)

? dudi.coa()

data_mat_non_vide <- data_mat_non_vide %>% 
  filter(data_mat_non_vide$Plot != 'Com_15397' & 
           data_mat_non_vide$Plot != 'Com_21373' &
           data_mat_non_vide$Plot != 'Com_11223')

data_eco_non_vide <- data_eco_non_vide %>% 
  filter(data_eco_non_vide$Plot != 'Com_15397'& 
           data_eco_non_vide$Plot != 'Com_21373' &
           data_eco_non_vide$Plot != 'Com_11223')

afc_dta = dudi.coa(data_mat_non_vide, scannf = T) ## Fonction pour produire l'afc. 'scannf' permet d'indiquer si le graphe des valeurs propres doit être affichés ou non.
## Si oui, alors il faudra déterminer ensuite le nombre d'axe à garder. Sinon, 'nf' permet d'indiquer le nombre d'axes dès le départ
## Pour choisir le nombre d'axes à garder, vous pouvez employer la méthode du "coude". Attention, ce coude n'est pas toujours présent.

afc_dta$tab # Matrice site~espèce standardisée
afc_dta$eig # Valeurs propres

afc_dta$li # Coordonnées des sites sur les différents axes
afc_dta$co # Coordonnées des espèces sur les différents axes

##### REPRESENTATION #####

require(factoextra)

?fviz_ca

afc_all = fviz_ca(afc_dta, axes = c(1, 2), repel = TRUE)  ## Représentation de l'ensemble des sites et espèces en même temps sur les deux premiers axes
afc_all

afc_site = fviz_ca_row(afc_dta, axes = c(1, 2), repel = TRUE)  ## Représentation des sites uniquement
afc_site

afc_esp = fviz_ca_col(afc_dta, axes = c(1, 2), repel = TRUE)  ## Représentation des sespèces uniquement
afc_esp



### Nous pouvons également colorer les sites à partir de la variable d'ancienneté de la forêt
# coloration des points
afc_site = fviz_ca_row(
  afc_dta,
  repel = FALSE,
  axes = c(1, 2),
  geom = "text",
  habillage = groupe,
  palette = c("blue", "red", 'forestgreen', 'orange'),
  # ellipses
  addEllipses = TRUE,
  ellipse.level = 0.95,
  mean.point = FALSE
) +
  
  # thème ggplot
  theme_minimal(base_size = 10) +
  labs(title = "AFC des sites", color = "Biogéographie") +
  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# affichage
afc_site

### B. PCOA #####
###### CALCUL ######

dta_pco = dudi.pco(jaccard) ## La PCOA représente les dissimilarités dans un espace euclidien. Or, la dissimilarité de Bray-Curtis n'est pas métrique.
## C'est pour cela que des valeurs propres sont négatives, et donc qu'il y a une distorsion des distances sur la PCOA
## Pour retrouver une distance euclidienne et résoudre le problème, une solution est d'utiliser la racine carrée
dta_pco = dudi.pco(sqrt(jaccard))


###### REPRESENTATION ######

## Projection des mares
s.label(dta_pco$li, boxes = F)

groupe = factor(data_eco_non_vide$Biogeo_max)
cols = c('blue', 'red',"forestgreen", "orange")

# s.label(dta_pco$li, fac = groupe, col = cols, cellipse = 1.5, cstar = 0, clabel = 0.8)


coord <- dta_pco$li
coord$Site <- rownames(coord)
coord <- coord %>% 
  filter(coord$Site %in% data_eco_non_vide$Site)
coord$Groupe <- factor(data_eco_non_vide$Biogeo_max)

ggplot(coord, aes(A1, A2, label = Site, colour = Groupe)) +
  stat_ellipse() +
  scale_colour_manual(values = c('blue', 'forestgreen',"orange", "red")) +
  theme_minimal()

### Représentation des espèces commes des variables supplémentaires
sp = supcol(dta_pco, data_mat_non_vide)
s.label(dta_pco$co)
sp = list('tabsup' = data_mat_non_vide %>% select(-tot, -Plot), 'cosup' = dta_pco$co)
s.arrow(sp$cosup, boxes = T)

### C. NMDS #####

###### CALCUL ######

mds_pa_taxo_J <- metaMDS(data_mat_non_vide %>% select(-tot, -Plot), distance = "jaccard", trymax = 20)
mds_pa_taxo_J <- metaMDS(jaccard, distance = "jaccard", trymax = 100)

stressplot(mds_pa_taxo_J)

##### REPRESENTATION #####

##V1
ordiplot(mds_pa_taxo_J, type = "text")

##V2
ord <-  ordiplot(mds_pa_taxo_J, type = "n") |>
  points("sites", col = c('red', 'blue',"orange", "forestgreen")[factor(data_eco_non_vide$Biogeo_max
  )])

factor(data_eco_non_vide$Biogeo_max)

?ordiplot
text(
  ord,
  col = c('blue', 'red',"forestgreen", "orange")[factor(data_eco_non_vide$Biogeo_max
  )]
)

# ordiellipse(
#   mds_pa_taxo_J,
#   groups = data_eco$Forest.continuity, kind = "se",
#   conf = 0.95,
#   draw = "polygon",
#   border = c("forestgreen", "orange"),
#   col = adjustcolor(c("forestgreen", "orange"), alpha.f = 0.2)
# )
#
ordihull(
  mds_pa_taxo_J,
  groups = groupe,
  draw = "polygon",
  border = c("forestgreen", "orange")
)

ordispider(mds_pa_taxo_J,
           groups = groupe,
           col = c("forestgreen", "orange"))

## Avec bray curtis
##V1
ordiplot(mds_pa_taxo_B, type = "text")

##V2
ord <-  ordiplot(mds_pa_taxo_B)

text(
  ord,
  display = "sites",
  what = "sites",
  col = c("forestgreen", "orange")[factor(data_eco$Forest.continuity)]
)

# ordiellipse(
#   mds_pa_taxo_J,
#   groups = data_eco$Forest.continuity, kind = "se",
#   conf = 0.95,
#   draw = "polygon",
#   border = c("forestgreen", "orange"),
#   col = adjustcolor(c("forestgreen", "orange"), alpha.f = 0.2)
# )
#
ordihull(
  mds_pa_taxo_B,
  groups = groupe,
  draw = "polygon",
  border = c("forestgreen", "orange")
)

ordispider(mds_pa_taxo_B,
           groups = groupe,
           col = c("forestgreen", "orange"))
## Version Finale
NMDS_taxo <- data.frame(NMDS1 = mds_pa_taxo_J$points[, 1], NMDS2 = mds_pa_taxo_J$points[, 2])

scatterPlot <- ggplot(NMDS_taxo,
                      aes(NMDS1, NMDS2, color = data_eco$Forest.continuity)) +
  stat_density2d(alpha = 0.5) + #h=0.3) +
  geom_point(size = 3.5) +
  scale_color_manual(values = c("#996600", "#669900")) +
  theme_bw() +
  xlim(-0.6, 0.65) +
  ylim(-0.7, 0.5)

scatterPlot <-  scatterPlot + theme (
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = "none",
  strip.text.x = element_blank (),
  strip.text.y = element_blank (),
  strip.background = element_blank(),
  axis.title.y = element_text(size =
                                8, face = 1, hjust = 0.5),
  axis.title.x = element_text(size =
                                8, face = 1, hjust = 0.5),
  axis.text.x = element_text(size =
                               8, face = 1),
  title = element_text(
    size = 8,
    face = 2,
    hjust = -0.06
  ),
  axis.text.y = element_text(angle =
                               90),
  text = element_text(size =
                        8)
)

# Marginal density plot of x (top panel)
xdensity <- ggplot(NMDS_taxo, aes(NMDS1, fill = data_eco$Forest.continuity)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("#996600", "#669900")) +
  theme(legend.position = "none")  +
  theme_classic() +
  xlim(-0.6, 0.65)

xdensity <-  xdensity + theme (
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = "none",
  strip.text.x = element_blank (),
  strip.text.y = element_blank (),
  strip.background = element_blank(),
  axis.title.y = element_text(size =
                                8, face = 1, hjust = 0.5),
  axis.title.x = element_blank (),
  axis.text.x = element_text(size =
                               8, face = 1),
  title = element_text(),
  axis.text.y = element_text(angle =
                               90),
  text = element_text(size = 8)
)

# Marginal density plot of y (right panel)
ydensity <- ggplot(NMDS_taxo, aes(NMDS2, fill = data_eco$Forest.continuity)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("#996600", "#669900")) +
  theme(legend.position = "none")  +
  theme_classic() +
  coord_flip() +
  xlim(-0.7, 0.5)

ydensity <-  ydensity + theme (
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = "none",
  strip.text.x = element_blank (),
  strip.text.y = element_blank (),
  strip.background = element_blank(),
  axis.title.y = element_blank(),
  axis.title.x = element_text(size =
                                8, face = 1, hjust = 0.5),
  axis.text.x = element_text(size =
                               8, face = 1),
  title = element_text(),
  axis.text.y = element_text(angle =
                               90),
  text = element_text(size = 8)
)

# Create a blank placeholder plot :

blankPlot <- ggplot() + geom_blank(aes(1, 1)) +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

require(gridExtra)
nmds_taxo <- grid.arrange(
  xdensity,
  blankPlot,
  scatterPlot,
  ydensity,
  ncol = 2,
  nrow = 2,
  widths = c(4, 1.4),
  heights = c(1.4, 4)
)


### D. PERMANOVA #####

###### CALCUL ######
adonis2(jaccard2$beta.jac ~ data_eco$Forest.continuity, permutation = 999) ## Permet de réaliser la PERMANOVA

intra = betadisper(jaccard2$beta.jac, data_eco$Forest.continuity) ## Mais avant de l'interpréter, il faut vérifier l'homoscédasticité

## Réaliser le test
anova(intra)

## Test de permutation
permutest(intra, pairwise = TRUE, permutations = 999)    ## La dispersion intra-groupe n'est pas homogène, il faut donc faire attention à l'interprétation
## Une partie du signal détecté par la PERMANOVA peut provenir :
### d'une différence de composition,
### mais aussi d'une différence de dispersion.
## La PERMANOVA est alors plus difficile à interpréter sans visualisation.

boxplot(intra) ## Permet de comprendre quel est la modalité avec les communautés les plus dispersées
## Pour continuer à visualiser ces résultats, il faut se référer à la PCOA ou à la NMDS
