###################################################################################################
###################################################################################################
###################################################################################################
#Author: Henri Cuny
#Date: 04/02/2025
#This code is designed to help potential users import and manipulate the XyloDensMap dataset provided at https://doi.org/10.6084/m9.figshare.c.7515396
#It shows how to import data and some examples of data manipulation
#It has been created using R-studio version 2024.09.0 and R version 4.3.2
#Packages required: data.table, ggplot2, maps
###################################################################################################
###################################################################################################
###################################################################################################


#########################
#Preparing R environment#
#########################

#Importing needed packages
#.libPaths("D:/Espace_Travail/R/win-library/4.0.3") #Specify library access path if necessary
library(data.table)
library(ggplot2)
library(maps)


#####################################
#1.Importing the XyloDensMap dataset#
#####################################

#Download the files from https://doi.org/10.6084/m9.figshare.c.7515396

#Setting the directory where you have saved the files
setwd("D:/Espace_Travail/Projets/XyloDensMap/Article_data_paper/BDD")

#Importing the file with individual wood density measurements (xdm_individual_density_data.csv file)
XDM_individual_density_data = data.table(read.table("xdm_individual_density_data.csv", sep = ",", dec = ".", header = TRUE))

#Importing the file with wood density statistics at species level (XDM_species_density_data.csv file)
XDM_species_density_data = data.table(read.table("xdm_species_density_data.csv", sep = ",", dec = ".", header = TRUE))

#Importing the file with individual volumetric shrinkage measurements (xdm_individual_shrinkage_data.csv file)
XDM_individual_shrinkage_data = data.table(read.table("xdm_individual_shrinkage_data.csv", sep = ",", dec = ".", header = TRUE))


#############################################################################################
#2.Examples of basic operations on the table containing individual wood density measurements#
#############################################################################################

#Checking the number of individual observations in the individual wood density dataset
nrow(XDM_individual_density_data) #110,763 individual measurements of wood density in the dataset

#Checking the number of forest plots
nlevels(as.factor(XDM_individual_density_data$Plot_ID)) #Increments cores come from 20,697 forest plots of the French NFI

#Checking the number of tree species
nlevels(as.factor(XDM_individual_density_data$Species)) #156 species in the dataset
nrow(XDM_individual_density_data[is.na(Species)]) #75 rows for which species = NA; this corresponds to rare cases where the species could not be identified in the field

#Calculating the number of observations and the weighted mean basic wood density (WDbw) by species (for species with N > 10)
XDM_individual_density_data[, ":="(N_Cores = .N), by = c("Botanical_Class", "Species")] #Adding the information on the number of observations for each species in the individual wood density measurements dataset
data_species = XDM_individual_density_data[N_Cores > 10, .(N_Cores = .N, Mean = round(mean(WDbw), 1)), by = c("Botanical_Class", "Species")] #Calculating the mean wood density for every species with N_Cores > 10
data_species[order(-N_Cores)] #Order file by decreasing number of observations

#Comparing these statistics with the ones given in the table with wood density statistics at species level provided in the dataset
data_species[Species == "Quercus robur"]
XDM_species_density_data[Species == "Quercus robur", c("Botanical_Class", "Species", "N_Cores", "Mean")]
#It's exactly the same; the file "xdm_species_density_data.csv" provided in the XyloDensMap collection already contains statistics calculated by species for the weighted mean basic wood density


#########################################################################
#3.Examples of visualizations using individual wood density measurements#
#########################################################################

#Boxplots of wood density (weighted mean basic wood density) by species (species with at least 1000 measurements)
ggplot(data = XDM_individual_density_data[N_Cores > 1000], aes(x = reorder(Species, WDbw, FUN = median, order=TRUE), y = WDbw, fill = Species)) +
  geom_boxplot(show.legend = FALSE) +
  coord_flip() +
  facet_grid(rows = vars(Botanical_Class), scales = "free_y", space = "free") +
  xlab(label = "Species") +
  ylab(label = bquote('Wood density '(kg.m^-3)))

#Map of the average wood density (weighted mean basic wood density) on each NFI forest plot (using approximate coordinates, as the exact coordinates of the NFI plots are not available for statistical confidentiality reasons)
france = map_data('france') #Extracting the map of France using the map_data function of maps package
XDM_map_density_data = XDM_individual_density_data[, .(mean = mean(WDbw)), by = c("Plot_ID", "X", "Y")] #Creating a new table with the average wood density (weighted mean basic wood density) for each NFI forest plot
ggplot(data = france, aes(x = long,y = lat, group = group)) + #Drawing the map
  geom_polygon(fill = "white", colour = "black") +
  geom_point(data = XDM_map_density_data, size = 0.5, aes(x = X, y = Y, group = 1, colour = mean)) +
  scale_color_gradient2(low = "blue", mid = "tan", high = "red", midpoint = 600)

#Scatter plot of the relationship between tree diameter at breast height (DBH) and wood density in Pinus nigra
ggplot(data = XDM_individual_density_data[Species == "Pinus nigra"], aes(x = DBH, y = WDbw)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10)) + #Adding a smooth line based on generalized additive models
  xlab(label = "Diameter at breast height (m)") +
  ylab(label = bquote('Wood density '(kg.m^-3)))

#Scatter plot of the relationship between the mean basic wood density and the weighted mean basic wood density in Picea abies
ggplot(data = XDM_individual_density_data[Species == "Picea abies"], aes(x = WDb, y = WDbw)) +
  geom_point() +
  geom_smooth(method = "lm") + #Adding the linear regression line
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") + #Adding the Y=X line
  xlab(label = bquote('Mean basic wood density '(kg.m^-3))) +
ylab(label = bquote('Weighted mean basic wood density '(kg.m^-3)))

###################################################################################
#4.Examples of biomass calculation using the species means calculated above or the#
#precalculated species means provided in the XDM_species_density_data.csv file#
###################################################################################

#Example 1: Consider a forest plot with 30 m3 of above-ground volume, including 10 m3 of Picea abies, 15 m3 of Abies alba and 5 m3 of Fagus sylvatica
#The biomass can be easily estimated using the species mean wood density:
(10*XDM_species_density_data[Species == "Picea abies", Mean] +
   15*XDM_species_density_data[Species == "Abies alba", Mean] +
   5*XDM_species_density_data[Species == "Fagus sylvatica", Mean])/1000 #Biomass = 13.27 tonnes of dry matter (don't forget to divide by 1000 if you want the biomass in tons, as wood density is expressed in kg/m3)
#The same calculation can obviously be done using the mean values we calculated above (see Part 2.)
(10*data_species[Species == "Picea abies", Mean] +
    15*data_species[Species == "Abies alba", Mean] +
    5*data_species[Species == "Fagus sylvatica", Mean])/1000 #Same result

#Example 2: A forest with ~150 m3 per hectare of above-ground volume, with approximately 25% of Quercus robur, 50% of Fagus sylvatica and 25% of Carpinus betulus
#We can first calculate an average density of the forest based on the weight of each species in the volume and the mean wood density of each species
(0.5 * XDM_species_density_data[Species == "Quercus robur", Mean] +
    0.25 * XDM_species_density_data[Species == "Fagus sylvatica", Mean] +
    0.25 * XDM_species_density_data[Species == "Carpinus betulus", Mean]) #~617 kg/m3
#And then estimate the biomass from this mean wood density
150*617/1000 #~93 tonnes of dry matter per hectare in the considered forest


###############################################################################################################################################################
#5.Example of calculation of a mean weighted basic wood density using the shrinkage values measured and provided in the xdm_individual_shrinkage_data.csv file#
###############################################################################################################################################################

#The basic wood densities provided in the XyloDensMap dataset (variables WDb and WDbw) have been calculated using shrinkage values gathered from a literature review
#However, shrinkage has also been estimated on a subset of increment cores during XyloDensMap project, and corresponding values are available in the xdm_individual_shrinkage_data.csv file
#We here used these values to calculate a new variable of weighted mean basic wood density
#For that, we use the mean volumetric shrinkage by species, but finer modelling approach might be suitable (not tested here)

#We first create a table with the mean volumetric shrinkage by species calculated from the individual measurements of volumetric shrinkage available in the XDM_individual_shrinkage_data.csv file
XDM_species_shrinkage_data = XDM_individual_shrinkage_data[, .(mean_WS_measured = mean(WS)), by = c("Botanical_Class", "Species")]

#We then add these mean values to the individual density measurements provided in the XDM_individual_density_data.csv file
XDM_individual_density_data = merge(XDM_individual_density_data, XDM_species_shrinkage_data, by = c("Botanical_Class", "Species"), all.x = TRUE)

#We can finally Calculate of novel mean basic wood density using the mean values of measured volumetric shrinkage instead of the values from the literature
XDM_individual_density_data[, ":="(WDbw2 = WD0w * (1 - mean_WS_measured/100))]

#Let's look at the difference with the mean basic wood density calculated from literature shrinkage coefficients for Quercus robur
ggplot(data = XDM_individual_density_data[Species == "Quercus robur"], aes(x = WDbw, y = WDbw2)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") + #Adding the Y=X line
  xlab(label = bquote('WDbw (literature shrinkage values) '(kg.m^-3))) +
  xlab(label = bquote('WDbw2 (measured shrinkage values) '(kg.m^-3)))
#The basic wood density estimated  using the mean value of shrinkage measurements is lower than the one estimated using literature shrinkage value
XDM_individual_density_data[Species == "Quercus robur", c("WS", "mean_WS_measured")] #This is logical, as for this species the mean value of shrinkage measurements (mean_WS_measured; ~17.5%) is greater than the literature shrinkage value (WS, 13%)


###############################################################################################################################################################
#6.Analyse sur 1 espèce
###############################################################################################################################################################

# Création d'un tableau avec uniquement 1 espèce : Quercus robur
library(dplyr)
dta_Qrobur = filter(XDM_individual_density_data, XDM_individual_density_data$Species == "Quercus robur")
dta_Qrobur = dta_Qrobur%>% filter(!is.na(Age))

# Vérification de la présence de données manquantes
colSums(is.na(dta_Qrobur))

## Visualisations

#Boxplots of wood density (weighted mean basic wood density) by species (species with at least 1000 measurements)
ggplot(data = dta_Qrobur[N_Cores > 1000], aes(x = reorder(Core_Type, WDbw, FUN = median, order=TRUE), y = WDbw, fill = Core_Type)) +
  geom_boxplot(show.legend = FALSE) +
  coord_flip() +
  facet_grid(rows = vars(Botanical_Class), scales = "free_y", space = "free") +
  xlab(label = "Species") +
  ylab(label = bquote('Wood density '(kg.m^-3)))

#Map of the average wood density (weighted mean basic wood density) on each NFI forest plot (using approximate coordinates, as the exact coordinates of the NFI plots are not available for statistical confidentiality reasons)
france = map_data('france') #Extracting the map of France using the map_data function of maps package
dta_Qrobur_map = dta_Qrobur[, .(mean = mean(WDbw)), by = c("Plot_ID", "X", "Y")] #Creating a new table with the average wood density (weighted mean basic wood density) for each NFI forest plot
ggplot(data = france, aes(x = long,y = lat, group = group)) + #Drawing the map
  geom_polygon(fill = "white", colour = "black") +
  geom_point(data = dta_Qrobur_map, size = 0.5, aes(x = X, y = Y, group = 1, colour = mean)) +
  scale_color_gradient2(low = "blue", mid = "tan", high = "red", midpoint = 600)

#Scatter plot of the relationship between tree diameter at breast height (DBH) and wood density in Quercus robur
ggplot(data = dta_Qrobur, aes(x = DBH, y = WDbw)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10)) + #Adding a smooth line based on generalized additive models
  xlab(label = "Diameter at breast height (m)") +
  ylab(label = bquote('Wood density '(kg.m^-3)))

#Scatter plot of the relationship between the mean basic wood density and the weighted mean basic wood density in Picea abies
ggplot(data = dta_Qrobur, aes(x = WDb, y = WDbw)) +
  geom_point() +
  geom_smooth(method = "lm") + #Adding the linear regression line
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") + #Adding the Y=X line
  xlab(label = bquote('Mean basic wood density '(kg.m^-3))) +
  ylab(label = bquote('Weighted mean basic wood density '(kg.m^-3)))

## Analyses

# ANCOVA



# Model formulation
mod1<-lm(WD0 ~ X+Y+Z+Core_Type+Core_Length+Age+DBH+H+TM+P
         ,data=dta_Qrobur)
# Then we check for significance
drop1(mod1,test="F")
