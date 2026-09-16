title: "Exploration données sur les wood densities"
author: "BARON Anaëlle" 
date: "2026-09-16 10:20:40 CEST "
bibliography: references.bib
execute: 
  freeze: auto
editor:
  markdown:
    canonical: true
    wrap: 72
format:
  html:
  theme: cosmo
css: theme.css
toc: true
toc_float: true

## ouvrir dataset
dta <- read.table("data_wood_density.csv", header = TRUE, sep = ",", dec = ".")
