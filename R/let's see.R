---
title: "The Effect of Soil Moisture Content on the Growth and Hexadecane Remediation Capacity of *Pseudomonas putida*"
author: "Y3948024"
date: "2025-04-15"
output: pdf_document
---

# Experimental Overview --------------------------------------------------------

## This study explored the influence of soil moisture levels on the 
## proliferation of the saprophytic bacterium *Pseudomonas putida* and its 
## ability to degrade hexadecane—a common hydrocarbon pollutant. Our aim is to 
## inform assessments of the suitability of *P. putida* for bioremediation of o
## il spills in environments varying in aridity and humidity, such as the 2024 
## Libya pipeline rupture (Rédaction Africanews, 2024) and recent contamination 
## events in the Ecuadorian Amazon (Hanbury, 2025).
## Bacterial growth was quantified using colony-forming unit (CFU) counts from
## spot plates prepared via serial dilution in one-quarter strength Ringer 
## solution (Ringer, 1883). Each condition was represented by four spot 
## replicates across three plate replicates.
## Bacterial identity was verified through a series of tests: oxidase reaction, 
## light microscopy, growth on selective media, and PCR followed by gel 
## electrophoresis.

# Data Description -------------------------------------------------------------

## Mean colony counts were normalised to CFU per gram of dry soil. 
## Relevant datasets include:
  
## - General data: `cfu_per_g_d_soil_allgroups.xlsx`
## - Group 3 (spot replicate) data: `cfu_per_g_d_soil_group3.xlsx`
## - Pre-incubation values: Sheet 4 in the general dataset
## - Gel image (PCR): `data_gel_image`

## All files are stored under the directory: `/Y3948024/R/data`.

# Analytical Overview ----------------------------------------------------------

## All data processing and visualisation were conducted in 
## **R (v2024.12.0+467)** via **RStudio**, using **Git** for version control 
## on macOS Sequoia (v15.3.1).

## Tests used:
  
## - **Normality**: Shapiro–Wilk test (Shapiro & Wilk, 1965)
## - **Variance homogeneity**: ANOVA and Tukey’s HSD test (Fisher, 1925; Tukey, 1949)
## - **Non-parametric comparison**: Scheirer–Ray–Hare test (Scheirer et al., 1976), with post-hoc **Dunn’s tests** (Dunn, 1964)

# Software and Packages --------------------------------------------------------

## {r packages}
library(tidyverse)     # Data manipulation
library(ggplot2)       # Data visualisation
library(FSA)           # Scheirer-Ray-Hare and Dunn's tests
library(readxl)        # Reading Excel files
library(scales)        # Visual formatting
library(vegan)         # Multivariate analysis
library(rcompanion)    # Additional stats
library(ggpubr)        # Plot annotation

## All packages were obtained from CRAN (Accessed 14 April 2025).

# Analysis: Plate Replicates (All Groups) --------------------------------------

## Data Import and Summary

### Define Path
path <- 'data/cfu_per_g_d_soil_allgroups.xlsx'
data_allgroups <- excel_sheets(path)
data_tidy_list <- lapply(data_allgroups, function(sheet) {
  data <- read_excel(path, sheet = sheet)
  data$group <- sheet
  return(data)
})

### Bind and Coerce to Factors
data_tidy <- do.call(rbind, data_tidy_list)
data_tidy$plate <- as.factor(data_tidy$plate)
data_tidy$Contamination <- as.factor(data_tidy$Contamination)

## Summarise Data
data_summary <- data_tidy %>%
  filter(group != 4) %>%
  group_by(plate, Contamination) %>%
  summarise(mean = mean(cfu_count),
            sd = sd(cfu_count),
            n = n(),
            se = sd / sqrt(n))

## Statistical Testing

### 
mod <- lm(cfu_count ~ group + Contamination, data = data_tidy)
shapiro.test(mod$residuals)
```

```{r plate-variance}
mutated <- data_tidy %>% mutate(Combined = interaction(plate, Contamination))
cfu_dist <- dist(mutated$cfu_count)
dispersion <- betadisper(cfu_dist, group = mutated$Combined)
anova(dispersion)
```

```{r plate-srh}
SRH <- scheirerRayHare(cfu_count ~ plate * Contamination, data = filter(data_tidy, group != 4))
SRH
dunnTest(cfu_count ~ Contamination, data = filter(data_tidy, group != 4), method = "bonferroni")
```

## Visualisation

```{r plate-plot}
theme_custom <- theme(panel.spacing.x = unit(1, "lines"),
                      legend.position = 'right',
                      panel.grid.major = element_line(colour = "#e3e1e1"),
                      axis.text.x = element_text(hjust = 0.5))

ggplot(data_tidy, aes(x = factor(plate), y = cfu_count, colour = group)) +
  geom_point() +
  geom_smooth(aes(group = interaction(group, Contamination)), method = "loess", se = FALSE, linewidth = 0.3) +
  geom_point(data = data_summary, aes(y = mean), colour = "black", size = 2) +
  geom_errorbar(data = data_summary, aes(ymin = mean - se, ymax = mean + se), width = 0.1, colour = "black") +
  geom_smooth(data = data_summary, aes(y = mean, group = Contamination), method = "loess", colour = "black", se = FALSE, linewidth = 1) +
  labs(title = expression(italic("Pseudomonas putida") ~ "CFU count per gram of dry soil by replicate"),
       x = "Soil moisture level (% of field capacity)",
       y = expression(log[10] ~ "CFU/g dry soil"),
       colour = "Replicates") +
  facet_grid(~ Contamination) +
  scale_y_log10(labels = scales::trans_format("log10", math_format(10^.x)), limits = c(1e6, 1e10)) +
  cowplot::theme_cowplot() +
  theme_custom
```

# Analysis: Spot Replicates (Group 3)

```{r spot-data-import}
group3 <- read_excel('data/cfu_per_g_d_soil_group3.xlsx')
group3$Plate <- as.factor(group3$Plate)
group3$Contamination <- as.factor(group3$Contamination)
```

```{r spot-summary}
data_summary2 <- group3 %>%
  group_by(Plate, Contamination) %>%
  summarise(mean = mean(cfu_count),
            sd = sd(cfu_count),
            n = n(),
            se = sd / sqrt(n))
```

```{r spot-stats}
mod2 <- lm(cfu_count ~ Plate + Contamination, data = group3)
shapiro.test(mod2$residuals)

SRH2 <- scheirerRayHare(cfu_count ~ Plate * Contamination, data = group3)
SRH2
dunnTest(cfu_count ~ Contamination, data = group3, method = "bonferroni")
```

## Visualisation

```{r spot-plot}
pairwise_p <- data.frame(
  group1 = c("30", "40", "30", "40"),
  group2 = c("40", "60", "60", "60"),
  y.position = c(2.05e9, 2.10e9, 3e8, 5e8),
  p.adj = c(0.034, 0.0022, 0.0279, 0.0225),
  Contamination = c("With hexadecane", "With hexadecane", "Without hexadecane", "Without hexadecane")
)
pairwise_p$label <- paste0("p.adj = ", signif(pairwise_p$p.adj, 3))

mean_CFU_count2 <- ggplot(group3, aes(x = factor(Plate), y = cfu_count, colour = Contamination)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.3) +
  geom_point(data = data_summary2, aes(y = mean), colour = 'black', size = 2) +
  geom_errorbar(data = data_summary2, aes(ymin = mean - se, ymax = mean + se), width = 0.1, colour = "black") +
  geom_smooth(data = data_summary2, aes(y = mean, group = Contamination), method = "loess", colour = "black", se = FALSE, linewidth = 1) +
  labs(title = expression(italic("Pseudomonas putida") ~ "CFU count per gram of dry soil"),
       x = "Soil moisture level (% of field capacity)",
       y = "CFU/g dry soil",
       colour = "Contamination") +
  stat_pvalue_manual(pairwise_p, label = "label", xmin = "group1", xmax = "group2", y.position = "y.position", tip.length = 0.01) +
  cowplot::theme_cowplot() +
  theme_custom

mean_CFU_count2
```

# References

- Rédaction Africanews (2024). *Libya pipeline rupture and spill*. Africanews.
- Hanbury, S. (2025). *Oil spill impacts in the Ecuadorian Amazon*. Mongabay.
- Ringer, S. (1883). *Preliminary account of an investigation upon the influence of mechanical and chemical stimuli on the rhythm of the heart of the frog*. *J Physiol*.
- Shapiro, S.S. & Wilk, M.B. (1965). An analysis of variance test for normality. *Biometrika*, 52(3/4), 591–611.
- Fisher, R.A. (1925). *Statistical Methods for Research Workers*. Oliver & Boyd.
- Tukey, J.W. (1949). Comparing individual means in the analysis of variance. *Biometrics*, 5(2), 99–114.
- Dunn, O.J. (1964). Multiple comparisons using rank sums. *Technometrics*, 6(3), 241–252.
- Scheirer, C.J., Ray, W.S., & Hare, N. (1976). The analysis of ranked data derived from completely randomised factorial designs. *Biometrics*, 32(2), 429–434.
