---
title: "The Effect of Soil Moisture Content on the Growth and Hexadecane Remediation Capacity of \\emph{Pseudomonas putida}"
author: "Y3948024"
date: 15-04-2025
output:
  pdf_document:
    keep_tex: true
    fig_caption: true
    number_sections: true
    toc: false
    highlight: tango
    latex_engine: xelatex
    includes:
  in_header: preamble.tex
header-includes:
  - \usepackage{fvextra}
  - \DefineVerbatimEnvironment{Highlighting}{Verbatim}{breaklines=true,breakanywhere=true,commandchars=\\\{\}}
---

# Experimental overview

This study investigated how moisture level in the growth media affects the proliferation of saprophytic bacterium *Pseudomonas putida* and its ability to degrade hexadecane, a common hydrocarbon pollutant. We aim to advise on efficacy of *P. putida* to clean oil spills in environments with different aridity and humidity, such as the 2024 Libya pipe burst (Rédaction Africanews, 2024) and the recent spills in the Ecuadorian Amazon (Shanna Hanbury, 2025).

Bacterial growth was quantified using colony-forming unit (CFU) counts from spot plates prepared via serial dilution of the soil suspended in $\frac{1}{4}$ strength Ringer solution (Ringer, 1883). 4 replicates were used per spot plate, with 3 plates per sample.

The identity of the bacteria was confirmed by a series of tests, including oxidase testing, light microscopy, culturing on selective media, and PCR followed by gel electrophoresis.

## Description of data

Mean colony counts were normalised to CFU per gram of dry soil. Relevant datasets include:

- General data: `cfu_per_g_d_soil_allgroups.xlsx`
- Group 3 (spot replicate) data: `cfu_per_g_d_soil_group3.xlsx`
- Pre-incubation values: Sheet 4 in the general dataset
- Gel image (PCR): `data_gel_image`

All files are stored under the directory: `/Y3948024/data`.

## Analytical Overview

All data processing and visualisation were conducted in **R version 4.4.1 (2024-06-14) -- "Race for Your Life"** via **RStudio**, using **Git** for version control on macOS Sequoia (v15.3.1).

### Tests used:
- **Normality**: Shapiro–Wilk test (Shapiro & Wilk, 1965)
- **Variance homogeneity**: ANOVA and Tukey’s HSD test (Fisher, 1925; Tukey, 1949)
- **Non-parametric comparison**: Scheirer–Ray–Hare test (Scheirer et al., 1976), with post-hoc **Dunn’s tests** (Dunn, 1964)

### Packages and Software

All packages were obtained from CRAN (Accessed 14 April 2025). Citations for each package are included inline using standard references.

- **`tidyverse`** (Wickham et al., 2019)
- **`ggplot2`** (Wickham, 2016)
- **`FSA`** (Ogle et al., 2024)
- **`readxl`** (Wickham and Bryan, 2023)
- **`scales`** (Wickham and Seidel, 2022)
- **`vegan`** (Oksanen et al., 2022)
- **`rcompanion`** (Mangiafico, 2024)
- **`ggpubr`** (Kassambara, 2023)
- **`car`** (Fox and Weisberg, 2019)

# Load required libraries
```{r setup, include=TRUE, warning=FALSE, message=FALSE}
library(tidyverse)      # data manipulation + wrangling (Wickham et al., 2019)
library(ggplot2)        # data visualisation (Wickham, 2016)
library(FSA)            # non-parametric testing (Ogle et al., 2024)
library(readxl)         # read Excel files (Wickham and Bryan, 2023)
library(scales)         # scale customisation for plots (Wickham and Seidel, 2022)
library(vegan)          # multivariate ecology tools (Oksanen et al., 2022)
library(rcompanion)     # statistics for extension work (Mangiafico, 2024)
library(ggpubr)         # ggplot2 enhancements (Kassambara, 2023)
library(car)            # regression diagnostics (Fox and Weisberg, 2019)
```

# Statistical analysis and visualisation of data from plate replicates

## Read in all Excel sheets and add a new column indicating the group each row came from

```{r load-allgroups, warning=FALSE}
path <- 'cfu_per_g_d_soil_allgroups.xlsx'
data_allgroups <- excel_sheets(path)
data_tidy_list <- lapply(data_allgroups, function(sheet) {
  data <- read_excel(path, sheet = sheet)  # read each sheet
  data$group <- sheet                      # tag with group label
  return(data)
})
```

## Combine all data into a single data frame and coerce variables to factors

```{r combine-allgroups, warning=FALSE}
data_tidy <- do.call(rbind, data_tidy_list)
data_tidy$plate <- as.factor(data_tidy$plate)
data_tidy$Contamination <- as.factor(data_tidy$Contamination)
data_tidy$group <- as.factor(data_tidy$group)
```

## Summarise data

Calculate means, standard deviation, sample size and standard error

```{r summarise-allgroups, warning=FALSE, message=FALSE}
# Exclude pre-incubation data (group 4)
data_summary <- data_tidy %>%
  filter(group != "4") %>%
  group_by(plate, Contamination) %>%
  summarise(mean = mean(cfu_count),
            sd = sd(cfu_count),
            n = n(),
            se = sd / sqrt(n))
```{r print-summary-allgroups, warning=FALSE}
data_summary
```

## Statistical tests

### Fit a linear model on CFU count by moisture level and contamination status

```{r stats-allgroups, warning=FALSE}
mod <- lm(cfu_count ~ plate + Contamination, data = data_tidy %>% filter(group != "4"))
```

### Use the Freedman-Diaconis rule to determine optimal bin width for this histogram

```{r histogram-binwidth, echo=TRUE, warning=FALSE}
iqr <- IQR(data_tidy$cfu_count)
n <- nrow(data_tidy)
bin_width <- 2 * iqr / (n^(1/3))
```

Bin width = 237686445

### Visualise residual distribution to check normality

```{r plot-residuals-allgroups, echo=TRUE, warning=FALSE}
resid_df <- tibble(residuals = mod$residuals)
ggplot(resid_df, aes(x = residuals)) +
  geom_histogram(binwidth = bin_width)
```

The data appears to have a positive skew, with a long tail on the right. This suggests that the data may not be normally distributed.

### Shapiro-Wilk test for normality

```{r shapiro-allgroups, echo=TRUE, warning=FALSE}
shapiro.test(mod$residuals)
```

p = 2.775e-05 < 0.05. Therefore there is evidence to suggest that the data is not normally distributed.

### Levene's test for homogeneity of variance

#### Filter out group 4 (pre-incubation) for homogeneity testing

```{r levene-allgroups, warning=FALSE}
data_filtered <- data_tidy %>% filter(group != "4")
```

#### Fit linear model and conduct Levene's test

```{r a1, warning=FALSE, fig.width=8, fig.height=4}
levmod <- lm(cfu_count ~ plate + Contamination, data = data_filtered)
leveneTest(residuals(levmod) ~ data_filtered$group)
```
p = 0.2903 > 0.05. Therefore, there is not enough evidence to suggest that the variances are not homogenous across groups.

#### Plot boxplot of residuals grouped by replicate group to visualise the spread of variance

```{r a, warning=FALSE, fig.width=8}
data_filtered$resid <- residuals(levmod)
ggplot(data_filtered, aes(x = group, y = resid)) +
  geom_boxplot() +
  labs(title = "Residual Variance by Group")
```

## Testing for differences in mean CFU count between soil moisture levels and contamination status

### We will perform a non-parametric test, eg. the Scheirer-Ray-Hare test, to observe overall significance

```{r b, warning=FALSE, fig.width=8}
scheirerRayHare(cfu_count ~ plate * Contamination, data = filter(data_tidy, group != 4))
```

p(Plate:Contamination) = 0.78996 > 0.05, therefore there is not enough evidence to suggest that soil moisture content affects the growth of P. putida, using data from all groups.

### Post-hoc pairwise testing

#### Post-hoc for Contamination
####
```{r c, fig.width=8, warning=FALSE}
dunnTest(cfu_count ~ Contamination, data = filter(data_tidy, group != 4), method = "bonferroni")
```

p.adj = 0.2040239 > 0.05, therefore there is not enough evidence to suggest that hexadecane presence does affect growth of P. putida.

#### Post-hoc for Plate

##### Contamination = With hexadecane
#####
```{r d, warning=FALSE, fig.width=8}
dunnTest(cfu_count ~ plate, data = filter(data_tidy, Contamination == "With hexadecane", group != 4), method = "bh")
```

##### Contamination = Without hexadecane
#####
```{r e, warning=FALSE, fig.width=8}
dunnTest(cfu_count ~ plate, data = filter(data_tidy, Contamination == "Without hexadecane", group != 4), method = "bh")
```

With hexadecane: No significant differences.
Without hexadecane: No significant differences.

Therefore there is not enough evidence to suggest that soil moisture content affects the  growth of P. putida, using meaned data from all 3 groups.

## Visualise group-level CFU count with mean, SE, and LOESS lines

### Design a custom theme for the plot

```{r f, warning=FALSE, fig.width=8, fig.height=4}
theme_custom <- theme(
  panel.spacing.x = unit(1, "lines"),
  legend.position = 'right',
  panel.grid.major.y = element_line(colour = "#e3e1e1", linetype = 1),
  panel.grid.major.x = element_line(colour = "#e3e1e1", linetype = 1),
  panel.grid.minor.y = element_line(colour = "#e3e1e1", linetype = 1),
  axis.text.x = element_text(hjust = 0.5),
  plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
  plot.margin = margin(10, 10, 10, 10),
  plot.subtitle = element_text(vjust = -250, hjust = 1)
)
```

### Create plot

```{r visualise-allgroups, warning=FALSE, fig.width=8, fig.height=4}
mean_CFU_count <- ggplot(data = data_tidy) +
  
  #### Plotting replicate data points
  geom_point(aes(x = factor(plate), 
                 y = cfu_count, 
                 colour = group)) +
  
  #### Plotting LOESS regression lines for replicates from data_tidy
  geom_smooth(aes(x = factor(plate), 
                  y = cfu_count, 
                  colour = group,
                  group = interaction(group, Contamination)), 
              method = "loess", 
              se = FALSE,
              linewidth = 0.3) +
  
  #### Plotting means from data_summary
  geom_point(data = data_summary, 
             aes(x = factor(plate), 
                 y = mean), 
             colour = "black", 
             size = 2) +
  
  #### Plotting error bars for means from data_summary
  geom_errorbar(data = data_summary,
                aes(x = factor(plate), 
                    ymin = mean - se, 
                    ymax = mean + se), 
                width = 0.1, 
                colour = "black") +
  
  #### Plotting LOESS regression lines for the means
  geom_smooth(data = data_summary,
              aes(x = factor(plate), 
                  y = mean,
                  group = Contamination), 
              method = "loess", 
              colour = "black",
              se = FALSE, 
              linewidth = 1) +
  
  #### Adding labels and changing axis
  labs(title = expression(italic("Pseudomonas putida")~"\nCFU count per gram of dry soil, by replicate"),
       x = expression("Soil moisture level (% of field capacity)"),
       y = expression("log"[10]~"CFU count /g dry soil"),
       colour = "Replicates") +
  
  #### Faceting by Contamination
  facet_grid(~ Contamination) +
  
  #### Adjusting the axes and legend
  scale_x_discrete(labels = c("30", "40", "50", "60"),
                   expand = c(0, 0)
  ) +
  scale_y_log10(
    labels = trans_format("log10", math_format(10^.x)),
    expand = c(0, 0),
    limits = c(10^6, 10^10),
    breaks = c(10^6, 10^7, 10^8, 10^9, 10^10),
    minor_breaks = rep(1:9, each = 1) * 10 ^ rep(6:9, times = 9)) +
  scale_colour_manual(values = c('1' = '#F8766D', '2' = '#7CAE00', '3' = '#00BFC4', '4' = '#C77CFF'),
                      labels = c('1', '2', '3', 
                                 'Before incubation at \nrespective soil moisture level')) +
  
  #### Theming
  cowplot::theme_cowplot() +
  theme_custom
theme(axis.text.x = element_text(angle = 0, hjust = 1))

mean_CFU_count
```

# Statistical analysis and visualisation of data from spot replicates

## Read in group 3 data from Excel file

```{r group3-analysis, warning=FALSE}
group3 <- read_excel('cfu_per_g_d_soil_group3.xlsx')
```

## Convert relevant columns to factors for analysis

```{r group3-factors, warning=FALSE}
# Convert relevant columns to factors for analysis
group3$Plate <- as.factor(group3$Plate)
group3$Contamination <- as.factor(group3$Contamination)
group3$spot <- as.factor(group3$spot)
```

## Summarise data

Calculate means, standard deviation, sample size and standard error

```{r summarise-group3, warning=FALSE}
data_summary2 <- group3 %>%
  group_by(Plate, Contamination) %>%
  summarise(mean2 = mean(cfu_count),
            sd2 = sd(cfu_count),
            n2 = n(),
            se2 = sd2 / sqrt(n2))
```

## Statistical tests

### Fit a linear model on CFU count by moisture level and contamination status

```{r stats-group3, warning=FALSE}
mod2 <- lm(cfu_count ~ Plate + Contamination, data = group3)
```

### Use the Freedman-Diaconis rule to determine optimal bin width for histogram

```{r histogram-binwidth-group3, echo=TRUE, warning=FALSE}
iqr2 <- IQR(group3$cfu_count)
n2 <- nrow(group3)
bin_width2 <- 2 * iqr2 / (n2^(1/3))
```

Bin width = 237686445

### Visualise residual distribution to check normality

```{r residuals-group3, echo=TRUE, warning=FALSE}
ggplot(group3, aes(x = mod2$residuals)) +
  geom_histogram(binwidth = bin_width2)
```

### Shapiro-Wilk test for normality

```{r shapiro-group3, echo=TRUE, warning=FALSE}
shapiro.test(mod2$residuals)
```

p = 0.4519 > 0.05, there is not evidence to suggest that the data is not normally distributed.

### Levene's test for homogeneity of variance

#### Fit linear model and conduct Levene's test

```{r levene-group3, warning=FALSE}
levmod2 <- lm(cfu_count ~ Plate + Contamination, data = group3)
leveneTest(residuals(levmod2) ~ group3$spot)
```

p = 0.9954 > 0.05, therefore there is not enough evidence to suggest that the data is not homogeneously distributed.

### Plot boxplot of residuals grouped by spot to visualise the spread of variance

```{r plot-residuals-group3, warning=FALSE}
group3$resid <- residuals(levmod2)
ggplot(group3, aes(x = spot, y = resid)) +
  geom_boxplot() +
  labs(title = "Residual Variance by Spot")
```

## Testing for differences in mean CFU count between soil moisture levels and contamination status

### We will perform a non-parametric test, eg. the Scheirer-Ray-Hare test, to observe overall significance

```{r srh, warning=FALSE}
scheirerRayHare(cfu_count ~ Plate * Contamination, data = group3)
```

p(Plate:Contamination) = 0.001737 < 0.05, therefore there is evidence to suggest that soil moisture content does affect growth of P. putida.

### Post-hoc pairwise testing

#### Post-hoc for Contamination
####
```{r d2, warning=FALSE}
dunnTest(cfu_count ~ Contamination, data = group3, method = "bonferroni")
```

p.adj = 0.001356023 < 0.05, therefore there is evidence to suggest that hexadecane presence does affect growth of P. putida.

#### Post-hoc for Plate

##### Contamination = With hexadecane
#####
```{r d3, warning=FALSE}
dunnTest(cfu_count ~ Plate, data = filter(group3, Contamination == "With hexadecane"), method = "bh")
```

##### Contamination = Without hexadecane
#####
```{r d4, warning=FALSE}
dunnTest(cfu_count ~ Plate, data = filter(group3, Contamination == "Without hexadecane"), method = "bh")
```

With hexadecane: Only significant difference between 30-40% and 40-60%, 
p.adj = 0.034969349 and 0.002187043, respectively.
Without hexadecane: Only significant difference between 30-60% and 40-60%, 
p.adj = 0.02788163 and 0.02251052, respectively.
Therefore there is evidence to suggest that soil moisture content does affect the growth of P. putida, using data from group 3.
However, as p.adj (With hexadecane, 30-40%) > 0.025 and the difference is positive, we cannot conclude that the difference between 30% and 40% is significant with a one-tailed test.
The same is true for p.adj (Without hexadecane, 30-60%).

## Visualise group 3 CFU count with group means, SE, smoothed lines, and statistical annotations

### Preparing statistical annotations.

```{r pairwise_p, warning=FALSE}
pairwise_p <- data.frame(
  group1 = c("30", "40", "30", "40"),
  group2 = c("40", "60", "60", "60"),
  y.position = c(2.05e9, 2.10e9, 300000000, 500000000),
  p.adj = c(0.034969349, 0.002187043, 0.02788163, 0.02251052),
  Contamination = c("With hexadecane", "With hexadecane", 
                    "Without hexadecane", "Without hexadecane")
)

pairwise_p$label <- paste0("p.adj = ", signif(pairwise_p$p.adj, 3))
```

### Create plot

```{r visualise-group3, warning=FALSE, fig.width=8, fig.height=4}
mean_CFU_count2 <- ggplot() +
  
  #### Plotting data points.
  geom_point(data = group3,
             aes(x = factor(Plate), 
                 y = cfu_count, 
                 colour = Contamination)) +
  
  #### Plotting LOESS regression lines from data_tidy.
  geom_smooth(data = group3,
              aes(x = factor(Plate), 
                  y = cfu_count, 
                  colour = Contamination), 
              method = "loess", 
              se = FALSE,
              linewidth = 0.3) +
  
  #### Plotting means from data_summary.
  geom_point(data = data_summary2, 
             aes(x = factor(Plate), 
                 y = mean2), 
             colour = 'black', 
             size = 2) +
  
  #### Plotting error bars for means from data_summary.
  geom_errorbar(data = data_summary2,
                aes(x = factor(Plate), 
                    ymin = mean2 - se2, 
                    ymax = mean2 + se2), 
                width = 0.1, 
                colour = "black") +
  
  #### Plotting LOESS regression lines for the means.
  geom_smooth(data = data_summary2,
              aes(x = factor(Plate), 
                  y = mean2,
                  group = Contamination), 
              method = "loess", 
              colour = "black",
              se = FALSE, 
              linewidth = 1) +
  
  #### Adding labels and changing axis.
  labs(title = expression(italic("Pseudomonas putida")~"\nCFU count per gram of dry soil"),
       x = expression("Soil moisture level /% of field capacity"),
       y = expression("CFU count /g dry soil"),
       colour = "Contamination") +
  
  #### Adjusting the axes and legend.
  scale_x_discrete(labels = c("30", "40", "50", "60"),
                   expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = function(x) {
      sapply(x, function(val) {
        if (val == 0) {
          "0"
        } else {
          formatted <- formatC(val / 10^floor(log10(val)), digits = 2, format = "f")
          exponent <- floor(log10(val))
          parse(text = paste0(formatted, " %*% 10^", exponent))
        }
      })
    },
    breaks = seq(0, 2.25e9, by = 2.5e8),
    limits = c(0, 2.25e9),
    expand = c(0, 0)
  ) +
  scale_colour_manual(values = c('With hexadecane' = '#F8766D', 'Without hexadecane' = '#00BFC4', '4' = '#C77CFF'),
                      labels = c('With hexadecane', 'Without hexadecane', 
                                 'Before incubation at \nrespective soil moisture level')) +
  geom_point(data = group3,
             aes(x = factor(Plate), 
                 y = cfu_count, 
                 colour = Contamination)) +
  
  #### Adding statistical annotations.
  stat_pvalue_manual(pairwise_p, 
                     label = "label", 
                     xmin = "group1", xmax = "group2", 
                     y.position = "y.position", 
                     tip.length = 0.01,
                     size = 3,
                     bracket.size = 0.3) +
  
  #### Theming.
  cowplot::theme_cowplot() +
  theme_custom +
theme(axis.text.x = element_text(angle = 0, hjust = 1))

mean_CFU_count2
```

## References

```{refs}
Dunn, O.J. (1964) 'Multiple comparisons using rank sums', *Technometrics*, 6(3), pp. 241–252.  
Fisher, R.A. (1925) *Statistical Methods for Research Workers*. Edinburgh: Oliver and Boyd.  
Fox, J. and Weisberg, S. (2019) *An R Companion to Applied Regression*. 3rd edn. Thousand Oaks, CA: Sage.  
Kassambara, A. (2023) *ggpubr: 'ggplot2'-Based Publication Ready Plots*. R package version 0.6.0.  
Mangiafico, S. (2024) *rcompanion: Functions to Support Extension Education Program Evaluation*. R package version 2.4.30.  
Ogle, D.H. et al. (2024) *FSA: Fisheries Stock Analysis*. R package version 0.9.5.  
Oksanen, J. et al. (2022) *vegan: Community Ecology Package*. R package version 2.6-4.  
R Core Team (2024) *R: A Language and Environment for Statistical Computing*. Vienna: R Foundation for Statistical Computing.  
Scheirer, C.J., Ray, W.S. and Hare, N. (1976) 'The analysis of ranked data derived from completely randomized factorial designs', *Biometrics*, 32(2), pp. 429–434.  
Shapiro, S.S. and Wilk, M.B. (1965) 'An analysis of variance test for normality (complete samples)', *Biometrika*, 52(3/4), pp. 591–611.  
Tukey, J.W. (1949) 'Comparing individual means in the analysis of variance', *Biometrics*, 5(2), pp. 99–114.  
Wickham, H. (2016) *ggplot2: Elegant Graphics for Data Analysis*. New York: Springer.  
Wickham, H. et al. (2019) *Welcome to the tidyverse*. R package version 2.0.0.  
Wickham, H. and Bryan, J. (2023) *readxl: Read Excel Files*. R package version 1.4.3.  
Wickham, H. and Seidel, D. (2022) *scales: Scale Functions for Visualization*. R package version 1.3.0.
```
