
# The effect of soil moisture content on growth and hexadecane remediation 
# of *Pseudomonas putida* -------------------------------------------------------

## Experimental overview --------------------------------------------------------

### This study investigated how moisture level in the growth media affects
### the proliferation of saprophytic bacterium *Pseudomonas putida* and its
### ability to degrade hexadecane, a common hydrocarbon pollutant. We aim to
### advise on efficacy of *P. putida* to clean oil spills in environments with
### different aridity and humidity, such as the 2024 Libya pipe burst 
### (Rédaction Africanews, 2024) and the recent spills in the Ecuadorian Amazon
### (Shanna Hanbury, 2025).

### The growth was assessed by colony count on spot plates of a serial dilution
### of the soil suspended in $\frac{1}{4}$ strength Ringer solution
### (Ringer, 1883). 4 replicates were used per spot plate, with 3 plates
### per sample.

### The identity of the bacteria was confirmed by a series of tests, including
### oxidase testing, light microscopy, culturing on selective media, and PCR
### followed by gel electrophoresis.

## Description of data ---------------------------------------------------------

### The data for the mean colony counts has been processed to assess the
### CFU count per gram of dry soil. The data is located at '/Y3948024/R/data'.
### The data for the plate replicates and spot replicates for group 3 are in:
###    'cfu_per_g_d_soil_allgroups.xlsx' and
###    'cfu_per_g_d_soil_group3.xlsx' respectively. 
### The data for the plate replicates is ordered with each sheet being a
### different replicate, performed by a different group. Sheet 4 contains the
### CFU count for before incubation at the respective soil moisture content and
### contamination status.
### The data for the spot replicates was performed by group 3, and has n = 4
### for each sample.
### A gel electrophoresis image of the PCR products is located at
### 'Y3948024/R/data_gel_image'

## Analysis overview -----------------------------------------------------------

### The manipulation, analysis, and visualisation was performed in R using
### RStudio version 2024.12.0+467, using git for version control, on a MacBook 
### Pro with macOS Sequoia version 15.3.1.
### The report includes the gel image which is not analysed here.
### Normality in each dataset was assessed using the Shapiro-Wilk test 
### (Shapiro & Wilk, 1965), and homogeneity of variance was assessed using
### ANOVA (Fisher, 1925) and post-hoc Tukey tests (Tukey, 1949).
### The data was tested for significant differences in CFU count between
### the different soil moisture levels and contamination status using a
### Scheirer-Ray-Hare test (Scheirer, Ray and Hare, 1976) and post-hoc Dunn's 
### tests (Dunn, 1964).

## Packages and software -------------------------------------------------------

### Posit (2024) RStudio: Integrated development environment for R. 
### Version 2024.12.0+467. Boston, MA: Posit Software, PBC. 
### Available at: https://posit.co/products/open-source/rstudio/ 
### (Accessed: 14 April 2025).
### R Core Team (2024) R: A language and environment for statistical computing. 
### Version 2024.12.0+467. Vienna: R Foundation for Statistical Computing. 
### Available at: https://www.R-project.org/ (Accessed: 14 April 2025).

library(tidyverse)
### For data manipulation.
#### Wickham, H. et al. (2019) 
#### Welcome to the tidyverse.
#### R package version 2.0.0. 
#### Available at: https://CRAN.R-project.org/package=tidyverse 
#### (Accessed: 14 April 2025).

library(ggplot2)
### For data visualisation.
#### Wickham, H. (2016)
#### ggplot2: Elegant graphics for data analysis. 
#### New York: Springer-Verlag. 
#### R package version 3.5.1. 
#### Available at: https://CRAN.R-project.org/package=ggplot2 
#### (Accessed: 14 April 2025).

library(FSA)
### For Scheirer-Ray-Hare test and Dunn's post-hoc test.
#### Ogle, D.H., Doll, J.C., Wheeler, P. and Dinno, A. (2024)
#### FSA: Fisheries stock analysis. 
#### R package version 0.9.5. A
#### vailable at: https://CRAN.R-project.org/package=FSA 
#### (Accessed: 14 April 2025).

library(readxl)
### For reading in Excel files.
#### Wickham, H. and Bryan, J. (2023)
#### readxl: Read Excel files. 
#### R package version 1.4.3. 
#### Available at: https://CRAN.R-project.org/package=readxl 
#### (Accessed: 14 April 2025).

library(scales)
### For figure aesthetics.
#### Wickham, H. and Seidel, D. (2022)
#### scales: Scale functions for visualization. 
#### R package version 1.3.0. 
#### Available at: https://CRAN.R-project.org/package=scales
#### (Accessed: 14 April 2025).

library(vegan)
### For multivariate analysis.
#### Oksanen, J. et al. (2022
#### vegan: Community ecology package.
#### R package version 2.6-4. 
#### Available at: https://CRAN.R-project.org/package=vegan 
#### (Accessed: 14 April 2025).

library(rcompanion)
### For Scheirer-Ray-Hare test and Dunn's post-hoc test.
#### Mangiafico, S. (2024)
#### rcompanion: Functions to support extension education program evaluation.
#### R package version 2.4.30.
#### Available at: https://CRAN.R-project.org/package=rcompanion
#### (Accessed: 14 April 2025).

library(ggpubr)
### For building stats on top of ggplot2 plots.
#### Kassambara, A. (2023)
#### ggpubr: 'ggplot2' based publication ready plots.
#### R package version 0.6.0.
#### Available at: https://CRAN.R-project.org/package=ggpubr
#### (Accessed: 14 April 2025).

# Plate replicates using 'allgroups' dataset -----------------------------------

## Read in the data sheetwise and process.

### Define the path.
path <- 'data/cfu_per_g_d_soil_allgroups.xlsx'

### Load the sheet names into a variable.
data_allgroups <- excel_sheets(path)

### Read the sheets and compile them into one dataframe.
data_tidy_list <- lapply(data_allgroups, function(sheet) {
  data <- read_excel(path, sheet = sheet)
  data$group <- sheet
  return(data)
})

### Combine into one dataframe and coerce stuff to factors to make analysis 
### easier.
data_tidy <- do.call(rbind, data_tidy_list)
data_tidy$plate <- as.factor(data_tidy$plate)
data_tidy$Contamination <- as.factor(data_tidy$Contamination)

## Summarise data.
data_summary <- data_tidy |>
  filter(group != 4) |> 
  group_by(plate, Contamination) |> 
  summarise(mean = mean(cfu_count),
            sd = sd(cfu_count),
            n = length(plate),
            se = sd / sqrt(n)
  )

## Statistical tests.

### Check for normality in each sheet.

### Fit linear model.
mod <- lm(cfu_count ~ group + Contamination, data = data_tidy)
summary(mod)

#### Call:
#### lm(formula = cfu_count ~ Plate + Contamination, data = group3)
#### Residuals:
####  Min         1Q     Median         3Q        Max 
#### -638869021 -313778026   32627500  238697854  847561625 

#### Coefficients:
####  Estimate Std. Error
#### (Intercept)                      449952849  171020372
#### Plate40                          828925109  216325561
#### Plate50                          325771412  216325561
#### Plate60                            -492229  216325561
#### ContaminationWithout hexadecane -619799354  152965271
#### t value Pr(>|t|)    
#### (Intercept)                       2.631 0.013896 *  
####  Plate40                           3.832 0.000689 ***
####  Plate50                           1.506 0.143696    
#### Plate60                          -0.002 0.998201    
#### ContaminationWithout hexadecane  -4.052 0.000386 ***
####   ---
####   Signif. codes:  
####   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### Residual standard error: 432700000 on 27 degrees of freedom
#### Multiple R-squared:  0.572,	Adjusted R-squared:  0.5086 
#### F-statistic: 9.021 on 4 and 27 DF,  p-value: 9.225e-05

#### Freedman-Diaconis rule for bin width.
iqr <- IQR(data_tidy$cfu_count)
n <- nrow(data_tidy)
bin_width <- 2 * iqr / (n^(1/3))

##### bin_width = 237686445

#### Plot residuals to visually check for normality.
histo <- ggplot(mapping = aes(x = mod$residuals),
                data = data_tidy
) +
  geom_histogram(binwidth = bin_width)
histo
### Save the plot as pdf.
ggsave(filename = 'allgroups_mod.residuals_histo.pdf',
       plot = histo,
       width = 10,
       height = 6,
       dpi = 300
)
##### Data appears to have a positive skew.

### Perform Shapiro-Wilk's test to check for normality.
shap <- shapiro.test(mod$residuals)
shap
####
##### p = 1.497e-7 < 0.05, there is evidence to suggest that the data is not 
##### normally distributed.

### Test for uniform distribution of variance.

#### Group data by plate and contamination.
mutated <- data_tidy %>%
  mutate(Mutated_column = interaction(plate, Contamination))
cfu_dist <- dist(mutated$cfu_count)
dispersion <- betadisper(cfu_dist, group = mutated$Mutated_column)
#### Check for homogeneity of variance.
anova(dispersion)
TukeyHSD(dispersion)

## Visualise and save as pdf.
pdf("allgroups_variance_boxplot.pdf", width = 10, height = 6)
boxplot(dispersion)
dev.off()
#### p = 0.05328 > 0.05, therefore there is evidence to suggest that the data
#### is homogeneously distributed.
#### The boxplot shows that the data is homogeneously distributed.
#### The variance is not uniform across groups.

### We will perform a non-parametric test, eg. the Scheirer-Ray-Hare test.
SRH <- scheirerRayHare(cfu_count ~ plate * Contamination, data = filter(data_tidy, group != 4))
SRH
#### p(Plate:Contamination) = 0.78996 > 0.05, therefore there is no evidence to
#### suggest that soil moisture content does affect growth of P. putida, 
#### using data from all groups.

### Post-hoc for Contamination
dunnTest(cfu_count ~ Contamination, data = filter(data_tidy, group != 4), method = "bonferroni")
#### p.adj = 0.2040239 > 0.05, therefore there is no evidence to suggest that 
#### hexadecane presence does affect growth of P. putida.

### Post-hoc for Plate
dunnTest(cfu_count ~ plate, data = filter(data_tidy, Contamination == "With hexadecane", group != 4), method = "bh")
dunnTest(cfu_count ~ plate, data = filter(data_tidy, Contamination == "Without hexadecane", group != 4), method = "bh")
#### With hexadecane: No significant differences.
#### Without hexadecane: No significant differences.

## Visualising the data.

### Custom theme.
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

### Plotting the data.
mean_CFU_count <- ggplot(data = data_tidy) +
  
#### Plotting replicate data points.
  geom_point(aes(x = factor(plate), 
                 y = cfu_count, 
                 colour = group)) +
  
#### Plotting LOESS regression lines for replicates from data_tidy.
  geom_smooth(aes(x = factor(plate), 
                  y = cfu_count, 
                  colour = group,
                  group = interaction(group, Contamination)), 
              method = "loess", 
              se = FALSE,
              linewidth = 0.3) +
  
#### Plotting means from data_summary.
  geom_point(data = data_summary, 
             aes(x = factor(plate), 
                 y = mean), 
             colour = "black", 
             size = 2) +
  
#### Plotting error bars for means from data_summary.
  geom_errorbar(data = data_summary,
                aes(x = factor(plate), 
                    ymin = mean - se, 
                    ymax = mean + se), 
                width = 0.1, 
                colour = "black") +
  
#### Plotting LOESS regression lines for the means.
  geom_smooth(data = data_summary,
              aes(x = factor(plate), 
                  y = mean,
                  group = Contamination), 
              method = "loess", 
              colour = "black",
              se = FALSE, 
              linewidth = 1) +
  
#### Adding labels and changing axis.
  labs(title = expression(italic("Pseudomonas putida")~"\nCFU count per gram of dry soil, by replicate"),
       x = expression("Soil moisture level (% of field capacity)"),
       y = expression("log"[10]~"CFU count /g dry soil"),
       colour = "Replicates") +
  
#### Faceting by Contamination.
  facet_grid(~ Contamination) +
  
#### Adjusting the axes and legend.
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
  
#### Theming.
  cowplot::theme_cowplot() +
  theme_custom
theme(axis.text.x = element_text(angle = 0, hjust = 1)) 

### Print final plot.
mean_CFU_count

### Save the plot as jpg.
ggsave(filename = 'allgroups.jpg',
       plot = mean_CFU_count,
       width = 10,
       height = 6,
       dpi = 300
)
### Save the plot as pdf.
ggsave(filename = 'allgroups.pdf',
       plot = mean_CFU_count,
       width = 10,
       height = 6,
       dpi = 300
)

# Spot replicates using 'group3' dataset ---------------------------------------

## Read data into tibble and coerce to factors to make analysis easier.
group3 <- read_excel('data/cfu_per_g_d_soil_group3.xlsx')
group3$Plate <- as.factor(group3$Plate)
group3$Contamination <- as.factor(group3$Contamination)

## Summarise data.
data_summary <- group3 |> 
  group_by(Plate, Contamination) |> 
  summarise(mean = mean(cfu_count),
            sd = sd(cfu_count),
            n = length(Plate),
            se = sd / sqrt(n)
  )

## Statistical tests.

### Check for normality.
mod <- lm(cfu_count ~ Plate + Contamination, data = group3)

#### Freedman-Diaconis rule for bin width.
iqr <- IQR(group3$cfu_count)
n <- nrow(group3)
bin_width <- 2 * iqr / (n^(1/3))

#### Plot residuals to visually check for normality.
histo <- ggplot(mapping = aes(x = mod$residuals),
                data = group3
) +
  geom_histogram(binwidth = bin_width)
histo
### Save the plot as pdf.
ggsave(filename = 'group3_mod.residuals_histo.pdf',
       plot = histo,
       width = 10,
       height = 6,
       dpi = 300
)
#### Data appears to have a positive skew.

### Perform Shapiro-Wilk's test to check for normality.
shap <- shapiro.test(mod$residuals)
shap
#### p = 0.0008261 < 0.05, there is evidence to suggest that the data is not 
#### normally distributed.

### Test for uniform distribution of variance.

#### Group data by plate and contamination.
mutated <- group3 %>%
  mutate(Group = interaction(Plate, Contamination))
cfu_dist <- dist(mutated$cfu_count)
dispersion <- betadisper(cfu_dist, group = mutated$Group)

#### Check for homogeneity of variance.
anova(dispersion)
TukeyHSD(dispersion)

#### Visualise and save as pdf.
pdf("group3_variance_boxplot.pdf", width = 10, height = 6)
boxplot(dispersion)
dev.off()
##### p = 0.002974 < 0.05, therefore there is evidence to suggest that the data 
##### is not homogeneously distributed.
##### The boxplot shows that the data is not homogeneously distributed.
##### The variance is not uniform across groups.

### Scheirer-Ray-Hare test.
SRH <- scheirerRayHare(cfu_count ~ Plate * Contamination, data = group3)
SRH
##### p(Plate:Contamination) = 0.001737 < 0.05, therefore there is evidence to 
##### suggest that soil moisture content does affect growth of P. putida.

#### Post-hoc for Contamination
dunnTest(cfu_count ~ Contamination, data = group3, method = "bonferroni")
##### p.adj = 0.001356023 < 0.05, therefore there is evidence to suggest that 
##### hexadecane presence does affect growth of P. putida.

#### Post-hoc for Plate
dunnTest(cfu_count ~ Plate, data = filter(group3, Contamination == "With hexadecane"), method = "bh")
dunnTest(cfu_count ~ Plate, data = filter(group3, Contamination == "Without hexadecane"), method = "bh")
##### With hexadecane: Only significant difference between 30-40% and 40-60%, 
##### p.adj = 0.034969349 and 0.002187043, respectively.
##### Without hexadecane: Only significant difference between 30-60% and 40-60%, 
##### p.adj = 0.02788163 and 0.02251052, respectively.

## Visualising the data.

### Custom theme.
theme_custom <- theme(
  panel.spacing.x = unit(1, "lines"),
  legend.position = 'right',
  panel.grid.major.y = element_line(colour = "#e3e1e1", linetype = 1),
  panel.grid.major.x = element_line(colour = "#e3e1e1", linetype = 1),
  panel.grid.minor.y = element_line(colour = "#e3e1e1", linetype = 1),
  axis.text.x = element_text(hjust = 0.5),
  plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
  plot.margin = margin(10, 10, 10, 10),
  plot.subtitle = element_text(vjust = -250, hjust = 1)
)

### Preparing for statistical annotations.
pairwise_p <- data.frame(
  group1 = c("30", "40", "30", "40"),
  group2 = c("40", "60", "60", "60"),
  y.position = c(2.05e9, 2.10e9, 300000000, 500000000),
  p.adj = c(0.034969349, 0.002187043, 0.02788163, 0.02251052),
  Contamination = c("With hexadecane", "With hexadecane", 
                    "Without hexadecane", "Without hexadecane")
)

pairwise_p$label <- paste0("p.adj = ", signif(pairwise_p$p.adj, 3))

### Plotting the data.
mean_CFU_count <- ggplot() +
  
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
  geom_point(data = data_summary, 
             aes(x = factor(Plate), 
                 y = mean), 
             colour = 'black', 
             size = 2) +
  
#### Plotting error bars for means from data_summary.
  geom_errorbar(data = data_summary,
                aes(x = factor(Plate), 
                    ymin = mean - se, 
                    ymax = mean + se), 
                width = 0.1, 
                colour = "black") +
  
#### Plotting LOESS regression lines for the means.
  geom_smooth(data = data_summary,
              aes(x = factor(Plate), 
                  y = mean,
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

### Print final plot.
mean_CFU_count

### Save the plot as jpg.
ggsave(filename = 'group3.jpg',
       plot = mean_CFU_count,
       width = 10,
       height = 6,
       dpi = 300
)

### Save the plot as pdf.
ggsave(filename = 'group3.pdf',
       plot = mean_CFU_count,
       width = 10,
       height = 6,
       dpi = 300
)

# Things to do:
## Get rid of duplicate code eg. designing the theme twice.
## Make sure that dataframes and variables do not have the same name!!
## Continue writing tibbles and printing stuff out in RFMD.