
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

### Bacterial growth was quantified using colony-forming unit (CFU) counts from
### spot plates prepared via serial dilution of the soil suspended in 
### $\frac{1}{4}$ strength Ringer solution
### (Ringer, 1883). 4 replicates were used per spot plate, with 3 plates
### per sample.

### The identity of the bacteria was confirmed by a series of tests, including
### oxidase testing, light microscopy, culturing on selective media, and PCR
### followed by gel electrophoresis.

## Description of data ---------------------------------------------------------

### Mean colony counts were normalised to CFU per gram of dry soil. 
### Relevant datasets include:

### - General data: `cfu_per_g_d_soil_allgroups.xlsx`
### - Group 3 (spot replicate) data: `cfu_per_g_d_soil_group3.xlsx`
### - Pre-incubation values: Sheet 4 in the general dataset
### - Gel image (PCR): `data_gel_image`

### All files are stored under the directory: `/Y3948024/R/data`.

## Analytical Overview ----------------------------------------------------------

### All data processing and visualisation were conducted in 
### **R version 4.4.1 (2024-06-14) -- "Race for Your Life"** via **RStudio**, 
### using **Git** for version control 
### on macOS Sequoia (v15.3.1).

### Tests used:

### - **Normality**: Shapiro–Wilk test (Shapiro & Wilk, 1965)
### - **Variance homogeneity**: ANOVA and Tukey’s HSD test 
###   (Fisher, 1925; Tukey, 1949)
### - **Non-parametric comparison**: Scheirer–Ray–Hare test 
###   (Scheirer et al., 1976), with post-hoc **Dunn’s tests** (Dunn, 1964)

### The report includes the gel image which is not analysed here.

## Packages and software -------------------------------------------------------

### All packages were obtained from CRAN (Accessed 14 April 2025).

### Posit (2024) RStudio: Integrated development environment for R. 
### Version 2024.12.0+467. Boston, MA: Posit Software, PBC. 
### Available at: https://posit.co/products/open-source/rstudio/ 
### (Accessed: 14 April 2025).
### R Core Team (2024) R: A language and environment for statistical computing. 
### Version 2024.12.0+467. Vienna: R Foundation for Statistical Computing. 
### Available at: https://www.R-project.org/ (Accessed: 14 April 2025).

library(tidyverse)
### For data manipulation.
### Wickham, H. et al. (2019) 
### Welcome to the tidyverse.
### R package version 2.0.0. 
### Available at: https://CRAN.R-project.org/package=tidyverse 
### (Accessed: 14 April 2025).

library(ggplot2)
### For data visualisation.
### Wickham, H. (2016)
### ggplot2: Elegant graphics for data analysis. 
### New York: Springer-Verlag. 
### R package version 3.5.1. 
### Available at: https://CRAN.R-project.org/package=ggplot2 
### (Accessed: 14 April 2025).

library(FSA)
### For Scheirer-Ray-Hare test and Dunn's post-hoc test.
### Ogle, D.H., Doll, J.C., Wheeler, P. and Dinno, A. (2024)
### FSA: Fisheries stock analysis. 
### R package version 0.9.5. A
### vailable at: https://CRAN.R-project.org/package=FSA 
### (Accessed: 14 April 2025).

library(readxl)
### For reading in Excel files.
### Wickham, H. and Bryan, J. (2023)
### readxl: Read Excel files. 
### R package version 1.4.3. 
### Available at: https://CRAN.R-project.org/package=readxl 
### (Accessed: 14 April 2025).

library(scales)
### For figure aesthetics.
### Wickham, H. and Seidel, D. (2022)
### scales: Scale functions for visualization. 
### R package version 1.3.0. 
### Available at: https://CRAN.R-project.org/package=scales
### (Accessed: 14 April 2025).

library(vegan)
### For multivariate analysis.
### Oksanen, J. et al. (2022
### vegan: Community ecology package.
### R package version 2.6-4. 
### Available at: https://CRAN.R-project.org/package=vegan 
### (Accessed: 14 April 2025).

library(rcompanion)
### For Scheirer-Ray-Hare test and Dunn's post-hoc test.
### Mangiafico, S. (2024)
### rcompanion: Functions to support extension education program evaluation.
### R package version 2.4.30.
### Available at: https://CRAN.R-project.org/package=rcompanion
### (Accessed: 14 April 2025).

library(ggpubr)
### For building stats on top of ggplot2 plots.
### Kassambara, A. (2023)
### ggpubr: 'ggplot2' based publication ready plots.
### R package version 0.6.0.
### Available at: https://CRAN.R-project.org/package=ggpubr
### (Accessed: 14 April 2025).

library(car)
### For Levene's test.
### Fox, J. and Weisberg, S. (2019)
### car: An R Companion to Applied Regression.
### R package car version 3.1-2.
### Available at: https://CRAN.R-project.org/package=car
### (Accessed: 14 April 2025).

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

### Combine into one dataframe and coerce to factors.
data_tidy <- do.call(rbind, data_tidy_list)
data_tidy$plate <- as.factor(data_tidy$plate)
data_tidy$Contamination <- as.factor(data_tidy$Contamination)
data_tidy$group <- as.factor(data_tidy$group)
str(data_tidy)
### Call:
### tibble [32 × 4] (S3: tbl_df/tbl/data.frame)
### $ plate        : Factor w/ 4 levels "30","40","50",..: 1 2 3 4 1 2 3 4 1 2 ...
### $ Contamination: Factor w/ 2 levels "With hexadecane",..: 1 1 1 1 2 2 2 2 1 1 ...
### $ cfu_count    : num [1:32] 2.84e+08 2.60e+08 6.98e+08 1.27e+09 7.14e+06 ...
### $ group        : chr [1:32] "1" "1" "1" "1" ...

## Summarise data.

data_summary <- data_tidy |>
  filter(group != 4) |> 
  group_by(plate, Contamination) |> 
  summarise(mean = mean(cfu_count),
            sd = sd(cfu_count),
            n = length(plate),
            se = sd / sqrt(n)
  )

str(data_summary)
### Call:
### gropd_df [8 × 6] (S3: grouped_df/tbl_df/tbl/data.frame)
### $ plate        : Factor w/ 4 levels "30","40","50",..: 1 1 2 2 3 3 4 4
### $ Contamination: Factor w/ 2 levels "With hexadecane",..: 1 2 1 2 1 2 1 2
### $ mean         : num [1:8] 3.53e+08 4.82e+07 2.65e+09 8.61e+07 1.22e+09 ...
### $ sd           : num [1:8] 3.58e+08 4.35e+07 4.35e+09 4.24e+07 1.55e+09 ...
### $ n            : int [1:8] 3 3 3 3 3 3 3 3
### $ se           : num [1:8] 2.07e+08 2.51e+07 2.51e+09 2.45e+07 8.93e+08 ...
### - attr(*, "groups")= tibble [4 × 2] (S3: tbl_df/tbl/data.frame)
### ..$ plate: Factor w/ 4 levels "30","40","50",..: 1 2 3 4
### ..$ .rows: list<int> [1:4] 
### .. ..$ : int [1:2] 1 2
### .. ..$ : int [1:2] 3 4
### .. ..$ : int [1:2] 5 6
### .. ..$ : int [1:2] 7 8
### .. ..@ ptype: int(0) 
### ..- attr(*, ".drop")= logi TRUE

## Statistical tests.

### Check for normality in each sheet.

#### Fit linear model.
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
bin_width
#### bin_width = 237686445

#### Plot residuals to visually check for normality.
histo <- ggplot(mapping = aes(x = mod$residuals),
                data = data_tidy
) +
  geom_histogram(binwidth = bin_width)
histo

#### Save the plot as pdf.
ggsave(filename = 'allgroups_mod.residuals_histo.pdf',
       plot = histo,
       width = 10,
       height = 6,
       dpi = 300
)

#### Data appears to have a positive skew.

#### Perform Shapiro-Wilk's test to check for normality.
shap <- shapiro.test(mod$residuals)
shap
#### Call:
#### data:  mod$residuals
#### W = 0.64622, p-value = 1.496e-07

#### p = 1.497e-7 < 0.05, there is evidence to suggest that the data is not 
#### normally distributed.

### Test for uniform distribution of variance.

#### Filter out group 4
data_filtered <- data_tidy %>% 
  filter(group != "4")

#### Fit linear model.
levmod <- lm(cfu_count ~ plate + Contamination, data = data_filtered)
levmod
#### Call:
#### lm(formula = cfu_count ~ plate + Contamination, data = data_filtered)

#### Coefficients:
####   (Intercept)                          plate40  
#### 683976017                       1165251806  
#### plate50                          plate60  
#### 581025979                        246068785  
#### ContaminationWithout hexadecane  
#### -966837917  

#### Levene's test for homogeneity of variance.
lev <- leveneTest(residuals(levmod) ~ data_filtered$group)
lev
#### Call:
#### Df F value Pr(>F)
#### group  2  1.3126 0.2903
#### 21    

### Visualise and save as pdf.
data_filtered$resid <- residuals(levmod)
boxplot <- ggplot(data_filtered, aes(x = group, y = resid)) +
  geom_boxplot() +
  labs(title = "Residual Variance by Group")
boxplot

ggsave(filename = 'allgroups_variance_boxplot.pdf',
       plot = boxplot,
       width = 10,
       height = 6,
       dpi = 300
)

#### p = 0.2903 > 0.05, therefore there is no evidence to suggest that the
#### data is not homogeneously distributed.

## Testing for differences in means.

### We will perform a non-parametric test, eg. the Scheirer-Ray-Hare test.
SRH <- scheirerRayHare(cfu_count ~ plate * Contamination, data = filter(data_tidy, group != 4))
SRH
### Call:
###                     Df Sum Sq      H p.value
### plate                3  98.33 1.9667 0.57935
### Contamination        1  80.67 1.6133 0.20402
### plate:Contamination  3  52.33 1.0467 0.78996
### Residuals           16 918.67 
### p(Plate:Contamination) = 0.78996 > 0.05, therefore there is no evidence to
### suggest that soil moisture content does affect growth of P. putida, 
### using data from all groups.

## Post-hoc pairwise testing

### Post-hoc for Contamination
dunnTest(cfu_count ~ Contamination, data = filter(data_tidy, group != 4), method = "bonferroni")
### Call:
###                             Comparison        Z   P.unadj
### 1 With hexadecane - Without hexadecane 1.270171 0.2040239
### P.adj
### 1 0.2040239

### p.adj = 0.2040239 > 0.05, therefore there is no evidence to suggest that 
### hexadecane presence does affect growth of P. putida.

### Post-hoc for Plate
dunnTest(cfu_count ~ plate, data = filter(data_tidy, Contamination == "With hexadecane", group != 4), method = "bh")
### Call:
###   Comparison          Z P.unadj P.adj
### 1    30 - 40 -0.1132277 0.90985     1
### 2    30 - 50 -0.1132277 0.90985     1
### 3    40 - 50  0.0000000 1.00000     1
### 4    30 - 60  0.0000000 1.00000     1
### 5    40 - 60  0.1132277 0.90985     1
### 6    50 - 60  0.1132277 0.90985     1
dunnTest(cfu_count ~ plate, data = filter(data_tidy, Contamination == "Without hexadecane", group != 4), method = "bh")
### Call:
###   Comparison          Z    P.unadj     P.adj
### 1    30 - 40 -0.5661385 0.57129962 0.6855595
### 2    30 - 50 -1.6984156 0.08942936 0.5365762
### 3    40 - 50 -1.1322770 0.25751798 0.5150360
### 4    30 - 60 -1.5851878 0.11292366 0.3387710
### 5    40 - 60 -1.0190493 0.30817955 0.4622693
### 6    50 - 60  0.1132277 0.90985003 0.9098500

### With hexadecane: No significant differences.
### Without hexadecane: No significant differences.
### Therefore there is no evidence to suggest that soil moisture content
### affects the  growth of P. putida, using meaned data from all 3 groups.

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

## Read in the data.

### Read data into tibble and coerce to factors.
group3 <- read_excel('data/cfu_per_g_d_soil_group3.xlsx')
group3$Plate <- as.factor(group3$Plate)
group3$Contamination <- as.factor(group3$Contamination)
group3$spot <- as.factor(group3$spot)
str(group3)
### Call:
### tibble [32 × 4] (S3: tbl_df/tbl/data.frame)
### $ Plate        : Factor w/ 4 levels "30","40","50",..: 1 1 1 1 2 2 2 2 3 3 ...
### $ spot         : num [1:32] 1 2 3 4 1 2 3 4 1 2 ...
### $ Contamination: Factor w/ 2 levels "With hexadecane",..: 1 1 1 1 1 1 1 1 1 1 ...
### $ cfu_count    : num [1:32] 2.63e+08 3.44e+08 2.53e+08 1.67e+08 1.88e+09 ...

## Summarise data.

data_summary2 <- group3 |> 
  group_by(Plate, Contamination) |> 
  summarise(mean = mean(cfu_count),
            sd = sd(cfu_count),
            n = length(Plate),
            se = sd / sqrt(n)
  )
str(data_summary2)
### Call:
### gropd_df [8 × 6] (S3: grouped_df/tbl_df/tbl/data.frame)
### $ Plate        : Factor w/ 4 levels "30","40","50",..: 1 1 2 2 3 3 4 4
### $ Contamination: Factor w/ 2 levels "With hexadecane",..: 1 2 1 2 1 2 1 2
### $ mean         : num [1:8] 2.57e+08 2.34e+07 1.92e+09 2.17e+07 7.42e+08 ...
### $ sd           : num [1:8] 7.22e+07 2.89e+06 1.63e+08 2.35e+06 1.22e+08 ...
### $ n            : int [1:8] 4 4 4 4 4 4 4 4
### $ se           : num [1:8] 36124433 1445813 81695780 1173376 60883157 ...
### - attr(*, "groups")= tibble [4 × 2] (S3: tbl_df/tbl/data.frame)
### ..$ Plate: Factor w/ 4 levels "30","40","50",..: 1 2 3 4
### ..$ .rows: list<int> [1:4] 
### .. ..$ : int [1:2] 1 2
### .. ..$ : int [1:2] 3 4
### .. ..$ : int [1:2] 5 6
### .. ..$ : int [1:2] 7 8
### .. ..@ ptype: int(0) 
### ..- attr(*, ".drop")= logi TRUE

## Statistical tests.

### Check for normality.

#### Fit linear model.
mod2 <- lm(cfu_count ~ Plate + Contamination, data = group3)
summary(mod2)
#### Call:
#### lm(formula = cfu_count ~ Plate + Contamination, data = group3)

#### Residuals:
####  Min         1Q     Median         3Q        Max 
#### -638869021 -313778026   32627500  238697854  847561625 

#### Coefficients:
####  Estimate Std. Error t value Pr(>|t|)    
#### (Intercept)                      449952849  171020372   2.631 0.013896 *  
####  Plate40                          828925109  216325561   3.832 0.000689 ***
####  Plate50                          325771412  216325561   1.506 0.143696    
####  Plate60                            -492229  216325561  -0.002 0.998201    
#### ContaminationWithout hexadecane -619799354  152965271  -4.052 0.000386 ***
####    ---
##¢#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### Residual standard error: 432700000 on 27 degrees of freedom
#### Multiple R-squared:  0.572,	Adjusted R-squared:  0.5086 
#### F-statistic: 9.021 on 4 and 27 DF,  p-value: 9.225e-05

#### Freedman-Diaconis rule for bin width.
iqr2 <- IQR(group3$cfu_count)
n2 <- nrow(group3)
bin_width2 <- 2 * iqr2 / (n2^(1/3))
bin_width2
#### bin_width = 237686445

#### Plot residuals to visually check for normality.
histo2 <- ggplot(mapping = aes(x = mod2$residuals),
                 data = group3
) +
  geom_histogram(binwidth = bin_width2)
histo2

#### Save the plot as pdf.
ggsave(filename = 'group3_mod.residuals_histo.pdf',
       plot = histo2,
       width = 10,
       height = 6,
       dpi = 300
)

#### Data appears to have a negative skew.

#### Perform Shapiro-Wilk's test to check for normality.
shap2 <- shapiro.test(mod2$residuals)
shap2
#### Call:
#### data:  mod2$residuals
#### W = 0.96822, p-value = 0.4519

#### p = 0.4519 > 0.05, there is not evidence to suggest that the data is not 
#### normally distributed.

### Test for uniform distribution of variance.

#### Fit linear model.
levmod2 <- lm(cfu_count ~ Plate + Contamination, data = group3)
levmod2
#### Call:
#### lm(formula = cfu_count ~ Plate + Contamination, data = group3)

#### Coefficients:
####   (Intercept)                          Plate40  
#### 449952849                        828925109  
#### Plate50                          Plate60  
#### 325771411                          -492229  
#### ContaminationWithout hexadecane  
#### -619799354  

#### Levene's test for homogeneity of variance.
lev2 <- leveneTest(residuals(levmod2) ~ group3$spot)
lev2
#### Call:
#### Df F value Pr(>F)
#### group  3  0.0223 0.9954
#### 28 

#### Visualise and save as pdf.
group3$resid <- residuals(levmod2)
boxplot2 <- ggplot(group3, aes(x = spot, y = resid)) +
  geom_boxplot() +
  labs(title = "Residual Variance by Spot")
boxplot2

ggsave(filename = 'group3_variance_boxplot.pdf',
       plot = boxplot2,
       width = 10,
       height = 6,
       dpi = 300
)

#### p = 0.9954 > 0.05, therefore there is no evidence to suggest that the
#### data is not homogeneously distributed.

## Testing for differences between means.

### Scheirer-Ray-Hare test.
SRH2 <- scheirerRayHare(cfu_count ~ Plate * Contamination, data = group3)
SRH2
### Call:
### Df  Sum Sq       H  p.value
### Plate                3  336.25  3.8217 0.281369
### Contamination        1  903.12 10.2647 0.001356
### Plate:Contamination  3 1328.13 15.0951 0.001737
### Residuals           24  160.00

### p(Plate:Contamination) = 0.001737 < 0.05, therefore there is evidence to 
### suggest that soil moisture content does affect growth of P. putida.

## Post-hoc pairwise testing.

### Post-hoc for Contamination
dunnTest(cfu_count ~ Contamination, data = group3, method = "bonferroni")
### Call:
### Comparison        Z     P.unadj       P.adj
### 1 With hexadecane - Without hexadecane 3.203852 0.001356023 0.001356023

### p.adj = 0.001356023 < 0.05, therefore there is evidence to suggest that 
### hexadecane presence does affect growth of P. putida.

### Post-hoc for Plate
dunnTest(cfu_count ~ Plate, data = filter(group3, Contamination == "With hexadecane"), method = "bh")
#### Call:
#### Comparison         Z      P.unadj       P.adj
#### 1    30 - 40 -2.376354 0.0174846744 0.034969349
#### 2    30 - 50 -1.188177 0.2347636628 0.234763663
#### 3    40 - 50  1.188177 0.2347636628 0.281716395
#### 4    30 - 60  1.188177 0.2347636628 0.352145494
#### 5    40 - 60  3.564531 0.0003645072 0.002187043
#### 6    50 - 60  2.376354 0.0174846744 0.052454023
dunnTest(cfu_count ~ Plate, data = filter(group3, Contamination == "Without hexadecane"), method = "bh")
#### Call:
#### Comparison          Z     P.unadj      P.adj
#### 1    30 - 40  0.2972629 0.766265789 0.76626579
#### 2    30 - 50 -1.8578932 0.063184176 0.09477626
#### 3    40 - 50 -2.1551562 0.031149616 0.06229923
#### 4    30 - 60 -2.6010505 0.009293876 0.02788163
#### 5    40 - 60 -2.8983135 0.003751754 0.02251052
#### 6    50 - 60 -0.7431573 0.457386453 0.54886374

#### With hexadecane: Only significant difference between 30-40% and 40-60%, 
#### p.adj = 0.034969349 and 0.002187043, respectively.
#### Without hexadecane: Only significant difference between 30-60% and 40-60%, 
#### p.adj = 0.02788163 and 0.02251052, respectively.

## Visualising the data.

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
                 y = mean), 
             colour = 'black', 
             size = 2) +
  
  #### Plotting error bars for means from data_summary.
  geom_errorbar(data = data_summary2,
                aes(x = factor(Plate), 
                    ymin = mean - se, 
                    ymax = mean + se), 
                width = 0.1, 
                colour = "black") +
  
  #### Plotting LOESS regression lines for the means.
  geom_smooth(data = data_summary2,
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
mean_CFU_count2

### Save the plot as jpg.
ggsave(filename = 'group3.jpg',
       plot = mean_CFU_count2,
       width = 10,
       height = 6,
       dpi = 300
)

### Save the plot as pdf.
ggsave(filename = 'group3.pdf',
       plot = mean_CFU_count2,
       width = 10,
       height = 6,
       dpi = 300
)
