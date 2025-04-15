# Plate replicates using 'allgroups' dataset -----------------------------------

## Load packages
library(tidyverse)
library(ggplot2)
library(FSA)
library(readxl)
library(scales)
library(vegan)
library(rcompanion)
library(ggpubr)
library(car)

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
