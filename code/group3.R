# Spot replicates using 'group3' dataset ---------------------------------------

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
