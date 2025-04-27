# Statistical analysis and visualisation of data from spot replicates

# Load required libraries
library(tidyverse)      # data manipulation + wrangling (Wickham et al., 2019)
library(ggplot2)        # data visualisation (Wickham, 2016)
library(FSA)            # non-parametric testing (Ogle et al., 2024)
library(readxl)         # read Excel files (Wickham and Bryan, 2023)
library(scales)         # scale customisation for plots (Wickham and Seidel, 2022)
library(vegan)          # multivariate ecology tools (Oksanen et al., 2022)
library(rcompanion)     # statistics for extension work (Mangiafico, 2024)
library(ggpubr)         # ggplot2 enhancements (Kassambara, 2023)
library(car)            # regression diagnostics (Fox and Weisberg, 2019)

## Read in group 3 data from Excel file

group3 <- read_excel('data/cfu_per_g_d_soil_group3.xlsx')

## Convert relevant columns to factors for analysis

# Convert relevant columns to factors for analysis
group3$Plate <- as.factor(group3$Plate)
group3$Contamination <- as.factor(group3$Contamination)
group3$spot <- as.factor(group3$spot)

## Summarise data

# Calculate means, standard deviation, sample size and standard error

data_summary2 <- group3 %>%
  group_by(Plate, Contamination) %>%
  summarise(mean2 = mean(cfu_count),
            sd2 = sd(cfu_count),
            n2 = n(),
            se2 = sd2 / sqrt(n2))

## Statistical tests

### Fit a linear model on CFU count by moisture level and contamination status

mod2 <- lm(cfu_count ~ Plate + Contamination, data = group3)

### Use the Freedman-Diaconis rule to determine optimal bin width for histogram

iqr2 <- IQR(group3$cfu_count)
n2 <- nrow(group3)
bin_width2 <- 2 * iqr2 / (n2^(1/3))

# Bin width = 237686445

### Visualise residual distribution to check normality

histo2 <- ggplot(group3, aes(x = mod2$residuals)) +
  geom_histogram(binwidth = bin_width2)

# Save the histogram
print(histo2)
ggsave("figures/statistical_figures/group3/group3_mod.residuals_histo.pdf",
       plot = histo2, width = 10, height = 6, dpi = 300)

### Shapiro-Wilk test for normality

shapiro.test(mod2$residuals)

# p = 0.4519 > 0.05, there is not evidence to suggest that the data is not normally distributed.

### Levene's test for homogeneity of variance

#### Fit linear model and conduct Levene's test

levmod2 <- lm(cfu_count ~ Plate + Contamination, data = group3)
leveneTest(residuals(levmod2) ~ group3$spot)

# p = 0.9954 > 0.05, therefore there is not enough evidence to suggest that the data is not homogeneously distributed.

### Plot boxplot of residuals grouped by spot to visualise the spread of variance

group3$resid <- residuals(levmod2)
box2 <- ggplot(group3, aes(x = spot, y = resid)) +
  geom_boxplot() +
  labs(title = "Residual Variance by Spot")

# Save the boxplot
print(box2)
ggsave("figures/statistical_figures/group3/group3_variance_boxplot.pdf",
       plot = box2, width = 10, height = 6, dpi = 300)

## Testing for differences in mean CFU count between soil moisture levels and contamination status

### We will perform a non-parametric test, eg. the Scheirer-Ray-Hare test, to observe overall significance

scheirerRayHare(cfu_count ~ Plate * Contamination, data = group3)

# p(Plate:Contamination) = 0.001737 < 0.05, therefore there is evidence to suggest that soil moisture content does affect growth of P. putida.

### Post-hoc pairwise testing

#### Post-hoc for Contamination
dunnTest(cfu_count ~ Contamination, data = group3, method = "bonferroni")

# p.adj = 0.001356023 < 0.05, therefore there is evidence to suggest that hexadecane presence does affect growth of P. putida.

#### Post-hoc for Plate

##### Contamination = With hexadecane
dunnTest(cfu_count ~ Plate, data = filter(group3, Contamination == "With hexadecane"), method = "bh")

##### Contamination = Without hexadecane
dunnTest(cfu_count ~ Plate, data = filter(group3, Contamination == "Without hexadecane"), method = "bh")

# With hexadecane: Only significant difference between 30-40% and 40-60%, 
# p.adj = 0.034969349 and 0.002187043, respectively.
# Without hexadecane: Only significant difference between 30-60% and 40-60%, 
# p.adj = 0.02788163 and 0.02251052, respectively.
# Therefore there is evidence to suggest that soil moisture content does affect the growth of P. putida, using data from group 3.
# However, as p.adj (With hexadecane, 30-40%) > 0.025 and the difference is positive, we cannot conclude that the difference between 30% and 40% is significant with a one-tailed test.
# The same is true for p.adj (Without hexadecane, 30-60%).
                   
## Visualise group 3 CFU count with group means, SE, smoothed lines, and statistical annotations
                   
### Preparing statistical annotations.
                   

pairwise_p <- data.frame(
                     group1 = c("30", "40", "30", "40"),
                     group2 = c("40", "60", "60", "60"),
                     y.position = c(2.05e9, 2.10e9, 300000000, 500000000),
                     p.adj = c(0.034969349, 0.002187043, 0.02788163, 0.02251052),
                     Contamination = c("With hexadecane", "With hexadecane", 
                                       "Without hexadecane", "Without hexadecane")
                   )
                   
                   pairwise_p$label <- paste0("p.adj = ", signif(pairwise_p$p.adj, 3))
                   
                   # Design a custom theme for the plot
                   
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
                   
                   ### Create plot
                   
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
                   
# Save the plot
ggsave("figures/data_processed_figures/group3/group3.pdf",
       plot = mean_CFU_count2, width = 10, height = 6, dpi = 300)

ggsave("figures/data_processed_figures/group3/group3.jpg",
       plot = mean_CFU_count2, width = 10, height = 6, dpi = 300)
