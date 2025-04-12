# Load necessary libraries.
library(tidyverse)
library(ggplot2)
library(readxl)
library(FSA)
library(ggstatsplot)
library(readxl)
library(scales)
library(devtools)
library(pairwiseAdonis)
library(rstatix)
library(rcompanion)

# Load the data.

## Read data into tibble and coerce to factors.
group3 <- read_excel('/Users/jacobpollard/Desktop/Uni/1.2/BABS-2 /Report and figure/Report/Y3948024/figure_and_stats/data/cfu_per_gram_dry_soil/cfu_per_g_d_soil_group3.xlsx')
group3$Plate <- as.factor(group3$Plate)
group3$Contamination <- as.factor(group3$Contamination)


# Summarise data.

## group3.
data_summary <- group3 |> 
  group_by(Plate, Contamination) |> 
  summarise(mean = mean(cfu_count),
            sd = sd(cfu_count),
            n = length(Plate),
            se = sd / sqrt(n)
  )

# Statistical tests.

## Check for normality.
mod <- lm(cfu_count ~ Plate + Contamination, data = group3)

### Freedman-Diaconis rule for bin width.
iqr <- IQR(group3$cfu_count)
n <- nrow(group3)
bin_width <- 2 * iqr / (n^(1/3))

### Plot residuals to visually check for normality.
histo <- ggplot(mapping = aes(x = mod$residuals),
                data = group3
) +
  geom_histogram(binwidth = bin_width)
histo
#### Data appears to have a positive skew.

## Perform Shapiro-Wilk's test to check for normality.
shap <- shapiro.test(mod$residuals)
shap
#### p = 0.0008261 < 0.05, there is evidence to suggest that the data is not normally distributed.

## Test for uniform distribution of variance.

### Group data by plate and contamination.
mutated <- group3 %>%
  mutate(Group = interaction(Plate, Contamination))
cfu_dist <- dist(mutated$cfu_count)
dispersion <- betadisper(cfu_dist, group = mutated$Group)

### Check for homogeneity of variance.
anova(dispersion)
TukeyHSD(dispersion)

## Visualise
boxplot(dispersion)
#### p = 0.002974 < 0.05, therefore there is evidence to suggest that the data is not homogeneously distributed.
#### The boxplot shows that the data is not homogeneously distributed. The variance is not uniform across groups.

## Scheirer-Ray-Hare test.
SRH <- scheirerRayHare(cfu_count ~ Plate * Contamination, data = group3)
SRH
#### p(Plate:Contamination) = 0.001737 < 0.05, therefore there is evidence to suggest that soil moisture content does affect growth of P. putida.

### Post-hoc for Contamination
dunnTest(cfu_count ~ Contamination, data = group3, method = "bonferroni")
#### p.adj = 0.001356023 < 0.05, therefore there is evidence to suggest that hexadecane presence does affect growth of P. putida.

### Post-hoc for Plate
dunnTest(cfu_count ~ Plate, data = filter(group3, Contamination == "With hexadecane"), method = "bonferroni")
dunnTest(cfu_count ~ Plate, data = filter(group3, Contamination == "Without hexadecane"), method = "bonferroni")
#### With hexadecane: Only significant difference between 40 and 60 %, p.adj = 0.002187043
#### Without hexadecane: Only significant difference between 40 and 60 %, p.adj = 0.02251052

# Visualising the data.

## Custom theme.
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

## Plotting the data.
mean_CFU_count <- ggplot() +
  
  ### Plotting data points.
  geom_point(data = group3,
             aes(x = factor(Plate), 
                 y = cfu_count, 
                 colour = Contamination)) +
  
  ### Plotting LOESS regression lines from data_tidy.
  geom_smooth(data = group3,
              aes(x = factor(Plate), 
                  y = cfu_count, 
                  colour = Contamination), 
              method = "loess", 
              se = FALSE,
              linewidth = 0.3) +
  
  ### Plotting means from data_summary.
  geom_point(data = data_summary, 
             aes(x = factor(Plate), 
                 y = mean), 
             colour = 'black', 
             size = 2) +
  
  ### Plotting error bars for means from data_summary.
  geom_errorbar(data = data_summary,
                aes(x = factor(Plate), 
                    ymin = mean - se, 
                    ymax = mean + se), 
                width = 0.1, 
                colour = "black") +
  
  ### Plotting LOESS regression lines for the means.
  geom_smooth(data = data_summary,
              aes(x = factor(Plate), 
                  y = mean,
                  group = Contamination), 
              method = "loess", 
              colour = "black",
              se = FALSE, 
              linewidth = 1) +
  
  ### Adding labels and changing axis.
  labs(title = expression(italic("Pseudomonas putida")~"\nCFU count per gram of dry soil"),
       x = expression("Soil moisture level /% of field capacity"),
       y = expression("CFU count /g dry soil"),
       colour = "Contamination") +
  
  ### Adjusting the axes and legend.
  scale_x_discrete(labels = c("30", "40", "50", "60"),
                   expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = c(0, 0),
    breaks = c(0, 250000000, 500000000, 750000000, 1000000000, 1250000000, 1500000000, 1750000000, 2000000000, 2250000000),
    limits = c(0, 2.25e9)) +
  scale_colour_manual(values = c('With hexadecane' = '#F8766D', 'Without hexadecane' = '#00BFC4', '4' = '#C77CFF'),
                      labels = c('With hexadecane', 'Without hexadecane', 
                                 'Before incubation at \nrespective soil moisture level')) +
  geom_point(data = group3,
             aes(x = factor(Plate), 
                 y = cfu_count, 
                 colour = Contamination)) +
  
  ### Theming.
  cowplot::theme_cowplot() +
  theme_custom +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

## Print final plot.
mean_CFU_count

