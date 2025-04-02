# Load necessary libraries.
library(tidyverse)
library(ggplot2)
library(readxl)
library(FSA)
library(ggstatsplot)
library(readxl)
library(scales)

# Read in the data sheetwise and omit missing data.

## Define the path.
path <- '/Users/jacobpollard/Desktop/Uni/1.2/BABS-2 /Report and figure/Report/Y3948024/figure_and_stats/data/cfu_per_gram_dry_soil/cfu_per_g_d_soil_allgroups.xlsx'

## Load the sheet names into a variable.
data_allgroups <- excel_sheets(path)

## Read the sheets and compile them into one dataframe.
data_tidy_list <- lapply(data_allgroups, function(sheet) {
  data <- read_excel(path, sheet = sheet)
  data$group <- sheet
  return(data)
})

## Combine into one dataframe.
data_tidy <- do.call(rbind, data_tidy_list)

# Summarise data.
data_summary <- data_tidy |>
  filter(group != 4) |> 
  group_by(plate, Contamination) |> 
  summarise(mean = mean(cfu_count),
            sd = sd(cfu_count),
            n = length(plate),
            se = sd / sqrt(n)
  )

# Statistical tests.

## Check for normality in each sheet.
mod <- lm(cfu_count ~ group, data = data_tidy)

### Freedman-Diaconis rule for bin width.
iqr <- IQR(data_tidy$cfu_count)
n <- nrow(data_tidy)
bin_width <- 2 * iqr / (n^(1/3))

### Plot residuals to visually check for normality.
histo <- ggplot(mapping = aes(x = mod$residuals),
                data = data_tidy
) +
  geom_histogram(binwidth = bin_width)
histo
#### Data appears to have a positive skew.

## Perform Shapiro-Wilk's test to check for normality.
shap <- shapiro.test(data_tidy$cfu_count)
shap
#### p = 2.25e-10 < 0.05, there is evidence to suggest that the data is not normally distributed.

### Therefore we will perform a non-parametric test, eg. the Kruskal-Wallace test.
krus <- kruskal.test(cfu_count ~ plate, data = data_tidy)
print(krus)
#### p = 0.8372 > 0.05 therefore there is no evidence to suggest that moisture level nor presence of hexadecane impacts growth of P. putida.

# Visualising the data.

## Custom theme.
theme_custom <- theme(
  panel.spacing.x = unit(1, "lines"),
  legend.position = 'right',
  panel.grid.major.y = element_line(colour = "#e3e1e1",
                                    linetype = 1),
  panel.grid.major.x = element_line(colour = "#e3e1e1",
                                    linetype = 1),
  axis.text.x = element_text(hjust = 0.5),
  plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
  plot.margin = margin(10, 10, 10, 10),
  plot.subtitle = element_text(vjust = -250,
                               hjust = 1)
)

## Plotting the data.
mean_CFU_count <- ggplot(data = data_tidy) +
  
  ### Plotting replicate data points.
  geom_point(aes(x = factor(plate), 
                 y = cfu_count, 
                 colour = group)) +
  
  ### Plotting LOESS regression lines for replicates from data_tidy.
  geom_smooth(aes(x = factor(plate), 
                  y = cfu_count, 
                  colour = group,
                  group = interaction(group, Contamination)), 
              method = "loess", 
              se = FALSE,
              linewidth = 0.3) +
  
  ### Plotting means from data_summary.
  geom_point(data = data_summary, 
             aes(x = factor(plate), 
                 y = mean), 
             colour = "black", 
             size = 2) +
  
  ### Plotting error bars for means from data_summary.
  geom_errorbar(data = data_summary,
                aes(x = factor(plate), 
                    ymin = mean - se, 
                    ymax = mean + se), 
                width = 0.1, 
                colour = "black") +
  
  ### Plotting LOESS regression lines for the means.
  geom_smooth(data = data_summary,
              aes(x = factor(plate), 
                  y = mean, 
                  group = Contamination), 
              method = "loess", 
              colour = "black",
              se = FALSE, 
              linewidth = 1) +
  
  ### Adding labels and changing axis.
  labs(title = expression(italic("Pseudomonas putida")~"\nCFU count per gram of dry soil, by replicate"),
       x = expression("Soil moisture level /% of field capacity"),
       y = expression("log"[10]~"CFU count /g dry soil"),
       colour = "Replicates") +
  
  ### Faceting by Contamination.
  facet_grid(~ Contamination) +
  
  ### Adjusting the axes and legend.
  scale_x_discrete(labels = c("30", "40", "50", "60"),
                   expand = c(0, 0)) +
  scale_y_log10(labels = scales::comma,
                expand = c(0,0),
                limits = c(1000000, 10000000000)) +
  scale_colour_manual(values = c('1' = '#F8766D', '2' = '#7CAE00', '3' = '#00BFC4', '4' = '#C77CFF'),
                      labels = c('1', '2', '3', 
                                 'Before incubation at \nrespective soil moisture level')) +
  
  ### Theming.
  cowplot::theme_cowplot() +
  theme_custom
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

## Print final plot.
mean_CFU_count

