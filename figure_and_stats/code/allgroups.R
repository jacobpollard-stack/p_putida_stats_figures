# Load necessary libraries.
library(tidyverse)
library(ggplot2)
library(readxl)
library(FSA)
library(ggstatsplot)
library(readxl)

# Read in the data sheetwise and omit missing data.
## Define a variable as the path
path <- '/Users/jacobpollard/Desktop/Uni/1.2/BABS-2 /Report and figure/Report/Y3948024/figure_and_stats/data/cfu_per_gram_dry_soil/cfu_per_g_d_soil_allgroups.xlsx'
# Load the sheet names into a variable.
data_allgroups <- excel_sheets(path)
# Read the sheets and compile them into one dataframe.
data_tidy_list <- lapply(data_allgroups, function(sheet) {
  data <- read_excel(path, sheet = sheet)  # Fix: use 'path' instead of 'data_allgroups'
  data$group <- sheet
  return(data)
})
# Combine into one dataframe
data_tidy <- do.call(rbind, data_tidy_list)
# Summarise data.
data_summary <- data_tidy |>
  group_by(plate) |> 
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
### Data appears to have a positive skew.
shap <- shapiro.test(data_tidy$cfu_count)
shap
### p = 2.038e-8 < 0.05, there is evidence to suggest that the data is not normally distributed.
## Therefore we will perform a non-parametric test, eg. the Kruskal-Wallace test.
krus <- kruskal.test(cfu_count ~ plate, data = data_tidy)
print(krus)
### p = 0.5794 > 0.05 therefore there is no evidence to suggest that moisture level nor presence of hexadecane impacts growth of P. putida.

# Visualise data.
mean_CFU_count <- ggplot() +
  geom_point(data = data_tidy,
             mapping = aes(x = plate, 
                           y = cfu_count, 
                           colour = group
                      )
  ) +
  labs(title = 'CFU count per gram of dry soil \n by group and mean with error bars',
       x = 'Soil moisture level /% of field capacity',
       y = 'CFU Count /µL',
       colour = 'Replicate'
  ) +
  geom_smooth(data = data_tidy,
              mapping = aes(x = plate, 
                            y = cfu_count, 
                            colour = group),
              method = 'loess', 
              se = FALSE
  ) +
  scale_y_continuous(labels = scales::comma)
mean_CFU_count
### The standard deviations are very high for each group, suggesting that the insignificance is due largely to variation between groups.

) +
  geom_point(data = data_summary, 
             mapping = aes(x = plate, 
                           y = mean), 
             colour = 'black', 
             size = 1
  ) +
  geom_errorbar(data = data_summary, 
                mapping = aes(x = plate, 
                              ymin = mean - se, 
                              ymax = mean + se), 
                width = 0.2, 
                colour = 'black'
                
                
                
                
                
                
                
                
                
mean_CFU_count <- ggplot() +
                  geom_point(data = data_tidy,
                             mapping = aes(x = plate, 
                                           y = cfu_count, 
                                           colour = group, 
                                           shape = Contamination),
                             position = position_dodge(width = 0.3)  
                  ) +
                  labs(title = 'CFU count per gram of dry soil \n by group and mean with error bars',
                       x = 'Soil moisture level /% of field capacity',
                       y = 'CFU Count /µL',
                       colour = 'Replicate',
                       shape = 'Contamination'
                  ) +
                  geom_smooth(data = data_tidy,
                              mapping = aes(x = plate, 
                                            y = cfu_count, 
                                            colour = group),
                              method = 'loess', 
                              se = FALSE
                  ) +
                  geom_point(data = data_summary, 
                             mapping = aes(x = plate, 
                                           y = mean), 
                             colour = 'black', 
                             size = 1
                  ) +
                  geom_errorbar(data = data_summary, 
                                mapping = aes(x = plate, 
                                              ymin = mean - se, 
                                              ymax = mean + se), 
                                width = 0.2, 
                                colour = 'black'
                  ) +
                  scale_y_continuous(labels = scales::comma)
                mean_CFU_count
                