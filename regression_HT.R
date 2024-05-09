# HT updated on 2024-05-07
# renamed since file from Elise did not have .R extension

#### Run Litter_Decomp_2024.R first to import and clean the .csv file

##Linear regression for shrub litter
# Subset data where Litter_substrate is "Shrub"
shrub_data <- Litter %>% # HT: I don't see file "Litter170424" anywhere, changed name
  filter(Litter_substrate == "Shrub")

# Plotting
pd <- position_dodge(0.1)  # Adjust as needed

# Perform linear regression separately

regression_lines <- lm(Pct_mass_remaining_ashfree ~ pct_ash + microsite + Month, data = shrub_data)

# Calculate R-squared value
r_squared <- summary(regression_lines)$r.squared

# Print the R-squared value
print(r_squared)

# Filter out non-finite values in pct_ash
shrub_data_filtered <- shrub_data[is.finite(shrub_data$pct_ash), ]

# Extract regression line data
regression_data <- data.frame(
  pct_ash = rep(seq(min(shrub_data_filtered$pct_ash), max(shrub_data_filtered$pct_ash), length.out = 100), times = 8),
  microsite = rep(rep(unique(shrub_data_filtered$microsite), each = 100), times = 4),
  Month = rep(rep(unique(shrub_data_filtered$Month), each = 100), times = 2)
)

# Predict values using the linear model
regression_data$Ash_free_mass_remaining <- predict(regression_lines, newdata = regression_data)

# Define shapes for months
month_shapes <- c(16, 18, 15, 17, 20, 21, 22)

# Define colors for microsites
microsite_colors <- c("blue", "brown")

# Define colors for regression lines (different from microsite colors)
regression_line_colors <- c("red", "blue", "green", "purple", "orange", "brown", "pink", "black")

# Plotting
p <- ggplot(shrub_data_filtered, aes(x = pct_ash, y = Ash_free_mass_remaining, color = factor(microsite), shape = factor(Month))) +
  geom_point(position = pd, size = 3) +
  geom_smooth(data = filter(shrub_data_filtered, !duplicated(Month)), 
              method = "lm", se = FALSE, aes(group = Month), 
              size = 1, linetype = "solid", color = regression_line_colors) +
  labs(x = "In(% ash)", y = "ln(% mass remaining)", title = "Linear Regression (Shrub Data)") +
  theme_classic() +
  scale_color_manual(values = microsite_colors) +
  scale_shape_manual(values = month_shapes) +
  scale_y_continuous(limits = c(30, max(shrub_data_filtered$Ash_free_mass_remaining)), breaks = seq(30, max(shrub_data_filtered$Ash_free_mass_remaining), by = 10))+
  theme(axis.text.x = element_text(hjust = 1, color = "black", size = 11, vjust = 1),
        axis.text.y = element_text(angle = 0, color = "black", size = 11, hjust = 1, vjust = 1))

# Display the plot
print(p)

# Add regression lines with different linetypes for each month
p + 
  geom_smooth(method = "lm", se = FALSE, aes(group = Month, linetype = as.factor(Month)), size = 1, color = "black") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")) + # Add more linetypes as needed
  annotate("text", x = Inf, y = Inf, label = paste("R² =", r_squared), hjust = 1, vjust = 1, size = 4, color = "black") ##HT, this is the part of the script I need to modify to get fit a single curve to all the data.

------------------------------------------------------------------------------------------------------------------------------------------------------
  ##Written May 05, 2024
  #This is based on the last month only, which in my case is Month 14
  
  #Subset the 14 months data from the Litter file
  Month_14<- subset(Litter170424, Month=="14")

Month_14_means <- ddply(Month_14, c("Litter_substrate", "microsite"), summarise,
                        N_mass_remaining  = sum(!is.na(Ash_free_mass_remaining)),
                        mean_Ash_free_mass_remaining = mean(Ash_free_mass_remaining, na.rm=TRUE),
                        sd_Ash_free_mass_remaining   = sd(Ash_free_mass_remaining, na.rm=TRUE),
                        se_Ash_free_mass_remaining   = sd_Ash_free_mass_remaining / sqrt(N_mass_remaining ),
                        N_perc_remaining  = sum(!is.na(pct_ash)),
                        mean_pct_ash = mean(pct_ash, na.rm=TRUE),
                        sd_pct_ash   = sd(pct_ash, na.rm=TRUE),
                        se_pct_ash   = sd_pct_ash / sqrt(N_perc_remaining))

print(Month_14_means)
write.csv(Month_14_means, file="Month_14_means.csv")

#Check for normalization first before you run ANOVA  

install.packages("bestNormalize")
install.packages('psych')
library(bestNormalize) 
library(plyr) 
library(psych) 

# check for normality
qqnorm(Month_14$Ash_free_mass_remaining)#Month_14 is my dataframe, and Ash_free_mass_remaining is the column name of the data I want to normalize.
qqline(Month_14$Ash_free_mass_remaining) # visualize the data - if normal the data should fall fairly well along the line
shapiro.test(Month_14$Ash_free_mass_remaining) # formal statistical test for normality - if P<0.05 then the data do NOT fit the assumptions of normality 

#perform three-way ANOVA
Month_14Deco <- aov(Ash_free_mass_remaining ~ Litter_substrate * microsite, data=Month_14)
summary(Month_14Deco)

TukeyHSD(Month_14Deco, "Litter_substrate")
TukeyHSD(Month_14Deco, "microsite")
TukeyHSD(Month_14Deco, "Litter_substrate*microsite")

#########################################################################################################################
##Calculate the relationship between as Perc_ash and Ash free Mass Remaining (%) using "shrub_data_Month_14" as the dataframe.

##This is a linear regression for shrub
library(ggplot2)
library(tidyr)
library(dplyr)

Month_14 <- read.csv("Month_14.csv")

shrub_data_Month_14 <- Month_14 %>%
  filter(Litter_substrate == "Shrub")
# Plotting
pd <- position_dodge(0.1)  # Adjust as needed

# Perform linear regression separately

regression_lines <- lm(Ash_free_mass_remaining ~ pct_ash + microsite + Month, data = shrub_data_Month_14)

##I modified this script below (line 274) on April 19 to only fit a single fit to all the data.

# Fit a single curve to all the data
regression_line <- lm(Ash_free_mass_remaining ~ pct_ash, data = shrub_data_Month_14)  # Fit a linear regression curve using only the pct_ash variable

# Calculate R-squared value
r_squared <- summary(regression_line)$r.squared

# Print the R-squared value
print(r_squared)

# Filter out non-finite values in pct_ash
shrub_data_filtered_shrub_data_Month_14<- shrub_data_Month_14[is.finite(shrub_data_Month_14$pct_ash), ]

# Extract regression line data
regression_data <- data.frame(
  pct_ash = rep(seq(min(shrub_data_filtered_shrub_data_Month_14$pct_ash), max(shrub_data_filtered_shrub_data_Month_14$pct_ash), length.out = 100), times = 8),
  microsite = rep(rep(unique(shrub_data_filtered_shrub_data_Month_14$microsite), each = 100), length.out = 800),
  Month = rep(rep(unique(shrub_data_filtered_shrub_data_Month_14$Month), each = 100), length.out = 800)
)

# Predict values using the linear model
regression_data$Ash_free_mass_remaining <- predict(regression_line, newdata = regression_data)

# Define shapes for months
month_shapes <- c(16, 17, 15, 20, 21)

# Define colors for microsites
microsite_colors <- c("blue", "brown")

# Define colors for regression lines (different from microsite colors)
regression_line_colors <- c("red", "blue", "green", "purple", "orange", "brown", "pink", "black")

# Plotting
p <- ggplot(shrub_data_filtered_shrub_data_Month_14, aes(x = pct_ash, y = Ash_free_mass_remaining, color = factor(microsite), shape = factor(Month))) +
  geom_point(position = pd, size = 3) +
  geom_smooth(data = filter(shrub_data_filtered_shrub_data_Month_14, !duplicated(Month)), 
              method = "lm", se = FALSE, aes(group = Month), 
              linewidth = 1, linetype = "solid", color = regression_line_colors) +
  labs(x = "Percent Ash", y = "Mass remaining (%)", title = "Linear Regression (Shrub Data)") +
  theme_classic() +
  scale_color_manual(values = microsite_colors) +
  scale_shape_manual(values = month_shapes) +
  scale_y_continuous(limits = c(30, max(shrub_data_filtered$Ash_free_mass_remaining)), breaks = seq(30, max(shrub_data_filtered_shrub_data_Month_14$Ash_free_mass_remaining), by = 10))+
  theme(axis.text.x = element_text(hjust = 1, color = "black", size = 11, vjust = 1),
        axis.text.y = element_text(angle = 0, color = "black", size = 11, hjust = 1, vjust = 1))

# Display the plot
print(p)

# Add regression lines with different linetypes for each month
p + 
  geom_smooth(method = "lm", se = FALSE, aes(group = Month, linetype = as.factor(Month)), size = 1, color = "black") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")) + # Add more linetypes as needed
  annotate("text", x = Inf, y = Inf, label = paste("R² =", r_squared), hjust = 1, vjust = 1, size = 4, color = "black")

########################################################################################

##This is a linear regression for Grass


Grass_data_Month_14 <- Month_14 %>%
  filter(Litter_substrate == "Grass")
# Plotting
pd <- position_dodge(0.1)  # Adjust as needed


# Perform linear regression separately

regression_lines <- lm(Ash_free_mass_remaining ~ pct_ash + microsite + Month, data = Grass_data_Month_14)

##I modified this script below (line 274) on April 19 to only fit a single fit to all the data.

# Fit a single curve to all the data
regression_line <- lm(Ash_free_mass_remaining ~ pct_ash, data = Grass_data_Month_14)  # Fit a linear regression curve using only the pct_ash variable

# Calculate R-squared value
r_squared <- summary(regression_line)$r.squared

# Print the R-squared value
print(r_squared)

# Filter out non-finite values in pct_ash
Grass_data_filtered_Grass_data_Month_14<- Grass_data_Month_14[is.finite(Grass_data_Month_14$pct_ash), ]

# Extract regression line data
regression_data <- data.frame(
  pct_ash = rep(seq(min(Grass_data_filtered_Grass_data_Month_14$pct_ash), max(Grass_data_filtered_Grass_data_Month_14$pct_ash), length.out = 100), times = 8),
  microsite = rep(rep(unique(Grass_data_filtered_Grass_data_Month_14$microsite), each = 100), length.out = 800),
  Month = rep(rep(unique(Grass_data_filtered_Grass_data_Month_14$Month), each = 100), length.out = 800)
)

# Predict values using the linear model
regression_data$Ash_free_mass_remaining <- predict(regression_line, newdata = regression_data)

# Define shapes for months
month_shapes <- c(16, 17, 15, 20, 21)


# Define colors for microsites
microsite_colors <- c("blue", "brown")

# Define colors for regression lines (different from microsite colors)
regression_line_colors <- c("red", "blue", "green", "purple", "orange", "brown", "pink", "black")

# Plotting
p <- ggplot(Grass_data_filtered_Grass_data_Month_14, aes(x = pct_ash, y = Ash_free_mass_remaining, color = factor(microsite), shape = factor(Month))) +
  geom_point(position = pd, size = 3) +
  geom_smooth(data = filter(Grass_data_filtered_Grass_data_Month_14, !duplicated(Month)), 
              method = "lm", se = FALSE, aes(group = Month), 
              linewidth = 1, linetype = "solid", color = regression_line_colors) +
  labs(x = "Percent Ash (%)", y = "Mass remaining (%)", title = "Linear Regression (Grass Data)") +
  theme_classic() +
  scale_color_manual(values = microsite_colors) +
  scale_shape_manual(values = month_shapes) +
  scale_y_continuous(limits = c(30, max(Grass_data_filtered$Ash_free_mass_remaining)), breaks = seq(30, max(Grass_data_filtered_Grass_data_Month_14$Ash_free_mass_remaining), by = 10))+
  theme(axis.text.x = element_text(hjust = 1, color = "black", size = 11, vjust = 1),
        axis.text.y = element_text(angle = 0, color = "black", size = 11, hjust = 1, vjust = 1))

# Display the plot
print(p)

# Add regression lines with different linetypes for each month
p + 
  geom_smooth(method = "lm", se = FALSE, aes(group = Month, linetype = as.factor(Month)), size = 1, color = "black") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")) + # Add more linetypes as needed
  annotate("text", x = Inf, y = Inf, label = paste("R² =", r_squared), hjust = 1, vjust = 1, size = 4, color = "black")
