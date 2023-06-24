# Data load
data <- read.table("ais.txt", header = TRUE)

data <- data[, 0:12]


# Column list
cols <- c("Sex", "Ht", "Wt", "LBM", "RCC", "WCC", "Hc", "Hg", "Ferr", "SSF", "Bfat")

# Outlier remove
for (col in cols){
  data <- data[!is.element(data[[col]], boxplot.stats(data[[col]])$out), ]
}

# Do regression analysis with fixing sex=0

# Sex = 0
filtered_data_1 <- subset(data, Sex == 0)

# Regression 
model <- lm(BMI ~ Ht + Wt + LBM + RCC + WCC + Hc + Hg + Ferr + SSF + Bfat, data = filtered_data_1)

# Regression Result print
summary(model)

# Do regression analysis with fixing sex=1

# Sex = 1
filtered_data_2 <- subset(data, Sex == 1)

# Regression 
model <- lm(BMI ~ Ht + Wt + LBM + RCC + WCC + Hc + Hg + Ferr + SSF + Bfat, data = filtered_data_2)

# Regression Result print
summary(model)


# Complete regression 

# Interaction term + Stepwise Selection
step(lm(BMI ~ Sex + Ht + Wt + LBM + RCC + WCC + Hc + Hg + Ferr + SSF + Bfat + Sex:Ht + Sex:Wt + Sex:LBM + Sex:RCC + Sex:WCC + Sex:Hc + Sex:Hg + Sex:Ferr + Sex:SSF + Sex:Bfat, data = data), scope = list(lower = ~1, upper = ~Sex + Ht + Wt + LBM + RCC + WCC + Hc + Hg + Ferr + SSF + Bfat + Sex:Ht + Sex:Wt + Sex:LBM + Sex:RCC + Sex:WCC + Sex:Hc + Sex:Hg + Sex:Ferr + Sex:SSF + Sex:Bfat), direction = "both")


# Feature selection, Complete regression
model <- lm(BMI ~ Sex + Ht + Wt + LBM + WCC + Hc + SSF + Bfat + Sex:Ht + Sex:Wt + Sex:LBM + Sex:Bfat, data = data)


# Regression result
summary(model)

plot(model, which = 1)  # Residuals vs Fitted - Linearity
plot(model, which = 2)  # Normality
plot(model, which = 3)  # Scale-Location - Homoscedasticity
plot(model, which = 4)  # Cook's distance
