############################################### Data preprocessing ###################################################
rm(list = ls())

install.packages("lmtest")
library(lmtest)

data0 <- read.csv("All_GPUs.csv")
summary(data0)

# Omitting NAs and hidden NAs values
data0[data0 == "none" | data0 == "" | data0 == "\n- " | data0 == "\nUnknown Release Date " | data0 == "None"] <- NA
data0[data0 == "Yes"] <- 1
data0[data0 == "No" ] <- 0

summary(data0)
data1 <- data0[complete.cases(data0$Release_Price),]
# Reformatting Architecture and Name variables
data1$Architecture <- sub("([A-Za-z]+).*", "\\1", data1$Architecture)

data1$Series <- sub("([A-Za-z]+).*", "\\1", data1$Name)
data1 <- data1[,-19]

# Omitting units and turning to numeric variables
data1$Boost_Clock <- as.numeric(gsub("[^0-9.]+", "", data1$Boost_Clock))
data1$Core_Speed <- as.numeric(gsub("[^0-9.]+", "", data1$Core_Speed))
data1$L2_Cache <- as.numeric(gsub("[^0-9.]+", "", data1$L2_Cache))
data1$Max_Power <- as.numeric(gsub("[^0-9.]+", "", data1$Max_Power))
data1$Memory <- as.numeric(gsub("[^0-9.]+", "", data1$Memory))
data1$Memory_Bandwidth <- as.numeric(gsub("[^0-9.]+", "", data1$Memory_Bandwidth))
data1$Memory_Bus <- as.numeric(gsub("[^0-9.]+", "", data1$Memory_Bus))
data1$Memory_Speed <- as.numeric(gsub("[^0-9.]+", "", data1$Memory_Speed))
data1$Pixel_Rate <- as.numeric(gsub("[^0-9.]+", "", data1$Pixel_Rate))
data1$ROPs <- as.numeric(gsub("[^0-9.]+", "", data1$ROPs))
data1$Release_Price <- as.numeric(gsub("[^0-9.]+", "", data1$Release_Price))
data1$Texture_Rate <- as.numeric(gsub("[^0-9.]+", "", data1$Texture_Rate))

# Reformatting Best_Resolution, Resolution_WxH, PSU, Release_Date
split_value1 <- strsplit(data1$Best_Resolution, " x ")
best_width <- sapply(split_value1, function(x) as.numeric(x[1]))
best_height <- sapply(split_value1, function(x) as.numeric(x[2]))
Best_Resolution <- data.frame(Best_Width = best_width, Best_Height = best_height)
data1 <- subset(data1, select = -Best_Resolution)
data1 <- cbind(data1, Best_Resolution)

split_value2 <- strsplit(data1$Resolution_WxH, "x")
resolution_w <- sapply(split_value2, function(x) as.numeric(x[1]))
resolution_h <- sapply(split_value2, function(x) as.numeric(x[2]))
Resolution_WxH <- data.frame(Resolution_W = resolution_w, Resolution_H = resolution_h)
data1 <- subset(data1, select = -Resolution_WxH)
data1 <- cbind(data1, Resolution_WxH)

split_value3 <- strsplit(data1$PSU, " & ")
psu_v <- sapply(split_value2, function(x) as.numeric(gsub("[^0-9.]+", "", x[1])))
psu_amp <- sapply(split_value2, function(x) as.numeric(gsub("[^0-9.]+", "", x[2])))
PSU <- data.frame(PSU_V = psu_v, PSU_Amp = psu_amp)
data1 <- subset(data1, select = -PSU)
data1 <- cbind(data1, PSU)

current_date <- Sys.Date()
Release_Date <- read.table(text = data1$Release_Date, header = FALSE, stringsAsFactors = FALSE)
Release_Date1 <- as.character(Release_Date)
Release_Date2 <- eval(parse(text = Release_Date1))
Release_Date3 <- as.Date(Release_Date2, format = "%d-%b-%Y")
data1$Days_since_RD <- as.numeric(difftime(current_date, Release_Date3, units = "days"))
data1 <- data1[,-24]

# Reorder variables
library(dplyr)
data2 <- data1 %>%
  select(Release_Price, Architecture, Series, Manufacturer, Dedicated, Integrated, 
         Notebook_GPU, SLI_Crossfire, Boost_Clock, Core_Speed, Memory_Type, Memory, 
         Memory_Speed, Memory_Bandwidth, Memory_Bus, Process, Max_Power, PSU_Amp, 
         PSU_V, Shader, Direct_X, Open_GL, Texture_Rate, Pixel_Rate, 
         TMUs, ROPs, L2_Cache, Best_Height, Best_Width, Resolution_W, 
         Resolution_H, DVI_Connection, DisplayPort_Connection, HDMI_Connection, VGA_Connection, Power_Connector, 
         Days_since_RD)

data10 <- data2
data2 <- data2[complete.cases(data2),]
summary(data2)

# One hot encoding

encoded_architecture <- model.matrix(~ data2$Architecture - 1, data = data2)
encoded_series <- model.matrix(~ data2$Series - 1, data = data2)
encoded_manufacturer <- model.matrix(~ data2$Manufacturer - 1, data = data2)
encoded_memorytype <- model.matrix(~ data2$Memory_Type - 1, data = data2)
encoded_process <- model.matrix(~ data2$Process - 1, data = data2)
encoded_directx <- model.matrix(~ data2$Direct_X - 1, data = data2)
encoded_opengl <- model.matrix(~ data2$Open_GL - 1, data = data2)
encoded_powerconnector <- model.matrix(~ data2$Power_Connector - 1, data = data2)

encoded_data <- cbind(encoded_architecture, encoded_series, encoded_manufacturer, encoded_memorytype,
                      encoded_opengl, encoded_directx, encoded_process, encoded_powerconnector)

column_to_exclude <- c(2, 3, 4, 11, 16, 21, 22, 36)

data3 <- data2[, -column_to_exclude]

data4 <- cbind(data3, encoded_data)

## Linear regression condition

# Linearity
data4 <- as.data.frame(sapply(data4, as.numeric))
correlations <- sapply(data4[, -1], function(column) cor(data4[[1]], column))
strong_correlations <- correlations[abs(correlations) >= 0.6]
print(strong_correlations)

data5 <- data4 %>%
  select(Release_Price, Memory, 
         Memory_Bandwidth, Max_Power, Texture_Rate, Pixel_Rate, 
         TMUs, ROPs, L2_Cache, Best_Height, Best_Width)

# Multicolinearity 
library(corrplot)
cor_matrix <- cor(data5)
corrplot(cor_matrix)
print(cor_matrix)
data5 <- data4 %>%
  select(Release_Price, Memory, 
         Memory_Bandwidth, Max_Power, Texture_Rate, Pixel_Rate, 
         TMUs, ROPs, L2_Cache, Best_Width)

data10 <- data10 %>%
  select(Release_Price, Memory, 
         Memory_Bandwidth, Max_Power, Texture_Rate, Pixel_Rate, 
         TMUs, ROPs, L2_Cache, Best_Width)

data11 <- data10[complete.cases(data10),]
################################################# Descriptive statistics #############################################
# Linear regression model
lrm <- lm(data11$Release_Price ~ ., data = data11)
print(lrm)

# Independence, normality and homoscedasticity of residuals
residuals <- residuals(lrm)

plot(lrm$fitted.values, residuals, 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values Plot")

hist(residuals, main = "Histogram of Residuals")

bptest(lrm)

# testing with NVIDIA GeForce RTX 306, NVIDIA GeForce RTX 3070, 
# AMD Radeon RX 6700 XT, NVIDIA GeForce GTX 1660 Super, NVIDIA GeForce GT 1030
new_data <- data.frame(
  Release_Price <- c(329, 499, 479, 299, 90),
  Memory <- c(12228, 8192, 12228, 6144, 2048),
  Memory_Bandwidth <- c(360, 448, 384, 336, 48),
  Max_Power <- c(170,220, 230, 125, 30),
  Texture_Rate <- c(192, 328.6, 308.6, 136.8, 24.6),
  Pixel_Rate <- c(95.5, 127.2, 121.3, 65, 9.8),
  TMUs <- c(120, 184, 192, 88, 24),
  ROPs <- c(48, 96, 64, 48, 16),
  L2_Cache <- c(3072,5120, 4096, 1536, 512),
  Best_Width <- c(3840, 3840, 3840, 3840, 1920)
)

prediction <- predict(lrm, newdata = new_data)
print(prediction)
residuals <- new_data$Release_Price - prediction
print(residuals)

# Visualization, run codes for plots

boxplot(data10$Release_Price, 
        main = "GPU Price Distribution",
        xlab = "Price in USD",
        col = "lightblue",
        border = "black",
        horizontal = TRUE)
hist(data10$Release_Price, 
     main = "GPU Price Distribution",
     xlab = "Price in USD",
     col = "lightblue",
     border = "black",
     breaks = 10)  


plot(data10$Memory, data10$Release_Price, 
     main = "Release price distribution \n with respect to memory",
     xlab = "Memory", ylab = "Release price", col = "blue", pch = 16)

plot(data10$Memory_Bandwidth, data10$Release_Price, 
     main = "Release price distribution \n with respect to memory bandwidth",
     xlab = "Memory bandwidth", ylab = "Release price", col = "blue", pch = 16)


plot(data10$Max_Power, data10$Release_Price, 
     main = "Release price distribution \n with respect to max power",
     xlab = "Max power", ylab = "Release price", col = "blue", pch = 16)

plot(data10$Texture_Rate, data10$Release_Price, 
     main = "Release price distribution \n with respect to texture rate",
     xlab = "Texture rate", ylab = "Release price", col = "blue", pch = 16)

plot(data10$Pixel_Rate, data10$Release_Price, 
     main = "Release price distribution \n with respect to pixel rate",
     xlab = "Pixel rate", ylab = "Release price", col = "blue", pch = 16)

plot(data10$TMUs, data10$Release_Price, 
     main = "Release price distribution \n with respect to TMUs",
     xlab = "TMUs", ylab = "Release price", col = "blue", pch = 16)

plot(data10$ROPs, data10$Release_Price, 
     main = "Release price distribution \n with respect to ROPs",
     xlab = "ROPs", ylab = "Release price", col = "blue", pch = 16)

plot(data10$L2_Cache, data10$Release_Price, 
     main = "Release price distribution \n with respect to L2 cache",
     xlab = "L2 cache", ylab = "Release price", col = "blue", pch = 16)

plot(data10$Best_Width, data10$Release_Price, 
     main = "Release price distribution \n with respect to best width",
     xlab = "Best width", ylab = "Release price", col = "blue", pch = 16)







