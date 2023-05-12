# Checking Directory 
getwd()

# Installing packages
install.packages("tidyverse")
install.packages("GGally")
install.packages("ggfortify")
install.packages("broom")
install.packages("stargazer")
install.packages("caTools")
install.packages("vtable")
library(tidyverse)
library(GGally)
library(ggfortify)
library(broom)
library(stargazer)
library(caTools)
library(vtable)
library(stringr)
library(dplyr)
library(ggplot2)
library(car)
# Importing Data
movies <- read.csv("movie_data.csv", header = TRUE,  sep = ",")
head(movies, 5)
str(movies)

# Cleaning data / removing blank cells
movies2 <- movies[ , c("Series_Title", "Runtime", "Genre", "Meta_score", "Gross")] #filtering relevant columns
movies3 <- movies2[!(is.na(movies2$Gross) | movies2$Gross==""),] #filtering blank cells
movies4 <- movies3[!(is.na(movies3$Meta_score) | movies3$Meta_score=="NA"),] #filtering NA cells
movies4$Runtime <- str_sub(movies4$Runtime, end=-4) # Removing "mins" from runtime
movies4$Genre <- sub("\\,.*","\\",movies4$Genre) # Removing multiple genres, ONE PRIMARY GENRE
str(movies4)

# Converting classes 
movies5 = movies4
movies5$Genre <- as.factor(movies5$Genre) #Converting "Genre" from CHR to Factor 
movies5$Gross <- as.numeric(gsub(",", "", movies5$Gross)) #Converting "Gross" from CHR to NUM and removing commas
movies5$Runtime <- as.integer(movies5$Runtime) # Converting "Runtime" from CHR to INT
unique(movies5$Genre) # Checking Factor Levels
str(movies5) 


sumtable(movies5) # Descriptive Statistics
vartable <- vtable(movies5,out='latex') # Table of descriptive statistics 

# Building Original Model
model <- lm(Gross ~ Runtime + Genre + Meta_score, data = movies5) # OG Full Model
summary(model)



# Assumptions for original model
model.diag.metrics <- augment(model)
head(model.diag.metrics)
autoplot(model) # Gives 4*4 Residual*Fitted, Normal Q-Q, Scale-Location, and Residual vs. Leverage Plots
plot(model) # Gives one at a time Residual*Fitted, Normal Q-Q, Scale-Location, and Residual vs. Leverage Plots
plot(model, 4) # Cook's distance
shapiro.test(model$residuals) # Shapiro-Wilks Normality Test

# Building Log(Y) Transformation Model
model2 <- lm(log(Gross) ~ Runtime + Genre + Meta_score, data = movies5) # Applied a Log(Y) Transformation 
summary(model2)

# Cleaning Model 2
influence <- cooks.distance(model2) # Scaling cooks distance for every point
leverage <- hatvalues(model2) # Scaling leverage for every point

movies6 <- movies5 %>% # removing points that were influential / outliers in our model
  mutate(cooks_distance = influence, leverage = leverage) %>%
  filter(cooks_distance < 0.01) %>%
  filter(leverage < 0.05)

cleaned_model <-lm(log(Gross) ~ Runtime + Genre + Meta_score, data = movies6) # New cleaned FULL model
summary(cleaned_model)

sumtable(movies6)
vartable2 <- vtable(movies6,out='latex')

# Assumptions Model 2
plot(cleaned_model)
plot(cleaned_model, 4)
ggplot(data = movies6, aes(x = cleaned_model$residuals)) + geom_histogram(fill = 'steelblue', color = 'black') + labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency') # Histogram for residuals
shapiro.test(cleaned_model$residuals)



# Model 2 linear regression testing 
mod1 = cleaned_model
mod2 <- lm(log(Gross) ~ Runtime + Genre, data = movies6) # Reduced Model X1|X2
summary(mod2)
mod3 <- lm(log(Gross) ~ Runtime + Meta_score, data = movies6) # Reduced Model X1|X3
summary(mod3)
mod4 <- lm(log(Gross) ~ Genre + Meta_score, data = movies6) # Reduced Model X2|X3
summary(mod4)
stargazer(mod1, mod2, mod3, mod4,
          title = "Linear Regression Testing",
          column.labels = c("Test 1", "Test 2", "Test 3", "Test 4"),
          covariate.labels = c("Runtime", "Genre: Adventure", "Genre: Animation", "Genre: Biography", "Genre: Comedy", "Genre: Crime", "Genre: Drama", "Metascore"),
          header = FALSE, # to get rid of r package output text
          single.row = TRUE, # to put coefficients and standard errors on same line
          no.space = FALSE, # to remove the spaces after each line of coefficients
          column.sep.width = "-10pt", # to reduce column width
          font.size = "tiny", # to make font size smaller
          report = ("vc*p"), # Get p-values
          out = "/Users/malachypercy-campbell/Documents/STAT 3494W/STAT3494_LinearModels") # Cleaner regression summary of ALL models
vif(mod1)
Model44 <- lm(log(Gross) ~ Meta_score, data = movies6)
summary(Model44)
