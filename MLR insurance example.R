# Insurance dataset 
# Load the dataset into a data frame first
# refer to notes on Blackboard for discusisons on 
# dummy variables and how they are generated

insurance_data <- read.csv("insurance.csv", na = "")
str(insurance_data)

# several variables need to be converted
# sex - male = 0, female = 1
# Smoker - yes = 1, no = 0
# Region contains 4 categories
# N = 4, so we need n-1 indicator variables
# = 3 indicator variables
# Code variables in alphabetical order
head(insurance_data$region, 15)

# Convert variables as described above
names(insurance_data)
attach(insurance_data)

insurance_data$sex <- factor(sex,
                             levels = c("male", "female"), 
                             ordered = FALSE)

insurance_data$smoker <- factor(smoker, 
                                levels = c("yes", "no"), 
                                ordered = FALSE)

insurance_data$region <- factor(region,  
                                levels = c("northeast", "northwest", "southeast", "southwest"), 
                                ordered = FALSE)

str(insurance_data)

# View the split of categorical variables within the data frame
# to examine balance
table(insurance_data$sex)
table(insurance_data$smoker)
table(insurance_data$region)


# Initial investigation of data variables
# and their correlations
# Be careful of your interpretation of this chart
pairs(insurance_data)
install.packages("psych")
library(psych)

# Seems there could be a positive correlation between 
# smoker and charges, perhaps charges and age
# and BMI and charges
pairs.panels(insurance_data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# if we build the model now, R will automatically split the
# factor variables
# Alternatively we will control this process

# in linear regression, model represented by:
# y = b0 + B1x1 + B2x2 + B3x3..... + e
# where y = insurance charges
# x1 = age of the person
# x2  sex of the person
# x3 = bmi of the person
# x4 = children
# x5 = smoker
# x6 = region
# It is clear that x1, and x3 are continuous and x2, x4, x5, x6 are categorical
# therefore we need to create dummy variables for the categorical
# variables
# Eg for the smoker variable x5
# x5 = 1 if person is smoker
# x5 = 0 if person is non-smoker

# Initial build of th MLR model
# Dummy varaibles created automatically by R
set.seed(1)
model <- lm(formula = charges ~ 
              age + 
              sex + 
              bmi + 
              children + 
              smoker + 
              region, 
            data = insurance_data)

model
summary(model)

# Obviously age, bmi, children, smokerno have an
# influence over the dependent variable "charges"
# p-value less than the sig value
# sexfemale is not influential on the model
# Keep region for now as I need to use it
# for my research question

# Extract relevant variables
insurance_data <- insurance_data[c(1, 3:7)]

# Round variables that require modification
insurance_data$bmi <- round(bmi, 1)
insurance_data$charges <- round(charges, 2)

# Create the model again with the amended changes
model <- lm(formula = charges ~ 
              age + 
              bmi + 
              children + 
              smoker + 
              region, 
            data = insurance_data)
  
summary(model)

# --------------------------------------
# Now we need to check model assumptions
# --------------------------------------

# Linearity
# We can examine whether a linear 
# correlation exists between continuous variables

scatter.smooth(x = age, 
               y = charges, 
               main = "Insurance charges ~ age", 
               ylab = "Insurance charges (,000)", 
               xlab = "Age (years)")

scatter.smooth(x = bmi, 
               y = charges, 
               main = "Insurance charges ~ bmi", 
               ylab = "Insurance charges (,000)", 
               xlab = "Age (years)")

scatter.smooth(x = children, 
               y = charges, 
               main = "Insurance charges ~ children", 
               ylab = "Insurance charges (,000)", 
               xlab = "Children")

# No info available for plot of continuous versus categorical varaibles
# Instead we can examine relationship with dependent
# and categorical data using a box plot
attach(insurance_data)
plot(smoker, 
     charges, 
     main = "Charges by smoker status", 
     xlab = "Smoker", 
     ylab = "Insurance charges")

plot(region, 
     charges, 
     main = "Charges by region", 
     xlab = "Region", 
     ylab = "Insurance charges")

# Only use this method if you are
# sure about the underlying nature 
# of the variables you are correlating
# Easily open to misinterpretation 
insurance_data$cor_smoker <- ifelse(smoker == "yes", 1, 0)
cor(charges, cor_smoker)


# Normality

with(insurance_data, {
  qqnorm(age, 
         main = "Normality analysis of age data")
  qqline(age)
  })

# Repeat for other continuous data
with(insurance_data, {
  qqnorm(charges[smoker == "yes"], 
         main = "Normality analysis of smoker = yes data")
  qqline(charges[smoker == "yes"])
  })

with(insurance_data, {
  qqnorm(charges[smoker == "no"], 
         main = "Normality analysis of smoker = 'no' data")
  qqline(charges[smoker == "no"])
})

# Regions
with(insurance_data, {
  qqnorm(charges[region == "southwest"], 
         main = "Normality analysis of regional southwest data")
  qqline(charges[region == "southwest"])
})

with(insurance_data, {
  qqnorm(charges[region == "southeast"], 
         main = "Normality analysis of regional southeast data")
  qqline(charges[region == "southeast"])
})

with(insurance_data, {
  qqnorm(charges[region == "northwest"], 
         main = "Normality analysis of regional Northwest data")
  qqline(charges[region == "northwest"])
})

with(insurance_data, {
  qqnorm(charges[region == "northeast"], 
         main = "Normality analysis of regional Northeast data")
  qqline(charges[region == "northeast"])
})

qqnorm(age)
qqline(age, col = "red")

# Check for normality using R methods
# If p-value is lower than 0.05
# then data is not normally distributed

normality_test <- shapiro.test(insurance_data$age)
normality_test$p.value

# We can check the normality in each variable
# using the tapply() function
# We cannot use the test for dichotomous variables
# so we refer to the histogram instead
with(insurance_data, tapply(charges, smoker, shapiro.test))

# Colinearity

# Measure colinearity which is the relationship
# between multiple variables.
# The "tolerance" is an indication of the percent
# variance in the predictor that cannot
# be acounted for by the other predictors
# hence very small values indicate a predictor is redundant

# VIF score should be close to 1 but under 5
# 10+ indicates that the variable is not needed 
# and can be removed from the model

library(car)
vif(model)
