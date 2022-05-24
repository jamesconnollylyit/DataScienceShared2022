# USA murder rate dataset
states <- as.data.frame(state.x77)

colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"

variables_of_interest <- c("Murder",
                           "Population",
                           "HS_Grad",
                           "Illiteracy",
                           "Income",
                           "Life_Exp",
                           "Area",
                           "Frost")
pairs(states[variables_of_interest])

scatter.smooth(x = states$Population,
               y = states$Murder,
               xlab = "Population (,000)",
               ylab = "Murder %", 
               main = "Correlation of murder ~ population")

cor(states$Murder, states$Population)

scatter.smooth(x = states$Frost,
                 y = states$Murder,
                 main = "Correlation of Murder ~ Frost",
                 xlab = "Frost",
                 ylab = "Murder %")

cor(states$Murder, states$Frost)

paste("Correlation for Murder and Frost: ", cor(states$Murder, states$Frost))
paste("Correlation for Murder and Illiteracy: ", cor(states$Murder, states$Illiteracy))
paste("Correlation for Murder and Population: ", cor(states$Murder, states$Population))
paste("Correlation for Murder and HS Grad: ", cor(states$Murder, states$HS_Grad))
paste("Correlation for Murder and Income: ", cor(states$Murder, states$Income))
paste("Correlation for Murder and Life Exp: ", cor(states$Murder, states$Life_Exp))
paste("Correlation for Murder and Area: ", cor(states$Murder, states$Area))

opar <- par(no.readonly = TRUE)
par(mfrow = c(4, 2)) # divide graph area in 3 rows by 2 columns
attach(states)
boxplot(Murder,
        main = "Murder",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Murder)$out)) # box plot for 'murder'
boxplot(Population,
        main = "Population",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Population)$out)) # box plot for 'Population'
boxplot(states$HS_Grad,
        main = "Graduation",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$HS_Grad)$out)) # box plot for 'HS Grad'
boxplot(Illiteracy,
        main = "Illiteracy",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Illiteracy)$out)) # box plot for 'HS Grad'
boxplot(Income,
        main = "Income",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Income)$out)) # box plot for 'HS Grad'
boxplot(Frost,
        main = "Frost",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Frost)$out)) # box plot for 'HS Grad'
boxplot(states$Life_Exp,
        main = "Life Exp",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$Life_Exp)$out)) # box plot for 'HS Grad'
detach(states)
par(opar)

# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(states$Population)$out # outlier values.
paste("Population outliers: ", paste(outlier_values, collapse=", "))

# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(states$Income)$out # outlier values.
paste("Income outliers: ", paste(outlier_values, collapse=", "))

# Remove population outliers
states <- subset(states,
                 states$Population != 21198
                 & states$Population != 11197
                 & states$Population != 18076
                 & states$Population != 11860
                 & states$Population != 12237)
# Remove Income outliers
states <- subset(states, states$Income != 6315)


