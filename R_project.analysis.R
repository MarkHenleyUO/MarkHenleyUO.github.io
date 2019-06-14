# Correlation test of U.S. hurricane frequency and sea surface annual temperature anomalies 1980-2015

#Fist we load the data into R.

# Read in Hurricane Data file csv files
HD <- read.table("/Users/MarkHenley/Desktop/Hurricane_Data.csv", 
                 header = TRUE,
                 sep = ",")

# Read in Sea surface temp table csv files
SST <- read.table("/Users/MarkHenley/Desktop/Sea_surface_temp.csv", 
                  header = TRUE,
                  sep = ",")

# Packages used for analysis.


install.packages('plyr')
library(plyr)

install.packages("tidyverse")
library(tidyverse)

library("ggpubr")

install.packages("scatterplot3d")

# We now subsetted both data.tables so that the year ranges matched 1880-2014.

# Subset data for HD, so that the min years match SST data min years with dates after 1880
HD_date <- HD[ which(HD$Year > 1879), ]

# Subset data for HD again, so that the max years are uniform with SST data max years with dates before 2015.
Hurricane <- HD_date[ which(HD_date$Year < 2016), ]

# Subset data for SST, so that the max years are uniform with Hurricane data max years with dates before 2015.
#Sea_surf <- SST[ which(SST$Year < 2016), ]


# We now need to join the three tables so that they can be utilized within the same table. 


# merge two data frames by variable name
Sea.temp.hurr <- merge(Hurricane,SST,by="Year", all=FALSE)


# We now have one table with all our variable on it.

# We can run a simple scatterplot to look at the anomaly data and hurricane strength data over a timeline of our year ranges

# Scatterplot for sea temperature anomaly between 1880-2015
attach(Sea.temp.hurr)
plot(Year, Annual.anomaly, main="Sea Temperature Annual Anomaly",
     xlab="Year ", ylab="Temperature Anomaly °F", pch=16) 

# Scatterplot to see how hurricane strength has varied between 1880-2015
plot(Saffir.Simpson.Category, Year, main="Hurricane Strength Variability 1880-2015",
     xlab="Saffir.Simpson.Category", ylab="Year", pch=16) 

# Knowing that it will be difficult to see a correlation due to varying fluctuation in sea surface temp anomalies over so many data point; thus, we will combine years by ranges using group value. 

year.range.anomaly <- aggregate( Annual.anomaly ~ Group, Sea.temp.hurr, mean )

# We now have a mean annual anomaly for each group.

# Next we need to merge the year.range.anomaly table just created with the Sea.temp.hurrr

# merge two data frames by variable name
Combined_data <- merge(Sea.temp.hurr,year.range.anomaly,by="Group", all=FALSE)

# Because the new data table has a column named 'Annual.anomaly.x' and 'Annual.anomaly.y' now we want to change the last columns name.
Combination <- Combined_data %>% 
  rename(
    Annual.anomaly = Annual.anomaly.x,
    Group.anomaly = Annual.anomaly.y
  )

# Now to get a look at our combined anomaly ranges we will take a quick look at a few scatterplot.

attach(Combination)
plot(Year, Group.anomaly, main="Sea Temperature Annual Anomaly",
     xlab="Year ", ylab="Temperature Anomaly °F", pch=16) 

# 3D Scatterplot with Coloring and Vertical Drop Lines
library(scatterplot3d) 
attach(Combination) 
scatterplot3d(Year,Group.anomaly,Saffir.Simpson.Category, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")

# Count frequency of storms per Group


myFreqs3 <- Combination %>% 
  group_by(Group) %>%
  summarise(Freq = n())
myFreqs3 

# merge the myFreqs3 data table with Combination by variable name
Analysis.data <- merge(Combination,myFreqs3,by="Group", all=FALSE)

# Remove duplicated rows based on Group
Analysis.ready <- Analysis.data %>% 
  distinct(Group, .keep_all = TRUE)

# Run 3d scatterplot again with new data

# 3D Scatterplot with Coloring and Vertical Drop Lines
library(scatterplot3d) 
attach(Analysis.ready) 
scatterplot3d(Year,Group.anomaly,Freq, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")


# Visualize variable Group.anomaly compared to Freq
ggscatter(Analysis.ready, x = "Group.anomaly", y = "Freq", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Year Range Mean Anomaly", ylab = "Total Hurricanes")

# Visualize variables Freq compared to Group.anomaly
ggscatter(Analysis.ready, x = "Freq", y = "Group.anomaly", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total Hurricanes", ylab = "Year Range Mean Anomaly")

# Visualize variable year compared to Group.anomaly
ggscatter(Analysis.ready, x = "Year", y = "Group.anomaly", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Year", ylab = "Year Range Mean Anomaly")

# Visualize variable Saffir.Simpson.Category compared to Group.anomaly
ggscatter(Analysis.ready, x = "Saffir.Simpson.Category", y = "Group.anomaly", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Saffir.Simpson.Category", ylab = "Year Range Mean Anomaly")

# Visualize variable  Group.anomaly compared to Saffir.Simpson.Category
ggscatter(Analysis.ready, x = "Group.anomaly", y = "Saffir.Simpson.Category", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Year Range Mean Anomaly", ylab = "Saffir.Simpson.Category")

# Test with Shapiro-Wilks test

# Shapiro-Wilk normality test for Group.anomaly
shapiro.test(Analysis.ready$Group.anomaly) # W = 0.93093, p-value = 0.08156
# Shapiro-Wilk normality test for Freq
shapiro.test(Analysis.ready$Freq) # => p = W = 0.91996, p-value = 0.04487

# Plot using Q-Q plots for correlation between a given sample and the normal distribution.

# Group.anomaly
ggqqplot(Analysis.ready$Group.anomaly, ylab = "Year Range Anomaly")
# Freq
ggqqplot(Analysis.ready$Freq, ylab = "Frequency")

# We can conclude from the plots that both populations come from normal distributions

# Test correlation using Pearson
Corr.Ga.Hf <- cor.test(Analysis.ready$Group.anomaly, Analysis.ready$Freq, 
                method = "pearson")
Corr.Ga.Hf

# Extract the p.value
Corr.Ga.Hf$p.value # [1] 0.3821151

# Extract the correlation coefficient
Corr.Ga.Hf$estimate # cor -0.1788134 


# Interpretting correlation coefficient
# -1 indicates a strong negative correlation : this means that every time x increases, y decreases
# 0 means that there is no association between the two variables (x and y)
# 1 indicates a strong positive correlation : this means that y increases with x 

