# Read in Hurricane Data file csv files
HD <- read.table("/Users/MarkHenley/Desktop/Hurricane_Data.csv", 
                 header = TRUE,
                 sep = ",")

# Read in Sea surface temp table csv files
SST <- read.table("/Users/MarkHenley/Desktop/Sea_surface_temp.csv", 
                 header = TRUE,
                 sep = ",")

# Subset data for Hurricane Data file so that the min years match Sea_surface_temp file with dates after 1880
HD_date <- HD[ which(HD$Year > 1879), ]


install.packages('plyr')

library(plyr)

HD_Frequency = count(Combination,"Year")

# Subset data again for Hurricane Data file so that the max years match Sea_surface_temp file with dates after 1880
Hurr_date <- HD_Frequency[ which(HD_Frequency$Year < 2016), ]

install.packages("tidyverse")

ggplot(HD_Frequency, aes(x = Year)) +
  geom_histogram(aes(y =..count..), binwidth = 5) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Aggregated Annual North American Hurricanes")

ggplot(HD_Frequency, aes(x = Year)) +
  geom_histogram(aes(y = ..density..), binwidth = 5) +
  stat_function(fun = dnorm, colour = "red",
                arg = list(mean = mean(HD_Frequency$Year,),
                           sd = sd(HD_Frequency$Year,)))

SST_Freq = summary(SST, 'Annual.anomaly')

# Simple Scatterplot for hurricane frequency
attach(Hurr_date)
plot(Year, freq, main="Hurricane Annual Frequency",
     xlab="Year ", ylab="Aggregated Annual North American Hurricanes ", pch=1) 

# Simple Scatterplot for SST
attach(SST)
plot(Year, Annual.anomaly, main="Sea Surface Temperature Anomaly",
     xlab="Year ", ylab="Sea Surface Anomaly from Mean Temperature ", pch=1) 

#Set tables to join
setkey(Hurr_date,SST)
setkey(Year,Year)

# Perform join between Hurr_date table and SST table using Year.
Result <- merge(Hurr_date,SST, all= TRUE)

# merge two data frames by variable name
total <- merge(Hurr_date,SST,by="Year", all=FALSE)

# Correlations/covariances among numeric variables in 
# data frame total. Use listwise deletion of missing data. 
cor(Annual.anomaly, freq, use="complete.obs", method=c("pearson"))
cor.test(Annual.anomaly, freq, use="complete.obs", method=c("pearson"))
cov(Annual.anomaly, freq, use="complete.obs")

library("ggpubr")

ggscatter(total, x = "Annual.anomaly", y = "freq", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Annual Anomaly", ylab = "Agregate Hurricane Total for U.S.")

# Annual.anomaly
ggqqplot(total$Annual.anomaly, ylab = "Annual Anomaly")
# freq
ggqqplot(total$freq, ylab = "Hurricane Frequency")


kendal_test <- cor.test(total$Annual.anomaly, total$freq,  method="kendall")
kendal_test

spearman_test <- cor.test(total$Annual.anomaly, total$freq,  method="spearman")
spearman_test

library(corrplot)

library(dplyr)

total <- select(Year,Annual.anomaly,freq)
head(total)

#Let's compute the correlation matrix
matrix <- round(cor(total), 2)
matrix

library("Hmisc")

matrix2 <- rcorr(as.matrix(total))
matrix2

str(matrix2)

matrix2$P

matrix2$r

install.packages("lubridate")
library(lubridate)



aggregate(Annual.anomaly ~ combine(Year=1880-1885,1886-1890,1891-1895,1896-1900,1901-1905,1906-1910,1911-1915,1916-1920,1921-1925,1926-1930,1931-1935,1936-1940,1941-1945,1946-1950,1951-1956,1960-1965,1966-1970,1971-1975,1976-1980,1981-1985,1986-1990,1991-1995,1996-2000,2001-2005,2006-2010,2011-2015), total, mean)


library(data.table)

"
cut(Year, breaks=c(1880,1885,1890,1895,1900,1905,1910,1915,1920,1925,1930,1935,1940,1945,1950,1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015),right=FALSE,labels=FALSE)
year.ranges <- (1880-1885,1886-1890,1891-1895,1896-1900,1901-1905,1906-1910,1911-1915,1916-1920,1921-1925,1926-1930,1931-1935,1936-1940,1941-1945,1946-1950,1951-1956,1960-1965,1966-1970,1971-1975,1976-1980,1981-1985,1986-1990,1991-1995,1996-2000,2001-2005,2006-2010,2011-2015)"

help.search("combine values by range of other variable")


# 3D Scatterplot with Coloring and Vertical Lines
# and Regression Plane
library(scatterplot3d)
attach(Sea.temp.hurr)
s3d <-scatterplot3d(Year,Group.anomaly,, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")
fit <- lm(mpg ~ wt+disp)
s3d$plane3d(fit)
