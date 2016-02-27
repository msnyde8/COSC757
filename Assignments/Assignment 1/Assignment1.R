#### Input data set autompg into Data Frame "autompg"
autompg <- read.csv(file = "C:/Users/maryjoyce/Documents/Towson/Spring 2016/COSC 757/Assignments/Assignment1/UCI data/auto-mpg.data-original.txt", header = FALSE, sep = "", stringsAsFactors = TRUE, col.names = c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model_year", "origin", "car_name") )
autompg[1:20,]

### Exploratory Data Analysis:
## Descriptive Statistics
# Summarize
summary(autompg)

## Distribution of the attributes (histograms)
hist(autompg$mpg,
     breaks = 80,
     xlim = c(5,50),
     col = "lightblue",
     ylab = "Count",
     xlab = "MPG",
     main = "Histogram of MPG")
hist(autompg$cylinders,
     breaks = 10,
     xlim = c(2,9),
     col = "lightblue",
     ylab = "Count",
     xlab = "Cylinders",
     main = "Histogram of Cylinders")
hist(autompg$displacement,
     breaks = 80,
     xlim = c(65,500),
     col = "lightblue",
     ylab = "Count",
     xlab = "Displacement",
     main = "Histogram of Displacement")
hist(autompg$horsepower,
     breaks = 80,
     xlim = c(45,250),
     col = "lightblue",
     ylab = "Count",
     xlab = "Horsepower",
     main = "Histogram of Horsepower")
hist(autompg$weight,
     breaks = 80,
     xlim = c(1600,5500),
     col = "lightblue",
     ylab = "Count",
     xlab = "Weight",
     main = "Histogram of Weight")
hist(autompg$acceleration,
     breaks = 80,
     xlim = c(5,25),
     col = "lightblue",
     ylab = "Count",
     xlab = "Acceleration",
     main = "Histogram of Acceleration")
hist(autompg$model_year,
     breaks = 40,
     xlim = c(65,85),
     col = "lightblue",
     ylab = "Count",
     xlab = "Model Year",
     main = "Histogram of Model Year")
hist(autompg$origin,
     breaks = 5,
     xlim = c(0,5),
     col = "lightblue",
     ylab = "Count",
     xlab = "Origin",
     main = "Histogram of Origin")


## Relationships between attributes (scatter plots)
plot(autompg$displacement,
     autompg$mpg,
     xlim = c(65,500),
     ylim = c(0,50),
     xlab = "Displacement",
     ylab = "MPG",
     main = "Scatterplot of MPG by Displacement",
     type = "p",
     pch = 16,
     col = "green")
points(autompg$displacement,
       autompg$mpg,
       type = "p",
       col = "black")

plot(autompg$horsepower,
     autompg$mpg,
     xlim = c(45,250),
     ylim = c(0,50),
     xlab = "Horsepower",
     ylab = "MPG",
     main = "Scatterplot of MPG by Horsepower",
     type = "p",
     pch = 16,
     col = "blue")
points(autompg$horsepower,
       autompg$mpg,
       type = "p",
       col = "black")

plot(autompg$weight,
     autompg$mpg,
     xlim = c(1600,5500),
     ylim = c(0,50),
     xlab = "Weight",
     ylab = "MPG",
     main = "Scatterplot of MPG by Weight",
     type = "p",
     pch = 16,
     col = "red")
points(autompg$weight,
       autompg$mpg,
       type = "p",
       col = "black")


## Distance between objects
pairs(~autompg$mpg+autompg$horsepower+autompg$weight+autompg$displacement)



### Data Preprocessing
## Normalize the numerical attributes of this dataset using min-max normalization, z-scores, and decimal scaling
nona_mpg<-autompg$mpg[!is.na(autompg$mpg)]
nona_mpg
nona_horsepower<-autompg$horsepower[!is.na(autompg$horsepower)]
nona_horsepower

# Min-Max normalization
#-mpg
summary(nona_mpg)
#mmnorm.mpg<-(autompg$mpg - min(autompg$mpg))/(max(autompg$mpg) - min(autompg$mpg))
mmnorm.mpg<-(nona_mpg - min(nona_mpg))/(max(nona_mpg) - min(nona_mpg))
summary(mmnorm.mpg)
#-cylinders
summary(autompg$cylinders)
mmnorm.cylinders<-(autompg$cylinders - min(autompg$cylinders))/(max(autompg$cylinders) - min(autompg$cylinders))
summary(mmnorm.cylinders)
#-displacement
(383 - min(autompg$displacement))/(max(autompg$displacement) - min(autompg$displacement))
summary(autompg$displacement)
mmnorm.displacement<-(autompg$displacement - min(autompg$displacement))/(max(autompg$displacement) - min(autompg$displacement))
summary(mmnorm.displacement)
#-horsepower
summary(nona_horsepower)
#mmnorm.horsepower<-(autompg$horsepower - min(autompg$horsepower))/(max(autompg$horsepower) - min(autompg$horsepower))
mmnorm.horsepower<-(nona_horsepower - min(nona_horsepower))/(max(nona_horsepower) - min(nona_horsepower))
summary(mmnorm.horsepower)
#-weight
summary(autompg$weight)
mmnorm.weight<-(autompg$weight - min(autompg$weight))/(max(autompg$weight) - min(autompg$weight))
summary(mmnorm.weight)
#-acceleration
summary(autompg$acceleration)
mmnorm.acceleration<-(autompg$acceleration - min(autompg$acceleration))/(max(autompg$acceleration) - min(autompg$acceleration))
summary(mmnorm.acceleration)

# Z-scores
#-mpg
summary(nona_mpg)
zscore.mpg <-(nona_mpg-mean(nona_mpg))/sd(nona_mpg)
summary(zscore.mpg)
#-cylinders
summary(autompg$cylinders)
zscore.cylinders <-(autompg$cylinders-mean(autompg$cylinders))/sd(autompg$cylinders)
summary(zscore.cylinders)
#-displacement
summary(autompg$displacement)
zscore.displacement <-(autompg$displacement-mean(autompg$displacement))/sd(autompg$displacement)
summary(zscore.displacement)
#-horsepower
mean(nona_horsepower)
sd(nona_horsepower)
(198-mean(nona_horsepower))/sd(nona_horsepower)
summary(nona_horsepower)
zscore.horsepower <-(nona_horsepower-mean(nona_horsepower))/sd(nona_horsepower)
summary(zscore.horsepower)
#-weight
summary(autompg$weight)
zscore.weight <-(autompg$weight-mean(autompg$weight))/sd(autompg$weight)
summary(zscore.weight)
#-acceleration
summary(autompg$acceleration)
zscore.acceleration <-(autompg$acceleration-mean(autompg$acceleration))/sd(autompg$acceleration)
summary(zscore.acceleration)

# Decimal Scaling
#-mpg
summary(nona_mpg)
digits_mpg<-nchar(floor(max(abs(nona_mpg))))
decscale.mpg <-(nona_mpg/(10^digits_mpg))
summary(decscale.mpg)
#-cylinders
summary(autompg$cylinders)
digits_cylinders<-nchar(floor(max(abs(autompg$cylinders))))
decscale.cylinders <-(autompg$cylinders/(10^digits_cylinders))
summary(decscale.cylinders)
#-displacement
3090/(10^4)
summary(autompg$displacement)
digits_displacement<-nchar(floor(max(abs(autompg$displacement))))
decscale.displacement <-(autompg$displacement/(10^digits_displacement))
summary(decscale.displacement)
#-horsepower
summary(nona_horsepower)
digits_horsepower<-nchar(floor(max(abs(nona_horsepower))))
decscale.horsepower <-(nona_horsepower/(10^digits_horsepower))
summary(decscale.horsepower)
#-weight
summary(autompg$weight)
max(abs(autompg$weight))
digits_weight<-nchar(floor(max(abs(autompg$weight))))
digits_weight
decscale.weight <-(autompg$weight/(10^digits_weight))
summary(decscale.weight)
#-acceleration
summary(autompg$acceleration)
digits_acceleration<-nchar(floor(max(abs(autompg$acceleration))))
decscale.acceleration <-(autompg$acceleration/(10^digits_acceleration))
summary(decscale.acceleration)


## Experiment with at least two methods to bin the variable into discrete categories
n.size<-length(autompg$weight)
n.bins<-3
whichbin<-c(rep(0,n.size))
# Equal frequency
freq<-n.size/n.bins
x.sorted<-sort(autompg$weight)
for(i in 1:n.bins){
  for(j in 1:n.size){
    if((i-1)*freq < j && j <= i*freq)
      whichbin[j]<-i
  }
}
whichbin
# K-means
#kmeansclustering <- kmeans(x.sorted, centers = n.bins)
autompg$weight
kmeansclustering <- kmeans(autompg$weight, centers = n.bins)
whichbin <- kmeansclustering$cluster
whichbin
hist(whichbin,
     breaks = 5,
     xlim = c(0,4),
     col = "lightgreen",
     ylab = "Count",
     xlab = "Bin",
     main = "Histogram of Binned Weight")
# Equal width
r.autompg<-max(autompg$weight) - min(autompg$weight) + 1
binwidth<-r.autompg/n.bins
binwidth
for (i in 1:n.bins){
  for(j in 1:n.size){
    if((i-1)*binwidth < autompg$weight[j] && autompg$weight[j] <= (i)*binwidth)
      whichbin[j]<-i
  }
}
whichbin
hist(whichbin,
     breaks = 5,
     xlim = c(0,4),
     col = "lightblue",
     ylab = "Count",
     xlab = "Bin",
     main = "Histogram of Binned Weight")

## Use the natural log, square root, and inverse square root transformations to make an attempt to achieve normality
#Measuring Skewness
sqrt_weight<-sqrt(autompg$weight)
skewness.sqrt<-3*(mean(sqrt_weight)-median(sqrt_weight))/sd(sqrt_weight)
skewness.sqrt
natlog_weight<-log(autompg$weight)
skewness.natlog<-3*(mean(natlog_weight)-median(natlog_weight))/sd(natlog_weight)
skewness.natlog
invsqrt_weight<-1/sqrt(autompg$weight)
skewness.invsqrt<-3*(mean(invsqrt_weight)-median(invsqrt_weight))/sd(invsqrt_weight)
skewness.invsqrt
#Checking for normality
par(mfrow=c(1,1))
qqnorm(invsqrt_weight,
       datax = TRUE,
       col="red",
       ylim=c(0.01,0.03),
       main="Normal Q-Q Plot of Inverse Square Root of Weight")
qqline(invsqrt_weight,
       col="blue",
       datax = TRUE)


### Regression Analysis
nonaall_mpg<-autompg$mpg[!is.na(autompg$mpg) & !is.na(autompg$horsepower)]
length(nonaall_mpg)
nonaall_horsepower<-autompg$horsepower[!is.na(autompg$horsepower) & !is.na(autompg$mpg)]
length(nonaall_horsepower)
nonaall_weight<-autompg$weight[!is.na(autompg$horsepower) & !is.na(autompg$mpg)]
length(nonaall_weight)


fit.horsepower<-lm(nonaall_mpg ~ nonaall_horsepower)
summary(fit.horsepower)
fit.weight<-lm(nonaall_mpg ~ nonaall_weight)
summary(fit.weight)

hypothesis<-cbind(nonaall_mpg,
                  nonaall_horsepower,
                  nonaall_weight)
horsepowerTest<-cor.test(nonaall_mpg,
                          nonaall_horsepower)
weightTest<-cor.test(nonaall_mpg,
                      nonaall_weight)
mpgTest<-cor.test(nonaall_horsepower,
                   nonaall_weight)
round(cor(hypothesis),
      4)
horsepowerTest$p.value
weightTest$p.value
mpgTest$p.value
