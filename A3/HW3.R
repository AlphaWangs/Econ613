# Exercise 1
# Link the database
setwd("C:/Users/38470/Documents/GitHub/Econ613HW/A3")
crime_long<- read.table("crime_long.csv", head= TRUE, sep=",", na.strings=c("","NA"))
officers<- read.table("officers.csv", head= TRUE, sep=",", na.strings=c("","NA"))
population<- read.table("population.csv", head= TRUE, sep=",", na.strings=c("","NA"))
crime_long<- dplyr::rename(crime_long, month=crime_month)
officers<- dplyr::rename(officers, district=unit)
totaldata<- dplyr::left_join(population, crime_long, by=c("month","district"))
totaldata<- dplyr::left_join(head(totaldata,20000), head(officers,20000), c("month","district"))
head(na.omit(totaldata),20)
# Exercise 2
# Exercise 2.1 Calculate total crime per month and plot the time series of crime.
library(reshape)
md<- melt(crime_long, id="month","crimes")
md1<- cast(md, month~variable, sum)
plot(as.numeric(format(as.Date(md1$month), "%Y%m%d")),md1$crimes, xlab="Date", ylab="crimes",cex.axis=0.8)
# Exercise 2.2 Merge the two datasets by districts-units and period
TWO<- dplyr::left_join(population, crime_long, by=c("month","district"))
head(TWO,20)
# Exercise 2.3 Construct a panel data of unit over time with the following variables
library(reshape)
md2<- melt(TWO, id=c("month", "period", "district", "tot_pop", "tot_white", "tot_black", "tot_hisp",  "p50_inc", "crime_type"))
md2<- cast(md2, month+period+district+ tot_pop+ tot_white+ tot_black +tot_hisp+  p50_inc+ crime_type~variable, sum)
md2$crimes<-md2$crimes/md2$tot_pop
md2<- dplyr::rename(md2, Total_crimes_per_resident=crimes)
md3<- melt(TWO, id=c("month", "period", "district", "tot_pop", "tot_white", "tot_black", "tot_hisp",  "p50_inc", "crime_type"))
md3<- md3[md3$crime_type=="violent",]
md3<- cast(md3, month+period+district+ tot_pop+ tot_white+ tot_black +tot_hisp+  p50_inc+ crime_type~variable, sum)
md3$crimes<-md3$crimes/md3$tot_pop
md3<- dplyr::rename(md3, Violent_crimes_per_resident=crimes)
md3<- md3[,-11]
md4<- melt(TWO, id=c("month", "period", "district", "tot_pop", "tot_white", "tot_black", "tot_hisp",  "p50_inc", "crime_type"))
md4<- md4[md4$crime_type=="property",]
md4<- cast(md4, month+period+district+ tot_pop+ tot_white+ tot_black +tot_hisp+  p50_inc+ crime_type~variable, sum)
md4$crimes<-md4$crimes/md4$tot_pop
md4<- dplyr::rename(md4, Property_crimes_per_resident=crimes)
md4<- md4[,-11]
md5<- dplyr::left_join(md2, md3, by=c("month", "period", "district", "tot_pop", "tot_white", "tot_black", "tot_hisp",  "p50_inc"))
md5<- dplyr::left_join(md5, md4, by=c("month", "period", "district", "tot_pop", "tot_white", "tot_black", "tot_hisp",  "p50_inc"))
head(md5,20)
# Exercise 3
library(reshape)
md6<- melt(totaldata, id=c("month", "period", "district", "tot_pop", "tot_white", "tot_black", "tot_hisp",  "p50_inc", "crime_type", "NUID", "tenure", "arrest"))
md6<- cast(md6, month+period+district+tot_pop +tot_white+ tot_black+ tot_hisp + p50_inc +crime_type+ NUID+ tenure+ arrest~variable, sum)
md6<- dplyr::rename(md6, Total_crimes=crimes)
lm_model<- lm(md6$arrest~md6$tenure+md6$Total_crimes+md6$p50_inc+md6$tot_white+md6$tot_black+md6$tot_hisp-1)
summary(lm_model)
# Exercise 4
lm_model1<- lm(md6$arrest~md6$tenure+md6$Total_crimes+md6$p50_inc+md6$tot_white+md6$tot_black+md6$tot_hisp+md6$district+factor(md6$month)-1)
summary(lm_model1)
# Exercise 5
X <- as.matrix(md6[,c(-1,-2,-3,-4)])
Y <- as.matrix(md6[,4])
md7 <- lm(Y ~ X-1)
summary(md7)
X <- as.matrix(md6[,c(-1,-2,-5,-6)])
Y <- as.matrix(md6[,2])
md8 <- lm(Y ~ X -1)
summary(md8)
X <- as.matrix(md6[,c(-1,-2,-7,-8)])
Y <- as.matrix(md6[,4])
md9 <- lm(Y ~ X-1)
summary(md9)
GMM = (Y[1] - X[1,]%*%md6$coefficient[,1])[1,1]*t(X[1,])
GMM$coefficient