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
###########################period?
# Exercise 2.3 Construct a panel data of unit over time with the following variables
library(reshape)
md2<- melt(crime_long, id=c("month", "district", "crime_type"))
md2<- cast(md2, month+district~variable, sum)
md2<- dplyr::rename(md2, Total_crimes_per_resident=crimes)
md3<- melt(crime_long, id=c("month", "district", "crime_type"))
md3<- md3[md3$crime_type=="violent",]
md3<- cast(md3, month+district~variable, sum)
md3<- dplyr::rename(md3, Violent_crimes_per_resident=crimes)
md4<- melt(crime_long, id=c("month", "district", "crime_type"))
md4<- md4[md4$crime_type=="property",]
md4<- cast(md4, month+district~variable, sum)
md4<- dplyr::rename(md4, Property_crimes_per_resident=crimes)
md5<- dplyr::left_join(md2, md3, by=c("month","district"))
md5<- dplyr::left_join(md5, md4, by=c("month","district"))
md5<- dplyr::left_join(md5, population, by=c("month","district"))
md5<- na.omit(md5[,c(-6,-7)])
md5<- md5[,c(1:5,9,7,8,6)]
head(md5,20)
# Exercise 3
library(reshape)
md6<- melt(totaldata, id=c("month", "period", "district", "tot_pop", "tot_white", "tot_black", "tot_hisp",  "p50_inc", "crime_type", "NUID", "tenure", "arrest"))
md6<- cast(md6, month+period+district+tot_pop +tot_white+ tot_black+ tot_hisp + p50_inc +crime_type+ NUID+ tenure+ arrest~variable, sum)
md6<- dplyr::rename(md6, Total_crimes_per_resident=crimes)
lm_model<- lm(md6$arrest~md6$tenure+md6$Total_crimes_per_resident+md6$p50_inc+md6$tot_white+md6$tot_black+md6$tot_hisp-1)
summary(lm_model)
# Exercise 4
lm_model1<- lm(md6$arrest~md6$tenure+md6$Total_crimes_per_resident+md6$p50_inc+md6$tot_white+md6$tot_black+md6$tot_hisp+md6$district+as.numeric(format(as.Date(md6$month), "%Y%m%d"))-1)
summary(lm_model1)
# Exercise 5
lm_model2<- lm(md6$arrest~md6$tenure+md6$Total_crimes_per_resident+md6$p50_inc+md6$tot_white+md6$tot_black+md6$tot_hisp+md6$district+as.numeric(format(as.Date(md6$month), "%Y%m%d")))
summary(lm_model2)
