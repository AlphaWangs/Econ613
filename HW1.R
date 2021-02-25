#Exercise 1
#input the database
setwd("C:/Users/38470/Documents/GitHub/Econ613/Assignments/A1/dat")
students<- read.table("datstu.csv", head= TRUE, sep=",", na.strings=c("","NA"))
#1.1 Count Number of students
n=nrow(students)
paste("The number of students is", n)
#1.2 Number of schools
school=students[ , c(1,5:10)]
#install.packages("reshape")
library(reshape)
md<- melt(school, id="X")
schoolcode<- md[ , 3]
schoolnumber=length(unique(na.omit(schoolcode)))
paste("The number of schools is", schoolnumber)
#1.3 Number of programs
school=students[ , c(1,11:16)]
#install.packages("reshape")
library(reshape)
md2<- melt(school, id="X")
programcode<- md2[ , 3]
programnumber=length(unique(na.omit(programcode)))
paste("The number of programs is", programnumber)
#1.4 Number of choices
md3<-cbind(schoolcode,programcode)
choicecode<- unique(na.omit(md3))
choicenumber<- length(choicecode)
paste("The number of choices is", choicenumber)
#1.5 Missing test score
t<- students[ , 2]
miscore<- sum(is.na(t))
paste("The number of missing test score is", miscore)
#1.6 Apply to the same school (different programs)
md4<-unique(na.omit(md3))
nssdp<-length(md4)
paste("The number of Apply to the same school (different programs) is", nssdp)
#1.7 Apply to less than 6 choices
c7<- students[ ,16]
t7<- sum(is.na(c7))
paste("The number of Apply to less than 6 choices is", t7)