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
#Exercise 2
#================================
#Build a scoresheet for every choice
score<- read.table("datstu.csv", head= TRUE, sep=",", na.strings=c("","NA","99"))
score<- na.omit(score)
a1<- score[which(score$rankplace == 1), ]
a1<- a1[ , c(-6,-7,-8,-9,-10,-12,-13,-14,-15,-16)]
names(a1) [5:6]<-c("schoolcode","choicepgm")
a2<- score[which(score$rankplace == 2), ]
a2<- a2[ , c(-5,-7,-8,-9,-10,-11,-13,-14,-15,-16)]
names(a2) [5:6]<-c("schoolcode","choicepgm")
a3<- score[which(score$rankplace == 3), ]
a3<- a3[ , c(-5,-6,-8,-9,-10,-11,-12,-14,-15,-16)]
names(a3) [5:6]<-c("schoolcode","choicepgm")
a4<- score[which(score$rankplace == 4), ]
a4<- a4[ , c(-5,-6,-7,-9,-10,-11,-12,-13,-15,-16)]
names(a4) [5:6]<-c("schoolcode","choicepgm")
a5<- score[which(score$rankplace == 5), ]
a5<- a5[ , c(-5,-6,-7,-8,-10,-11,-12,-13,-14,-16)]
names(a5) [5:6]<-c("schoolcode","choicepgm")
a6<- score[which(score$rankplace == 6), ]
a6<- a6[ , c(-5,-6,-7,-8,-9,-11,-12,-13,-14,-15)]
names(a6) [5:6]<-c("schoolcode","choicepgm")
scoresheet<- rbind(a1,a2,a3,a4,a5,a6)
scoresheet<-scoresheet[order(scoresheet$schoolcode,scoresheet$choicepgm, scoresheet$score),]
#================================
scoresheet$choice<- paste0(scoresheet$schoolcode,scoresheet$choicepgm)
Offersheet<- scoresheet[ , c(2,9)]
library(reshape)
mdd<- melt(Offersheet, id="choice")
cutoff<- cast(mdd, choice~variable, min)
quality<- cast(mdd, choice~variable, mean)
size<- cast(mdd, choice~variable, length)
Choicescore<- cbind(cutoff, quality, size)
Choicescore<- Choicescore[ ,c(-3,-5)]
names(Choicescore) [2:4]<-c("cutoff","quality", "size")
Choicescore<- merge(scoresheet, Choicescore, by="choice")
datsss<- na.omit(read.table("datsss.csv", head= TRUE, sep=",", na.strings=c("","NA")))
datsss<- datsss[ ,-1]
Choicescore<- unique(merge(Choicescore, datsss, by="schoolcode"))
paste("The answer for Exercise 2 is Choicescore")
#Exercise 3
datjss<- na.omit(read.table("datjss.csv", head= TRUE, sep=",", na.strings=c("","NA")))
datjss<- datjss[ ,-1]
Dis<- merge(Choicescore, datjss, by="jssdistrict")
Dis<- Dis[order(Dis$X),]
Dis$Distance<- sqrt(((69.172*(Dis$ssslong-Dis$point_x)*cos(Dis$point_y/57.3))^2+(69.172*(Dis$ssslat-Dis$point_y))^2))
paste("The answer for Exercise 3 is the variable Distance in the dataframe Dis")
#Exercise 4
Rank<- Dis[order(Dis$rankplace, Dis$X),]
paste("For 1st Rankplace")
paste("The average of the cutoff is", mean(Rank$cutoff[Rank$rankplace== 1]))
paste("The sd of the cutoff is", sd(Rank$cutoff[Rank$rankplace== 1]))
paste("The average of the Quality is", mean(Rank$quality[Rank$rankplace== 1]))
paste("The sd of the Quality is", sd(Rank$quality[Rank$rankplace== 1]))
paste("The average of the Distance is", mean(Rank$Distance[Rank$rankplace== 1]))
paste("The sd of the Distance is", sd(Rank$Distance[Rank$rankplace== 1]))
paste("For 2nd Rankplace")
paste("The average of the cutoff is", mean(Rank$cutoff[Rank$rankplace== 2]))
paste("The sd of the cutoff is", sd(Rank$cutoff[Rank$rankplace== 2]))
paste("The average of the Quality is", mean(Rank$quality[Rank$rankplace== 2]))
paste("The sd of the Quality is", sd(Rank$quality[Rank$rankplace== 2]))
paste("The average of the Distance is", mean(Rank$Distance[Rank$rankplace== 2]))
paste("The sd of the Distance is", sd(Rank$Distance[Rank$rankplace== 2]))
paste("For 3rd Rankplace")
paste("The average of the cutoff is", mean(Rank$cutoff[Rank$rankplace== 3]))
paste("The sd of the cutoff is", sd(Rank$cutoff[Rank$rankplace== 3]))
paste("The average of the Quality is", mean(Rank$quality[Rank$rankplace== 3]))
paste("The sd of the Quality is", sd(Rank$quality[Rank$rankplace== 3]))
paste("The average of the Distance is", mean(Rank$Distance[Rank$rankplace== 3]))
paste("The sd of the Distance is", sd(Rank$Distance[Rank$rankplace== 3]))
paste("For 4th Rankplace")
paste("The average of the cutoff is", mean(Rank$cutoff[Rank$rankplace== 4]))
paste("The sd of the cutoff is", sd(Rank$cutoff[Rank$rankplace== 4]))
paste("The average of the Quality is", mean(Rank$quality[Rank$rankplace== 4]))
paste("The sd of the Quality is", sd(Rank$quality[Rank$rankplace== 4]))
paste("The average of the Distance is", mean(Rank$Distance[Rank$rankplace== 4]))
paste("The sd of the Distance is", sd(Rank$Distance[Rank$rankplace== 4]))
paste("For 5th Rankplace")
paste("The average of the cutoff is", mean(Rank$cutoff[Rank$rankplace== 5]))
paste("The sd of the cutoff is", sd(Rank$cutoff[Rank$rankplace== 5]))
paste("The average of the Quality is", mean(Rank$quality[Rank$rankplace== 5]))
paste("The sd of the Quality is", sd(Rank$quality[Rank$rankplace== 5]))
paste("The average of the Distance is", mean(Rank$Distance[Rank$rankplace== 5]))
paste("The sd of the Distance is", sd(Rank$Distance[Rank$rankplace== 5]))
paste("For 6th Rankplace")
paste("The average of the cutoff is", mean(Rank$cutoff[Rank$rankplace== 6]))
paste("The sd of the cutoff is", sd(Rank$cutoff[Rank$rankplace== 6]))
paste("The average of the Quality is", mean(Rank$quality[Rank$rankplace== 6]))
paste("The sd of the Quality is", sd(Rank$quality[Rank$rankplace== 6]))
paste("The average of the Distance is", mean(Rank$Distance[Rank$rankplace== 6]))
paste("The sd of the Distance is", sd(Rank$Distance[Rank$rankplace== 6]))
#=========================================
Quantiles<- Dis[order(Dis$score, Dis$X),]
q<- quantile(Quantiles$score)
paste("For 1st quantile")
paste("The average of the cutoff is", mean(Quantiles$cutoff[Quantiles$score<q[2]]))
paste("The sd of the cutoff is", sd(Quantiles$cutoff[Quantiles$score<q[2]]))
paste("The average of the Quality is", mean(Quantiles$quality[Quantiles$score<q[2]]))
paste("The sd of the Quality is", sd(Quantiles$qualit[Quantiles$score<q[2]]))
paste("The average of the Distance is", mean(Quantiles$Distance[Quantiles$score<q[2]]))
paste("The sd of the Distance is", sd(Quantiles$Distance[Quantiles$score<q[2]]))
paste("For 2nd quantile")
paste("The average of the cutoff is", mean(Quantiles$cutoff[Quantiles$score<q[3]]))
paste("The sd of the cutoff is", sd(Quantiles$cutoff[Quantiles$score<q[3]]))
paste("The average of the Quality is", mean(Quantiles$quality[Quantiles$score<q[3]]))
paste("The sd of the Quality is", sd(Quantiles$qualit[Quantiles$score<q[3]]))
paste("The average of the Distance is", mean(Quantiles$Distance[Quantiles$score<q[3]]))
paste("The sd of the Distance is", sd(Quantiles$Distance[Quantiles$score<q[3]]))
paste("For 3rd quantile")
paste("The average of the cutoff is", mean(Quantiles$cutoff[Quantiles$score<q[4]]))
paste("The sd of the cutoff is", sd(Quantiles$cutoff[Quantiles$score<q[4]]))
paste("The average of the Quality is", mean(Quantiles$quality[Quantiles$score<q[4]]))
paste("The sd of the Quality is", sd(Quantiles$qualit[Quantiles$score<q[4]]))
paste("The average of the Distance is", mean(Quantiles$Distance[Quantiles$score<q[4]]))
paste("The sd of the Distance is", sd(Quantiles$Distance[Quantiles$score<q[4]]))
paste("For 4th quantile")
paste("The average of the cutoff is", mean(Quantiles$cutoff[Quantiles$score<=q[5]]))
paste("The sd of the cutoff is", sd(Quantiles$cutoff[Quantiles$score<=q[5]]))
paste("The average of the Quality is", mean(Quantiles$quality[Quantiles$score<=q[5]]))
paste("The sd of the Quality is", sd(Quantiles$qualit[Quantiles$score<=q[5]]))
paste("The average of the Distance is", mean(Quantiles$Distance[Quantiles$score<=q[5]]))
paste("The sd of the Distance is", sd(Quantiles$Distance[Quantiles$score<=q[5]]))
#Exercise 5
set.seed(1234)
X1<- runif(10000, min=1, max=3)
X2<- rgamma(10000, shape=3, rate=0.5)
X3<- rbinom(10000, 1, 0.3)
e<- rnorm(10000, mean=2, sd=1)
Y<- 0.5 + 1.2*X1-0.9*X2 + 0.1*X3 + e
Y_hat=mean(Y)
ydum <- ifelse(Y>Y_hat, 1, 0)
#Exercise 6
r<- cor(Y, X1)
X<- cbind(1, X1, X2, X3)
b<- solve(t(X)%*%X)%*%t(X)%*%Y
diff<- b[2]-1.2
paste("The correlation coefficient between Y and X1 is", r)
paste("The regression coefficient between Y and X1 is", b[2])
paste("The difference from 1.2 is", diff)
cat("The regression result is Y=",b[1],"+",b[2],"X1+",b[3],"X2+",b[4],"X3+e")
cat("The coefficients are",b)
Varb<- var(e)*solve(t(X)%*%X)%*%t(X)%*%X%*%solve(t(X)%*%X)
Se <- sqrt(abs(Varb))
cat("The standard errors in this OLS are", Se[1,1],Se[2,2],Se[3,3],Se[4,4])
#Exercise 7
Probit<-glm(ydum~X1+X2+X3, family=binomial(link="probit"))
# flike and optim functions below are from professor's GitHub Class Note
flike = function(par,X1,X2,X3,ydum)
{
  xbeta           = par[1] + par[2]*X1 + par[3]*X2 + par[4]*X3
  pr              = pnorm(xbeta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = ydum*log(pr) + (1-ydum)*log(1-pr)
  return(-sum(like))
}
test_par = Probit$coefficients
flike(test_par,X1,X2,X3,ydum)
logLik(Probit)
out = mat.or.vec(100,4)
for (i0 in 1:100)
{
  start    = runif(4,-10,10)
  res  = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),X1=X1,X2=X2,X3=X3,ydum=ydum,hessian=TRUE)
  out[i0,] = res$par
}
start = runif(4)
res  = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),X1=X1,X2=X2,X3=X3,ydum=ydum,hessian=TRUE)
fisher_info = solve(res$hessian) 
prop_sigma  = sqrt(diag(fisher_info))
est = cbind(b ,summary(Probit)$coefficients[, 1],Probit$par,summary(Probit)$coefficients[, 2],prop_sigma)
colnames(est) = c("True parameter","R: GLM : est","R: own :se","R: GLM :se")
est
#Logit Model========================================
Logit<-glm(ydum~X1+X2+X3, family=binomial(link="logit"))
flike2 = function(par,X1,X2,X3,ydum)
{
  xbeta           = par[1] + par[2]*X1 + par[3]*X2 + par[4]*X3
  pr              = exp(xbeta)/(1+exp(xbeta))
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like            = ydum*log(pr) + (1-ydum)*log(1-pr)
  return(-sum(like))
}
test_par2 = Logit$coefficients
flike2(test_par2,X1,X2,X3,ydum)
logLik(Logit)
out = mat.or.vec(100,4)
for (i0 in 1:100)
{
  start    = runif(4,-10,10)
  res2     = optim(start,fn=flike2,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),X1=X1,X2=X2,X3=X3,ydum=ydum,hessian=TRUE)
  out[i0,] = res2$par
}
start = runif(4)
res2  = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),X1=X1,X2=X2,X3=X3,ydum=ydum,hessian=TRUE)
fisher_info2 = solve(res2$hessian) 
prop_sigma2  = sqrt(diag(fisher_info2))
est2 = cbind(b ,summary(Logit)$coefficients[, 1],summary(Logit)$coefficients[, 2],Logit$par,prop_sigma)
colnames(est2) = c("True parameter","R: GLM : est","R: own :se","R: GLM :se")
est2
#linear probability model========================================
LPM<-lm(ydum~X1+X2+X3)
est3 = cbind(b ,summary(Probit)$coefficients[, 1],summary(Logit)$coefficients[, 1],summary(LPM)$coefficients[, 1])
colnames(est3) = c("True parameter","Probit","Logit","LPM")
est3
paste("Conclusion: the positive and negative signal of coefficients are same")
paste("The positive signals mean the variables hold positive influecne on ydum, and vice versa")
paste("For the intercept, X1 and X2, all the three models hold 0.001 significant level")
paste("But for X3, the Probit and Logit models hold 0.01 significant level. But for LPM, the X3 is not significant.")
#Exercise 8
Datause<- as.data.frame(cbind(ydum, X1, X2, X3))
# Use the "mfx" package
> # install.packages("mfx")
> library(mfx)
> paste("The answers of Exercise 8 are in the tables below")
[1] "The answers of Exercise 8 are in the tables below"
> probitmfx(formula=ydum~X1+X2+X3, data=Datause)
Call:
probitmfx(formula = ydum ~ X1 + X2 + X3, data = Datause)

Marginal Effects:
        dF/dx  Std. Err.        z   P>|z|    
X1  0.4924149  0.0177859  27.6857 < 2e-16 ***
X2 -0.3669907  0.0075174 -48.8187 < 2e-16 ***
X3  0.0539974  0.0186723   2.8918 0.00383 ** 
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

dF/dx is for discrete change for the following variables:

[1] "X3"
Warning message:
glm.fit: fitted probabilities numerically 0 or 1 occurred 
> logitmfx(formula=ydum~X1+X2+X3, data=Datause)
Call:
logitmfx(formula = ydum ~ X1 + X2 + X3, data = Datause)

Marginal Effects:
        dF/dx  Std. Err.       z   P>|z|    
X1  0.5547272  0.0207585  26.723 < 2e-16 ***
X2 -0.4132169  0.0093871 -44.020 < 2e-16 ***
X3  0.0608904  0.0209894   2.901 0.00372 ** 
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

dF/dx is for discrete change for the following variables:

[1] "X3"
Warning message:
glm.fit: fitted probabilities numerically 0 or 1 occurred
# If don't use the package
# probitmfest function is from the website about the mfx package and I changed some parts to fit this question
probitmfxest <-
  function(formula, data, atmean = TRUE, robust = FALSE, clustervar1 = NULL, 
           clustervar2 = NULL, start = NULL, control = list()){
    
    if(is.null(formula)){
      stop("formula is missing")
    }
    if(!is.data.frame(data)){
      stop("data arguement must contain data.frame object")
    }
    
    # cluster sort part
    if(is.null(clustervar1) & !is.null(clustervar2)){
      stop("use clustervar1 arguement before clustervar2 arguement")
    }    
    if(!is.null(clustervar1)){
      if(is.null(clustervar2)){
        if(!(clustervar1 %in% names(data))){
          stop("clustervar1 not in data.frame object")
        }    
        data = data.frame(model.frame(formula, data, na.action=NULL),data[,clustervar1])
        names(data)[dim(data)[2]] = clustervar1
        data=na.omit(data)
      }
      if(!is.null(clustervar2)){
        if(!(clustervar1 %in% names(data))){
          stop("clustervar1 not in data.frame object")
        }    
        if(!(clustervar2 %in% names(data))){
          stop("clustervar2 not in data.frame object")
        }    
        data = data.frame(model.frame(formula, data, na.action=NULL),
                          data[,c(clustervar1,clustervar2)])
        names(data)[c(dim(data)[2]-1):dim(data)[2]] = c(clustervar1,clustervar2)
        data=na.omit(data)
      }
    }
    
    fit = glm(formula, data=data, family = binomial(link = "probit"), x=T,
              start = start, control = control)
    
    # terms needed
    x1 = model.matrix(fit)
    if (any(alias <- is.na(coef(fit)))) {
      x1 <- x1[, !alias, drop = FALSE]
    }
    xm = as.matrix(colMeans(x1))
    be = as.matrix(na.omit(coef(fit)))
    k1 = length(na.omit(coef(fit)))
    xb = t(xm) %*% be
    fxb = ifelse(atmean==TRUE, dnorm(xb), mean(dnorm(x1 %*% be)))
    
    # get variances
    vcv = vcov(fit)
    
    if(robust){
      if(is.null(clustervar1)){
        # white correction
        vcv = vcovHC(fit, type = "HC0")
      } else {
        if(is.null(clustervar2)){
          vcv = clusterVCV(data=data, fm=fit, cluster1=clustervar1,cluster2=NULL)
        } else {
          vcv = clusterVCV(data=data, fm=fit, cluster1=clustervar1,cluster2=clustervar2)
        }
      }
    }
    
    if(robust==FALSE & is.null(clustervar1)==FALSE){
      if(is.null(clustervar2)){
        vcv = clusterVCV(data=data, fm=fit, cluster1=clustervar1,cluster2=NULL)
      } else {
        vcv = clusterVCV(data=data, fm=fit, cluster1=clustervar1,cluster2=clustervar2)
      }
    }
    
    mfx = data.frame(mfx=fxb*be, se=NA)
    
    # get standard errors
    if(atmean){
      gr = as.numeric(fxb)*(diag(k1) - as.numeric(xb) *(be %*% t(xm)))
      mfx$se = sqrt(diag(gr %*% vcv %*% t(gr)))            
    } else {
      gr = apply(x1, 1, function(x){
        as.numeric(as.numeric(dnorm(x %*% be))*(diag(k1) - as.numeric(x %*% be)*(be %*% t(x))))
      })
      gr = matrix(apply(gr,1,mean),nrow=k1)
      mfx$se = sqrt(diag(gr %*% vcv %*% t(gr)))                
    }
    output = list(fit=fit, mfx=mfx)
    return(output)
  }
# logitmfest function is from the website about the mfx package and I changed some parts to fit this question
logitmfxest <-
  function(formula, data, atmean = TRUE, robust = FALSE, clustervar1 = NULL, 
           clustervar2 = NULL, start = NULL, control = list()){
    
    if(is.null(formula)){
      stop("formula is missing")
    }
    if(!is.data.frame(data)){
      stop("data arguement must contain data.frame object")
    }
    # cluster sort part
    if(is.null(clustervar1) & !is.null(clustervar2)){
      stop("use clustervar1 arguement before clustervar2 arguement")
    }    
    if(!is.null(clustervar1)){
      if(is.null(clustervar2)){
        if(!(clustervar1 %in% names(data))){
          stop("clustervar1 not in data.frame object")
        }    
        data = data.frame(model.frame(formula, data, na.action=NULL),data[,clustervar1])
        names(data)[dim(data)[2]] = clustervar1
        data=na.omit(data)
      }
      if(!is.null(clustervar2)){
        if(!(clustervar1 %in% names(data))){
          stop("clustervar1 not in data.frame object")
        }    
        if(!(clustervar2 %in% names(data))){
          stop("clustervar2 not in data.frame object")
        }    
        data = data.frame(model.frame(formula, data, na.action=NULL),
                          data[,c(clustervar1,clustervar2)])
        names(data)[c(dim(data)[2]-1):dim(data)[2]] = c(clustervar1,clustervar2)
        data=na.omit(data)
      }
    }
    fit = glm(formula, data=data, family = binomial(link = "logit"), x=T, 
              start = start, control = control)    
    
    # terms needed
    x1 = model.matrix(fit)
    if (any(alias <- is.na(coef(fit)))) {
      x1 <- x1[, !alias, drop = FALSE]
    }
    xm = as.matrix(colMeans(x1))
    be = as.matrix(na.omit(coef(fit)))
    k1 = length(na.omit(coef(fit)))
    xb = t(xm) %*% be
    fxb = ifelse(atmean==TRUE, plogis(xb)*(1-plogis(xb)), mean(plogis(x1 %*% be)*(1-plogis(x1 %*% be))))  
    # get variances
    vcv = vcov(fit)
    
    if(robust){
      if(is.null(clustervar1)){
        # white correction
        vcv = vcovHC(fit, type = "HC0")
      } else {
        if(is.null(clustervar2)){
          vcv = clusterVCV(data=data, fm=fit, cluster1=clustervar1,cluster2=NULL)
        } else {
          vcv = clusterVCV(data=data, fm=fit, cluster1=clustervar1,cluster2=clustervar2)
        }
      }
    }
    
    if(robust==FALSE & is.null(clustervar1)==FALSE){
      if(is.null(clustervar2)){
        vcv = clusterVCV(data=data, fm=fit, cluster1=clustervar1,cluster2=NULL)
      } else {
        vcv = clusterVCV(data=data, fm=fit, cluster1=clustervar1,cluster2=clustervar2)
      }
    }
    
    mfx = data.frame(mfx=fxb*be, se=NA)
    
    # get standard errors
    if(atmean){
      gr = (as.numeric(fxb))*(diag(k1) + as.numeric(1 - 2*plogis(xb))*(be %*% t(xm)))
      mfx$se = sqrt(diag(gr %*% vcv %*% t(gr)))            
    } else {
      gr = apply(x1, 1, function(x){
        as.numeric(as.numeric(plogis(x %*% be)*(1-plogis(x %*% be)))*
                     (diag(k1) - (1 - 2*as.numeric(plogis(x %*% be)))*(be %*% t(x))))
      })  
      gr = matrix(apply(gr,1,mean),nrow=k1)
      mfx$se = sqrt(diag(gr %*% vcv %*% t(gr)))                
    }
    output = list(fit=fit, mfx=mfx)
    return(output)
  }
paste("The answers of Exercise 8 are in the tables below")
probitmfxest(formula=ydum~X1+X2+X3, data=Datause)
logitmfxest(formula=ydum~X1+X2+X3, data=Datause)