#Exercise 1
library(bayesm)
data("margarine")
ml<- merge.data.frame(margarine$choicePrice, margarine$demos)
#1.1 Average and dispersion in product characteristics
line<- apply(as.matrix(margarine$choicePrice[,3:12]), 2, mean)
line
apply(as.matrix(margarine$choicePrice[,3:12]), 2, quantile)
#1.2 Market share, and market share by product characteristics
nrow(as.matrix(margarine$choicePrice[,2]))
v<- table(margarine$choicePrice[,2])
proportions(table(margarine$choicePrice[,2]))
library(dplyr)
ml=ml %>% mutate(choiceset = ifelse(choice==1,PPk_Stk,
                                             ifelse(choice==2,PBB_Stk,
                                                    ifelse(choice==3,PFl_Stk,
                                                           ifelse(choice==4,PHse_Stk,
                                                                  ifelse(choice==5,PGen_Stk,
                                                                         ifelse(choice==6,PImp_Stk,
                                                                                ifelse(choice==7,PSS_Tub,
                                                                                       ifelse(choice==8,PPk_Tub,
                                                                                              ifelse(choice==9,PFl_Tub,PHse_Tub))))))))))
choice1<- subset(ml,ml$choice==1)
choice1up<- subset(choice1,choice1$choiceset>line[1])
up1<- nrow(choice1up)
down1<- v[1]-up1
paste("Take the first product PPk_Stk as an example, the market share above the average price is", up1,", while the one below the average price is", down1)
#1.3 Illustrate the mapping between observed attributes and choices
summary(lm(as.matrix(ml[,2])~as.matrix(ml[,13:19])))
#Exercise 2
#2.1 We are interested in the effect of price on demand. Propose a model specification.
# Below the package part is for my model results double-check, not the result to this question
# The answer of this question is Conditional Logit Model and Multinomial Logit Model. As for the details, it is in 2.2 below
# Conditional Logit Model for double-check only
library(mclogit)
clm<- mclogit(cbind(choice, hhid) ~ PPk_Stk+PBB_Stk+PFl_Stk+PHse_Stk+PGen_Stk+PImp_Stk+PSS_Tub+PPk_Tub+PFl_Tub+PHse_Tub, data = ml)
summary(clm)
logLik(clm)
# Multinomial Logit Model for double-check only
library(nnet)
mlm<- multinom(choice ~ PPk_Stk+PBB_Stk+PFl_Stk+PHse_Stk+PGen_Stk+PImp_Stk+PSS_Tub+PPk_Tub+PFl_Tub+PHse_Tub, data = ml)
summary(mlm) 
logLik(mlm)
#2.2 Write the likelihood and optimize the model.
#===============================================================================
#Below is just my thought to calculate the likelihood and optimization for conditional logit model
#However, since I don't know how to calculate the r[i], I can only show it here without following calculation
#To deal with this question, I will choose to use the Multinomial Logit Model, since there are not r[i]s in that model
clm_flike = function(par,X,yvar)
{
  xbeta = as.matrix(X)%*%as.matrix(par)
  e = sd(yvar-xbeta)
  for (i in 0:10)
  {ifelse(r[i] <= xbeta & xbeta < r[i+1], out  = pnorm((xbeta-r[i])/e)-pnorm((xbeta-r[i+1])/e))}
  out[out>0.999999] = 0.99999;
  out[out<0.000001] = 0.00001;
  return(-sum(log(out)));
}
set.seed(123)
start    = runif(11,-10,10)
res_clm  = optim(start,fn=clm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:12]),yvar=ml$choice,hessian=TRUE)
#===============================================================================
#Below is the likelihood and optimization for Multinomial Logit Model
mlm_flike = function(par,X,yvar)
{
  xbeta = as.matrix(X)%*%as.matrix(par)
  pr = exp(xbeta)/(1+exp(xbeta))
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}
# Calculate each logit models in the Multinomial Logit Model
sub0<- subset(ml, ml$choice==1)
sub0<- sweep(sub0, 2, 1)
# 1/9
sub<- subset(ml, ml$choice<3)
sub<- dplyr::mutate(sub,choice2=sub$choice-1)
set.seed(1234)
start = runif(11,-1,1)
res_mlm01  = optim(start,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:12]),yvar=sub$choice2,hessian=TRUE)
res_mlm01$par
# 2/9
sub2<- subset(ml, ml$choice==3)
sub2<- sweep(sub2, 2, 2)
sub2<- rbind(sub2,sub0)
set.seed(12342)
start2 = runif(11,-1,1)
res_mlm02  = optim(start2,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:12]),yvar=sub2$choice,hessian=TRUE)
res_mlm02$par
# 3/9
sub3<- subset(ml, ml$choice==4)
sub3<- sweep(sub3, 2, 3)
sub3<- rbind(sub3,sub0)
set.seed(12343)
start3 = runif(11,-1,1)
res_mlm03  = optim(start3,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:12]),yvar=sub3$choice,hessian=TRUE)
res_mlm03$par
# 4/9
sub4<- subset(ml, ml$choice==5)
sub4<- sweep(sub3, 2, 4)
sub4<- rbind(sub4,sub0)
set.seed(12344)
start4 = runif(11,-1,1)
res_mlm04  = optim(start4,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:12]),yvar=sub4$choice,hessian=TRUE)
res_mlm04$par
# 5/9
sub5<- subset(ml, ml$choice==6)
sub5<- sweep(sub3, 2, 5)
set.seed(12345)
start5 = runif(11,-1,1)
res_mlm05  = optim(start5,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:12]),yvar=sub5$choice,hessian=TRUE)
res_mlm05$par
# 6/9
sub6<- subset(ml, ml$choice>5 & ml$choice<8)
sub6<- dplyr::mutate(sub6,choice7=sub6$choice-6)
set.seed(12346)
start6 = runif(11,-1,1)
res_mlm06  = optim(start6,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:12]),yvar=sub6$choice7,hessian=TRUE)
res_mlm06$par
# 7/9
sub7<- subset(ml, ml$choice>6 & ml$choice<9)
sub7<- dplyr::mutate(sub7,choice8=sub7$choice-7)
set.seed(12347)
start7 = runif(11,-1,1)
res_mlm07  = optim(start7,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:12]),yvar=sub7$choice8,hessian=TRUE)
res_mlm07$par
# 8/9
sub8<- subset(ml, ml$choice>7 & ml$choice<10)
sub8<- dplyr::mutate(sub8,choice9=sub8$choice-8)
set.seed(12348)
start8 = runif(11,-1,1)
res_mlm08  = optim(start8,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:12]),yvar=sub8$choice9,hessian=TRUE)
res_mlm08$par
# 9/9
sub9<- subset(ml, ml$choice>8)
sub9<- dplyr::mutate(sub9,choice10=sub9$choice-9)
set.seed(12349)
start9 = runif(11,-1,1)
res_mlm09  = optim(start9,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:12]),yvar=sub9$choice10,hessian=TRUE)
res_mlm09$par
tab<- rbind(res_mlm01$par,res_mlm02$par,res_mlm03$par,res_mlm04$par,res_mlm05$par,res_mlm06$par,res_mlm07$par,res_mlm08$par,res_mlm09$par)
colnames(tab) <- c("intercept", "PPk_Stk", "PBB_Stk", "PFl_Stk", "PHse_Stk", "PGen_Stk", "PImp_Stk", "PSS_Tub", "PPk_Tub", "PFl_Tub", "PHse_Tub")
tab
#2.3 Interpret the coefficient on price.
print("The positive coefficient means increasing this variable, constumers tend to buy this product (repersented by this model) relative to the reference group (PPk_Stk), and vice versa.")
#Exercise 3
#3.1 We are interested in the effect of family income on demand. Propose a model specification.
# With Multinomial Logit Model, we can get the conclusion
# Below is the firtt method using package "nnet" which is not the result but for our double-check
# Our result is in 3.2 below
library(nnet)
mlm1<- multinom(choice ~ Income, data = ml)
summary(mlm1) 
logLik(mlm1)
#3.2 Write the likelihood and optimize the model.
# Below is the second method using our own function in Exercise 2
# 1/9
set.seed(1)
start01 = runif(2,-1,1)
res_mlm1  = optim(start01,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,13]),yvar=sub$choice2,hessian=TRUE)
res_mlm1$par
# 2/9
set.seed(2)
start02 = runif(2,-1,1)
res_mlm2  = optim(start02,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,13]),yvar=sub2$choice3,hessian=TRUE)
res_mlm2$par
# 3/9
set.seed(3)
start03 = runif(2,-1,1)
res_mlm3  = optim(start03,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,13]),yvar=sub3$choice4,hessian=TRUE)
res_mlm3$par
# 4/9
set.seed(4)
start04 = runif(2,-1,1)
res_mlm4  = optim(start04,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,13]),yvar=sub4$choice5,hessian=TRUE)
res_mlm4$par
# 5/9
set.seed(5)
start05 = runif(2,-1,1)
res_mlm5  = optim(start05,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,13]),yvar=sub5$choice6,hessian=TRUE)
res_mlm5$par
# 6/9
set.seed(6)
start06 = runif(2,-1,1)
res_mlm6  = optim(start06,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,13]),yvar=sub6$choice7,hessian=TRUE)
res_mlm6$par
# 7/9
set.seed(7)
start07 = runif(2,-1,1)
res_mlm7  = optim(start07,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,13]),yvar=sub7$choice8,hessian=TRUE)
res_mlm7$par
# 8/9
set.seed(8)
start08 = runif(2,-1,1)
res_mlm8  = optim(start08,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,13]),yvar=sub8$choice9,hessian=TRUE)
res_mlm7$par
# 9/9
set.seed(9)
start09 = runif(2,-1,1)
res_mlm9  = optim(start09,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,13]),yvar=sub8$choice9,hessian=TRUE)
res_mlm9$par
tab1<- rbind(res_mlm1$par,res_mlm2$par,res_mlm3$par,res_mlm4$par,res_mlm5$par,res_mlm6$par,res_mlm7$par,res_mlm8$par,res_mlm9$par)
colnames(tab1) <- c("intercept", "Income")
tab1
#3.3 Interpret the coefficient on family
paste("Take the first model as example, when family's income increase, since the parameter in our model is negative, they tend to buy the first product PPk_Stk relative to the second product PBB_Stk")
#Exercise 4
# The marginal effect for the first model (the Conditional Logit Model)
# Below is \deta h method: marginal_effect=(f(x+\deta h))-f(x)/(\deta h)
X1<- apply(ml[,3:12],2,mean)
XB1<- sum(X1*clm$coefficients)
X2<- apply(ml[,3:12],2,function(x) mean(x)+0.00001)
XB2<- sum(X2*clm$coefficients)
ME<- (XB2-XB1)/0.00001
paste("The marginal effect at mean is", ME)
# The marginal effect for the second model
# Since the Multinomial Logit Model is series of Linear equation of one variable, the coefficients in that model is just the margianl effects
summary(mlm1)$coefficients[,2]
#Exercise 5
# 5.1 We are still interested in the effect of price and family income. Write and optimize the likelihood of the mixed logit.
# 1/9
set.seed(01)
s1 = runif(12,-1,1)
res1  = optim(s1,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:13]),yvar=sub$choice2,hessian=TRUE)
res1$par
# 2/9
set.seed(02)
s2 = runif(12,-1,1)
res2  = optim(s2,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:13]),yvar=sub2$choice3,hessian=TRUE)
res2$par
# 3/9
set.seed(03)
s3 = runif(12,-1,1)
res3  = optim(s3,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:13]),yvar=sub3$choice4,hessian=TRUE)
res3$par
# 4/9
set.seed(04)
s4 = runif(12,-1,1)
res4  = optim(s4,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:13]),yvar=sub4$choice5,hessian=TRUE)
res4$par
# 5/9
set.seed(05)
s5 = runif(12,-1,1)
res5  = optim(s5,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:13]),yvar=sub5$choice6,hessian=TRUE)
res5$par
# 6/9
set.seed(06)
s6 = runif(12,-1,1)
res6  = optim(s6,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:13]),yvar=sub6$choice7,hessian=TRUE)
res6$par
# 7/9
set.seed(07)
s7 = runif(12,-1,1)
res7  = optim(s7,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:13]),yvar=sub7$choice8,hessian=TRUE)
res7$par
# 8/9
set.seed(08)
s8 = runif(12,-1,1)
res8  = optim(s8,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:13]),yvar=sub8$choice9,hessian=TRUE)
res8$par
# 9/9
set.seed(09)
s9 = runif(12,-1,1)
res9  = optim(s9,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,3:13]),yvar=sub9$choice10,hessian=TRUE)
res9$par
tab2<- rbind(res1$par,res2$par,res3$par,res4$par,res5$par,res6$par,res7$par,res8$par,res9$par)
colnames(tab2) <- c("intercept", "PPk_Stk", "PBB_Stk", "PFl_Stk", "PHse_Stk", "PGen_Stk", "PImp_Stk", "PSS_Tub", "PPk_Tub", "PFl_Tub", "PHse_Tub", "Income")
tab2
# 5.2 Consider an alternative specification, where we remove data from one choice.
# We delete the first product PPk_Stk as example
# 1/9
set.seed(001)
st1 = runif(11,-1,1)
r1  = optim(st1,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,4:13]),yvar=sub$choice2,hessian=TRUE)
r1$par
# 2/9
set.seed(002)
st2 = runif(11,-1,1)
r2  = optim(st2,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,4:13]),yvar=sub2$choice3,hessian=TRUE)
r2$par
# 3/9
set.seed(003)
st3 = runif(11,-1,1)
r3  = optim(st3,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,4:13]),yvar=sub3$choice4,hessian=TRUE)
r3$par
# 4/9
set.seed(004)
st4 = runif(11,-1,1)
r4  = optim(st4,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,4:13]),yvar=sub4$choice5,hessian=TRUE)
r4$par
# 5/9
set.seed(005)
st5 = runif(11,-1,1)
r5  = optim(st5,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,4:13]),yvar=sub5$choice6,hessian=TRUE)
r5$par
# 6/9
set.seed(006)
st6 = runif(11,-1,1)
r6  = optim(st6,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,4:13]),yvar=sub6$choice7,hessian=TRUE)
r6$par
# 7/9
set.seed(007)
st7 = runif(11,-1,1)
r7  = optim(st7,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,4:13]),yvar=sub7$choice8,hessian=TRUE)
r7$par
# 8/9
set.seed(008)
st8 = runif(11,-1,1)
r8  = optim(st8,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,4:13]),yvar=sub8$choice9,hessian=TRUE)
r8$par
# 9/9
set.seed(009)
st9 = runif(11,-1,1)
r9  = optim(st9,fn=mlm_flike,method="BFGS",control=list(trace=6,maxit=1000),X=cbind(1,ml[,4:13]),yvar=sub9$choice10,hessian=TRUE)
r9$par
tab3<- rbind(r1$par,r2$par,r3$par,r4$par,r5$par,r6$par,r7$par,r8$par,r9$par)
colnames(tab3) <- c("intercept", "PBB_Stk", "PFl_Stk", "PHse_Stk", "PGen_Stk", "PImp_Stk", "PSS_Tub", "PPk_Tub", "PFl_Tub", "PHse_Tub", "Income")
tab3
# 5.3 Compute the test statistics:
tab4<- tab2[,-2]
HM<- (tab4-tab3)%*% (var(tab4)-var(tab3)) %*% t(tab4-tab3)
HM
# 5.4 Conclude on IIA
chisq.test(abs(HM))
paste("According to the result, we can conclude that the IIA doesn't hold.")