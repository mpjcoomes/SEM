#Part a.

#Input data
library(foreign)
dir <- 'E:/SEMdata/Value.sav'
x <- read.spss(dir, use.value.labels = FALSE, to.data.frame = TRUE, max.value.labels = Inf,
trim.factor.names = FALSE, trim_values = TRUE, use.missings = TRUE)
x <- x[,1:15]

#KMO & BTS:
library(rela)
base <- paf(as.matrix(x), eigcrit=1, convcrit=.001)
KMO <- paste('Kaiser-Meyer-Olkin Measure of Sampling Adequacy =', round(as.numeric(base[5]),3))
df <- (dim(x)[2]^2-dim(x)[2])/2
Bartlett <-  paste('Bartlett's Test of Sphericity =', round(as.numeric(base[7]),3),
', df =', df,', Sig.p =', pchisq(as.numeric(base[7]), lower.tail=FALSE, df), sep=' ')
KMO;Bartlett

#Total Variance Explained Table (Eigen Values)
ls <- factanal(x, factors=3, rotation='none')$loadings
Total <- eigen(cor(x))$values
PercentVariance <- 100*Total/sum(Total)
CumulativePercent <- 100*cumsum(Total/sum(Total))
TotalSSLoadings <- as.numeric(c(sum(ls[,1]^2), sum(ls[,2]^2), sum(ls[,3]^2),rep('NA',12)))
SSPV <- as.numeric(c(100*sum(ls[,1]^2)/length(Total), 100*sum(ls[,2]^2)/length(Total),
100*sum(ls[,3]^2)/length(Total),rep('NA',12)))
CumSSPercentV <- c(SSPV[1], sum(SSPV[1:2]), sum(SSPV[1:3]),rep(NA,12))
TVAtable <- cbind(Total,PercentVariance,CumulativePercent,TotalSSLoadings,SSPV,CumSSPercentV )
rownames(TVAtable) <- paste('Factor', 1:length(Total))
TVAtable <- round(TVAtable,3)
print('Total Variance Explained'); print(TVAtable, na.print='')

#Scree Plot
library(nFactors)
nScree(x)
par(oma=c(0,0,0,0),font.lab=6); plotnScree(nScree(x), main='', legend=T, xlab='Factor Number')

#Pattern Matrix
library(psych)
Q1a_model1 <- fa(x, nfactors=3, n.iter=1, rotate='oblimin', residuals=TRUE, SMC=TRUE, fm='ml')
print(Q1a_model1$loadings, digits=3, cutoff=.2, sort=TRUE)


#______________________________________________________________________
#Part b.

#Removing low-Comm vars
Communalities <- 1-Q1a_model1$uniquenesses
data.frame(round(Communalities,4)[order(Communalities)])

z <- x[,names(x)!='honest' &
names(x)!='curious']
Q1b_model1 <- fa(z, nfactors=3, n.iter=1, rotate='oblimin', residuals=TRUE, SMC=TRUE, fm='ml')
Communalities <- 1-Q1b_model1$uniquenesses
data.frame(round(Communalities,4)[order(Communalities)])

#Extraction Methods
i <- 'minres'
for(i in c('minres', 'wls', 'gls', 'pa', 'ml')) {
p <- fa(z, nfactors=3, n.iter=1, rotate='oblimin', residuals=TRUE,SMC=TRUE, min.err=0.001,
max.iter=50, warnings=TRUE,fm=i,alpha=.1,p=.05,oblique.scores=FALSE)
print(c(p$rotation,p$fm));print(c(p$STATISTIC,p$RMSEA[1],p$BIC, p$*R*^2^),digits=3)
print(p$loadings, digits=3, cutoff=.2, sort=TRUE)
}
Q1b_model2 <- fa(z, nfactors=3, n.iter=1, rotate='oblimin', residuals=TRUE, SMC=TRUE, fm='pa')

#Factor Number
i <- 1
for(i in c(2,4)) {
p <- fa(z, nfactors=i, n.iter=1, rotate='oblimin', residuals=TRUE,SMC=TRUE, min.err=0.001,
max.iter=50, warnings=TRUE,fm='ml',alpha=.1,p=.05,oblique.scores=FALSE)
print(c(p$rotation,p$fm));print(c(p$STATISTIC,p$RMSEA[1],p$BIC, p$*R*^2^),digits=3)
print(p$loadings, digits=3, cutoff=.2, sort=TRUE)
}
Q1b_model3 <- fa(z, nfactors=2, n.iter=1, rotate='oblimin', residuals=TRUE, SMC=TRUE, fm='ml')
Q1b_model4 <- fa(z, nfactors=4, n.iter=1, rotate='oblimin', residuals=TRUE, SMC=TRUE, fm='ml')

#Rotation
i<- 'none'
for(i in c('none', 'varimax', 'quartimax', 'bentlerT', 'geominT','promax', 'oblimin', 'simplimax', 
'bentlerQ', 'geominQ', 'biquartimin','cluster', 'bifactor')) {
p <- fa(z, nfactors=3, n.iter=1, rotate=i, scores='regression', residuals=TRUE,
SMC=TRUE, min.err=0.001, max.iter=50, warnings=TRUE, fm='ml',alpha=.1,p=.05,oblique.scores=FALSE)
print(p$rotation);print(p$loadings, digits=3, cutoff=.2, sort=TRUE)
}

z <- x[,names(x)!='honest' & names(x)!='curious' & names(x)!='creative' & names(x)!='love']
Q1b_model5 <- fa(z, nfactors=3, n.iter=1, rotate='oblimin', residuals=TRUE, SMC=TRUE, fm='ml')
Q1b_model6 <- fa(z, nfactors=3, n.iter=1, rotate='promax', residuals=TRUE, SMC=TRUE, fm='ml')
print(Q1b_model5$loadings, digits=3, cutoff=.2, sort=TRUE)
print(Q1b_model6$loadings, digits=3, cutoff=.2, sort=TRUE)

#Final Chi-Square Optimize
z <- x[,names(x)!='honest' & names(x)!='curious' & names(x)!='creative' & names(x)!='love'
 & names(x)!='owngoals' ]
Q1b_model7 <- fa(z, nfactors=3, n.iter=1, rotate='promax', scores='regression', residuals=TRUE,
SMC=TRUE, min.err=0.001, max.iter=50, warnings=TRUE, fm='ml',alpha=.1,p=.05,oblique.scores=FALSE)
print(Q1b_model7$loadings, digits=3, cutoff=.2, sort=TRUE)


#Model comparison
m <- Q1a_model1
t1 <- cbind(m$fm,m$rotation,m$factors,'No',rbind(round(c(m$BIC,m$STATISTIC,m$PVAL,m$RMSEA[1],m$*R*^2^[1:3]),2)),
rbind(setdiff(names(x),names(m$weights[,1]))),rbind(rep(NA,5)))
m <- Q1b_model1
t2 <- cbind(m$fm,m$rotation,m$factors,'No',rbind(round(c(m$BIC,m$STATISTIC,m$PVAL,m$RMSEA[1],m$*R*^2^[1:3]),2)),
rbind(setdiff(names(x),names(m$weights[,1]))))
m <- Q1b_model2
t3 <- cbind(m$fm,m$rotation,m$factors,'No',rbind(round(c(m$BIC,m$STATISTIC,m$PVAL,m$RMSEA[1],m$*R*^2^[1:3]),2)),
rbind(setdiff(names(x),names(m$weights[,1]))))
m <- Q1b_model3
t4 <- cbind(m$fm,m$rotation,m$factors,'No',rbind(round(c(m$BIC,m$STATISTIC,m$PVAL,m$RMSEA[1],m$*R*^2^[1:3]),2)),
rbind(setdiff(names(x),names(m$weights[,1]))))
m <- Q1b_model4
t5 <- cbind(m$fm,m$rotation,m$factors,'No',rbind(round(c(m$BIC,m$STATISTIC,m$PVAL,m$RMSEA[1],m$*R*^2^[1:3]),2)),
rbind(setdiff(names(x),names(m$weights[,1]))))
m <- Q1b_model5
t6 <- cbind(m$fm,m$rotation,m$factors,'No',rbind(round(c(m$BIC,m$STATISTIC,m$PVAL,m$RMSEA[1],m$*R*^2^[1:3]),2)),
rbind(setdiff(names(x),names(m$weights[,1]))))
m <- Q1b_model6
t7 <- cbind(m$fm,m$rotation,m$factors,'Yes',rbind(round(c(m$BIC,m$STATISTIC,m$PVAL,m$RMSEA[1],m$*R*^2^[1:3]),2)),
rbind(setdiff(names(x),names(m$weights[,1]))))
m <- Q1b_model7
t8 <- cbind(m$fm,m$rotation,m$factors,'Yes',rbind(round(c(m$BIC,m$STATISTIC,m$PVAL,m$RMSEA[1],m$*R*^2^[1:3]),2)),
rbind(setdiff(names(x),names(m$weights[,1]))))
ModelComparison <- t1
for(i in list(t2,t3,t4,t5,t6,t7,t8)) { 
ModelComparison <- merge(ModelComparison,i,all.x=T,all.y=T, sort=F)
}
names(ModelComparison) <- c('EM','Rotate','F','Sim','BIC','Chi-Sq','p','RMSEA','F1*R*^2^','F2*R*^2^','F3*R*^2^',
rep('Excluded',5))
print(ModelComparison, na.print='')

#Final Model Pattern matrix
print('Pattern Matrix');print(Q1b_model7$loadings, digits=3, cutoff=.2, sort=TRUE)
print('Factor Correlation Matrix');round(Q1b_model7$Phi,3)
print('Variance Explained'); round(100*Q1b_model7$*R*^2^,1)
print('Chi-Square'); Q1b_model7$STATISTIC; print('p');Q1b_model7$PVAL

#______________________________________________________________________
#Part c.
#AMOS used for analysis, R for tables

#Table 6modelchi  df p CFI GFI AGFITLI  RMSEA AIC BCC   BIC CAIC
Q2c1 <- cbind('Initial', 99.289, 32, .000, .944, .950, .921, .922, .075, 145.289, 146.698, 235.361, 258.361)
Q2*X*^2^ <- cbind('-'Outliers'',91.480, 32, .000, .952, .954, .921, .932, .071, 137.480, 138.902, 227.366, 250.366)
Q2c3 <- cbind('-'broadmind'', 75.866, 24, .000, .954, .958, .920, .930, .077, 117.866, 119.043, 199.936, 220.936)
Q2c4 <- cbind('-'freedom'',73.279, 24, .000, .953, .958, .922, .930, .075, 115.279, 116.456, 197.349, 218.349)
Q2c5 <- cbind('-'devout'',51.175, 24, .001, .973, .970, .944, .960, .056, 93.175,  94.351,  175.245, 196.245)
Q2c6 <- cbind('-'environment'',71.196,24, .000, .955, .961, .928, .932, .073, 113.196, 114.372, 195.265, 216.265)
Q2c7 <- cbind('-'spiritual'',38.144, 24, .034, .986, .977, .957, .978, .040, 80.144,  81.321,  162.214, 183.214)
Q2c8 <- cbind('-'spir&broad'',25.511, 17, 0.084,.990, .983, .964, .984, .037, 63.511,  64.467,  137.765, 156.765)

ModelComparison <- Q2c1
for(i in list(Q2c1,Q2*X*^2^,Q2c3,Q2c4,Q2c5,Q2c6,Q2c7,Q2c8)) { 
ModelComparison <- merge(ModelComparison,i,all.x=T,all.y=T, sort=F)
}
names(ModelComparison) <- c('Model','ChiSq', 'DF', 'p', 'CFI', 'GFI', 'AGFI', 'TLI', 'RMSEA',
 'AIC', 'BCC', 'BIC', 'CAIC')
print(ModelComparison, na.print='')


#Table 7modelNPsHighest Mod Index Largest StRes'
Q2c1 <- cbind('Initial', '0  ','devout<-ML3;MI=16.748;PC=-.253','devout:freedom=-3.246')
Q2*X*^2^ <- cbind('-'Outliers'','0  ','devout<-ML3;MI=17.390;PC=-.254','devout:freedom=-3.296')
Q2c3 <- cbind('-'broadmind'', '0  ','devout<-ML3;MI=15.733;PC=-.245','devout:freedom=-3.332')
Q2c4 <- cbind('-'freedom'','0  ','devout<-ML3;MI=14.743;PC=-.242','devout:broadminded=-2.708')
Q2c5 <- cbind('-'devout'','0  ','e2<-->e4;MI=9.594;PC=.126','environment:broadminded=1.898')
Q2c6 <- cbind('-'environment'','0  ','devout<-ML3;MI=16.797;PC=-.250','devout:broadminded=-2.156')
Q2c7 <- cbind('-'spiritual'','0  ','e10<-->ML1;MI=7.124;PC=.086','devout:broadminded=1.865')
Q2c8 <- cbind('-'spir&broad'','0  ','e3<-->e7;MI=6.456;PC=.065','environment:equality=1.631')

ModelComparison <- Q2c1
for(i in list(Q2c1,Q2*X*^2^,Q2c3,Q2c4,Q2c5,Q2c6,Q2c7,Q2c8)) { 
ModelComparison <- merge(ModelComparison,i,all.x=T,all.y=T, sort=F)
}
names(ModelComparison) <- c('Model','p>.01 paths','Highest Modification Index', 'Largest Stand. Residual')
print(ModelComparison, na.print='')

#______________________________________________________________________
#Part d,e,f. AMOS or no calculations required.
#______________________________________________________________________
#Part g.

#Import data used for final model
library(foreign)
dir <- 'E:/SEMdata/ValueNoOutliers.sav'
x <- read.spss(dir, use.value.labels = F, to.data.frame = TRUE, max.value.labels = Inf,
trim.factor.names = FALSE, trim_values = TRUE, use.missings = TRUE)

#Calculate Summated scales
F1_EnvirConcern <- as.matrix(rowSums(x[,c('nature', 'beauty', 'environment')]))
F2_ReligiousObs <- as.matrix(rowSums(x[,c('devout', 'forgiving')]))
F3_PeacfulUnivr <- as.matrix(rowSums(x[,c('equality', 'harmony', 'freedom')]))
y <- data.frame(cbind(F1_EnvirConcern, F2_ReligiousObs, F3_PeacfulUnivr,
x[,'education'], x[,'goodhealth'], x[,'Future']))
names(y) <- c('F1_EnvirConcern','F2_ReligiousObs','F3_PeacfulUnivr','EducationLvl','GoodHealth','FutureAtts')

#Identify missing cases
summary(y)

#Make clean dataset and replaced dataset
x_noNAs <- na.omit(y)
x_NAsmeanreplaced <- y
x_NAsmeanreplaced$FutureAtts[is.na(x_NAsmeanreplaced$FutureAtts)] <- mean(x_NAsmeanreplaced$FutureAtts, na.rm=TRUE)
x_NAsmeanreplaced$GoodHealth[is.na(x_NAsmeanreplaced$GoodHealth)] <- mean(x_NAsmeanreplaced$GoodHealth, na.rm=TRUE)
summary(x_noNAs);print(paste('N =',nrow(x_noNAs)))
summary(x_NAsmeanreplaced);print(paste('N =',nrow(x_NAsmeanreplaced)))

#Build Corr Table Function (BERTOLT from MYOWELT,2008-04-09)
corstarsl <- function(x){ 
require(Hmisc) 
x <- as.matrix(x) 
R <- rcorr(x)$r 
p <- rcorr(x)$P 
mystars <- ifelse(p < .001, '***', ifelse(p < .01, '** ', ifelse(p < .05, '* ', ' ')))
R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
Rnew <- matrix(paste(R, mystars, sep=''), ncol=ncol(x)) 
diag(Rnew) <- paste(diag(R), ' ', sep='') 
rownames(Rnew) <- colnames(x) 
colnames(Rnew) <- paste(colnames(x), '', sep='') 
Rnew <- as.matrix(Rnew)
Rnew[upper.tri(Rnew, diag = TRUE)] <- ''
Rnew <- as.data.frame(Rnew) 
Rnew <- cbind(Rnew[1:length(Rnew)-1])
return(Rnew) 
}

#Spawn Corr Tables
corstarsl(x_noNAs);nrow(x_noNAs)
corstarsl(x_NAsmeanreplaced);nrow(x_NAsmeanreplaced)

#______________________________________________________________________
#Part h.

#Build data set for testing
F1_EnvirConcern <- as.matrix(rowSums(x[,c('nature', 'beauty', 'environment')]))
F2_ReligiousObs <- as.matrix(rowSums(x[,c('devout', 'forgiving')]))
F3_PeacfulUnivr <- as.matrix(rowSums(x[,c('equality', 'harmony', 'freedom')]))
y <- data.frame(cbind(F1_EnvirConcern, F2_ReligiousObs, F3_PeacfulUnivr,x[,'sex']))
names(y) <- c('F1_EnvirConcern','F2_ReligiousObs','F3_PeacfulUnivr','Sex')
summary(y)

sapply(y[y$Sex==1,],summary);sapply(y[y$Sex==1,],sd)
sapply(y[y$Sex==2,],summary);sapply(y[y$Sex==2,],sd)

library(nortest)
i <- 1
for(i in 1:3){
print(shapiro.test(y[,i]))
print(lillie.test(y[,i]))
}

i <- 1
for(i in 1:3){
print(wilcox.test(y[,i]~as.factor(y$Sex)), paired=F)
print(p.adjust(wilcox.test(y[,i]~y$Sex, paired=F)$p.value, method='bonferroni', n=3))
}
