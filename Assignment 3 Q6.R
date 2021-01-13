# NEEDED LIBRARIES
library(corrplot)
library(car)
library(QuantPsyc)
library(leaps)
source("PCA_Plot.R")
# Question 6 
# Part A
# loading the dataset
Intelligance = read.csv(file.choose())
options(max.print = 9999)
Intelligance

head(Intelligance)
factor(Intelligance)
summary(Intelligance)
str(Intelligance)

#------------------------------------------------part b-------------------------------------------------

# loading the library to perform 
library(car)
library(carData)
library(ggplot2)
library(corrplot)
library(psych)

cor_Plot = cor(Intelligance)
corrplot(cor_Plot)              # generated the corrplot for Intelligence

test = prcomp(Intelligance, scale = T)
summary(test)
print(test)
plot(test)
abline(1, 0, col="red")
# now we will fo the rotation 
round(test$rotation, 2)
print(test)

# Part b testing the correlation matrix
round(cor(Intelligance),2)
library(psych)
cor_test1 = corr.test(Intelligance, adjust = "none")
round(cor_test1$p, 2)

#--------------------------------------------------------------------------------------------------------

# Part c

PCA_Plot(test)

#-------------------------------------------------------------Part d----------------------------------------
# Part d KPrincipal varimax rotation 
# with loadings
library(psych)
Part_d = principal(Intelligance[,3:12], nfactors = 10, rot = "varimax")
print(Part_d$loadings, cutoff = .4, sort = T)
PCA_Plot_Psyc(Part_d)
#---------------------------------------------------------------------------------------------------------

#----------------------------------------- Part e ----------------------------------------------

test_Scores = as.data.frame(test$x)
test_Scores
head(test_Scores[order(test_Scores$PC1), 3:4])
head(test_Scores[order(test_Scores$PC4), 4:5])
head(test_Scores[order(test_Scores$PC5), 5:6])
head(test_Scores[order(test_Scores$PC6), 6:7])
head(test_Scores[order(test_Scores$PC7), 7:8])
head(test_Scores[order(test_Scores$PC8), 8:9])
head(test_Scores[order(test_Scores$PC9), 9:10])
head(test_Scores[order(test_Scores$PC10), 10:11])
head(test_Scores[order(test_Scores$PC11), 11:12])
head(test_Scores[order(test_Scores$PC12), 12:13])

tail(test_Scores[order(test_Scores$PC1), 3:4])
tail(test_Scores[order(test_Scores$PC4), 4:5])
tail(test_Scores[order(test_Scores$PC5), 5:6])
tail(test_Scores[order(test_Scores$PC6), 6:7])
tail(test_Scores[order(test_Scores$PC7), 7:8])
tail(test_Scores[order(test_Scores$PC8), 8:9])
tail(test_Scores[order(test_Scores$PC9), 9:10])
tail(test_Scores[order(test_Scores$PC10), 10:11])
tail(test_Scores[order(test_Scores$PC11), 11:12])
tail(test_Scores[order(test_Scores$PC12), 12:13])

#-----------------------Part G----------------------------------------------------------------------
# common factor analysis:
library(psych)
CFA = factanal(Intelligance[,2:13], 10)
print(CFA$loadings, cutoff=.4, sort=T)

summary(CFA)

CFA_score = as.data.frame(CFA_score$scores)
names(scores) = c("Order", "Image", "Support", "Products")
scores$NewProd = hbat$NewProd
scores$Satis = hbat$Satis
head(scores) 









