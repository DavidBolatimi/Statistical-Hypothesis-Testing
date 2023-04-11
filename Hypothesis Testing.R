library(readr)
library(tidyverse)
library(ggpubr)
library(glue)

#No1)
#test comparing proportion to known value
x <- 30
n <- 60
p <- 0.44
prop.test(x, n, p)
prop.test(x, n, p, alt='two.sided', conf.level=0.95, correct=FALSE)

#95% Confidence Interval is
# (0.3773502, 0.6226498)

#Uncertainty (0.6226498-0.3773502)/2 = 0.1226498

#No2)
#test comparing two proportions
#using prop.test
s1 <- 326
s2 <-167
n1 <- 423
n2 <- 192
prop.test(x = c(s1, s2), 
          n = c(n1, n2), alt='two.sided', correct=FALSE)

#No3 <- use same code, different parameters
s1 <- 326
s2 <-167
n1 <- 423
n2 <- 192
prop.test(x = c(s1, s2), 
          n = c(n1, n2), alt='two.sided', correct=FALSE)

#90% critical value from the mean (-1.645, 1.645)
#95% critical value from the mean (-1.96, 1.96)
#99% critical value from the mean (-2.81, 2.81)
#Method 3, check if test statistic falls within above confidence intervals.


#calculate one sample chi-square test for variance
library(EnvStats)
varTest(data, alt='two.sided', conf.level=0.95,sigma.squared = 3600)

#Testing difference of population variance F-test

#No4?
#NB! Check if you're testing variance
theme_set(theme_pubr())
cell = read.csv(file='C:\\Users\\Earlham College CGE\\Downloads\\cell_phone.csv',header=TRUE)   


phone <- cell$Phone
not <-cell$Not
#calculate two-sample t-test
#for equal variances
t.test(phone, not, var.equal=TRUE)

#part (b)
#Uncertainty (0.05664138-0.03293711)/2 = 0.01185213
#Estimate 0.6008828-0.6456720 = 0.0447892±0.01185213

#c) there is no consistency, because upper limit of estimate falls within the confidence interval,
#but we rejected the null hypothesis in (a)

#No5
mortgage = read.csv(file='C:\\Users\\Earlham College CGE\\Downloads\\mortgage_payment.csv',header=TRUE)

Now <- mortgage$This.Year
Then <- mortgage$X5.years.ago
#calculate two-sample t-test
#for equal variances
t.test(Now, Then, var.equal=TRUE)

#p-value = 0.589 > significance level = 0.05, therefore we fail to reject the null hypothesis. 
#there is no significant difference between the average mortgage
#payments from your two samples

#Uncertainty (57.51564-(-32.71377))/2 = 45.11471
#Estimate 937.9099 - 925.5089 = 12.401 ±45.11471




#Matched Paired Analysis
#SalaryOffer File
sal = read.csv(file='C:\\Users\\Earlham College CGE\\Downloads\\mba_offers_matched2.csv',header=TRUE) 

fin <- sal[,c(1:2)]
finsort <- fin[order(fin$GPA.Finance.),]
finsal <-finsort$Finance.Offer
market <- sal[,c(3:4)]
marketsort <- market[order(market$GPA.Marketing.),]
marketsal <- marketsort$Marketing.Offer
diff <- finsal - marketsal

#calculate one-sample t-test for difference vector
t.test(diff, mu=0, alt='two.sided')

#calculate two-sample t-test with matched pairs
t.test(finsal, marketsal, paired=TRUE, alt='two.sided')

#compare with two-sample t-test with independent samples
t.test(finsal, marketsal, alt='two.sided')