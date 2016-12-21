#Generate distribution of the mean of 40 exponential variables

library(ggplot2)

lambda <-.2
num_var<-40
num_sims<-1000
exp_mean<-1/lambda
exp_sd<-(1/lambda)/sqrt(num_var)

mns = NULL
for (i in 1 : num_sims) mns = c(mns, mean(rexp(num_var,lambda)))

norm_mns<-(mns-(exp_mean))/exp_sd

results_df<-data.frame(x = norm_mns)


## Plot results distribution alongside standard normal dist using the multiplot funciton


results_p<-ggplot(results_df,aes(x))+geom_density()+ stat_function(fun = dnorm, colour = "red")
results_p


#Report means and std devs for each emprical distribution

mean(norm_mns)
sd(norm_mns)



#Part 2: Analyze the Tooth Growth Dataset

tooth_scatter <- ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_point(shape=1)
tooth_scatter+facet_grid(.~supp)

ToothGrowth$group = interaction(ToothGrowth$supp,ToothGrowth$dose)

data_OJ.0.5<-ToothGrowth[ToothGrowth$group=="OJ.0.5",]
data_OJ.1<-ToothGrowth[ToothGrowth$group=="OJ.1",]
data_OJ.2<-ToothGrowth[ToothGrowth$group=="OJ.2",]
data_OJ_overall<-ToothGrowth[ToothGrowth$supp=="OJ",]

data_VC.0.5<-ToothGrowth[ToothGrowth$group=="VC.0.5",]
data_VC.1<-ToothGrowth[ToothGrowth$group=="VC.1",]
data_VC.2<-ToothGrowth[ToothGrowth$group=="VC.2",]
data_VC_overall<-ToothGrowth[ToothGrowth$supp=="VC",]

##Within Group Tests


#OJ group
t.test(data_OJ.0.5$len,data_OJ.1$len)
t.test(data_OJ.1$len,data_OJ.2$len)


#By transitive property, we already know that the mean of the OJ.2 group is higher than that of the OJ.0.5
#group because OJ.2 is significantly greater thant OJ.1, which is significantly greater than OJ.0.5

#These results suggest that within the OJ group, an increase in dosage will increase tooth growth


#VC group
t.test(data_VC.0.5$len,data_VC.1$len)$conf.int
t.test(data_VC.1$len,data_VC.2$len)$conf.int

#Again, the test between the 0.5 mg/day dosage group and the 2 mg/day dosage is made unnecessary by transitive
#property


#Between group tests: For a given dosage, how do the results of the two treatments differ
t.test(data_OJ.0.5$len,data_VC.0.5$len)$conf.int
t.test(data_OJ.1$len,data_VC.1$len)$conf.int
t.test(data_OJ.2$len,data_VC.2$len)$conf.int

#At the first two dosage levels, OJ is significantly more efficacious than VC, but the confidence interval
#of the difference between the two treatments at the 2mg/day level includes 0, and indicates that the difference 
#is insignificant. From the following test, however, we can conclude that 2mg/day of OJ causes more growth than
#1mg/day of VC

t.test(data_OJ.2$len,data_VC.1$len)$conf.int


#If we compare the different treatment type groups with the dosages pooled together, we find no statistically
#significant difference. That is to say that we do not have sufficient evidence to conclude that 
#"Subjects given some amount orange juice exhibited more tooth growth than subjects given some amoun of asorbic
#acid."

t.test(data_OJ_overall$len,data_VC_overall$len)$conf.int

