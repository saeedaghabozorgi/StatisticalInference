library(ggplot2)
library(datasets)
data(ToothGrowth)
head(ToothGrowth)
attach(ToothGrowth)
g1 <- ggplot(ToothGrowth, aes(x=factor(dose),y=len,fill=factor(dose)))
g1 + geom_boxplot(notch=F) + facet_grid(.~supp) + 
  scale_x_discrete("Dosage in mg") + 
  scale_y_continuous("Length of Teeth") + 
  ggtitle("Exploratory data analyses")

dose <- as.factor(dose)
summary(ToothGrowth)
table(supp, dose)


test.supp.equ <- t.test(len~supp, paired=F, var.equal=T, data=ToothGrowth)
test.supp.nequ <- t.test(len~supp, paired=F, var.equal=F, data=ToothGrowth)

data.frame("p-value"=c(test.supp.equ$p.value, test.supp.nequ$p.value),"Conf-Low"=c(test.supp.equ$conf[1],test.supp.nequ$conf[1]),
                          "Conf-High"=c(test.supp.equ$conf[2],test.supp.nequ$conf[2]), row.names=c("Equal Var","Unequal Var"))


dose.05 <- ToothGrowth[which(dose==.5),1]
dose.10 <- ToothGrowth[which(dose==1),1]
dose.20 <- ToothGrowth[which(dose==2),1]
dose0510.t1 <- t.test(dose.05, dose.10, paired=F, var.equal=T)
dose0510.t2 <- t.test(dose.05, dose.10, paired=F, var.equal=F)
dose0510.result <- data.frame("p-value"=c(dose0510.t1$p.value, dose0510.t2$p.value),"Conf-Low"=c(dose0510.t1$conf[1],dose0510.t2$conf[1]),"Conf-High"=c(dose0510.t1$conf[2],dose0510.t2$conf[2]), row.names=c("Equal Var","Unequal Var"), "Dose"="0.5 to 1")
dose0520.t1 <- t.test(dose.05, dose.20, paired=F, var.equal=T)
dose0520.t2 <- t.test(dose.05, dose.20, paired=F, var.equal=F)
dose0520.result <- data.frame("p-value"=c(dose0520.t1$p.value, dose0520.t2$p.value),"Conf-Low"=c(dose0520.t1$conf[1],dose0520.t2$conf[1]),"Conf-High"=c(dose0520.t1$conf[2],dose0520.t2$conf[2]), row.names=c("Equal Var","Unequal Var"), "Dose"="0.5 to 2")
dose1020.t1 <- t.test(dose.10, dose.20, paired=F, var.equal=T)
dose1020.t2 <- t.test(dose.10, dose.20, paired=F, var.equal=F)
dose1020.result <- data.frame("p-value"=c(dose1020.t1$p.value, dose1020.t2$p.value),"Conf-Low"=c(dose1020.t1$conf[1],dose1020.t2$conf[1]),"Conf-High"=c(dose1020.t1$conf[2],dose1020.t2$conf[2]), row.names=c("Equal Var","Unequal Var"), "Dose"="1 to 2")
rbind(dose0510.result,dose0520.result,dose1020.result)
