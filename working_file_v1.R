##Section 0

#getting the data set up
#added new variable for total amount paid and took the log of it
benjer = read.csv("BenAndJerry.csv")
priceper1 = (benjer$price_paid_deal + benjer$price_paid_non_deal)/benjer$quantity
lpriceper1 <- log(1+priceper1)


##Section 1
##ran some demographics on price, variable

plot(coupon_value ~ household_income, data=benjer)  ##shows that there's are greate values of coupons for richer household, which is somewhat surprising
plot(priceper1 ~ race, data=benjer)  ##shows greatest spread among whites
plot(priceper1 ~ hispanic_origin, data=benjer) ##shows greatest non-hispanics

plot(priceper1 ~ household_income, data=benjer) ##not interesting, just gets to be more spread out as get more income
plot(lpriceper1 ~ household_income, data=benjer) ##not interesting, just gets to be more spread out as get more income

Section 2
##ran the same process that he did to create a new data set
x <- benjer[,c("flavor_descr","size1_descr",
	"household_income","household_size")]
x$flavor_descr <- relevel(x$flavor_descr,"VAN")
x$usecoup = factor(benjer$coupon_value>0)
x$couponper1 <- benjer$coupon_value/benjer$quantity
x$region <- factor(benjer$region, 
	levels=1:4, labels=c("East","Central","South","West"))
	x$married <- factor(benjer$marital_status==1)
x$race <- factor(benjer$race,
	levels=1:4,labels=c("white","black","asian","other"))
x$hispanic_origin <- benjer$hispanic_origin==1
x$microwave <- benjer$kitchen_appliances %in% c(1,4,5,7)
x$dishwasher <- benjer$kitchen_appliances %in% c(2,4,6,7)
x$sfh <- benjer$type_of_residence==1
x$internet <- benjer$household_internet_connection==1
x$tvcable <- benjer$tv_items>1


fit <- glm(lpriceper1~.,data=x)
##problem with how it exists now is that the flavor description should be predicting all of the price. that is why household income coefficient is really small, but still significant - because all of the variation is explained in the price description. if were to improve, would drop several of the variables from x since what really interested in isn't what price peoplel pay based on the flavor (we set that), but rather what are predictors for how much different groups will pay. or at the very least take away things that we would assume to just be proxies for income, like microwave, cable, dishwasher, and internet

##Section 3
pvals <- summary(fit)$coef[-1,4] 
pvals
#this prints off all of the pvals

#Section +
#fdr_cut
source("fdr.R")
cutoff <- fdr_cut(pvals, 0.01) #shows several different values that are still statistically significant even if we keep the r level pretty low (happened even when changed pvals to .001)