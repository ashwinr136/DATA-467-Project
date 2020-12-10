library(readxl)
kc <- read_excel("kc_house_data_1.xlsx")


samp<-kc[sample(nrow(kc), 100), ]
m1<-lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+lat+long+sqft_living15+sqft_lot15,data=samp)
summary(m1)

hist(kc$sqft_living,breaks=20,prob=TRUE)
lines(seq(min(kc$sqft_living), max(kc$sqft_living), by=100),dnorm(seq(min(kc$sqft_living), max(kc$sqft_living), by=100),mean(kc$sqft_living), sd(kc$sqft_living)), col="blue")
hist(kc$yr_built,breaks=30)
hist(kc$long)
hist(kc$lat)

boxplot(price~bedrooms,data=samp)
hist(samp$price,breaks=30)

plot(samp$sqft_living,samp$price,xlab="Sqft Living",ylab="Price",main="Price vs sample sq.ft")
abline(lm(samp$price~samp$sqft_living),col="red")

m1_back_aic<-step(m1, direction="backward")

n<-length(resid(m1))
m1_back_bic<-step(m1, direction = "backward", k=log(n))

m1_start<-lm(price~1,data=samp)
m1_for_aic<-step(m1_start,scope=price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+lat+long+sqft_living15+sqft_lot15,direction="forward")

#forward and backward aic are the same

m1_for_bic<-step(m1_start,scope=price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+lat+long+sqft_living15+sqft_lot15,direction="forward",k=log(n))

#anova on for aic and bic -> aic, F=2.9629 on 3 and 91 df

m1_both_bic<-step(m1_start,scope=price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+lat+long+sqft_living15+sqft_lot15,direction="both",k=log(n))
m1_both_aic<-step(m1_start,scope=price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+lat+long+sqft_living15+sqft_lot15,direction="both")

#both and forward aic are the same


#price ~ bedrooms + sqft_living + sqft_lot + view + grade + yr_built + 
#lat + sqft_lot15

#F = 3.7747 on 3 and 94 df

m2<-lm(formula = price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
         floors + waterfront + view + condition + grade + sqft_above + 
         sqft_basement + yr_built + yr_renovated + lat + long + sqft_living15 + 
         sqft_lot15, data = kc)

m2_both_bic<-step(lm(price~1,data=kc),scope=price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+lat+long+sqft_living15+sqft_lot15,direction="both",k=log(length(resid(m2))))
m2_both_aic<-step(m2,scope=price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+lat+long+sqft_living15+sqft_lot15,direction="both")

#F = 3.4814 on 2 and 21596 df

m2_back_aic<-step(m2,scope=price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+lat+long+sqft_living15+sqft_lot15,direction="backward")
m2_for_bic<-step(lm(price~1,data=kc),scope=price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+lat+long+sqft_living15+sqft_lot15,direction="forward",k=log(length(resid(m2))))

#both bic better than both aic
#both bic -floors,sqft_lot

#F = 6.9182 on 1 and 21598 df



