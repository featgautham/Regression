library(gamlss)
data(plasma)
plasma

plasma2<-plasma[!(plasma$betaplasma==0),]
plasma2

hist(plasma2$betaplasma)
hist(sqrt(plasma2$betaplasma))
hist(log(plasma2$betaplasma))

plasma2[,13] <- log(plasma2[13])
plasma2

table(plasma2$sex)
table(plasma2$smokstat)
table(plasma2$vituse)

boxplot(betaplasma~sex, ylab="Betaplasma", xlab="Sex", main="Log Betaplasma vs Sex", data=plasma2)
boxplot(betaplasma~smokstat, ylab="Betaplasma", xlab="Smokstat", main=" Log Betaplasma vs Smokstat",  data=plasma2)
boxplot(betaplasma~vituse, ylab="Betaplasma", xlab="Vituse", main="Log Betaplasma vs Vituse", data=plasma2)

hist(plasma2$age)
hist(plasma2$bmi)
hist(plasma2$fiber)
hist(plasma2$alcohol)
hist(plasma2$betadiet)

hist(sqrt(plasma2$bmi))
hist(sqrt(plasma2$alcohol))
hist(sqrt(plasma2$betadiet))

hist(log(plasma2$bmi))
hist(log(plasma2$alcohol))
hist(log(plasma2$betadiet))

plasma2[,4] <- log(plasma2[4])
plasma2[,9] <- log(plasma2[9])
plasma2[,11] <- log(plasma2[11])

head(plasma2)

plasma_sub <- plasma2[c(1,4,8,9,11,13)]
head(plasma_sub)

library(GGally)
plasma_pair <- ggpairs(plasma_sub)
plasma_pair

model_age = lm(betaplasma ~ age, data=plasma2)
summary(model_age)

model_sex = lm(betaplasma ~ sex, data=plasma2)
summary(model_sex)

model_smokstat = lm(betaplasma ~ smokstat, data=plasma2)
summary(model_smokstat)

model_bmi = lm(betaplasma ~ bmi, data=plasma2)
summary(model_bmi)

model_vituse = lm(betaplasma ~ vituse, data=plasma2)
summary(model_vituse)

model_fiber = lm(betaplasma ~ fiber, data=plasma2)
summary(model_fiber)

model_alcohol = lm(betaplasma ~ alcohol, data=plasma2)
summary(model_alcohol)

model_betadiet = lm(betaplasma ~ betadiet, data=plasma2)
summary(model_betadiet)


model = lm(betaplasma ~ age + sex + smokstat + bmi + vituse + fiber, data=plasma2)
summary(model)

op = par(mfrow=c(3,2))
plot(model, which=1:6)