# a fake dataset with a continuous response and a continuous explanatory

d0 <- list()
d0$explanatory <- rnorm(66, 17.35, 0.4)
a <- 0.2
b <- 0.005
error <- rnorm(66, 0, 0.0013)
d0$response <- a + b*d0$explanatory + error

plot(response ~ explanatory , d0)  
abline(lm(response ~ explanatory , d0))

write.csv(as.data.frame(d0), file = "rbasics/d0.csv")

# a fake dataset of one continuous response, two fatorial explanatory variables, and a continuous covariable of little effect
d1 <- list()
d1$exp1 <- as.factor(rep(c("ae","ge","rw"), each = 20))
d1$exp2 <- as.factor(rep(rep(c("A","B","C","D"), each = 5), times = 3))
d1$cov1 <- rnorm(60, 3.35, sd = runif(4, 1.2,1.8)[as.integer(d1$exp1)])

a <- runif(3, 5,8)
b <- runif(4, 0,0.3)
c <- 0.24353
error <- rnorm(60, 0, sd = 1.2) 
d1$resp <-  a[as.integer(d1$exp1)] + b[as.integer(d1$exp2)] +  c*d1$cov1 + error
d1$resp <- round(d1$resp, 2)

model <- lm(resp ~ exp1 + exp2 , d1)
anova(model)
summary(model)

plot(resp ~ exp1, d1)
plot(predict(model), d1$resp, asp = 1)
abline(a = 0, b = 1)
TukeyHSD(aov(resp ~ exp1 , d1))

as.data.frame(d1)

write.csv(as.data.frame(d1), file = "d1.csv")

# a fake dataset with one continuous response, two continuous explanatories and a factorial cofactor 
d2 <- list()
d2$cof1 <- as.factor(rep(sample(c("a","b","c","d","e"), 40, replace = TRUE), times = 3))
d2$exp1 <- rnorm(120, 0.15, rnorm(5, 2.2131,0.42)[as.integer(d2$cof1)] )  
d2$exp2 <- rnorm(120, 43.1, 12.42) + rnorm(5, 0,6)[as.integer(d2$cof1)] 

a <- 44.1
b <- 0.212
c <- 0.64
d <- 0.06353
error <- rnorm(120, 0, sd = 18.2) 
d2$resp <- a + b * d2$exp1 + c * d2$exp2 +  d*d2$exp1*d2$exp2 + error
d2$resp <- as.integer(d2$resp)


model <- lm(resp ~ exp1 * exp2, d2)
anova(model)
summary(model)
model <- lm(resp ~ exp1 + exp2, d2)
anova(model)


plot(resp ~ exp2, d2)
summary(model)
abline(a = 46.935, b = 0.6152)

plot(model$residuals ~ exp1, d2)
summary(model)
abline(a = 0, b = 2.0999)

write.csv(as.data.frame(d2), file = "rbasics/d2.csv")


# fake bad data structure version of d2

df1 <- as.data.frame(d1)
df_bad <- cbind(subset(df1, exp1 == "ae"),subset(df1, exp1 == "ge")[,3:4],subset(df1, exp1 == "rw")[3:4])


write.csv(as.data.frame(df_bad), file = "rbasics/d_bad.csv")

