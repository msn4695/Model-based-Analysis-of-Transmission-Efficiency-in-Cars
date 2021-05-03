library(ggplot2)
data(mtcars)
head(mtcars,3)
str(mtcars)
mtcars
sapply(mtcars, function(x) {
    length(unique(x))
})
x <- c(2,8,9,10,11)
for (i in x) {
    mtcars[,i] <- as.factor(mtcars[,i])
}
sapply(mtcars, function(x) sum(is.na(x)))

fitAll <- lm(mpg ~ ., data = mtcars)
summary(fitAll)
stepfit <- step(fitAll)
summary(stepfit)
x <- noquote(paste("fit", 1:10, sep = ""))
nam <- names(mtcars)[-1]
fit1 <- lm(mpg ~ cyl, data = mtcars)
for (i in 2:10) {
    x[i] <- update(x[i-1], )
}
x[1]
anova(fitAll)
ggplot(mtcars[mtcars$cyl == 4,], aes(x = am, y = mpg)) + geom_boxplot(aes(fill = am))
table(mtcars$cyl)
library(dplyr)
mtcars %>% group_by(am) %>% group_by(cyl)
group_by(mtcars, am)
length(unique(mtcars[mtcars$am == 1,]))
table(mtcars)
table(mtcars$cyl)
ggplot(mtcars[mtcars$cyl == 8,])
?mtcars
mtcars8 <- mtcars[mtcars$cyl == 8,]
summary(lm(mpg ~ am, data = mtcars))$coef
for (i in unique(as.integer(as.character(mtcars$cyl)))) {
    print(i)
}
class(unique(mtcars$cyl))
x <- as.numeric(as.character(unique(mtcars$cyl)))
res <- matrix(ncol = 4)
for (i in x) {
    rbind(res, summary(lm(mpg ~ am, data = mtcars[mtcars$cyl == i,]))$coef)
}
summary(lm(mpg ~ am, data = mtcars[mtcars$cyl == 4,]))$coef
summary(lm(mpg ~ am, data = mtcars[mtcars$cyl == 6,]))$coef
summary(lm(mpg ~ am, data = mtcars[mtcars$cyl == 8,]))$coef
summary(lm(mpg ~ am, data = mtcars))$coef

?t.test
data(sleep)
head(sleep,3)
t.test(extra ~ group, data = sleep)
t.test(mpg ~ am, data = mtcars)
mtcars4 <- mtcars[mtcars$cyl == 4,]
mtcars6 <- mtcars[mtcars$cyl == 6,]
mtcars8 <- mtcars[mtcars$cyl == 8,]
t.test(mpg ~ am, mtcars4)
t.test(mpg ~ am, mtcars6)
t.test(mpg ~ am, mtcars8)
t.test(mpg ~ am, mtcars)
library(ggplot2)
g <- ggplot(mtcars, aes(x = am, y = mpg, fill = am))
g <- g + geom_boxplot()
g

for (i in seq_along(mtcars$am)) {
    if (mtcars$am[i] == "0") {
        mtcars$am[i] <- "Automatic"
    } else {
        mtcars$am[i] <- "Manual"
    }
} 

mtcars$am[mtcars$am == "0"] <- "Automatic"
mtcars$am[mtcars$am == "1"] <- "Manual"
head(mtcars)
data(mtcars)
mtcars$am

test <- t.test(mpg ~ am, data = mtcars)
test$conf.int
test$p.value
summary(test)
str(test)
test$statistic
mtcars[sample(1:nrow(mtcars), size = 3),]

summary(lm(mpg ~ am, data = mtcars))$coef
mpgc <- mtcars$mpg - mean(mtcars$mpg)
summary(lm(mpgc ~ am, data = mtcars))$coef

fit1 <- lm(mpg ~ am, data = mtcars)
sumfit <- summary(fit1)
fit1$coefficients
sumfit$coefficients[,4]

data(mtcars)
str(mtcars)

t.test(mpg ~ am, mtcars4)
summary(lm(mpg ~ am, mtcars4))$coef
t.test(mpg ~ am, mtcars6)
summary(lm(mpg ~ am, mtcars6))$coef
t.test(mpg ~ am, mtcars8)
summary(lm(mpg ~ am, mtcars8))$coef
t.test(mpg ~ am, mtcars)
summary(lm(mpg ~ am, mtcars))$coef

mtcars$am[mtcars$am == "0"] <- "Automatic"
mtcars$am[mtcars$am == "1"] <- "Manual"
fit3 <- lm(mpg ~ hp + am, data = mtcars)
head(mtcars)
g <- ggplot(mtcars, aes(x = hp, y = mpg, fill = am))
g <- g + geom_point()
g <- g + geom_smooth(aes(lm(mpg ~ hp, data = mtcars)))



plot(x = c(0,400), y = c(0, 35), type = "n", frame = F, pch = 19)
lines(x = mtcars$hp, y = predict(fit3, data.frame(hp = mtcars$hp, am = "Automatic")))
lines(x = mtcars$hp, y = predict(fit3, data.frame(hp = mtcars$hp, am = "Manual")))
title(main = "MPG vs HP + Transmission", xlab = "Horsepower(HP)", ylab = "Miles per Gallon (MPG)")


g <- ggplot(mtcars, aes(x = hp, y = mpg))
g <- g + labs(title = "MPG vs HP + Transmission", 
              x = "Horsepower(HP)",
              y = "Miles per Gallon (MPG)")
g <- g + geom_blank()
g <- g + geom_smooth(aes(x = hp, y = predict(fit3, data.frame(hp = hp, am = "Manual")), color = "red"))
g <- g + geom_smooth(aes(x = hp, y = predict(fit3, data.frame(hp = hp, am = "Automatic")), color = "blue"))
g <- g + scale_color_manual(name = "Transmission", values = c('red' = 'red', 'blue' = 'blue'), labels = c("Automatic", "Manual"))
g










