library(ISLR)
> require(ISLR)
> names(Smarket)
> head(Smarket)
> summary(Smarket)

> par(mfrow=c(1,8))
> for(i in 1:8) {hist(Smarket[,i], main=names(Smarket)[i])}

> for(i in 1:8) {boxplot(Smarket[,i], main=names(Smarket)[i])}
> library(corrplot)
> install.packages("corrplot")
> install.packages("corrplot")

> require(corrplot)
> library(corrplot)
> correlations = cor(Smarket[,1:8])
> corrplot(correlations, method = 'circle')

> pairs(Smarket, col=Smarket$Direction)
> install.packages("caret")

> library(caret)

> require(caret)


> glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data=Smarket)
> summary(glm.fit)


> glm.probs = predict(glm.fit, type = 'response')
> glm.probs[1:5]

> glm.pred = ifelse(glm.probs > 0.5, 'up', 'down')
> attach(Smarket)
> table(glm.pred, Direction)

> mean(glm.pred == Direction)

> train = Year < 2005


> glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket, family = binomial, subset = train)
> glm.probs = predict(glm.fit, newdata = Smarket[!train,], type = 'response')
> glm.pred = ifelse(glm.probs > 0.5, 'up', 'down')
> Direction.2005 = Smarket$Direction[!train]
> table(glm.pred, Direction.2005)
> mean(glm.pred == Direction.2005)
> glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3, data = Smarket, family = binomial, subset = train)
> glm.probs = predict(glm.fit, newdata = Smarket[!train,], type = 'response')
> glm.pred = ifelse(glm.probs > 0.5, 'up', 'down')
> table(glm.pred, Direction.2005)

> mean(glm.pred == Direction.2005)
                       
> summary(glm.fit)
                       
                       
                       
                       