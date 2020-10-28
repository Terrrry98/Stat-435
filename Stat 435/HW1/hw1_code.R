# part(a)
source("home1-part1-data.R")
ksmooth.train <- function(x.train, y.train, kernel, bandwidth, CV) {
  yhat.train <- list(rep(0, length(x.train)))
  if (kernel == "normal") {
    # find sigma
    sig = -0.25* bandwidth / qnorm(0.25, 0, 1)
    for (i in 1:length(x.train)) {
      x_curr = x.train[i]
      y_curr = y.train[i]
      y_abv = 0
      y_btm = 0
      for (j in 1:length(x.train)) {
        # if cv=T, i should not be used.
        if ( (CV & i!=j) | (!CV) ) {
          y_abv = y_abv + y.train[j] * dnorm(x_curr - x.train[j], 0, sig)
          y_btm = y_btm + dnorm(x_curr - x.train[j], 0, sig)
        }
      }
      yhat.train[i] = y_abv / y_btm
    }
  } else {
    #box kernel
    for (i in 1:length(x.train)) {
      total = c()
      for (j in 1:length(x.train)) {
        if ( ((!CV) | (CV & j!=i)) & abs(x.train[i] - x.train[j]) <= 0.5*bandwidth) {
          total = c(total, y.train[j])
        }
      }
      yhat.train[i] = sum(total) / length(total)
    }
  }
  yhat.train = unlist(yhat.train)
  df = data.frame(x.train, yhat.train)
}


#part(b)
ksmooth.predict <- function(ksmooth.train.out, x.query) {
  x = ksmooth.train.out$x.train
  y = ksmooth.train.out$yhat.train
  x_min = min(x)
  x_max = max(x)
  y_min = y[match(x_min, x)]
  y_max = y[match(x_max, x)]
  y_predict = c()
  for (i in x.query) {
    if (i >= x_min & i <= x_max) {
      # linear interpolate
      y_predict = c(y_predict, approx(x, y, xout = i, method = "linear")$y)
    } else {
      #constant extrapolate
      if (i < x_min) {
        y_predict = c(y_predict, y_min)
      } else {
        y_predict = c(y_predict, y_max)
      }
    }
  }
  return(y_predict)
}


# part(c)
wage.train = Wage.train$wage
age.train = Wage.train$age
plot(age.train, wage.train)
ksmooth.train.out = ksmooth.train(age.train, wage.train, "normal", 3, FALSE)
result_df = ksmooth.train.out[order(ksmooth.train.out[,1]),]
lines(result_df$x.train, result_df$yhat.train,lwd = 3, col = 'red')

# Calculate residual sum of squares
origin_df = data.frame(age.train, wage.train)
origin_df = origin_df[order(origin_df[,1]),]

rss = sum((origin_df$wage.train - result_df$yhat.train)^2)
rss
# 1625120.9467


# test for box kernel
# ksmooth.train.out = ksmooth.train(age.train, wage.train, "box", 3, FALSE)
# result_df = ksmooth.train.out[order(ksmooth.train.out[,1]),]
# lines(result_df$x.train, result_df$yhat.train,lwd = 3, col = 'blue')

# part(d)
wage.test = Wage.test$wage
age.test = Wage.test$age
wage.predict = ksmooth.predict(result_df,age.test)
plot(age.test, wage.test)
predict_df = data.frame(age.test, wage.predict)
predict_df = predict_df[order(predict_df[,1]),]
lines(predict_df, lwd = 3, col = "blue")

# Calculate residual sum of squares
origin_df = data.frame(age.test, wage.test)
origin_df = origin_df[order(origin_df[,1]),]

rss = sum((origin_df$wage.test - predict_df$wage.predict)^2)
# 3168000.16699


# part(e)
bd = seq(1, 10, 1)
ese_train = c()
for (i in bd) {
  train_df = ksmooth.train(age.train, wage.train, "normal", i, FALSE)
  ese_train = c(ese_train, sum((train_df$yhat.train-wage.train)^2) / length(wage.train))
}
plot(bd, ese_train)
print(ese_train)
hopt = match(min(ese_train), ese_train)
print(hopt)


# part(f)
ese_cv = c()
for (i in bd) {
  train_df = ksmooth.train(age.train, wage.train, "normal", i, TRUE)
  ese_cv = c(ese_cv, sum((train_df$yhat.train-wage.train)^2) / length(wage.train))
}
plot(bd, ese_cv)
print(ese_cv)
hopt = match(min(ese_cv), ese_cv)
print(hopt)


# part(g)
ese_test = c()
for (i in bd) {
  train_df = ksmooth.train(age.train, wage.train, "normal", i, FALSE)
  wage.predict = ksmooth.predict(train_df,age.test)
  ese_test = c(ese_test, sum((wage.predict-wage.test)^2) / length(wage.test))
}
plot(bd, ese_test)
print(ese_test)
hopt = match(min(ese_test), ese_test)
print(hopt)






i = 6
age.test = Wage.train$age[which(fold == i)]
age.train = Wage.train$age[-which(fold == i)]
wage.test = Wage.train$wage[which(fold == i)]
wage.train = Wage.train$wage[-which(fold == i)]
smooth_df = ksmooth.train(age.train, wage.train, "normal", i, FALSE)
predict_wage = ksmooth.predict(smooth_df, age.test)
ese_kfold = c(ese_kfold, sum((predict_wage$y_predict - wage.test)^2)/length(wage.test))





