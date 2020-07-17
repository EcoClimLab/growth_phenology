# convert measure to DBH (Use function in Dendrobands/Rscripts/analysis/convert_caliper_meas_to_dbh.R (I pasted it below for now))
# Caliper to DBH function by Ian##
objectiveFuncDendro <- function(diameter2, diameter1, gap1, gap2) {
  if (gap1 > diameter1) {
    return(20)
  }
  if (gap2 > diameter2) {
    return(20)
  }

  delta <- abs(diameter1 - diameter2 + (1 / pi) * diameter2 * asin(gap2 / diameter2) - (1 / pi) * diameter1 * asin(gap1 / diameter1))

  return(return(delta))
}

findOneDendroDBH <- function(dbh1, m1, m2, func = objectiveFuncDendro) {
  if (is.na(dbh1) | is.na(m1) | is.na(m2) | dbh1 <= 0) {
    return(NA)
  }

  if (m2 > 0) {
    upper <- dbh1 + m2
  } else {
    upper <- dbh1 + 1
  }
  if (m2 < m1) {
    lower <- 0
  } else {
    lower <- dbh1
  }

  result <- optimize(f = func, interval = c(lower, upper), diameter1 = dbh1, gap1 = m1, gap2 = m2)
  return(result$minimum)
}

findDendroDBH <- function(dbh1, m1, m2, func = objectiveFuncDendro) {
  records <- max(length(dbh1), length(m1), length(m2))

  if (length(dbh1) == 1) dbh1 <- rep(dbh1, records)
  if (length(m1) == 1) m1 <- rep(m1, records)
  if (length(m2) == 1) m2 <- rep(m2, records)

  dbh2 <- numeric()
  for (i in 1:records) dbh2[i] <- findOneDendroDBH(dbh1[i], m1[i], m2[i], func)
  return(dbh2)
}

###################################################
### code chunk number 1: Roptions
###################################################
library(MASS)


# Sean's functions -- I think they are supposed to be in the rdendrom package but I couldn't the package to work correctly.
###################################################
### code chunk number 2: lg5.functions
###################################################
# This function predicts a diameter given the day of the year and a vector of parameters for the lg5 model.
# It is called by the lg5.ss and lg5.plot functions.

lg5.pred <- function(params, doy) {
  L <- params[1] # min(dbh, na.rm = T)
  K <- params[2]
  doy.ip <- params[3]
  r <- params[4]
  theta <- params[5]
  dbh <- vector(length = length(doy))
  dbh <- L + ((K - L) / (1 + 1 / theta * exp(-(r * (doy - doy.ip) / theta))^theta))
  return(dbh)
}

lg5.ML <- function(params, doy, dbh, resid.sd) {
  pred.dbh <- lg5.pred(params, doy)
  pred.ML <- -sum(dnorm(dbh, pred.dbh, resid.sd, log = T))
  return(pred.ML)
}

lg5.ML.wt <- function(params, doy, dbh, resid.sd) {
  wts <- 1 / dnorm(abs(seq(-2, 2, length = length(doy))), 0, 1)
  pred.dbh <- lg5.pred(params, doy)
  pred.ML <- -sum((wts * dnorm(dbh, pred.dbh, resid.sd, log = T)))
  return(pred.ML)
}

get.doy <- function(x) {
  names.data <- names(x)
  doy.1 <- as.numeric(unlist(strsplit(names.data[grep("X", names.data)], "X")))
  doy <- doy.1[!is.na(doy.1)]
  return(doy)
}

get.lg5.resids <- function(params, doy, dbh) {
  lg5.resid <- dbh - lg5.pred(params, doy)
  return(lg5.resid)
}



###################################################
### code chunk number 3: upper.lower.functions
###################################################
# LOWER AND UPPER ASYMPTOTES

pred.doy <- function(params, a, diam.given = 0) {
  params <- as.numeric(params)
  L <- params[1] # min(dbh, na.rm = T)
  K <- params[2]
  doy.ip <- params[3]
  r <- params[4]
  theta <- params[5]
  a.par <- a
  .expr1 <- (K - L) / (a.par - L)
  .expr2 <- doy.ip * r - theta * log(((.expr1 - 1) * theta)^(1 / theta))
  .expr3 <- .expr2 / r
  dcrit <- .expr3

  return(dcrit)
}

lg5.pred.a <- function(a, params, doy, dbh, asymptote = "lower") {
  asymptote <- ifelse(length(a) > 1, "both", asymptote)
  L <- params[1] # min(dbh, na.rm = T)
  K <- params[2]
  doy.ip <- params[3]
  r <- params[4]
  theta <- params[5]
  diam <- vector(length = length(doy))
  if (asymptote == "lower") {
    d.crit <- pred.doy(params, a)
    diam[which(doy <= d.crit)] <- a
    diam[which(doy > d.crit)] <- L + ((K - L) / (1 + 1 / theta * exp(-(r * (doy[which(doy > d.crit)] - doy.ip) / theta))^theta))
  } else {
    if (asymptote == "upper") {
      d.crit <- pred.doy(params, a)
      diam[which(doy >= d.crit)] <- a
      diam[which(doy < d.crit)] <- L + ((K - L) / (1 + 1 / theta * exp(-(r * (doy[which(doy < d.crit)] - doy.ip) / theta))^theta))
    } else {
      if (asymptote == "both") {
        d.crit <- pred.doy(params, a)
        diam[which(doy <= d.crit[1])] <- a[1]
        diam[which(doy >= d.crit[2])] <- a[2]
        diam[which(doy > d.crit[1] & doy < d.crit[2])] <- L + ((K - L) / (1 + 1 / theta * exp(-(r * (doy[which(doy > d.crit[1] & doy < d.crit[2])] - doy.ip) / theta))^theta))
      }
    }
  }
  return(diam)
}

lg5.ML.a <- function(a, params, doy, dbh, resid.sd) {
  pred.dbh <- lg5.pred.a(a, params, doy)
  pred.ML <- -sum(dnorm(dbh, pred.dbh, resid.sd, log = T))
}

make.seq <- function(param, params, deviation = 0.1, len.seq = 50, CI = c(0, 0), asymptote = "lower", min.val = NULL, max.val = NULL) {
  if (asymptote == "lower") {
    if (CI[1] > 0) {
      lower.lim <- max(min.val, CI[1] * (1 - deviation), na.rm = T)
      par.seq <- seq(lower.lim, (CI[2] * (1 + deviation)), length = len.seq)
    } else {
      par.seq <- seq(min.val, (param + deviation * param), length = len.seq)
    }
  } else {
    if (CI[1] > 0) {
      upper.lim <- min(max.val, CI[2] * (1 + deviation), na.rm = T)
      par.seq <- seq((CI[1] * (1 - deviation)), upper.lim, length = len.seq)
    } else {
      par.seq <- seq((param - deviation * param), max.val, length = len.seq)
    }
  }
  return(par.seq)
}

start.diam <- function(params, seq.l, doy, dbh, deviation.val, figure = FALSE, resid.sd = 0.1) {
  complete <- complete.cases(doy, dbh)
  doy <- doy[complete]
  dbh <- dbh[complete]

  profile.like.vec <- vector(length = seq.l)
  pred.min <- lg5.pred(params, doy[1])
  min.val <- max(min(dbh), pred.min, min(params[1], params[2]))

  param.vec.tmp <- make.seq(param = min(dbh, na.rm = T), params, deviation = deviation.val, len.seq = seq.l, asymptote = "lower", min.val = min.val)
  for (p in 1:seq.l) {
    pred.dbh <- lg5.pred.a(a = param.vec.tmp[p], params, doy)
    profile.like.vec[p] <- sum(dnorm(dbh, pred.dbh, resid.sd, log = T))
  }
  xi.val <- max(profile.like.vec) - 1.92
  max.value <- param.vec.tmp[which(profile.like.vec == max(profile.like.vec))]
  values.above <- which(profile.like.vec > xi.val)
  ci.lower <- param.vec.tmp[min(values.above)]
  ci.upper <- param.vec.tmp[max(values.above)]
  ci.params <- c(ci.lower, ci.upper)

  param.vec.tmp <- make.seq(param = max.value, params, deviation = 0.0001, len.seq = seq.l, CI = ci.params, asymptote = "lower", min.val = min.val)
  for (p in 1:seq.l) {
    pred.dbh <- lg5.pred.a(a = param.vec.tmp[p], params, doy)
    profile.like.vec[p] <- sum(dnorm(dbh, pred.dbh, resid.sd, log = T))
  }
  xi.val <- max(profile.like.vec) - 1.92
  max.value <- param.vec.tmp[which(profile.like.vec == max(profile.like.vec))] # Max value returning 200 max values because all profile.like.vec are the same
  values.above <- which(profile.like.vec > xi.val)
  ci.lower <- param.vec.tmp[min(values.above)]
  ci.upper <- param.vec.tmp[max(values.above)]
  ci.params <- c(ci.lower, ci.upper)
  if (figure) {
    plot(param.vec.tmp, profile.like.vec, type = "l", xlab = "Parameter value", ylab = "Likelihood")
    abline(h = xi.val, col = "darkred")
    abline(v = c(ci.lower, ci.upper), col = "darkblue", lty = 2)
  }

  start.values <- c(max.value, ci.lower, ci.upper, max(profile.like.vec, na.rm = TRUE))
  return(start.values)
}

end.diam <- function(seq.l, params, doy, dbh, deviation.val, figure = FALSE, resid.sd = 0.1) {
  complete <- complete.cases(doy, dbh)
  doy <- doy[complete]
  dbh <- dbh[complete]

  profile.like.vec <- vector(length = seq.l)

  pred.max <- lg5.pred(params, doy[length(doy)])
  max.val <- min(max(dbh), pred.max, max(params[1], params[2]))

  param.vec.tmp <- make.seq(param = max(dbh, na.rm = T), params, deviation = deviation.val, len.seq = seq.l, asymptote = "upper", max.val = max.val)

  for (p in 1:seq.l) {
    pred.dbh <- lg5.pred.a(a = param.vec.tmp[p], params = params, doy, asymptote = "upper")
    profile.like.vec[p] <- sum(dnorm(dbh, pred.dbh, resid.sd, log = T))
  }

  xi.val <- max(profile.like.vec) - 1.92
  max.value <- param.vec.tmp[which(profile.like.vec == max(profile.like.vec))]
  values.above <- which(profile.like.vec > xi.val)
  ci.lower <- param.vec.tmp[min(values.above)]
  ci.upper <- param.vec.tmp[max(values.above)]
  ci.params <- c(ci.lower, ci.upper)

  param.vec.tmp <- make.seq(param = max.value, params, deviation = 0.0001, len.seq = seq.l, CI = ci.params, asymptote = "upper", max.val = max.val)

  for (p in 1:seq.l) {
    pred.dbh <- lg5.pred.a(a = param.vec.tmp[p], params = params, doy, asymptote = "upper")
    profile.like.vec[p] <- sum(dnorm(dbh, pred.dbh, resid.sd, log = T))
  }

  values.above <- which(profile.like.vec > xi.val)
  ci.lower <- param.vec.tmp[min(values.above)]
  ci.upper <- param.vec.tmp[max(values.above)]
  ci.params <- c(ci.lower, ci.upper)
  max.value <- param.vec.tmp[which(profile.like.vec == max(profile.like.vec))]

  if (figure) {
    plot(param.vec.tmp, profile.like.vec, type = "l", xlab = "Parameter value", ylab = "Likelihood")
    xi.val <- max(profile.like.vec) - 1.92
    abline(h = xi.val, col = "darkred")
    abline(v = c(ci.lower, ci.upper), col = "darkblue", lty = 2)
  }

  end.values <- c(max.value, ci.lower, ci.upper, max(profile.like.vec, na.rm = TRUE))
  return(end.values)
}

lg5.CH <- function(paras, doyCP, dbhCP) {
  pred.dbh <- lg5.pred(paras, doyCP)
  pred.ND <- sum(pred.dbh - dbhCP) # for "Negative Difference"
  return(pred.ND)
}

get.CH.resid <- function(rate, params, doy, dbh, log.rate = FALSE) {
  lg5.pred <- lg5.pred(params, doy)
  rates <- lg5.deriv(params, doy)
  if (log.rate) {
    resids <- lg5.pred - dbh * log(rates + 0.000001)
  } else {
    resids <- lg5.pred - dbh * rates
  }
  return(resids)
}

# This function takes the numerical derivative.
# 	Default values return a single day of growth.
# 	Using the 'growth' argument, the derivative
# 		can be returned, scaled by annual growth.
lg5.deriv <- function(paras, doy, growth = 1, shift = 0.5) {
  paras <- as.numeric(paras)
  .loVal <- lg5.pred(paras, (doy - shift))
  .hiVal <- lg5.pred(paras, (doy + shift))
  deriv.lg5 <- (.hiVal - .loVal) / (2 * shift)
  return(deriv.lg5 / growth)
}

max.growth.day <- function(paras) {
  days <- seq(365) # Change to appropriate growth window?
  .deriv <- lg5.deriv(paras, days)
  fastest.day <- max(days[which(.deriv == max(.deriv))], start.day, na.rm = TRUE)
  return(fastest.day)
}

max.growth.rate <- function(paras) {
  days <- seq(round(pred.doy(params, params$a)), 365)
  .deriv <- lg5.deriv(paras, days)
  growth.rate <- max(.deriv, na.rm = TRUE)
  return(growth.rate)
}

outer.hull <- function(params, doy, dbh, quant = 0.8) {
  a <- params$a
  b <- params$b
  paras <- as.numeric(params[1:5])
  curve.pure <- (which(doy > pred.doy(paras, a) & doy < pred.doy(paras, b)))
  # 	curve.pure <- (which(doy > pred.doy(paras, a)))
  doyP <- c(pred.doy(paras, a), doy[curve.pure], pred.doy(paras, b))
  dbhP <- c(a, dbh[curve.pure], b)
  residsP <- get.lg5.resids(params = paras, doy = doyP, dbh = dbhP)
  ln.data <- length(residsP)
  top.resids <- unique(c(1, which(residsP >= quantile(residsP, quant)), length(residsP)))
  doyP2 <- doyP[top.resids]
  dbhP2 <- dbhP[top.resids]
  SSstart <- function(doy = doyP2, dbh = dbhP2) {
    lm.fit <- lm(dbhP2 ~ doyP2 + I(doyP2^2))
    new.doy <- seq(range(doyP2)[1], range(doyP2)[2])
    new.dbh <- predict(lm.fit, newdata = data.frame(doyP2 = new.doy), type = c("response"))
  }
  optim.min <- c((min(dbhP2, na.rm = TRUE) * 0.99), quantile(dbhP2, 0.5, na.rm = TRUE), 0, 0, 0.01)

  optim.max <- c(min(dbhP2, na.rm = TRUE), max(dbhP2, na.rm = TRUE), 350, 0.1, 15)

  OH.fit <- optim(par = paras, fn = lg5.ML, resid.sd = resid.sd, method = "L-BFGS-B", lower = optim.min, upper = optim.max, hessian = FALSE, control = list(trace = 0), doy = doyP2, dbh = dbhP2)
  deriv.list <- lg5.deriv(OH.fit$par, doyP, growth = (log(b) - log(a)), shift = 0.05)
  resids.hull <- get.lg5.resids(OH.fit$par, doyP, dbhP)
  weighted.deficit <- resids.hull * deriv.list
  OH.list <- list(doyP2 = doyP2, dbhP2 = dbhP2, doyP = doyP, dbhP = dbhP, OH.fit = OH.fit, Derivatives = deriv.list, Deficit = resids.hull, Weighted.deficit = weighted.deficit)
  return(OH.list)
}

fit.outer.hull <- function(dbh, doy.full, params, quant = 0.8) {
  dbh <- as.numeric(dbh)
  complete <- complete.cases(dbh)
  dbh <- dbh[complete]
  doy <- doy.full[complete]
  out.fit <- outer.hull(params, doy, dbh)
}
