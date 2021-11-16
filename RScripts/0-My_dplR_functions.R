# Response and correlation functions ####
my.dcc <- function (chrono, clim, method = "response", start = -6, end = 9,
                 timespan = NULL, vnames = NULL, sb = TRUE, boot = TRUE,
                 ci = 0.05, ci2 = 0.002)
{
  month.ids <- c(-1:-12, 1:12)
  errormsg1 <- "start and end have to define an interval in [-1, -2, ..., -12, 1, 2, ..., 12]."
  if (!is.element(start, month.ids) || !is.element(end, month.ids) ||
      which(month.ids == start) > which(month.ids == end)) {
    stop(errormsg1)
  }

  clim <- climdispatch(clim)
  chrono.years <- as.numeric(row.names(chrono))
  clim.years <- sort(unique(clim[, 1]))

  if (chrono.years[1] <= clim.years[1]) {
    overlap <- na.omit(clim.years[match(chrono.years, clim.years)])
  } else {
    overlap <- na.omit(chrono.years[match(clim.years, chrono.years)])
  }

  if (is.null(timespan)) {
    start.year <- overlap[1]
    end.year <- tail(overlap, 1)
  }  else {
    if (start > 0) {
      if (!is.element(timespan[1], overlap) || !is.element(timespan[2],
                                                           overlap)) {
        errormsg3 <- paste("timespan has to be between ",
                           overlap[1], " and ", tail(overlap, 1), " for start dates in current year.",
                           sep = "")
        stop(errormsg3)
      } else {
        start.year <- timespan[1]
        end.year <- timespan[2]
      }
    } else {
      if (!is.element(timespan[1], overlap) || !is.element(timespan[2],
                                                           overlap)) {
        errormsg4 <- paste("timespan has to be between ",
                           overlap[1] + 1, " and ", tail(overlap, 1),
                           " for start dates in previous year.", sep = "")
        stop(errormsg4)
      } else {
        start.year <- timespan[1]
        end.year <- timespan[2]
      }
    }
  }
  if (start < 0 && is.na(match((start.year - 1), clim.years))) {
    offset <- 1
  } else {
    offset <- 0
  }
  if (start < 0) {
    interval.clim <- (start.year - 1 + offset):end.year
    interval.chrono <- (start.year + offset):end.year
  } else {
    interval.clim <- (start.year + offset):end.year
    interval.chrono <- (start.year + 1 + offset):end.year
  }
  if (start * end > 0) {
    no.params <- (dim(clim)[2] - 2) * length(start:end)
  } else {
    no.params <- (dim(clim)[2] - 2) * length(start:end) - 1
  }

  overlap.size <- length(start.year:end.year)

  if (no.params > overlap.size) {
    win.size.msg <- paste("Overlapping time span of chrono and climate records is smaller than number of parameters! Consider adapting the number of parameters to a maximum of ",
                          overlap.size, ".", sep = "")
    stop(win.size.msg)
  }

  a <- as.numeric(rownames(chrono)) %in% interval.chrono
  b <- clim[, 1] %in% interval.clim
  chrono.trunc <- chrono[a, 1]
  clim.trunc <- clim[b, ]

  p_analysis_time_frame <- pmat(clim.trunc, start, end, vnames)
  p_full_time_frame <- pmat(clim, start, end, vnames)

  METHOD <- match.arg(method, c("response", "correlation"))

  if (METHOD == "response") {
    if (boot) {
      dc <- my.brf(chrono.trunc, p_analysis_time_frame, sb = sb, vnames = vnames, ci2 = ci2)
    } else {
      dc <- nbrf(chrono.trunc, p_analysis_time_frame, vnames = vnames)
    }
  }
  if (METHOD == "correlation") {
    if (boot) {
      dc <- my.bcf(chrono.trunc, p_analysis_time_frame, sb = sb, vnames = vnames,
                ci = ci, ci2 = ci2)
    } else {
      dc <- nbcf(chrono.trunc, p_analysis_time_frame, vnames = vnames)
    }
  }
  cat("time span considered:", start.year, "-", end.year,
      "\n")

  # convert correlation to Linear Slope ####
  ## see https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fele.12650&file=ele12650-sup-0003-Methods.pdf

  sd.clim_analysis_time_frame <- apply(p_analysis_time_frame, 2, sd)
  sd.clim_full_time_frame <-  apply(p_full_time_frame, 2, sd)
  sd.chrono <- sd(chrono.trunc)

  dc$chg_rad_inc_clim <- dc$coef * (sd.chrono / sd.clim_analysis_time_frame)
  dc$chg_rad_inc_clim_ci.lower <- dc$ci.lower * (sd.chrono / sd.clim_analysis_time_frame) # naive calculation
  dc$chg_rad_inc_clim_ci.upper <- dc$ci.upper * (sd.chrono / sd.clim_analysis_time_frame)  # naive calculation


  dc$chg_rad_inc_1SD_clim <- dc$coef * (sd.chrono / sd.clim_analysis_time_frame) * sd.clim_full_time_frame # multiplying again by sd.clim to get the change in radius increment under 1SD increase in climate varibale


  # return

  dc
}

my.brf <- function (g, p, sb, vnames, ci = 0.05, ci2 = 0.002)
{
  n <- length(g)
  m <- dim(p)[2]
  param.matrix <- matrix(NA, nrow = m, ncol = 1000)
  if (sb) {
    pb <- txtProgressBar(min = 1, max = 1000, style = 3)
  }
  for (i in 1:1000) {
    boot.sample <- sample(1:n, n, replace = TRUE)
    boot.g <- g[boot.sample]
    boot.p <- p[boot.sample, ]
    boot.g <- (boot.g - mean(boot.g))/sd(boot.g)
    boot.p <- apply(boot.p, 2, function(x) {
      (x - mean(x))/sd(x)
    })
    cor.mat <- cor(boot.p)
    eigen.decomp <- eigen(cor.mat)
    eigenvectors <- eigen.decomp$vectors
    eigenvalues <- eigen.decomp$values
    cumprods <- cumprod(eigenvalues)
    reduced.eigenvectors <- eigenvectors[, cumprods > 1]
    pc.scores <- boot.p %*% reduced.eigenvectors
    k <- qr.solve(pc.scores, boot.g)
    zeros <- rep(0, length(which(cumprods < 1)))
    k <- c(k, zeros)
    b <- eigenvectors %*% k
    param.matrix[, i] <- b
    if (sb)
      setTxtProgressBar(pb, i)
  }
  brf.coef <- apply(param.matrix, 1, median)
  if (ci == 0.05) {
    ci.lower <- apply(param.matrix, 1, function(x) {
      sort(x)[25]
    })
    ci.upper <- apply(param.matrix, 1, function(x) {
      sort(x)[975]
    })
  } else {
    if (ci == 0.01) {
      ci.lower <- apply(param.matrix, 1, function(x) {
        sort(x)[5]
      })
      ci.upper <- apply(param.matrix, 1, function(x) {
        sort(x)[995]
      })
    } else {
      if (ci == 0.1) {
        ci.lower <- apply(param.matrix, 1, function(x) {
          sort(x)[50]
        })
        ci.upper <- apply(param.matrix, 1, function(x) {
          sort(x)[950]
        })
      } else {
        stop("`ci` must be either 0.1, 0.05, or 0.01.")
      }
    }
  }

  if (ci2 == 0.002) {

    ci.lower2 <- apply(param.matrix, 1, function(x) {
      sort(x)[1]
    })
    ci.upper2 <- apply(param.matrix, 1, function(x) {
      sort(x)[999]
    })

  } else {
    stop("`ci2` must be 0.002.")
  }

  is.sig <- is.sig2 <- logical(m)
  for (i in 1:m) {
    # for ci
    if (sign(ci.upper[i]) != sign(ci.lower[i])) {
      is.sig[i] <- FALSE
    }
    else {
      if (abs(brf.coef[i]) > abs((abs(ci.upper[i]) - abs(ci.lower[i]))/2)) {
        is.sig[i] <- TRUE
      }
      else {
        is.sig[i] <- FALSE
      }
    }

    # for ci2
    if (sign(ci.upper2[i]) != sign(ci.lower2[i])) {
      is.sig2[i] <- FALSE
    }
    else {
      if (abs(brf.coef[i]) > abs((abs(ci.upper2[i]) - abs(ci.lower2[i]))/2)) {
        is.sig2[i] <- TRUE
      }
      else {
        is.sig2[i] <- FALSE
      }
    }
  }


  out <- data.frame(coef = brf.coef, significant = is.sig, significant2 = is.sig2,
                    ci.lower = ci.lower, ci.upper = ci.upper)
  rownames(out) <- colnames(p)
  if (sb)
    close(pb)
  attributes(out)$npar <- attributes(p)$npar
  attributes(out)$vnames <- vnames
  out
}




my.bcf <- function (g, p, sb, vnames, ci = 0.05, ci2 = 0.002)
{
  n <- length(g)
  m <- dim(p)[2]
  param.matrix <- matrix(NA, nrow = m, ncol = 1000)
  if (sb) {
    pb <- txtProgressBar(min = 1, max = 1000, style = 3)
  }
  for (i in 1:1000) {
    boot.sample <- sample(1:n, n, replace = TRUE)
    boot.g <- g[boot.sample]
    boot.p <- p[boot.sample, ]
    boot.g <- (boot.g - mean(boot.g))/sd(boot.g)
    boot.p <- apply(boot.p, 2, function(x) {
      (x - mean(x))/sd(x)
    })
    for (j in 1:m) {
      param.matrix[j, i] <- qr.solve(boot.p[, j], boot.g)
    }
    if (sb)
      setTxtProgressBar(pb, i)
  }
  bcf.coef <- apply(param.matrix, 1, median)
  if (ci == 0.05) {
    ci.lower <- apply(param.matrix, 1, function(x) {
      sort(x)[25]
    })
    ci.upper <- apply(param.matrix, 1, function(x) {
      sort(x)[975]
    })
  }
  else {
    if (ci == 0.01) {
      ci.lower <- apply(param.matrix, 1, function(x) {
        sort(x)[5]
      })
      ci.upper <- apply(param.matrix, 1, function(x) {
        sort(x)[995]
      })
    }
    else {
      if (ci == 0.1) {
        ci.lower <- apply(param.matrix, 1, function(x) {
          sort(x)[50]
        })
        ci.upper <- apply(param.matrix, 1, function(x) {
          sort(x)[950]
        })
      }
      else {
        stop("`ci` must be either 0.1, 0.05, or 0.01.")
      }
    }
  }

  if (ci2 == 0.002) {

    ci.lower2 <- apply(param.matrix, 1, function(x) {
      sort(x)[1]
    })
    ci.upper2 <- apply(param.matrix, 1, function(x) {
      sort(x)[999]
    })

  } else {
    stop("`ci2` must be 0.002.")
  }

  is.sig <- is.sig2 <- logical(m)
  for (i in 1:m) {
    # for ci
    if (sign(ci.upper[i]) != sign(ci.lower[i])) {
      is.sig[i] <- FALSE
    }
    else {
      if (abs(bcf.coef[i]) > abs((abs(ci.upper[i]) - abs(ci.lower[i]))/2)) {
        is.sig[i] <- TRUE
      }
      else {
        is.sig[i] <- FALSE
      }
    }

    # for ci2
    if (sign(ci.upper2[i]) != sign(ci.lower2[i])) {
      is.sig2[i] <- FALSE
    }
    else {
      if (abs(bcf.coef[i]) > abs((abs(ci.upper2[i]) - abs(ci.lower2[i]))/2)) {
        is.sig2[i] <- TRUE
      }
      else {
        is.sig2[i] <- FALSE
      }
    }
  }
  out <- data.frame(coef = bcf.coef, significant = is.sig,  significant2 = is.sig2,
                    ci.lower = ci.lower, ci.upper = ci.upper)
  rownames(out) <- colnames(p)
  if (sb)
    close(pb)
  attributes(out)$npar <- attributes(p)$npar
  attributes(out)$vnames <- vnames
  out
}


# plotting functions ####

## for responses and correlations ####

my.dccplot <- function (x, sig, sig2, rescale = TRUE, main, method = c('correlation', 'response'), ...)
{
  if (!is.data.frame(x)) {
    x <- x$coef
  }

  # blues <- colorRamp(c("#FFFFFF", "#395cd4"))
  # reds <- colorRamp(c("#FFFFFF", "#dd291c"))
  blues <- colorRamp(c("#FFFFFF", "#4B9EF2", "blue4"))
  reds <- colorRamp(c("#FFFFFF", "#F25757", "red4"))

  m <- dim(x)[1]
  n <- dim(x)[2]

  if (rescale & method == "correlation")  {
    pos.max <-  0.6617472 #max(x)
    neg.max <-  0.6617472 # abs(min(x))
  }

  if (rescale & method == "response")  {
    pos.max <- max(x)
    neg.max <- abs(min(x))
  }

  op <- par(no.readonly = TRUE)
  par(oma = c(1, 5, 5, 3), mai = c(1, 0.5, 0.2, 1)) #par(oma = c(0, 3, 5, 0), mai = c(0.5, 0.8, 0.2, 2))
  plot(c(0.5, n + 0.5), c(0.5, m + 0.5), type = "n", xaxt = "n",
       yaxt = "n", ylab = "", xlab = "")
  axis(side = 3, at = 1:n, labels = colnames(x), las = 2) # change here
  axis(side = 2, at = 1:m, labels = rownames(x), las = 1)

  title(main, line = 4, outer = T)

  X.left <- X.right <- Y.bottom <- Y.top <- x

  X.left[] <- rep((1:n - 0.5), each = m)
  X.right[] <- rep((1:n + 0.5), each = m)
  Y.bottom[] <- rep(1:m - 0.5, n)
  Y.top[] <- rep(1:m + 0.5, n)


  x.left <- unlist(c(X.left))
  x.right <- unlist(c(X.right))
  y.bottom <- unlist(c(Y.bottom))
  y.top <- unlist(c(Y.top))

  xs <- unlist(c(x))
  xs.sig <- unlist(c(sig))
  xs.sig2 <- unlist(c(sig2))

  #hm <- reds(abs(xs[xs <= 0])/ neg.max)
  #hmm <- hm[is.na(hm)]
  #xs <- ifelse(xs > pos.max | xs < pos.max, pos.max, xs)

  color <- xs
  color[xs <= 0] <- rgb(reds(abs(xs[xs <= 0])/ neg.max), maxColorValue = 255)
  color[xs > 0] <- rgb(blues(xs[xs > 0]/ pos.max), maxColorValue = 255)

  rect(x.left, y.bottom , x.right, y.top, col = color, border = "white")

  points((x.left + x.right) /2 , (y.bottom + y.top) /2, bg = ifelse(xs.sig,  "white", "transparent"), col = ifelse(xs.sig, "black", "transparent"), pch = 8)
  points((x.left + x.right) /2 , (y.bottom + y.top) /2, bg = ifelse(xs.sig2,  "white", "transparent"), col = ifelse(xs.sig2, "black", "transparent"), pch = 24)
  # current vs previous year bars ####

  #par(xpd= NA)
  #  lines(x = 1:9, y = rep(nrow(x) + 3.2, 9), col = "grey", lwd = 2)
  #  lines(x = 10:17, y = rep(nrow(x) + 3.2, 8), lwd = 2)
  #  text(x = 5, y = nrow(x) + 3.2, labels = "previous year", col = "grey", pos = 3)
  #  text(x = 14, y = nrow(x) + 3.2, labels = "current year", pos = 3)


  # legend ####

  par(xpd = NA)
  leg.unit <- (m/15)
  start.unit <- leg.unit/4 + leg.unit
  right.pos <- n + n/5
  leg.width <- n/20
  values <- seq(-1, 1, length = 11)

  neg.rescaled.values <- round(seq(-neg.max, 0, length = 6),
                               2)
  pos.rescaled.values <- rev(round(seq(pos.max, 0, length = 6),
                                   2)[-6])
  rescaled.values <- c(neg.rescaled.values, pos.rescaled.values)
  for (i in 1:11) {
    if (values[i] <= 0) {
      polygon(c(right.pos, right.pos + leg.width, right.pos +
                  leg.width, right.pos), c(start.unit + ((i - 1) *
                                                           leg.unit), start.unit + ((i - 1) * leg.unit),
                                           start.unit + (i * leg.unit), start.unit + (i *
                                                                                        leg.unit)), col = rgb(reds(abs(values[i])),
                                                                                                              maxColorValue = 255), lty = 0)
      text(right.pos + leg.width , start.unit + (i *
                                                      leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i],
                                                                                     values[i]), pos = 4)
    }
    else {
      polygon(c(right.pos, right.pos + leg.width, right.pos +
                  leg.width, right.pos), c(start.unit + ((i - 1) *
                                                           leg.unit), start.unit + ((i - 1) * leg.unit),
                                           start.unit + (i * leg.unit), start.unit + (i *
                                                                                        leg.unit)), col = rgb(blues(values[i]), maxColorValue = 255),
              lty = 0)
      text(right.pos + leg.width, start.unit + (i *
                                                      leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i],
                                                                                     values[i]), pos = 4)
    }
  }

  text(x = right.pos +1, y = start.unit + (i * leg.unit) + 1, labels = method, font = 2)

  # par(op)
}




## for moving correlations ####
my.mdccplot <- function (x, sig = NULL, clim.ma = NULL, clim.sd = NULL, rescale = TRUE, main, ...)
{


  ## prepare parameters
  if (!is.data.frame(x)) {
    sig <- x$significant
    x <- x$coef

  }

  # blues <- colorRamp(c("#FFFFFF", "#395cd4"))
  # reds <- colorRamp(c("#FFFFFF", "#dd291c"))
  blues <- colorRamp(c("#FFFFFF", "#4B9EF2", "blue4"))
  reds <- colorRamp(c("#FFFFFF", "#F25757", "red4"))

  m <- dim(x)[1]
  n <- dim(x)[2]

  if (rescale) {
    pos.max <- max(x)
    neg.max <- abs(min(x))
  }

  y.axis.labs <- rep(NA, n)
  first.round.year <- which(substr(colnames(x), 4,4) %in% "0")[1]
  last.round.year <- rev(which(substr(colnames(x), 4,4) %in% "0"))[1]
  y.axis.labs[seq(first.round.year, last.round.year, 10)] <- colnames(x)[seq(first.round.year, last.round.year, 10)]

  ## plot
  if(is.null(clim.ma)) {
    op <- par(oma = c(1, 3, 5, 3), mai = c(1, 0.5, 0.2, 1)) #par(oma = c(0, 3, 5, 0), mai = c(0.5, 0.8, 0.2, 2))
  } else {
    op <- par(mfrow = c(2, 1), oma = c(6, 5, 5, 6), mai = c(1, 0.5, 0.2, 1), mar = c(0,0,0,0))
  }

  ### plot moving average and sd of climate variable ####
  if(!is.null(clim.ma)) {


    ## mov avg
    plot( clim.ma , type = "l", ylab = "", las = 1, xaxt = "n", xlab = "")
    axis(side = 1, at = c(1:length(clim.ma))[!is.na(y.axis.labs)], labels = F, tcl = 1, las = 2)
    axis(side = 1, at = 1:length(clim.ma), labels = y.axis.labs, las = 2, tcl = 0.5)
    mtext(side = 2, text = "moving average", line = 3.5)

    ##mov sd
    if(!is.null(clim.sd)) {
      par(new = T)
      plot(clim.sd, type = "l", ylab = "", yaxt = "n", col = "red", xaxt = "n")
      axis(4, col.ticks = "red", las = 1, col.axis = "red")
      mtext(4, text = "moving sd", line = 3, col = "red")
    }

  }



  ## plot "quilt" ####

  plot(c(0.5, n + 0.5), c(0.5, m + 0.5), type = "n", xaxt = "n",
       yaxt = "n", ylab = "", xlab = "")

  axis(side = 1, at = c(1:n)[!is.na(y.axis.labs)], labels = F, tcl = -1, las = 2)
  axis(side = 1, at = 1:n, labels = y.axis.labs, las = 2)

  axis(side = 2, at = 1:m, labels = rev(rownames(x)), las = 1)


  title(main, line = 1, outer = T)

  X.left <- X.right <- Y.bottom <- Y.top <- x

  X.left[] <- rep((1:n - 0.5), each = m)
  X.right[] <- rep((1:n + 0.5), each = m)
  Y.bottom[] <- rep(1:m - 0.5, n)
  Y.top[] <- rep(1:m + 0.5, n)


  x.left <- unlist(c(X.left))
  x.right <- unlist(c(X.right))
  y.bottom <- unlist(c(Y.bottom))
  y.top <- unlist(c(Y.top))

  xs <- unlist(lapply(c(x), rev))


  color <- xs
  color[xs <= 0] <- rgb(reds(abs(xs[xs <= 0])/ neg.max), maxColorValue = 255)
  color[xs > 0] <- rgb(blues(xs[xs > 0]/ pos.max), maxColorValue = 255)

  rect(x.left, y.bottom , x.right, y.top, col = color, border = "white")

  if(!is.null(sig)) {
     xs.sig <- unlist(lapply(c(sig), rev))
  points((x.left + x.right) /2 , (y.bottom + y.top) /2, bg = ifelse(xs.sig,  "white", "transparent"), col = ifelse(xs.sig, "black", "transparent"), pch = 21)
  }





  par(xpd = NA)
  leg.unit <- (m/15)
  start.unit <- leg.unit/4 + leg.unit
  right.pos <- n + n/10
  leg.width <- n/20
  values <- seq(-1, 1, length = 11)

  neg.rescaled.values <- round(seq(min(x), 0, length = 6),
                               2)
  pos.rescaled.values <- rev(round(seq(max(x), 0, length = 6),
                                   2)[-6])
  rescaled.values <- c(neg.rescaled.values, pos.rescaled.values)
  for (i in 1:11) {
    if (values[i] <= 0) {
      polygon(c(right.pos, right.pos + leg.width, right.pos +
                  leg.width, right.pos), c(start.unit + ((i - 1) *
                                                           leg.unit), start.unit + ((i - 1) * leg.unit),
                                           start.unit + (i * leg.unit), start.unit + (i *
                                                                                        leg.unit)), col = rgb(reds(abs(values[i])),
                                                                                                              maxColorValue = 255), lty = 0)
      text(right.pos + leg.width , start.unit + (i *
                                                   leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i],
                                                                                  values[i]), pos = 4)
    }
    else {
      polygon(c(right.pos, right.pos + leg.width, right.pos +
                  leg.width, right.pos), c(start.unit + ((i - 1) *
                                                           leg.unit), start.unit + ((i - 1) * leg.unit),
                                           start.unit + (i * leg.unit), start.unit + (i *
                                                                                        leg.unit)), col = rgb(blues(values[i]), maxColorValue = 255),
              lty = 0)
      text(right.pos + leg.width, start.unit + (i *
                                                  leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i],
                                                                                 values[i]), pos = 4)
    }
  }



  par(op)
}

