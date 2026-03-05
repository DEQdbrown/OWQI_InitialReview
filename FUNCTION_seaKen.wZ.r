seaKen.wZ <- function (x) 
{
  if (!is(x, "ts")) 
    stop("x must be a 'ts'")
  fr <- frequency(x)
  S <- 0
  varS <- 0
  miss <- NULL
  slopes <- NULL
  for (m in 1:fr) {
    xm <- x[cycle(x) == m]
    tm <- time(x)[cycle(x) == m]
    ken <- mannKen(ts(xm, start = start(x)[1], frequency = 1))
    S <- S + ken$S
    varS <- varS + ken$varS
    miss <- c(miss, ken$miss)
    outr <- outer(xm, xm, "-")/outer(tm, tm, "-")
    slopes.m <- outr[lower.tri(outr)]
    slopes <- c(slopes, slopes.m)
  }
  sen.slope <- median(slopes, na.rm = TRUE)
  sen.slope.pct <- 100 * sen.slope/abs(mean(x, na.rm = TRUE))
  Z <- (S - sign(S))/sqrt(varS)
  p.value <- 2 * pnorm(-abs(Z))
  names(miss) <- as.character(1:m)
  list(Z = Z, sen.slope = sen.slope, sen.slope.pct = sen.slope.pct, 
       p.value = p.value, miss = round(miss, 3)) 
} ## peter added the reporting of z statistic - not sure we need this 