spliceCorrection_copy = function (X, wav, splice = c(1000, 1830), interpol.bands = 10) 
{
  if (is.data.frame(X)) 
    X <- as.matrix(X)
  was.vec <- is.vector(X)
  if (is.vector(X)) {
    nms <- names(X)
    X <- matrix(X, ncol = length(X))
  }
  if (missing(wav)) 
    wav <- seq_len(ncol(X))
  if (length(wav) != ncol(X)) 
    stop("length(wav) should be equal to ncol(X)")
  index <- which(wav %in% splice)
  if (!length(index)) 
    stop("splice positions not found in wav")
  X1 <- X[, 1:index[1], drop = F]
  X2 <- X[, (index[1] + 1):index[2], drop = F]
  X3 <- X[, (index[2] + 1):ncol(X), drop = F]
  tmp1 <- X2[, 1:interpol.bands, drop = F]
  tmp2 <- X2[, (ncol(X2) - interpol.bands + 1):ncol(X2), drop = F]
  w1 <- wav[(index[1] + 1):(index[1] + interpol.bands)]
  w2 <- wav[(index[2] - interpol.bands + 1):index[2]]
  extrapfun <- function(x, y, xout) {
    fit <- lm(y ~ x)
    fit$coefficients[1] + fit$coefficients[2] * xout
  }
  pred.X1 <- apply(tmp1, 1, function(y) extrapfun(x = w1, y = y, 
                                                  xout = splice[1]))
  pred.X2 <- apply(tmp2, 1, function(y) extrapfun(x = w2, y = y, 
                                                  xout = splice[2]))
  offset1 <- X1[, ncol(X1)] - pred.X1
  offset2 <- X3[, 1] - pred.X2
  output <- cbind(sweep(X1, 1, offset1, "-"), X2, sweep(X3, 
                                                        1, offset2, "-"))
  if (was.vec) {
    output <- as.vector(output)
    names(output) <- nms
  }
  else {
    dimnames(output) <- list(rownames(X), colnames(X))
  }
  return(output)
}

filter_spectra_copy = function (spectra, type, n = 11, p = 2, m = 0, res = NULL, specType = NULL) 
{
  if (type == "S-Golay") {
    spectra <- as.matrix(spectra)
    sg <- aaply(spectra, 1, sgolayfilt, n = n, p = p, m = m)
    if (nrow(spectra) == 1) {
      sg <- matrix(sg, dim(spectra))
    }
    sg <- as.data.frame(sg)
    colnames(sg) <- colnames(spectra)
    return(sg)
  }
  if (type == "MSC") {
    meanSpec <- as.matrix(colMeans(spectra))
    mscMat <- matrix(NA, ncol = ncol(spectra), nrow = nrow(spectra))
    spectra <- as.matrix(spectra)
    for (i in 1:nrow(spectra)) {
      specLM <- lm(spectra[i, ] ~ meanSpec)
      specCE <- t(as.matrix(specLM$coefficients))
      mscMat[i, ] <- t(as.matrix((spectra[i, ] - specCE[1, 
                                                        1])/specCE[1, 2]))
    }
    mscMat <- as.data.frame(mscMat)
    colnames(mscMat) <- colnames(spectra)
    return(mscMat)
  }
  if (type == "SNV") {
    spectra <- as.matrix(spectra)
    snvMat <- matrix(NA, ncol = ncol(spectra), nrow = nrow(spectra))
    for (i in 1:nrow(spectra)) {
      snvMat[i, ] <- (spectra[i, ] - mean(spectra[i, ]))/sd(spectra[i, 
                                                                    ])
    }
    snvMat <- as.data.frame(snvMat)
    colnames(snvMat) <- colnames(spectra)
    return(snvMat)
  }
  if (type == "Wavelet") {
    nm2 <- 2^c(1:100)
    vs <- ncol(spectra)
    if (sum(nm2 == vs) != 1) {
      stop("Error: Number of columns in spectra table needs to equal 2^x")
    }
    else {
      wave_spectra <- matrix(NA, ncol = 2^res, nrow = nrow(spectra))
      for (i in 1:nrow(spectra)) {
        wds <- wd(as.matrix(spectra[i, ]), bc = "symmetric", 
                  filter.number = 10, family = "DaubExPhase", 
                  min.scale = 2)
        wave_spectra[i, ] <- accessC.wd(wds, level = res)
      }
      wave_spectra <- as.data.frame(wave_spectra)
      colnames(wave_spectra) <- seq((as.numeric(names(spectra)[1]) + 
                                       0.5 * (ncol(spectra)/(2^res))), as.numeric(names(spectra)[length(spectra)]), 
                                    by = ncol(spectra)/(2^res))
    }
    return(wave_spectra)
  }
  if (type == "C-hull") {
    c_hull.fix <- function(c_hull) {
      cc <- c_hull
      xs <- which(cc == 1)
      ccd <- sort(cc[which(cc == 1):length(cc)])
      if (cc[1] < cc[length(cc)]) {
        ccd <- ccd
      }
      else {
        for (f in 1:(xs - 1)) {
          if (cc[f] < cc[f + 1]) {
            ccd <- c(ccd, cc[f])
          }
          else {
            ccd <- c(ccd, cc[f])
            break
          }
        }
      }
      return(ccd)
    }
    interval <- c(1:ncol(spectra))
    hull_spectra <- matrix(NA, ncol = ncol(spectra), nrow = nrow(spectra))
    for (i in 1:nrow(spectra)) {
      tempSpect = as.matrix(spectra[i, ])
      data1 <- sortedXyData(interval, tempSpect)
      c_hull <- chull(data1)
      cc <- c_hull
      xs <- which(cc == 1)
      ccd <- sort(cc[which(cc == 1):length(cc)])
      if (cc[1] < cc[length(cc)]) {
        ccd <- ccd
      }
      else {
        for (f in 1:(xs - 1)) {
          if (cc[f] < cc[f + 1]) {
            ccd <- c(ccd, cc[f])
          }
          else {
            ccd <- c(ccd, cc[f])
            break
          }
        }
      }
      c_hull <- ccd
      linear_approx <- approx(data1[c_hull, ], xout = interval, 
                              method = "linear", ties = "mean")
      if (specType == 1) {
        hull_spectra[i, ] <- 1 - ((linear_approx[[2]] - 
                                     tempSpect)/linear_approx[[2]])
      }
      if (specType == 0) {
        hull_spectra[i, ] <- ((linear_approx[[2]] - tempSpect)/linear_approx[[2]])
      }
    }
    hull_spectra <- as.data.frame(hull_spectra)
    colnames(hull_spectra) <- colnames(spectra)
    return(hull_spectra)
  }
}


strip_spectra_copy = function (spectra, datawavs, wavlimits = range(datawavs), which = 1) 
{
  if (length(wavlimits) != 2) 
    stop("wavlimits should be of length 2")
  datawavs <- as.numeric(datawavs)
  limits <- which(datawavs %in% wavlimits)
  if (length(limits) != 2) 
    stop("There should be 2 limits. Are both elements of wavlimits present in datawavs?")
  kept_index <- seq(limits[1], limits[2], which)
  trimmed_spectra <- spectra[, kept_index]
  kept_names <- datawavs[kept_index]
  colnames(trimmed_spectra) <- kept_names
  attr(trimmed_spectra, "kept_names") <- kept_names
  attr(trimmed_spectra, "discarded_names") <- datawavs[!seq_along(datawavs) %in% 
                                                         kept_index]
  trimmed_spectra
}
                   
                   
goof2 = function (Observed, Predicted, 
                  coefficient = c("R2", "concordance","MSE", "RMSE", 
                                  "bias", "MSEc", "RMSEc", "RPD", 
                                  "RPIQ"),
                  plot = TRUE, ...) 
{
  if (any(!coefficient %in% c("R2", "concordance", "MSE", "RMSE", 
                              "bias", "MSEc", "RMSEc", "RPD", "RPIQ"))) 
    stop("Please choose a valid coefficient")
  rLM <- lm(Predicted ~ Observed)
  R2 <- as.matrix(summary(rLM)$adj.r.squared)
  SEP2 <- mean((Observed - Predicted)^2)
  SEP <- sqrt(SEP2)
  bias <- mean(Predicted) - mean(Observed)
  SEP2c <- sum(((Predicted - bias - Observed)^2)/length(Observed))
  SEPc <- sqrt(SEP2c)
  RPD <- sd(Observed)/SEP
  IQ <- c(quantile(Observed))[3] - c(quantile(Observed))[2]
  RPIQ <- IQ/SEP
  mx <- mean(Observed)
  my <- mean(Predicted)
  s2x <- var(Observed)
  s2y <- var(Predicted)
  sxy <- mean((Observed - mx) * (Predicted - my))
  ccc <- 2 * sxy/(s2x + s2y + (mx - my)^2)
  if (plot) {
    op = par("pty")
    on.exit(par(pty=op))
    par(pty="s")
    lims= c(min(c(Observed, Predicted)),max(c(Observed, Predicted)))
    plot(Observed, Predicted, ylim = lims, xlim = lims, asp = 1,...)
    abline(a = 0, b = 1, col = "grey")
  }
  coefs_tmp <- data.frame(R2 = R2, concordance = ccc, MSE = SEP2, 
                          RMSE = SEP, bias = bias, MSEc = SEP2c, RMSEc = SEPc, 
                          RPD = RPD, RPIQ = RPIQ, row.names = NULL)
  gf <- data.frame(coefs_tmp[, coefficient])
  return(gf)
}


