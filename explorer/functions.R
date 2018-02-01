# Up to now, we can generate models with 2 to 5 factors.
# Libraries ----

if ("pacman" %in% rownames(installed.packages())) {
  library(pacman)
} else {
  install.packages("pacman")
  library("pacman")
}
pacman::p_load(
  "pastecs",
  "dplyr",
  "reshape2",
  "vars",
  "psych",
  "scales",
  "car",
  "lavaan",
  "dyncomp",
  "pracma",
  "tseries"
)


# Helper Functions ----

zerovar <- function(dat) {
  out <- lapply(dat, function(x) length(unique(x)))
  want <- which(!out > 1)
  unlist(want)
}


var_to_df <- function(x) {
  if (x$K == 2) {
    coefs <- cbind(
      ML1 = x$varresult$ML1$coefficients,
      ML2 = x$varresult$ML2$coefficients
    )
    rsq <- cbind(
      ML1 = summary(x)$varresult$ML1$adj.r.squared,
      ML2 = summary(x)$varresult$ML2$adj.r.squared
    )
    rownames(rsq) <- "rsq"
    granger <- cbind(
      ML1 = causality(x, cause = "ML1")$Granger[3] < .05,
      ML2 = causality(x, cause = "ML2")$Granger[3] < .05
    )
    rownames(granger) <- "granger"
    df <- rbind(coefs[sort(rownames(coefs)), ],
                rsq,
                granger)
  }
  
  if (x$K == 3) {
    coefs <- cbind(
      ML1 = x$varresult$ML1$coefficients,
      ML2 = x$varresult$ML2$coefficients,
      ML3 = x$varresult$ML3$coefficients
    )
    rsq <- cbind(
      ML1 = summary(x)$varresult$ML1$adj.r.squared,
      ML2 = summary(x)$varresult$ML2$adj.r.squared,
      ML3 = summary(x)$varresult$ML3$adj.r.squared
    )
    rownames(rsq) <- "rsq"
    granger <- cbind(
      ML1 = causality(x, cause = "ML1")$Granger[3] < .05,
      ML2 = causality(x, cause = "ML2")$Granger[3] < .05,
      ML3 = causality(x, cause = "ML3")$Granger[3] < .05
    )
    rownames(granger) <- "granger"
    df <- rbind(coefs[sort(rownames(coefs)), ],
                rsq,
                granger)
  }
  
  if (x$K == 4) {
    coefs <- cbind(
      ML1 = x$varresult$ML1$coefficients,
      ML2 = x$varresult$ML2$coefficients,
      ML3 = x$varresult$ML3$coefficients,
      ML4 = x$varresult$ML4$coefficients
    )
    rsq <- cbind(
      ML1 = summary(x)$varresult$ML1$adj.r.squared,
      ML2 = summary(x)$varresult$ML2$adj.r.squared,
      ML3 = summary(x)$varresult$ML3$adj.r.squared,
      ML4 = summary(x)$varresult$ML4$adj.r.squared
    )
    rownames(rsq) <- "rsq"
    granger <- cbind(
      ML1 = causality(x, cause = "ML1")$Granger[3] < .05,
      ML2 = causality(x, cause = "ML2")$Granger[3] < .05,
      ML3 = causality(x, cause = "ML3")$Granger[3] < .05,
      ML4 = causality(x, cause = "ML3")$Granger[3] < .05
    )
    rownames(granger) <- "granger"
    df <- rbind(coefs[sort(rownames(coefs)), ],
                rsq,
                granger)
  }
  
  if (x$K == 5) {
    coefs <- cbind(
      ML1 = x$varresult$ML1$coefficients,
      ML2 = x$varresult$ML2$coefficients,
      ML3 = x$varresult$ML3$coefficients,
      ML4 = x$varresult$ML4$coefficients,
      ML5 = x$varresult$ML5$coefficients
    )
    rsq <- cbind(
      ML1 = summary(x)$varresult$ML1$adj.r.squared,
      ML2 = summary(x)$varresult$ML2$adj.r.squared,
      ML3 = summary(x)$varresult$ML3$adj.r.squared,
      ML4 = summary(x)$varresult$ML4$adj.r.squared,
      ML5 = summary(x)$varresult$ML5$adj.r.squared
    )
    rownames(rsq) <- "rsq"
    granger <- cbind(
      ML1 = causality(x, cause = "ML1")$Granger[3] < .05,
      ML2 = causality(x, cause = "ML2")$Granger[3] < .05,
      ML3 = causality(x, cause = "ML3")$Granger[3] < .05,
      ML4 = causality(x, cause = "ML4")$Granger[3] < .05,
      ML5 = causality(x, cause = "ML5")$Granger[3] < .05
    )
    rownames(granger) <- "granger"
    df <- rbind(coefs[sort(rownames(coefs)), ],
                rsq,
                granger)
  }
  
  return(df)
}

# Cubic spline interpolation due to irregular intervals
# Also converts dfs with values saved as strings

cubspline <- function(x, detrend = T, spline = T) {
  dat <-
    as.data.frame(matrix(
      data = as.numeric(as.matrix(x[, 1:(ncol(x) - 1)])) ,
      nrow = nrow(x),
      ncol = (ncol(x) - 1)
    ))
  colnames(dat) <- colnames(x[, 1:(ncol(x) - 1)])
  
  if (detrend) {
    for (i in 1:ncol(dat)) {
      if (kpss.test(dat[, i])$p.value < .05) {
        if (var(dat[, i]) > 0) {
          message(paste0("Detrending ", colnames(x)[i], "…"))
        }
        dat[, i] <- detrend(dat[, i], tt = "linear")
      }
    }
  }
  
  res <- as.data.frame(matrix(nrow = nrow(x), ncol = ncol(x)))
  cumsumT <- c(1, cumsum(as.numeric(diff(x$date))))
  
  if (spline) {
    for (i in 1:ncol(dat)) {
      res[, i] <- spline(x = cumsumT,
                         y = dat[, i],
                         nrow(x),
                         method = 'fmm')$y
    }
  }
  res[, ncol(x)] <- x$date
  colnames(res) <- colnames(x)
  if (spline) {
    return(res)
  } else {
    return(cbind(dat, date = x$date))
  }
}


# Models ----

generate.DATA.model <- function(x, nfact, show.info = F) {
  if (nfact == 1 |
      nfact > 5) {
    return("Only models with 2 to 5 factors are supported.")
  }
  x <- as.data.frame(na.exclude(x))
  #x <- cubspline(x)
  #x <- dplyr::select(x, -date, -time)
  if (!is.numeric(x)) {
    x <- data.frame(as.matrix(sapply(x, as.numeric)))
  }
  # Step 1: exploratory FA
  expfa <- fa(x, fm = "ml", nfactors = nfact)
  if (show.info) {
    print(expfa)
  }
  #if(expfa$TLI < .95 | expfa$crms > .09){return("No exploratory model with adequate fit was found.")}
  
  # Step 2: confirmatory FA
  exp <- expfa$loadings
  exp[which(expfa$communalities < .30), ] <-
    0# Drop items with low communalities
  exp[exp[, ] < .3 &
        exp[, ] >= -.29] <- 0 # Drop items with low loadings
  cfa_matrix <- car::recode(exp, '0=0;else=1')
  if (show.info) {
    print(cfa_matrix)
  }
  terms <- vector()
  cfacov <- vector()
  #cfanames <-
  
  for (i in 1:nfact) {
    terms[i] <-
      paste0("ML", i, " =~", paste(names(which(
        cfa_matrix[, sort(colnames(cfa_matrix))][, i] == 1
      )), sep = "'", collapse = " + "))
  }
  print(terms)
  
  MLS <- vector()
  for (i in 1:nfact) {
    MLS[i] <- paste0("ML", i, collapse = ",")
  }
  if (nfact == 2) {
    cfacov <- "ML1~~ML2"
  } else {
    for (i in 1:nfact) {
      cfacov[i] <- paste0(combn(MLS, 2)[, i], collapse = "~~")
    }
  }
  
  terms <- paste(c(terms, cfacov), collapse = "\n")
  if (show.info) {
    print(terms)
  } # debug
  cfa <-
    cfa(
      model = terms,
      data = x,
      missing = "listwise",
      std.ov = TRUE,
      std.lv = TRUE,
      estimator = "ML",
      control = list(iter.max = 50000)
    )
  #semPaths(cfa, "std", edge.label.cex = .5, title = F, curvePivot = F)
  if (lavInspect(cfa, "converged") == FALSE) {
    return("Confirmatory model did not converge.")
  }
  if (fitmeasures(cfa)[10] < .80  |
      fitmeasures(cfa)[29] > .09) {
    # Hu&Bentler(1999): In general (especially, under the nonrobustness condition), combinational rules of TLI < .95 and SRMR > .09 (or .10) are preferable when N< 500
    return(
      paste0(
        "Factor model could not be confirmed. TLI: ",
        round(fitmeasures(cfa)[10], 3),
        ", needs to be above .95. SRMR: ",
        round(fitmeasures(cfa)[29], 3),
        ", needs to be below .09"
      )
    )
  }
  
  # Step 3: VAR model
  cfa_loadings <- inspect(cfa, what = "std")$lambda
  y <-
    dplyr::select(x, one_of(c(names(x[, rowSums(cfa_matrix[, ]) > 0])))) # This removes items with no factor loadings in CFA
  write.table(y, "debug.txt")
  if (show.info) {
    print(y)
  }
  cfa_scores <-
    factor.scores(x = as.matrix(y[, rownames(cfa_loadings)]),
                  f = cfa_loadings,
                  method = "components")
  latent_variances <-
    colSums(inspect(cfa, what = "std")$lambda ^ 2) / 3 # Compute variance for all three factors. When using standardized loadings, the variance in symptom variation can be obtained via the sum of squared factor loadings for each factor, divided by the number of factors.
  lagselect <-
    VARselect(na.exclude(cfa_scores$scores),
              lag.max = round(sqrt(nrow(x))),
              type = "both")$selection[1]
  varmodel <-
    VAR(
      na.exclude(cfa_scores$scores),
      p = 2,
      type = "none",
      ic = "AIC"
    )
  vartable <- var_to_df(varmodel)
  if (show.info) {
    print(vartable)
  }
  
  # Step 4: DATA Factor Scores
  score <- vector()
  
  for (i in 1:nfact) {
    var <- sum(((cfa_loadings[, i] / nfact) ^ 2))
    reg <-
      sum((vartable[grep(paste0("ML", i), rownames(vartable)), ]) ^ 2) / nfact
    score[i] <- var * reg
  }
  factor.scores <- score <- score / max(score)
  if (show.info) {
    print(score)
  }
  # Step 5: DATA Item Scores
  if (show.info) {
    print(sort(colMeans(y[, rownames(expfa)], na.rm = TRUE)))
  }
  
  itemmeans <- sort(colMeans(y, na.rm = TRUE)) # [,rownames(expfa)]
  itemmeans.norm <- itemmeans / max(itemmeans)
  itemmeans.norm <- itemmeans.norm[order(names(itemmeans.norm))]
  loadings <- cfa_loadings[order(rownames(cfa_loadings)), ]
  #print(loadings)
  itemscores <-
    itemmeans.norm * rowSums(abs(loadings) * factor.scores)
  
  if (show.info) {
    print(itemscores)
  }
  stditemscores <-
    scales::rescale(sort(sort(itemscores * 10) / sort(itemscores * 10)[length(itemscores)], decreasing = T), to = c(0, 100))
  return(as.data.frame(cbind(score = stditemscores)))
}


df.complexity <- function(x, scaleMin, scaleMax, width) {
  x <- x %>% purrr::keep(is.numeric)
  df.comp <- matrix(data = NA,
                    nrow = nrow(x),
                    ncol = ncol(x))
  df.comp <- as.data.frame(df.comp)
  colnames(df.comp) <- colnames(x)
  for (i in 1:ncol(x)) {
    df.comp[, i] <- complexity(x[, i], scaleMin, scaleMax, width)
  }
  df.comp$time <- c(1:nrow(df.comp))
  return(df.comp)
}
