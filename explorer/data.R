# Up to now, we can generate models with 2 to 5 factors.
# Libraries ----
require(shiny)
require(pastecs)
require(dplyr)
require(reshape2)
require(vars)
require(psych)
require(scales)
require(car)
require(lavaan)



# Models ----

generate.DATA.model <-
  function(x, nfact) {
    if (nfact == 1 |
        nfact > 5) {
      return("Only models with 2 to 5 factors are supported.")
    }
    
    # Helper Functions ----
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
          ML2 = causality(x, cause =
                            "ML2")$Granger[3] < .05
        )
        rownames(granger) <-
          "granger"
        df <-
          rbind(coefs[sort(rownames(coefs)), ],
                rsq,
                granger)
      }
      
      if (x$K == 3) {
        coefs <- cbind(
          ML1 = x$varresult$ML1$coefficients,
          ML2 = x$varresult$ML2$coefficients,
          ML3 = x$varresult$ML3$coefficients
        )
        rsq <-
          cbind(
            ML1 = summary(x)$varresult$ML1$adj.r.squared,
            ML2 = summary(x)$varresult$ML2$adj.r.squared,
            ML3 = summary(x)$varresult$ML3$adj.r.squared
          )
        rownames(rsq) <-
          "rsq"
        granger <-
          cbind(
            ML1 = causality(x, cause = "ML1")$Granger[3] < .05,
            ML2 = causality(x, cause =
                              "ML2")$Granger[3] < .05,
            ML3 = causality(x, cause =
                              "ML3")$Granger[3] < .05
          )
        rownames(granger) <-
          "granger"
        df <-
          rbind(coefs[sort(rownames(coefs)), ],
                rsq,
                granger)
      }
      
      if (x$K ==
          4) {
        coefs <- cbind(
          ML1 = x$varresult$ML1$coefficients,
          ML2 = x$varresult$ML2$coefficients,
          ML3 = x$varresult$ML3$coefficients,
          ML4 = x$varresult$ML4$coefficients
        )
        rsq <-
          cbind(
            ML1 = summary(x)$varresult$ML1$adj.r.squared,
            ML2 = summary(x)$varresult$ML2$adj.r.squared,
            ML3 = summary(x)$varresult$ML3$adj.r.squared,
            ML4 = summary(x)$varresult$ML4$adj.r.squared
          )
        rownames(rsq) <-
          "rsq"
        granger <-
          cbind(
            ML1 = causality(x, cause = "ML1")$Granger[3] < .05,
            ML2 = causality(x, cause =
                              "ML2")$Granger[3] < .05,
            ML3 = causality(x, cause =
                              "ML3")$Granger[3] < .05,
            ML4 = causality(x, cause =
                              "ML3")$Granger[3] < .05
          )
        rownames(granger) <-
          "granger"
        df <-
          rbind(coefs[sort(rownames(coefs)), ],
                rsq,
                granger)
      }
      
      if (x$K ==
          5) {
        coefs <- cbind(
          ML1 = x$varresult$ML1$coefficients,
          ML2 = x$varresult$ML2$coefficients,
          ML3 = x$varresult$ML3$coefficients,
          ML4 = x$varresult$ML4$coefficients,
          ML5 = x$varresult$ML5$coefficients
        )
        rsq <-
          cbind(
            ML1 = summary(x)$varresult$ML1$adj.r.squared,
            ML2 = summary(x)$varresult$ML2$adj.r.squared,
            ML3 = summary(x)$varresult$ML3$adj.r.squared,
            ML4 = summary(x)$varresult$ML4$adj.r.squared,
            ML5 = summary(x)$varresult$ML5$adj.r.squared
          )
        rownames(rsq) <-
          "rsq"
        granger <-
          cbind(
            ML1 = causality(x, cause = "ML1")$Granger[3] < .05,
            ML2 = causality(x, cause =
                              "ML2")$Granger[3] < .05,
            ML3 = causality(x, cause =
                              "ML3")$Granger[3] < .05,
            ML4 = causality(x, cause =
                              "ML4")$Granger[3] < .05,
            ML5 = causality(x, cause =
                              "ML5")$Granger[3] < .05
          )
        rownames(granger) <-
          "granger"
        df <-
          rbind(coefs[sort(rownames(coefs)), ],
                rsq,
                granger)
      }
      
      return(df)
    }
    
    x <-
      as.data.frame(na.exclude(x))
    # Step 1: exploratory FA
    exp <-
      fa(x,
         rotate = "oblimin",
         fm = "ml",
         nfactors = nfact)
    if (exp$TLI < .95 |
        exp$crms > .09) {
      return("No exploratory model with adequate fit was found.")
    }
    
    # Step 2: confirmatory FA
    exp$loadings[exp$loadings[, ] < .3 &
                   exp$loadings[, ] >= -.29] <- 0
    cfa_matrix <-
      recode(exp$loadings, '0=0;else=1')
    terms <-
      vector()
    for (i in 1:nfact) {
      terms[i] <-
        paste0("ML", i, " =~", paste(names(which(
          cfa_matrix[, sort(colnames(cfa_matrix))][, i] == 1
        )), sep = "'", collapse = " + "))
    }
    terms <-
      paste(terms, collapse = "\n")
    cfa <-
      cfa(
        model = terms,
        data = x,
        missing = "listwise",
        estimator = "MLR",
        std.ov = TRUE,
        std.lv = TRUE
      )
    if (lavInspect(cfa, "converged") == FALSE) {
      return("Confirmatory model did not converge.")
    }
    fm.cfa <- fitmeasures(cfa, c("tli","srmr"))
    if (fm.cfa[1] < .95 |
        fm.cfa[2] > .09) {
      # Hu&Bentler(1999): In general (especially, under the nonrobustness condition), combinational rules of TLI < .95 and SRMR > .09 (or .10) are preferable when N<500
      return(
        paste0(
          "Insufficient confirmatory model fit indices. TLI: ",
          fm.cfa[1],
          ", needs to be above .95. SRMR: ",
          fm.cfa[2],
          ", needs to be below .09"
        )
      )
    }
    
    # Step 3: VAR model
    cfa_loadings <-
      inspect(cfa, what = "std")$lambda
    y <-
      dplyr::select(x, one_of(c(names(x[, rowSums(cfa_matrix[, ]) > 0])))) # This removes items with no factor loadings in CFA
    cfa_scores <-
      factor.scores(x = as.matrix(y[, rownames(cfa_loadings)]),
                    f = cfa_loadings,
                    method = "components")
    latent_variances <-
      colSums(inspect(cfa, what = "std")$lambda ^ 2) / 3 # Compute variance for all three factors. When using standardized loadings, the variance in symptom variation can be obtained via the sum of squared factor loadings for each factor, divided by the number of factors.
    lagselect <-
      VARselect(na.exclude(cfa_scores$scores),
                lag.max = 2,
                type = "both")$selection[1]
    varmodel <-
      VAR(
        na.exclude(cfa_scores$scores),
        p = lagselect,
        type = "both",
        ic = "AIC"
      )
    vartable <-
      var_to_df(varmodel)
    
    # Step 4: DATA Factor Scores
    score <-
      vector()
    
    for (i in 1:nfact) {
      var <- sum(((cfa_loadings[, i] / nfact) ^ 2))
      reg <-
        sum((vartable[grep(paste0("ML", i), rownames(vartable)), ]) ^ 2) / nfact
      score[i] <-
        var * reg
    }
    factor.scores <-
      score <- score / max(score)
    
    # Step 5: DATA Item Scores
    itemmeans <-
      sort(colMeans(y, na.rm = TRUE))
    itemmeans.norm <-
      itemmeans / max(itemmeans)
    itemmeans.norm <-
      itemmeans.norm[order(names(itemmeans.norm))]
    loadings <-
      cfa_loadings[order(rownames(cfa_loadings)), ]
    itemscores <-
      itemmeans.norm * rowSums(abs(loadings) * factor.scores)
    
    print(itemscores)
    stditemscores <-
      sort(itemscores * 10) / sort(itemscores * 10)[length(itemscores)]
    
    return(stditemscores)
    
  }
