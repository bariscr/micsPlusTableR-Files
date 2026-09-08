##' FIES 4 weeks
##'
##' This function a) equates the 4 weeks reference scale against the 12 months
##' reference scale,  b) run the Rasch model or the partial credit model over 
##' the FIES dataset while removing those items with high infits, c) equates 
##' the derived scale against the 4 weeks reference scale and d) produce the
##' prevalence rates.  
##'
##' @param fies.global.st The item severity parameter estimates for the FIES
##' global reference standard. If left unspecified, the 2014-2016 FAO global 
##' standard for the Food Insecurity Experience Scale (FIES) is set as default.
##' \code{fies.global.st}.
##' @param FIES_ref The item severity parameter estimates for the FIES
##' 4 weeks reference standard. If left unspecified, the FAO 4 weeks reference 
##' standard for the Food Insecurity Experience Scale (FIES) is set as default. 
##' \code{FIES_ref}.
##' @param Threshold_ref Thresholds (along the latent trait) set to compute 
##' comparable prevalence rates of the population in IPC phases 2+, 3+, 4+ and 5. 
##' Default values have been determined through an iterative process of calibration
##' of Household Hunger Score HHS-based classifications on the local scale. 
##' \code{Threshold_ref}.
##' @param global.4w.common The set of common items between the global reference
##' standard scale and the 4 weeks reference scale. When equals to NULL, the user
##' is required to enter the value once the function has been called. 
##' Default equals to NULL \code{global.4w.common}.
##' @param fies.df A data frame containing the 8 FIES variables named and ordered 
##' according to the following labels: "WORRIED","HEALTHY","FEWFOOD","SKIPPED",
##' "ATELESS","RUNOUT","HUNGRY","WHLDAY". The variables should be coded 0 for NO,
##' 1 for YES and NA for DON'T KNOW or REFUSE. In case frequency follow up questions are
##' asked to the items "RUNOUT","HUNGRY","WHLDAY", these can be assigned with the numeric
##' values of the answer, i.e. 0 for NO, 1 for Rarely, 2 for Sometimes, 3 for Often 
##' \code{fies.df}.
##' @param UID Identifier marking that particular record as unique from every other record.
##' Useful in case the resulting individual probabilities want to be used for further 
##' analysis. Default equal to NULL and not mandatory \code{UID}. 
##' @param wt The sampling household weights (numeric vector) \code{wt}.
##' @param wt.person The sampling individual weights (numeric vector) \code{wt.person}.
##' @param model Should specify the model to be used: "dichotomous" or "polytomous".\code{model}.
##' @param extr.par Logical. Specify whether to activate the assumption on the
##' extreme raw score parameters, computed based on the raw score distribution, in
##' the dichotomous model. Default equals to FALSE \code{extr.par}.
##' @param tol The minimum tolerance among common items to be reached in the 
##' equating between the scale derived by the application of the model and the
##' 4 weeks reference scale. Default is set to 0.97 \code{tol}.
##' @param disaggregation.df The data frame with the variables by which 
##' results need to be disaggregated \code{disaggregation}.
##' @param surveyName The name of the survey \code{surveyName}.
##' @param FIEScatplot Logical. Specify whether to print the FIES 
##' categories plot. Default equals to FALSE \code{FIEScatplot}.
##' @param maxit.RM.w is the maximum iterations allowed for the estimation method of dichotomous
##' model. Default equals to 1000 \code{maxit.RM.w}.
##' @param maxit.PC.w is the maximum iterations allowed for the estimation method of polytomous
##' model. Default equals to 1000 \code{maxit.PC.w}.
##' @param minconv.PC.w is the tolerance for the estimation method of polytomous
##' model. Default equals to 1e-12 \code{minconv.PC.w}.
##' @param infit.thresh is the upper threshold for infit statistics 
##' to be considered acceptable. Default equals to 1.3 \code{infit.thresh}.
##' @param max.cor.meth Logical. Is the equating performed automatically using
##' maximum correlation method? Default equals to TRUE \code{max.cor.meth}.
##' @param uniqueitems Name of unique items to be specified at hand (to be used only if
##'  \code{max.cor.meth=F}) Default equals to NULL \code{uniqueitems}.
##' @param aggr.cat.PC.w Numeric (0, 1, 2 or 3). Category aggregation for PC.w.
##' \code{aggr.cat.PC.w=0} does not imply any category aggregation. 
##' \code{aggr.cat.PC.w=1} aggregates the two more severe categories (for example, Sometimes and Always). 
##' \code{aggr.cat.PC.w=2} aggregates the two middle categories (for example, Rarely and Sometimes). 
##' \code{aggr.cat.PC.w=3} aggregates the two less severe categories (for example, Never and Rarely). 
##' Default equals to 0 \code{aggr.cat.PC.w}.
##' 
##'
##' @return 
##'
##' @export
##'
##' @examples
##' fies4w(global.4w.common = c(1,2,3,6:8),
##'        fies.df = data[, c("WORRIED","HEALTHY","FEWFOOD","SKIPPED","ATELESS","RUNOUT","HUNGRY","WHLDAY")],
##'        wt = data$wt, 
##'        model = "dichotomous",
##'        disaggregation = data[,"Province", drop = FALSE])

fies4w <- function(fies.global.st = c(-1.2230564, -0.847121, -1.1056616, 0.3509848, -0.3117999, 0.5065051, 0.7546138, 1.8755353),
                   FIES_ref = c(-0.9960251, -0.6119327, -0.8627930, -0.3925000, -0.7846981, 0.3537512, 0.6423115, 1.9105613),
                   Threshold_ref = c(NA, -0.3827969, 0.6217340, 3.1296938, 3.9817094),
                   global.4w.common = NULL,
                   fies.df = NULL,
                   UID = NULL,
                   wt = NULL,
                   wt.person = NULL,
                   model = c("dichotomous", "polytomous"),
                   extr.par = FALSE,
                   tol = 0.97, 
                   disaggregation.df = NULL,
                   surveyName = "Country",
                   FIEScatplot = FALSE,
                   maxit.RM.w=1000,
                   maxit.PC.w=1000,
                   minconv.PC.w=.000000000001,
                   infit.thresh=1.3,
                   max.cor.meth=T,
                   uniqueitems=NULL,
                   aggr.cat.PC.w=0
                   ) {
  
  
  ####################################
  ## Run some checks on the input data
  ####################################
  ## Check on the fies.global.st
  if (length(fies.global.st) != 8 | !is.numeric(fies.global.st)) stop("The fies.global.st argument is not correct!")
  if (missing(fies.global.st)) names(fies.global.st) <- c("WORRIED","HEALTHY","FEWFOOD","SKIPPED","ATELESS","RUNOUT","HUNGRY","WHLDAY")
  ## Check on the FIES_ref
  if (length(FIES_ref) != 8 | !is.numeric(FIES_ref)) stop("The FIES_ref argument is not correct!")
  ## Check on the Threshold_ref
  if (length(Threshold_ref) != 5 | !is.numeric(Threshold_ref)) stop("The Threshold_ref argument is not correct!")
  ## Check on the global.4w.common
  if (!is.null(global.4w.common)) {
    if (!is.numeric(global.4w.common) | !all(global.4w.common %in% c(1:8))) stop("The global.4w.common argument is not correct!")
  }
  ## Check on the fies.df
  if (ncol(fies.df) != 8 | 
      !all(colnames(fies.df) == c("WORRIED","HEALTHY","FEWFOOD","SKIPPED",
                                  "ATELESS","RUNOUT","HUNGRY","WHLDAY")) |
      !all(unique(unlist(fies.df)) %in% c(NA, 0:3)) |
      !all(unique(unlist(fies.df[,1:5])) %in% c(NA, 0, 1))) stop("The fies.df argument is not correct!")
  ## Check on UID
  if (!is.null(UID) & any(duplicated(UID))) stop("Duplicated values found in UID. Please check!")
  ## Check on the wt
  if (is.null(wt)) {
    wt <- rep(1, nrow(fies.df))
  } else {
    if (any(is.na(wt))) stop("Some household weigths are missing. Please check!")
    if (!is.numeric(wt)) stop("The provided vector of household weights 'wt' is not numeric. Please check!")
  }
  ## Check on the wt.person
  if (!is.null(wt.person)) {
    if (any(is.na(wt.person))) stop("Some individual weigths are missing. Please check!")
    if (!is.numeric(wt.person)) stop("The provided vector of individual weights 'wt.person' is not numeric. Please check!")
  }
  ## Check model
  model <- match.arg(model)
  ## Check extr.par
  if (!is.logical(extr.par)) stop("extr.par argument should be logical.")
  ## Check tol
  if (!is.numeric(tol) | tol < 0 | tol > 1) stop("tol argument should be numeric between 0 and 1.")
  if (tol < 0.8) warning("Setting a tolerance level below 0.8 doesn't really make sense.")
  ## Chek disaggregation
  if (!is.null(disaggregation.df)) {
    if (!is.data.frame(disaggregation.df)) {
      stop("disaggregation.df is not a data.frame. In case there is need to disaggregate by one single variable please specify drop = FALSE like disaggregation.df[, c('DisaggVar'), drop = FALSE]")
    } else {
      if (any(is.na(disaggregation.df))) {
        disaggregation.df[is.na(disaggregation.df)] <- "Missing"
      }
    }
  }
  ## Check surveyName
  if (!is.character(surveyName)) stop("The surveyName argument should be a character of length one.")
  ## Check plot
  if (!is.logical(FIEScatplot)) stop("FIEScatplot argument should be logical.")
  
  ##########################
  ## Assemble the data frame
  ##########################
  
  data.df <- fies.df
  data.df$wt <- wt
  if (!is.null(wt.person)) data.df$wt.person <- wt.person
  if (!is.null(disaggregation.df)) data.df <- cbind(data.df, disaggregation.df)
  if (!is.null(UID)) data.df$UID <- UID
  
  #########################################################################
  ## Equate the 4 weeks reference scale with the 12 months global reference
  ## scale
  #########################################################################
  if (is.null(global.4w.common)) {
    plot(fies.global.st, FIES_ref)
    text(fies.global.st, FIES_ref, paste(1:8, names(fies.global.st)), cex = 0.6, pos = 4)
    input <- readline("Set common items: ")
    global.4w.common <- eval(parse(text = input))
  } else {
    if (is.numeric(global.4w.common) & all(global.4w.common %in% c(1:8))) {
      global.4w.common <- global.4w.common
    } else {
      stop("global.4w.common has to be provided as a numeric vector like c(1,2,3,6:8).")
    }
  }
  adj_fies <- (fies.global.st - mean(fies.global.st[global.4w.common]))/sd(fies.global.st[global.4w.common]) *
    sd(FIES_ref[global.4w.common]) + mean(FIES_ref[global.4w.common])
  plot(adj_fies, FIES_ref,
       xlab = "Normalized fies.global.st", ylab = "FIES_ref")
  text(adj_fies, FIES_ref, paste(1:8,names(fies.global.st)), cex = 0.6, pos = 4)
  abline(0,1)
  
  #################################################
  ## Run the model and remove items with high infit
  #################################################
  if (model == "dichotomous") {
    ## Dichotomous model
    fies.df[fies.df > 1 & !is.na(fies.df)] <- 1
    if (extr.par) {
      k <- ncol(fies.df)
      distr <- sapply(0:k, function(i) sum(wt[rowSums(fies.df)==i], na.rm=T)/
                        sum(wt[!is.na(rowSums(fies.df)) & !rowSums(fies.df)==0]))
      par <- c(min((k-1)+.7, (k-1)+.5 + distr[k+1]))
      extr <- c(0.5, par)
      res <- RM.w(fies.df, wt, .d = extr, write.file = FALSE, country = surveyName,
                  max.it = maxit.RM.w)
    } else {
      res <- RM.w(fies.df, wt, write.file = FALSE, country = surveyName,
                  max.it = maxit.RM.w)
    }
    b <- res$b
    names <- names(b)
    infit <- res$infit
    se.b <- res$se.b
  } else {
    if (all(unique(unlist(fies.df)) %in% c(0,1,NA))) {
      print("Polytomous model set by the user but only dichotomous values found in the dataset. The dichotomous module is used instead.")
      model <- "dichotomous"
      res <- RM.w(fies.df, wt, write.file = FALSE, country = surveyName, 
                  max.it = maxit.RM.w)
      b <- res$b
      names <- names(b)
      infit <- res$infit
      se.b <- res$se.b
    } else {
      ## Polytomous model
      res <- try(PC.w(fies.df, wt, write.file = FALSE, country = surveyName,
                  maxiter = maxit.PC.w, minconv=minconv.PC.w,recode=aggr.cat.PC.w),
                 silent=T)
      if(class(res)=="try-error"){
        warning("PC.w function could not reach convergence due to low number of 
                Yes answers to the extreme categories. 
                The function is re-run aggregating the categories Sometimes and Often.")
        res <- PC.w(fies.df, wt, write.file = FALSE, country = surveyName,
                        maxiter = maxit.PC.w, minconv=minconv.PC.w,recode=1)
      }
      b <- res$b$X1
      names(b) <- res$b$i_names
      infit <- res$infit$X1
      outfit <- res$outfit$X1
      names(infit) <- res$b$i_names
      names(outfit) <- res$b$i_names
      se.b <- res$se.b$X1
      
      ## Plot to check the stability of the dichotomous vs polytomous scales
      ## We do not exclude items with high infits and we consider all the items
      ## common
      {
        tmp.df <- fies.df
        tmp.df[tmp.df > 1 & !is.na(tmp.df)] <- 1
        
        tmp <- RM.w(tmp.df, wt,max.it =  maxit.RM.w, write.file = FALSE,country=surveyName)
        
        mpoly = mean(b)
        mdich = mean(tmp$b)
        m2 = mean(FIES_ref)
        spoly = sd(b)
        sdich = sd(tmp$b)
        s2 = sd(FIES_ref)
        adj_b_poly = (b-mpoly)/spoly*s2+m2
        adj_b_dich = (tmp$b-mdich)/sdich*s2+m2
        
        pdf(file = paste0(path_fies_output, "/", surveyName, "_dich vs poly_stability.pdf")) 
        plot(adj_b_dich, adj_b_poly, xlim = c(floor(min(c(adj_b_dich,adj_b_poly))), ceiling(max(c(adj_b_dich,adj_b_poly)))), 
             ylim = c(floor(min(c(adj_b_dich,adj_b_poly))), ceiling(max(c(adj_b_dich,adj_b_poly)))),
             xlab = "Dich item sev pars after equating", ylab = "Poly item sev pars after equating")
        text(adj_b_dich, adj_b_poly, names(b), cex = 0.6, pos = 4)
        abline(0,1)
        dev.off()
        
        tmp <- data.frame("Dich_Infit" = tmp$infit,
                          "Poly_Infit" = infit,
                          "Dich_Outfit" = tmp$outfit,
                          "Poly_Outfit" = infit,
                          "Dich_Item_Sev_Pars" = tmp$b,
                          "Poly_Item_Sev_Pars" = b,
                          "Adj_Dich_Item_Sev_Pars" = adj_b_dich,
                          "Adj_Poly_Item_Sev_Pars" = adj_b_poly)
        write.csv(tmp, paste0(path_fies_output, "/", surveyName, "_Item severity pars.csv"), row.names = TRUE)
        
      }
    }
  }
  
  highInfitItems <- NULL
  while (any(infit > infit.thresh)) {
    highInfitItems <- c(highInfitItems, which(colnames(fies.df) == names(which(infit > infit.thresh))))
    if (length(highInfitItems) < 4) { ## Check to guarantee a scale of at least 5 items
      if (model == "dichotomous") {
        ## Dichotomous model
        if (extr.par) {
          k <- ncol(fies.df[,-highInfitItems])
          distr <- sapply(0:k, function(i) sum(wt[rowSums(fies.df[,-highInfitItems])==i], na.rm=T)/
                            sum(wt[!is.na(rowSums(fies.df[,-highInfitItems])) & !rowSums(fies.df[,-highInfitItems])==0]))
          par <- c(min((k-1)+.7, (k-1)+.5 + distr[k+1]))
          extr <- c(0.5, par)
          res <- RM.w(fies.df[,-highInfitItems], wt, .d = extr, write.file = TRUE, country = surveyName)
        } else {
          res <- RM.w(fies.df[,-highInfitItems], wt, write.file = TRUE, country = surveyName)
        }
        b <- res$b
        names <- names(b)
        infit <- res$infit
        se.b <- res$se.b
      } else {
        if (all(unique(unlist(fies.df)) %in% c(0,1,NA))) {
          stop("Polytomous model set by the user but all the polytomous variables have been removed because of high infits. Select the dichotomous module and try again.")
        } else {
          ## Polytomous model
          res <- try(PC.w(fies.df[,-highInfitItems], wt, write.file = TRUE, country = surveyName,
                      maxiter = maxit.PC.w, minconv=minconv.PC.w,recode=aggr.cat.PC.w),
                     silent=T)
          if(class(res)=="try-error"){
            warning("PC.w function could not reach convergence due to low number of 
                Yes answers to the extreme categories. 
                The function is re-run aggregating the categories Sometimes and Often.")
            res <- PC.w(fies.df[,-highInfitItems], wt, write.file = TRUE, country = surveyName,
                        maxiter = maxit.PC.w, minconv=minconv.PC.w,recode=1)
          }
          b <- res$b$X1
          names(b) <- res$b$i_names
          infit <- res$infit$X1
          names(infit) <- res$b$i_names
          se.b <- res$se.b$X1
        }
      }
    } else {
      stop("Too many items with high infits. The scale will not be sufficiently large.")
    }
  }
  
  if (model == "polytomous") {
    b <- b-mean(b)
  }
  
  #######################
  ## Perform the equating
  #######################
  if(max.cor.meth){
    ## Define all the possible combinations of unique items, generating a scale of
    ## at least 5 items, and compute the correlation among the common items.
    combs <- t(c(NA, NA, NA, NA, NA)) # All items common
    if (length(highInfitItems) <3)  combs <- rbind(combs, cbind(t(as.data.frame(combn(names(b), 1))), NA, NA, NA, NA)) # Combination of 1 unique item
    if (length(highInfitItems) <2)  combs <- rbind(combs, cbind(t(as.data.frame(combn(names(b), 2))), NA, NA, NA)) # Combination of 2 unique items
    if (length(highInfitItems) == 0)  combs <- rbind(combs, cbind(t(as.data.frame(combn(names(b), 3))), NA, NA)) # Combination of 3 unique items
    combs <- as.data.frame(combs)
    
    for (i in 1:nrow(combs)) {
      if (is.null(highInfitItems)) {
        m1 = mean(b[!names(b) %in% combs[i,]])
        m2 = mean(FIES_ref[!names(b) %in% combs[i,]])
        s1 = sd(b[!names(b) %in% combs[i,]])
        s2 = sd(FIES_ref[!names(b) %in% combs[i,]])
        adj_b = (b-m1)/s1*s2+m2 
        # mean(adj_b[!names(b) %in% combs[i,]]); m2 ## check
        # sd(adj_b[!names(b) %in% combs[i,]]); s2 ## check
        combs[i,4] = cor(FIES_ref[!names(b) %in% combs[i,]], 
                         adj_b[!names(b) %in% combs[i,]])
      } else {
        m1 = mean(b[!names(b) %in% combs[i,]])
        m2 = mean(FIES_ref[-highInfitItems][!names(b) %in% combs[i,]])
        s1 = sd(b[!names(b) %in% combs[i,]])
        s2 = sd(FIES_ref[-highInfitItems][!names(b) %in% combs[i,]])
        adj_b = (b-m1)/s1*s2+m2 
        # mean(adj_b[!names(b) %in% combs[i,]]); m2 ## check
        # sd(adj_b[!names(b) %in% combs[i,]]); s2 ## check
        combs[i,4] = cor(FIES_ref[-highInfitItems][!names(b) %in% combs[i,]], 
                         adj_b[!names(b) %in% combs[i,]])
      }
    }
    combs[,4] <- as.numeric(combs[,4])
    
    ## Select the smallest set of unique items determining a correlation
    ## among common items greater than the one specified by the user
    if (combs[1, 4] >= tol) {
      combs[1, 5] <- 1
    } else if (length(highInfitItems) <3 & 
               any(combs[!is.na(combs[,1]) & is.na(combs[,2]) & is.na(combs[,3]),4] >= tol)) {
      combs[which(combs[,4] ==
                    max(combs[!is.na(combs[,1]) & is.na(combs[,2]) & is.na(combs[,3]),4])), 5] <- 1
    } else if (length(highInfitItems) <2 & 
               any(combs[!is.na(combs[,1]) & !is.na(combs[,2]) & is.na(combs[,3]),4] >= tol)) {
      combs[which(combs[,4] ==
                    max(combs[!is.na(combs[,1]) & !is.na(combs[,2]) & is.na(combs[,3]),4])), 5] <- 1
    } else if (length(highInfitItems) == 0 &
               any(combs[!is.na(combs[,1]) & !is.na(combs[,2]) & !is.na(combs[,3]),4] >= tol)) {
      combs[which(combs[,4] ==
                    max(combs[!is.na(combs[,1]) & !is.na(combs[,2]) & !is.na(combs[,3]),4])), 5] <- 1
    } else {
      combs[which(combs[,4] == max(combs[,4])), 5] <- 1
      print(paste0("Tolerance of ", tol, " didn't reach. Max reacheable tolerance equal to ", max(combs[,4])))
    }
    colnames(combs) <- c("Unique item 1", "Unique item 2", "Unique item 3",
                         "Corr. common items", "Selected")
    write.csv(combs, paste0(path_fies_output, "/", surveyName, " CorrelationCommonItems.csv"), row.names = FALSE)
    
    uniqueItems <- na.omit(unique(as.character(combs[combs$Selected == 1 &
                                                       !is.na(combs$Selected),][, c(1,2,3)])))
  }
  if(!max.cor.meth){
    uniqueItems=uniqueitems
  }
  if (is.null(highInfitItems)) {
    m1 = mean(b[!names(b) %in% uniqueItems])
    m2 = mean(FIES_ref[!names(b) %in% uniqueItems])
    s1 = sd(b[!names(b) %in% uniqueItems])
    s2 = sd(FIES_ref[!names(b) %in% uniqueItems])
    adj_b = (b-m1)/s1*s2+m2
    adj_a = (res$a - m1)/s1*s2 + m2
    adj_se.a = res$se.a/s1*s2
    adj_se.b = se.b/s1*s2
    
    pdf(file = paste0(path_fies_output, "/", surveyName, " Equating plot.pdf")) 
    plot(FIES_ref, adj_b, xlim = c(-2.2,2.2), ylim = c(-2.2,2.2))
    points(FIES_ref[!names(b) %in% uniqueItems], adj_b[!names(b) %in% uniqueItems], col = "black")
    points(FIES_ref[names(b) %in% uniqueItems], adj_b[names(b) %in% uniqueItems], col = "red")
    text(FIES_ref, adj_b, names(adj_b), cex = 0.6, pos = 4, srt = 45)
    abline(0,1)
    if(max.cor.meth) corr=round(combs[combs$Selected == 1 & !is.na(combs$Selected),][,4], 4)
    if(!max.cor.meth) corr=round(cor(FIES_ref[!names(b) %in% uniqueItems], adj_b[!names(b) %in% uniqueItems]),4)
    text(1, -1, paste("N = ", length(adj_b[!names(b) %in% uniqueItems]), "; corr =", corr))
    title(main = surveyName)
    dev.off()
  } else {
    m1 = mean(b[!names(b) %in% uniqueItems])
    m2 = mean(FIES_ref[-highInfitItems][!names(b) %in% uniqueItems])
    s1 = sd(b[!names(b) %in% uniqueItems])
    s2 = sd(FIES_ref[-highInfitItems][!names(b) %in% uniqueItems])
    adj_b = (b-m1)/s1*s2+m2
    adj_a = (res$a - m1)/s1*s2 + m2
    adj_se.a = res$se.a/s1*s2
    adj_se.b = se.b/s1*s2
    
    pdf(file = paste0(here("Wave1/FIES-outputs/"), surveyName, " Equating plot.pdf")) 
    plot(FIES_ref[-highInfitItems], adj_b, xlim = c(-2.2,2.2), ylim = c(-2.2,2.2))
    points(FIES_ref[-highInfitItems][!names(b) %in% uniqueItems], adj_b[!names(b) %in% uniqueItems], col = "black")
    points(FIES_ref[-highInfitItems][names(b) %in% uniqueItems], adj_b[names(b) %in% uniqueItems], col = "red")
    text(FIES_ref[-highInfitItems], adj_b, names(adj_b), cex = 0.6, pos = 4, srt = 45)
    abline(0,1)
    if(max.cor.meth) corr=round(combs[combs$Selected == 1 & !is.na(combs$Selected),][,4], 4)
    if(!max.cor.meth) corr=round(cor(FIES_ref[-highInfitItems][!names(b) %in% uniqueItems], adj_b[-highInfitItems][!names(b) %in% uniqueItems]),4)
    text(1, -1, paste("N = ", length(adj_b[!names(b) %in% uniqueItems]), "; corr =", corr))
    title(main = surveyName)
    dev.off()
  }
  
  # if (is.null(highInfitItems)) {
  #   data.df$RS_valid <- rowSums(fies.df)
  # } else {
  #   data.df$RS_valid <- rowSums(fies.df[,-highInfitItems])
  # }
  data.df$RS_valid = rowSums(res$XX)
  
  ###############################
  ## Compute the prevalence rates
  ###############################
  
  ## Define the probabilities by raw score and threshold
  prob_tab = cbind(
    "RS" = sort(na.omit(unique(rowSums(res$XX)))),
    "p_mod" = 1 - pnorm(adj_fies[5], mean = adj_a, sd = adj_se.a),
    "p_sev" = 1 - pnorm(adj_fies[8], mean = adj_a, sd = adj_se.a),
    "p_IPC2plus" = 1 - pnorm(Threshold_ref[2], mean = adj_a, sd = adj_se.a),
    "p_IPC3plus" = 1 - pnorm(Threshold_ref[3], mean = adj_a, sd = adj_se.a),
    "p_IPC4plus" = 1 - pnorm(Threshold_ref[4], mean = adj_a, sd = adj_se.a),
    "p_IPC5" = 1 - pnorm(Threshold_ref[5], mean = adj_a, sd = adj_se.a))
  prob_tab[1,] = 0
  
  ## Assign the probability to each raw score in the dataset
  data.df <- merge(data.df, prob_tab[, c("RS", "p_mod")], by.x = "RS_valid", by.y = "RS", all.x = TRUE)
  data.df <- merge(data.df, prob_tab[, c("RS", "p_sev")], by.x = "RS_valid", by.y = "RS", all.x = TRUE)
  data.df <- merge(data.df, prob_tab[, c("RS", "p_IPC2plus")], by.x = "RS_valid", by.y = "RS", all.x = TRUE)
  data.df <- merge(data.df, prob_tab[, c("RS", "p_IPC3plus")], by.x = "RS_valid", by.y = "RS", all.x = TRUE)
  data.df <- merge(data.df, prob_tab[, c("RS", "p_IPC4plus")], by.x = "RS_valid", by.y = "RS", all.x = TRUE)
  data.df <- merge(data.df, prob_tab[, c("RS", "p_IPC5")], by.x = "RS_valid", by.y = "RS", all.x = TRUE)
  
  ## Export data.df
  write.csv(data.df, paste0(path_fies_output, "/", surveyName, " Household Probabilities.csv"), row.names = FALSE)
  
  ## Define the weights to be used in the computation of the prevalence rates.
  ## If individual weigths are available, these are used, otherwise the household
  ## weights are used.
  if (is.null(wt.person)) data.df$prevWeights <- data.df$wt else data.df$prevWeights <- data.df$wt.person
  
  ## Prevalence rates at national level
  results.df <-
    data.df %>%
    summarise(N = length(!is.na(RS_valid)),
              p_mod = Hmisc::wtd.mean(p_mod, prevWeights),
              p_sev = Hmisc::wtd.mean(p_sev, prevWeights),
              p_IPC2plus = Hmisc::wtd.mean(p_IPC2plus, prevWeights),
              p_IPC3plus = Hmisc::wtd.mean(p_IPC3plus, prevWeights),
              p_IPC4plus = Hmisc::wtd.mean(p_IPC4plus, prevWeights),
              p_IPC5 = Hmisc::wtd.mean(p_IPC5, prevWeights))
  results.df$Disaggregation <- "Total"
  results.df <- results.df[, c("Disaggregation", "N", "p_mod",	"p_sev", "p_IPC2plus", "p_IPC3plus", "p_IPC4plus", "p_IPC5")]
  moe.df <-
    data.df %>%
    summarise(MoE_p_mod = tryCatch(as.numeric(moe(prob = p_mod, rs = RS_valid, wt = prevWeights, conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA),
              MoE_p_sev = tryCatch(as.numeric(moe(prob = p_sev, rs = RS_valid, wt = prevWeights, conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA),
              MoE_p_IPC2plus = tryCatch(as.numeric(moe(prob = p_IPC2plus, rs = RS_valid, wt = prevWeights, conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA),
              MoE_p_IPC3plus = tryCatch(as.numeric(moe(prob = p_IPC3plus, rs = RS_valid, wt = prevWeights, conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA),
              MoE_p_IPC4plus = tryCatch(as.numeric(moe(prob = p_IPC4plus, rs = RS_valid, wt = prevWeights, conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA),
              MoE_p_IPC5 = tryCatch(as.numeric(moe(prob = p_IPC5, rs = RS_valid, wt = prevWeights, conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA))
  results.df <- cbind(results.df, moe.df)
  
  ## Prevalence rates at the first level of disaggregation
  if (!is.null(disaggregation.df)) {
    for (dl in colnames(disaggregation.df)){
      tmp.df <- 
        data.df %>% 
        group_by(eval(parse(text = dl))) %>% 
        summarise(N = length(!is.na(RS_valid)),
                  p_mod = Hmisc::wtd.mean(p_mod, prevWeights),
                  p_sev = Hmisc::wtd.mean(p_sev, prevWeights),
                  p_IPC2plus = Hmisc::wtd.mean(p_IPC2plus, prevWeights),
                  p_IPC3plus = Hmisc::wtd.mean(p_IPC3plus, prevWeights),
                  p_IPC4plus = Hmisc::wtd.mean(p_IPC4plus, prevWeights),
                  p_IPC5 = Hmisc::wtd.mean(p_IPC5, prevWeights))
      colnames(tmp.df) <- c("Disaggregation", "N", "p_mod",	"p_sev", "p_IPC2plus", "p_IPC3plus", "p_IPC4plus", "p_IPC5")
      
      moe.df <-
        data.df %>%
        group_by(eval(parse(text = dl))) %>% 
        summarise(MoE_p_mod = tryCatch(as.numeric(moe(prob = p_mod, rs = RS_valid, wt = prevWeights, conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA),
                  MoE_p_sev = tryCatch(as.numeric(moe(prob = p_sev, rs = RS_valid, wt = prevWeights, conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA),
                  MoE_p_IPC2plus = tryCatch(as.numeric(moe(prob = p_IPC2plus, rs = RS_valid, wt = prevWeights, conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA),
                  MoE_p_IPC3plus = tryCatch(as.numeric(moe(prob = p_IPC3plus, rs = RS_valid, wt = prevWeights, conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA),
                  MoE_p_IPC4plus = tryCatch(as.numeric(moe(prob = p_IPC4plus, rs = RS_valid, wt = prevWeights, conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA),
                  MoE_p_IPC5 = tryCatch(as.numeric(moe(prob = p_IPC5, rs = RS_valid, wt = prevWeights, conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA))
      moe.df[,1] <- NULL
      tmp.df <- cbind(tmp.df, moe.df)
      
      results.df[nrow(results.df) + 1,1] <- dl
      results.df <- rbind(results.df, tmp.df)
    }

  }
  
  data.df$prevWeights <- NULL
  
  ## Add the margin of error
  # results.df$MoE_p_mod <- 1.96 * sqrt((results.df$p_mod * (1-results.df$p_mod))/results.df$N)
  # results.df$MoE_p_sev <- 1.96 * sqrt((results.df$p_sev * (1-results.df$p_sev))/results.df$N)
  # results.df$MoE_p_IPC2plus <- 1.96 * sqrt((results.df$p_IPC2plus * (1-results.df$p_IPC2plus))/results.df$N)
  # results.df$MoE_p_IPC3plus <- 1.96 * sqrt((results.df$p_IPC3plus * (1-results.df$p_IPC3plus))/results.df$N)
  # results.df$MoE_p_IPC4plus <- 1.96 * sqrt((results.df$p_IPC4plus * (1-results.df$p_IPC4plus))/results.df$N)
  # results.df$MoE_p_IPC5 <- 1.96 * sqrt((results.df$p_IPC5 * (1-results.df$p_IPC5))/results.df$N)
  
  results.df$P_mod_MoE <- paste0(format(round(results.df$p_mod*100, 1), nsmall = 1)," (+/-", format(round(results.df$MoE_p_mod, 1), nsmall = 1), ")")
  results.df$P_sev_MoE <- paste0(format(round(results.df$p_sev*100, 1), nsmall = 1)," (+/-", format(round(results.df$MoE_p_sev, 1), nsmall = 1), ")")
  results.df$P_IPC2plus_MoE <- paste0(format(round(results.df$p_IPC2plus*100, 1), nsmall = 1)," (+/-", format(round(results.df$MoE_p_IPC2plus, 1), nsmall = 1), ")")
  results.df$P_IPC3plus_MoE <- paste0(format(round(results.df$p_IPC3plus*100, 1), nsmall = 1)," (+/-", format(round(results.df$MoE_p_IPC3plus, 1), nsmall = 1), ")")
  results.df$P_IPC4plus_MoE <- paste0(format(round(results.df$p_IPC4plus*100, 1), nsmall = 1)," (+/-", format(round(results.df$MoE_p_IPC4plus, 1), nsmall = 1), ")")
  results.df$P_IPC5_MoE <- paste0(format(round(results.df$p_IPC5*100, 1), nsmall = 1)," (+/-", format(round(results.df$MoE_p_IPC5, 1), nsmall = 1), ")")
  
  results.df[results.df$Disaggregation %in% colnames(disaggregation.df), 2:ncol(results.df)] <- ""
  
  ## Export the results
  colnames(results.df) <- c("Disaggregation", "N", "P_mod",	"P_sev", "P_IPC2+", "P_IPC3+", "P_IPC4+", "P_IPC5", 
                            "MoE P_mod", "MoE P_sev", "MoE P_IPC2+", "MoE P_IPC3+", "MoE P_IPC4+", "MoE P_IPC5",
                            "P_mod_MoE", "P_sev_MoE", "P_IPC2+_MoE", "P_IPC3+_MoE", "P_IPC4+_MoE", "P_IPC5_MoE")
  write.csv(results.df, paste0(path_fies_output, "/", surveyName, " Prevalence rates.csv"), row.names = FALSE)
  
  #######################
  ## FIES categories plot
  #######################
  
  if (FIEScatplot) {
    xlimmin <- round(min(adj_a)-1,0)
    xlimmax <- round(max(adj_a)+1,0)
    
    term.df <- data.frame(x = -0.5,
                          xend = 0.5,
                          y = seq(from = xlimmin, 
                                  to = xlimmax, by = 0.01),
                          yend = seq(from = xlimmin, 
                                     to = xlimmax, by = 0.01))
    mod.df <- data.frame(x = 1, xend = 2,
                         y = seq(from = xlimmin, 
                                 to = xlimmax, by = 0.01),
                         yend = seq(from = xlimmin, 
                                    to = xlimmax, by = 0.01))
    mod.df[mod.df$y < adj_fies[5], ] <- NA
    mod.df[!is.na(mod.df$y) & mod.df$y > adj_fies[8], ] <- NA
    
    sev.df <- data.frame(x = 2.5, xend = 3.5,
                         y = seq(from = xlimmin, 
                                 to = xlimmax, by = 0.01),
                         yend = seq(from = xlimmin, 
                                    to = xlimmax, by = 0.01))
    sev.df[sev.df$y < adj_fies[8], ] <- NA
    
    ipc2.df <- data.frame(x = -1, xend = -2,
                          y = seq(from = xlimmin, 
                                  to = xlimmax, by = 0.01),
                          yend = seq(from = xlimmin, 
                                     to = xlimmax, by = 0.01))
    ipc2.df[ipc2.df$y < Threshold_ref[2], ] <- NA
    ipc2.df[!is.na(ipc2.df$y) & ipc2.df$y > Threshold_ref[3], ] <- NA
    
    ipc3.df <- data.frame(x = -2.5, xend = -3.5,
                          y = seq(from = xlimmin, 
                                  to = xlimmax, by = 0.01),
                          yend = seq(from = xlimmin, 
                                     to = xlimmax, by = 0.01))
    ipc3.df[ipc3.df$y < Threshold_ref[3], ] <- NA
    # ipc3.df[!is.na(ipc3.df$y) & ipc3.df$y > Threshold_ref[4], ] <- NA
    
    tmp.plot <-
      ggplot(data = term.df, aes(x = x, xend = xend, y = y, yend = yend, color = y)) +
      geom_segment(size = 2, show.legend = FALSE) +
      geom_segment(data = mod.df, size = 2, show.legend = FALSE) +
      geom_segment(data = sev.df, size = 2, show.legend = FALSE) +
      geom_segment(data = ipc2.df, size = 2, show.legend = FALSE) +
      geom_segment(data = ipc3.df, size = 2, show.legend = FALSE) +
      scale_color_gradient2(low = "yellow", high = "darkred", mid = "red",
                            midpoint = max(term.df$y, na.rm = T)/2, na.value = "white") +
      # annotate("segment", x = 0, y = adj_fies[5], xend = 2, yend = adj_fies[5], size = 1) +
      # annotate("segment", x = 0, y = adj_fies[8], xend = 3.5, yend = adj_fies[8], size = 1) +
      # annotate("segment", x = 0, y = Threshold_ref[2], xend = -2, yend = Threshold_ref[2], size = 1) +
      # annotate("segment", x = 0, y = Threshold_ref[3], xend = -3.5, yend = Threshold_ref[3], size = 1) +
      coord_flip() +
      theme(axis.text.x=element_blank(),
            axis.title.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      annotate("text", x = -0.05, y = xlimmin+0.5, label = "Mild food\ninsecurity") +
      annotate("text", x = -0.05, y = xlimmax-0.5, label = "Severe food\ninsecurity") +
      annotate("text", x = 3, y = (adj_fies[8]+xlimmax)/2, label = "Recent\nsevere food\ninsecurity") +
      annotate("text", x = 1.5, y = (adj_fies[5]+adj_fies[8])/2, label = "Recent\nmoderate food\ninsecurity") +
      annotate("text", x = -3, y = (Threshold_ref[3]+xlimmax)/2, label = "IPC Phase 3+\nUrgent action required") +
      annotate("text", x = -1.5, y = (Threshold_ref[2]+Threshold_ref[3])/2, label = "IPC Phase 2\nStressed") +
      annotate("text", x = 2, y = xlimmin+0.75, label = "SDG monitoring process\nFIES-based recent\nfood insecurity") +
      annotate("text", x = -2, y = xlimmin+0.75, label = "IPC - Acute Food Insecurity\nFIES-based recent\nfood insecurity")
    
    pdf(paste0(path_fies_output, "/", surveyName, " FIES categories plot.pdf"), width = 10, height = 5)
    print(tmp.plot)
    dev.off()
    tmp.plot
  }
  here()
  ## Save equated parameters
  save(adj_b, adj_se.b, adj_a, adj_se.a,file = paste0(path_fies_output, "/", surveyName, " Equated parameters.RData"))
  
  ## Return some objects
  return(list(PrevalenceRates = results.df, 
              HouseholdProbs = data.df,
              adj_fies=adj_fies))
}
