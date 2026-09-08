
FIEStable <- function(data, fies.matrix, rr, disaggregation = NULL, adj.thres,
                      weights = NULL, lang = c("ENG", "FR")) {
  library(survey)
  ## Checks
  if (is.null(data)|is.null(fies.matrix)|is.null(rr)|is.null(adj.thres)) {
    stop("data, fies.matrix, rr or adj.thres is missing")
  }
  
  if (is.null(weights)) weights <- rep(1, nrow(data))
  
  if (is.null(disaggregation)) {
    
    ## National estimates
    prevtemp <- prob.assign(sthres = adj.thres,
                            flex = list(a = rr$a, se.a = rr$se.a, d = rr$d,
                                        XX = fies.matrix, wt = weights))$sprob*100
    
    ## OLD MOE
    # moestemp <- moes.fun(rr = rr, fies = fies.matrix, 
    #                      wt = weights, adj.thresh = adj.thres)
    
    ## NEW MOE
    prob_tab = cbind(
      "RS" = sort(na.omit(unique(rowSums(fies.matrix)))),
      "p_mod" = 1 - pnorm(adj.thres[1], mean = rr$a, sd = rr$se.a),
      "p_sev" = 1 - pnorm(adj.thres[2], mean = rr$a, sd = rr$se.a))
    tmp.matrix <- fies.matrix
    tmp.matrix$RS_valid = rowSums(tmp.matrix)
    tmp.matrix <- merge(tmp.matrix, prob_tab[, c("RS", "p_mod", "p_sev")], by.x = "RS_valid", by.y = "RS", all.x = TRUE)
    moestemp1 <- tryCatch(as.numeric(moe(prob = tmp.matrix$p_mod, rs = tmp.matrix$RS, wt = weights, conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA)
    moestemp2 <- tryCatch(as.numeric(moe(prob = tmp.matrix$p_sev, rs = tmp.matrix$RS, wt = weights, conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA)
    
    fiesTable <- matrix(0, nrow = 1, ncol = 6+length(colnames(fies.matrix)))
    rownames(fiesTable) <- "National"
    if (lang == "ENG") {
      colnames(fiesTable) <- c("Cases", "NonExtremeCases", colnames(fies.matrix), "FI_mod+sev", "MoE_mod+sev", "FI_sev","MoE_sev")
    } else if (lang == "FR") {
      colnames(fiesTable) <- c("Cas", "CasNonExtreme", colnames(fies.matrix), "IA_mod+grave", "EdM_mod+grave", "IA_grave","EdM_grave")
    }
    fiesTable[row.names(fiesTable) == "National", 1] <- nrow(fies.matrix)
    fiesTable[row.names(fiesTable) == "National", 2] <- rr$n.compl
    fiesTable[row.names(fiesTable) == "National", c(3:(3+length(colnames(fies.matrix))-1))] <- rr$b
    fiesTable[row.names(fiesTable) == "National", (3+length(colnames(fies.matrix)))] = prevtemp[1]
    # fiesTable[row.names(fiesTable) == "National", (3+length(colnames(fies.matrix))+1)] = moestemp[1]
    fiesTable[row.names(fiesTable) == "National", (3+length(colnames(fies.matrix))+1)] = moestemp1
    fiesTable[row.names(fiesTable) == "National", (3+length(colnames(fies.matrix))+2)] = prevtemp[2]
    # fiesTable[row.names(fiesTable) == "National", (3+length(colnames(fies.matrix))+3)] = moestemp[2]
    fiesTable[row.names(fiesTable) == "National", (3+length(colnames(fies.matrix))+3)] = moestemp2
    
  } else {

    ## Subregional estimates
    fiesTable <- matrix(0, nrow = length(na.omit(unique(data[, disaggregation]))), ncol = 6+length(colnames(fies.matrix)))
    rownames(fiesTable) <- na.omit(as.character(unique(data[, disaggregation])))
    if (lang == "ENG") {
      colnames(fiesTable) <- c("Cases", "NonExtremeCases", colnames(fies.matrix), "FI_mod+sev", "MoE_mod+sev", "FI_sev","MoE_sev")
    } else if (lang == "FR") {
      colnames(fiesTable) <- c("Cas", "CasNonExtreme", colnames(fies.matrix), "IA_mod+grave", "EdM_mod+grave", "IA_grave","EdM_grave")
    }

    if (length(unique(data[, disaggregation]) == 2)) par(mfrow=c(1,2))
    if (length(unique(data[, disaggregation]) == 3)) par(mfrow=c(1,3))
    if (length(unique(data[, disaggregation]) == 4)) par(mfrow=c(2,2))
    if (length(unique(data[, disaggregation]) %in% c(5,6))) par(mfrow=c(2,3))
    if (length(unique(data[, disaggregation]) > 6)) par(mfrow=c(3,3))

    pdf(paste0("Equating_", disaggregation))
    nsubregion <- 1
    
    res <- list()
    for (subregion in na.omit(unique(data[, disaggregation]))) {
      print(subregion)
      ## Check whether all the sub-groups have at least 5 non extreme cases for each item
      tmp.df <- fies.matrix[data[, disaggregation] == subregion,]
      rs_tmp <- rowSums(tmp.df)
      print(colSums(tmp.df[which(!(rs_tmp == 0 | rs_tmp == 8)),], na.rm = TRUE))
      print(sum(data[, disaggregation] == subregion))
      r = RM.w(fies.matrix[data[, disaggregation] == subregion &
                             !is.na(data[, disaggregation]),],
               weights[data[, disaggregation] == subregion&
                         !is.na(data[, disaggregation])],
               country = subregion,
               write.file = FALSE)

      prevtemp <- prob.assign(sthres = adj.thres,
                              flex = list(a = rr$a, se.a = rr$se.a, d = rr$d,
                                          XX = rr$XX[data[, disaggregation] == subregion,],
                                          wt = weights[data[, disaggregation] == subregion]))$sprob*100

      ## OLD MOE
      # moestemp <- moes.fun(rr = rr,
      #                      fies = fies.matrix[data[, disaggregation] == subregion &
      #                                           !is.na(data[, disaggregation]),],
      #                      wt = weights[data[, disaggregation] == subregion &
      #                                     !is.na(data[, disaggregation])],
      #                      adj.thresh = adj.thres)

      ## NEW MOE
      prob_tab = cbind(
        "RS" = sort(na.omit(unique(rowSums(tmp.df)))),
        "p_mod" = 1 - pnorm(adj.thres[1], mean = rr$a, sd = rr$se.a),
        "p_sev" = 1 - pnorm(adj.thres[2], mean = rr$a, sd = rr$se.a))
      tmp.matrix <- tmp.df
      tmp.matrix$RS_valid = rowSums(tmp.matrix)
      tmp.matrix <- merge(tmp.matrix, prob_tab[, c("RS", "p_mod", "p_sev")], by.x = "RS_valid", by.y = "RS", all.x = TRUE)
      moestemp1 <- tryCatch(as.numeric(moe(prob = tmp.matrix$p_mod, rs = tmp.matrix$RS, wt = weights[data[, disaggregation] == subregion], conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA)
      moestemp2 <- tryCatch(as.numeric(moe(prob = tmp.matrix$p_sev, rs = tmp.matrix$RS, wt = weights[data[, disaggregation] == subregion], conf.level = 0.9, sd = 2)$moe*100), error = function(err) NA)
    
      fiesTable[row.names(fiesTable) == subregion, 1] <- nrow(r$XX)
      fiesTable[row.names(fiesTable) == subregion, 2] <- r$n.compl
      fiesTable[row.names(fiesTable) == subregion, c(3:(3+length(colnames(fies.matrix))-1))] <- r$b/(sd(r$b)*sd(rr$b))
      fiesTable[row.names(fiesTable) == subregion, (3+length(colnames(fies.matrix)))] = prevtemp[1]
      # fiesTable[row.names(fiesTable) == subregion, (3+length(colnames(fies.matrix))+1)] = moestemp[1]
      fiesTable[row.names(fiesTable) == subregion, (3+length(colnames(fies.matrix))+1)] = moestemp1
      fiesTable[row.names(fiesTable) == subregion, (3+length(colnames(fies.matrix))+2)] = prevtemp[2]
      # fiesTable[row.names(fiesTable) == subregion, (3+length(colnames(fies.matrix))+3)] = moestemp[2]
      fiesTable[row.names(fiesTable) == subregion, (3+length(colnames(fies.matrix))+3)] = moestemp2

      res[[length(res) + 1]] <- r

      if (nsubregion %in% c(9, 18, 27, 36, 45)) {
        dev.off()
        pdf(paste0("Equating_", disaggregation, "_", nsubregion/9))
      }
      
      plot(rr$b, r$b/(sd(r$b)*sd(rr$b)), xlim=c(-4,4), ylim=c(-4,4),xlab="National", ylab=subregion)
      tmp <- r$b/(sd(r$b)*sd(rr$b))
      abline(lm(tmp ~ rr$b))

      nsubregion <- nsubregion +1
    }
    dev.off()
  }
  fiesTable
}