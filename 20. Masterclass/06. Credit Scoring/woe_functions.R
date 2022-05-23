iv_categorical=function(Target,VariableCategorica){
  Tabla_WOE=table(VariableCategorica,Target)
  DF_WOE=data.frame(FRACASOS=Tabla_WOE[,1],EXITOS=Tabla_WOE[,2])
  DF_WOE$EXITOS_PORC=DF_WOE$EXITOS/sum(DF_WOE$EXITOS)
  DF_WOE$FRACASOS_PORC=DF_WOE$FRACASOS/sum(DF_WOE$FRACASOS)
  DF_WOE$WOE=log(DF_WOE$EXITOS_PORC/DF_WOE$FRACASOS_PORC)
  DF_WOE$IV=(DF_WOE$EXITOS_PORC-DF_WOE$FRACASOS_PORC)*DF_WOE$WOE
  IV_variable<-as.numeric(sum(DF_WOE$IV))
  return(IV_variable)
}




iv.str<-
  function (df, x, y, verbose = FALSE) 
  {
    if (!(class(df) == "data.frame")) {
      stop("Parameter df has to be a data frame.")
    }
    if (!(is.character(df[, x]) || is.factor(df[, x]))) {
      stop(paste("Input is not a character or factor! Variable:", 
                 x))
    }
    if (!(is.numeric(df[, y]) || is.factor(df[, y]))) {
      stop("Outcome is not a number nor factor!")
    }
    if (length(unique(df[, y])) != 2) {
      if (verbose) 
        paste(cat(unique(df[, y])), "\n")
      stop("Not a binary outcome")
    }
    if (!(all(sort(unique(df[, y])) == c(0, 1))) && is.numeric(df[, 
                                                                  y])) {
      stop("Numeric outcome has to be encoded as 0 (good) and 1 (bad). \n")
    }
    if (is.factor(df[, y]) && all(levels(df[, y])[order(levels(df[, 
                                                                  y]))] == c("bad", "good"))) {
      if (verbose) 
        cat("Assuming good = level 'good' and bad = level 'bad' \n")
      total_1 <- sum(df[, y] == "bad")
    }
    else if (is.factor(df[, y])) {
      if (verbose) 
        cat("Factor: Assuming bad = level 2 and good = level 1 \n")
      total_1 <- sum(as.integer(df[, y]) - 1)
    }
    else {
      if (verbose) 
        cat("Numeric: Assuming bad = 1 and good = 0 \n")
      total_1 <- sum(df[, y])
    }
    outcome_0 <- outcome_1 <- NULL
    total_0 <- nrow(df) - total_1
    iv_data <- data.frame(unclass(table(df[, x], df[, y])))
    if (all(names(iv_data) == c("bad", "good"))) {
      iv_data <- iv_data[, c(2, 1)]
    }
    names(iv_data) <- c("outcome_0", "outcome_1")
    iv_data <- within(iv_data, {
      class <- row.names(iv_data)
      variable <- x
      pct_0 <- outcome_0/total_0
      pct_1 <- outcome_1/total_1
      odds <- pct_0/pct_1
      woe <- log(odds)
      miv <- (pct_0 - pct_1) * woe
    })
    if (is.factor(df[, x])) {
      iv_data$class <- factor(iv_data$class, levels = levels(df[, 
                                                                x]))
    }
    iv_data <- iv_data[c("variable", "class", "outcome_0", "outcome_1", 
                         "pct_0", "pct_1", "odds", "woe", "miv")]
    if (any(iv_data$outcome_0 == 0) | any(iv_data$outcome_1 == 
                                          0)) {
      warning("Some group for outcome 0 has zero count. This will result in -Inf or Inf WOE. Replacing - ODDS=1, WoE=0, MIV=0. \n The bin is either too small or suspiciously predictive. \n You should fix this before running any model. It does not make any sense to keep WoE = 0 for such bin.")
      iv_data$woe <- ifelse(is.infinite(iv_data$woe), 0, iv_data$woe)
      iv_data$miv <- ifelse(is.infinite(iv_data$miv), 0, iv_data$miv)
      iv_data$odds <- ifelse(is.infinite(iv_data$odds), 1, 
                             iv_data$odds)
    }
    rownames(iv_data) <- NULL
    cat(paste("Information Value", round(sum(iv_data$miv), 2), 
              "\n"))
    iv_data
  }



iv.multivariante<-
  function (df, y, summary = FALSE, vars = NULL, verbose = FALSE, 
            rcontrol = NULL) 
  {
    if (verbose) {
      cat(paste("Started processing of data frame:", deparse(substitute(df)), 
                "\n"))
    }
    if (is.null(vars)) {
      vars <- names(df)[names(df) != y]
    }
    ivlist <- lapply(vars, function(x) {
      if (is.numeric(df[, x])) {
        if (verbose) 
          cat(paste("Calling iv.num for variable:", x, 
                    "\n"))
        iv.num(df, x, y, verbose = verbose, rcontrol = rcontrol)
      }
      else {
        if (verbose) 
          cat(paste("Calling iv.str for variable:", x, 
                    "\n"))
        iv.str(df, x, y, verbose = verbose)
      }
    })
    if (summary) {
      if (verbose) 
        cat(paste("Preparing summary", "\n"))
      ivlist <- rbind.fill(ivlist)
      ivlist <- sqldf("select \n                        variable as Variable,\n                        sum(miv) as InformationValue, \n                        count(*) as Bins,\n                        sum(case when outcome_0 = 0 or outcome_1 = 0 then 1 else 0 end) as ZeroBins\n                     from ivlist \n                     group by variable \n                     order by InformationValue desc")
      ivlist$Strength[ivlist$InformationValue >= 1] <- 1
      ivlist$Strength[ivlist$InformationValue >= 0.5 & ivlist$InformationValue < 
                        1] <- 2
      ivlist$Strength[ivlist$InformationValue >= 0.2 & ivlist$InformationValue < 
                        0.5] <- 3
      ivlist$Strength[ivlist$InformationValue >= 0.1 & ivlist$InformationValue < 
                        0.2] <- 4
      ivlist$Strength[ivlist$InformationValue >= 0.02 & ivlist$InformationValue < 
                        0.1] <- 5
      ivlist$Strength[ivlist$InformationValue < 0.02] <- 6
      ivlist$Strength <- factor(ivlist$Strength, levels = c(1, 
                                                            2, 3, 4, 5, 6), labels = c("Suspicious", "Very strong", 
                                                                                       "Strong", "Average", "Weak", "Wery weak"))
    }
    ivlist
  }

smbinning_calc<-
  function (df, y, x, p = 0.05) 
  {
    if (!is.data.frame(df)) {
      return("Data is not a data.frame")
    }
    else if (is.numeric(y) | is.numeric(x)) {
      return("Characteristic name not string")
    }
    else if (grepl("[.]", y) | grepl("[.]", x)) {
      return("Name of a characteristic must not have a dot [.]")
    }
    else i = which(names(df) == y)
    j = which(names(df) == x)
    if (!is.numeric(df[, i])) {
      return("Target (y) not found or it is not numeric")
    }
    else if (max(df[, i], na.rm = T) != 1) {
      return("Maximum not 1")
    }
    else if (fn$sqldf("select count(*) from df where cast($x as text)='Inf' or cast($x as text)='-Inf'") > 
             0) {
      return("Characteristic (x) with an 'Inf' value (Divided by Zero). Replace by NA")
    }
    else if (min(df[, i], na.rm = T) != 0) {
      return("Minimum not 0")
    }
    else if (p <= 0 | p > 0.5) {
      return("p must be greater than 0 and lower than 0.5 (50%)")
    }
    else if (!is.numeric(df[, j])) {
      return("Characteristic (x) not found or it is not a number")
    }
    else if (length(unique(df[, j])) < 10) {
      return("Characteristic (x) has less than 10 uniques values")
    }
    else {
      ctree = ctree(formula(paste(y, "~", x)), data = df, 
                    control = ctree_control(minbucket = ceiling(round(p * 
                                                                        nrow(df)))))
      bins = length(ctree)
      if (bins < 2) {
        return("No Bins")
      }
      cutvct = data.frame(matrix(ncol = 0, nrow = 0))
      n = length(ctree)
      for (i in 1:n) {
        cutvct = rbind(cutvct, ctree[i]$node$split$breaks)
      }
      cutvct = cutvct[order(cutvct[, 1]), ]
      ivt = data.frame(matrix(ncol = 0, nrow = 0))
      n = length(cutvct)
      for (i in 1:n) {
        cutpoint = cutvct[i]
        ivt = rbind(ivt, fn$sqldf("select '<= $cutpoint' as Cutpoint,\n                  NULL as CntRec,\n                  NULL as CntGood,\n                  NULL as CntBad,\n                  sum(case when $x <= $cutpoint and $y in (1,0) then 1 else 0 end) as CntCumRec,\n                  sum(case when $x <= $cutpoint and $y=1 then 1 else 0 end) as CntCumGood,\n                  sum(case when $x <= $cutpoint and $y=0 then 1 else 0 end) as CntCumBad,\n                  NULL as PctRec,\n                  NULL as GoodRate,\n                  NULL as BadRate,\n                  NULL as Odds,\n                  NULL as LnOdds,\n                  NULL as WoE,\n                  NULL as IV\n                  from df where $x is not NULL and $y is not NULL"))
      }
      cutpoint = max(df[, j], na.rm = T)
      mincutpoint = min(df[, j], na.rm = T)
      ivt = rbind(ivt, fn$sqldf("select '<= $cutpoint' as Cutpoint,\n                NULL as CntRec,\n                NULL as CntGood,\n                NULL as CntBad,\n                sum(case when $x <= $cutpoint and $y in (1,0) then 1 else 0 end) as CntCumRec,\n                sum(case when $x <= $cutpoint and $y=1 then 1 else 0 end) as CntCumGood,\n                sum(case when $x <= $cutpoint and $y=0 then 1 else 0 end) as CntCumBad,\n                NULL as PctRec,\n               NULL as GoodRate,\n                NULL as BadRate,\n                NULL as Odds,\n                NULL as LnOdds,\n                NULL as WoE,\n                NULL as IV\n                from df where $x is not NULL and $y is not NULL"))
      x.na = fn$sqldf("select count(*) from df where $x is null")
      y.na = fn$sqldf("select count(*) from df where $y is null")
      if (x.na > 0) {
        ivt = rbind(ivt, fn$sqldf("select 'Missing' as Cutpoint,\n                  sum(case when $x is NULL and $y in (1,0) then 1 else 0 end) as CntRec,\n                  sum(case when $x is NULL and $y=1 then 1 else 0 end) as CntGood,\n                  sum(case when $x is NULL and $y=0 then 1 else 0 end) as CntBad,\n                  NULL as CntCumRec,\n                  NULL as CntCumGood,\n                  NULL as CntCumBad,\n                  NULL as PctRec,\n                  NULL as GoodRate,\n                  NULL as BadRate,\n                  NULL as Odds,\n                  NULL as LnOdds,\n                  NULL as WoE,\n                  NULL as IV\n                  from df where $y is not NULL"))
      }
      else {
        ivt = rbind(ivt, c("Missing", 0, 0, 0, NA, NA, NA, 
                           NA, NA, NA, NA, NA, NA))
      }
      ivt = rbind(ivt, fn$sqldf("select 'Total' as Cutpoint,\n                count(*) as CntRec,\n                sum(case when $y=1 then 1 else 0 end) as CntGood,\n                sum(case when $y=0 then 1 else 0 end) as CntBad,\n                NULL as CntCumRec,\n                NULL as CntCumGood,\n                NULL as CntCumBad,\n                NULL as PctRec,\n                NULL as GoodRate,\n                NULL as BadRate,\n                NULL as Odds,\n                NULL as LnOdds,\n                NULL as WoE,\n                NULL as IV\n                from df where $y is not NULL"))
      ncol = ncol(ivt)
      for (i in 2:ncol) {
        ivt[, i] = as.numeric(ivt[, i])
      }
      ivt[1, 2] = ivt[1, 5]
      ivt[1, 3] = ivt[1, 6]
      ivt[1, 4] = ivt[1, 7]
      n = nrow(ivt) - 2
      for (i in 2:n) {
        ivt[i, 2] = ivt[i, 5] - ivt[i - 1, 5]
        ivt[i, 3] = ivt[i, 6] - ivt[i - 1, 6]
        ivt[i, 4] = ivt[i, 7] - ivt[i - 1, 7]
      }
      ivt[2, 2] = ivt[2, 5] - ivt[1, 5]
      ivt[2, 3] = ivt[2, 6] - ivt[1, 6]
      ivt[2, 4] = ivt[2, 7] - ivt[1, 7]
      ivt[i + 1, 5] = ivt[i, 5] + ivt[i + 1, 2]
      ivt[i + 1, 6] = ivt[i, 6] + ivt[i + 1, 3]
      ivt[i + 1, 7] = ivt[i, 7] + ivt[i + 1, 4]
      options(scipen = 999)
      ivt[, 8] = round(ivt[, 2]/ivt[i + 2, 2], 4)
      ivt[, 9] = round(ivt[, 3]/ivt[, 2], 4)
      ivt[, 10] = round(ivt[, 4]/ivt[, 2], 4)
      ivt[, 11] = round(ivt[, 3]/ivt[, 4], 4)
      ivt[, 12] = round(log(ivt[, 3]/ivt[, 4]), 4)
      G = ivt[i + 2, 3]
      B = ivt[i + 2, 4]
      LnGB = log(G/B)
      ivt[, 13] = round(log(ivt[, 3]/ivt[, 4]) - LnGB, 4)
      ivt[, 14] = round(ivt[, 13] * (ivt[, 3]/G - ivt[, 4]/B), 
                        4)
      ivt[i + 2, 14] = 0
      for (k in 1:(nrow(ivt) - 1)) {
        if (is.finite(ivt[k, 14])) {
          mgiv = ivt[k, 14]
        }
        else {
          mgiv = 0
        }
        ivt[i + 2, 14] = ivt[i + 2, 14] + mgiv
      }
      iv = ivt[i + 2, 14]
    }
    bands = append(mincutpoint, cutvct)
    bands = append(bands, cutpoint)
    list(ivtable = ivt, iv = iv, ctree = ctree, bands = bands, 
         x = x, col_id = j, cuts = cutvct)
  }


smbinning.plotty<-
  function (ivout, option = "dist", sub = "") 
  {
    r = ifelse(ivout$ivtable[nrow(ivout$ivtable) - 1, 2] == 0, 
               2, 1)
    if (option == "dist") {
      x_upper = nrow(ivout$ivtable) - r
      y_upper = max(ivout$ivtable[1:x_upper, 8]) * 1.25
      ch_dist = barplot(ivout$ivtable[1:x_upper, 8], names.arg = ivout$ivtable[1:x_upper, 
                                                                               1], axes = F, main = "Percentage of Cases", ylim = c(0, 
                                                                                                                                    y_upper), col = gray.colors(length(unique(ivout$ivtable[1:x_upper, 
                                                                                                                                                                                            1]))))
      text(x = ch_dist, y = ivout$ivtable[1:x_upper, 8], label = round(ivout$ivtable[1:x_upper, 
                                                                                     8] * 100, 1), pos = 3, cex = 1)
      abline(h = 0)
      mtext(sub, 3)
    }
    else if (option == "goodrate") {
      x_upper = nrow(ivout$ivtable) - r
      y_upper = max(ivout$ivtable[1:x_upper, 9], na.rm = T) * 
        1.25
      ch_goodrate = barplot(ivout$ivtable[1:x_upper, 9], names.arg = ivout$ivtable[1:x_upper, 
                                                                                   1], axes = F, main = "Good Rate (%)", ylim = c(0, 
                                                                                                                                  y_upper), col = gray.colors(length(unique(ivout$ivtable[1:x_upper, 
                                                                                                                                                                                          1]))))
      text(x = ch_goodrate, y = ivout$ivtable[1:x_upper, 9], 
           label = round(ivout$ivtable[1:x_upper, 9] * 100, 
                         1), pos = 3, cex = 1)
      abline(h = 0)
      mtext(sub, 3)
    }
    else if (option == "badrate") {
      x_upper = nrow(ivout$ivtable) - r
      y_upper = max(ivout$ivtable[1:x_upper, 10], na.rm = T) * 
        1.25
      ch_badrate = barplot(ivout$ivtable[1:x_upper, 10], names.arg = ivout$ivtable[1:x_upper, 
                                                                                   1], axes = F, main = "Bad Rate (%)", ylim = c(0, 
                                                                                                                                 y_upper), col = gray.colors(length(unique(ivout$ivtable[1:x_upper, 
                                                                                                                                                                                         1]))))
      text(x = ch_badrate, y = ivout$ivtable[1:x_upper, 10], 
           label = round(ivout$ivtable[1:x_upper, 10] * 100, 
                         1), pos = 3, cex = 1)
      abline(h = 0)
      mtext(sub, 3)
    }
    else if (option == "WoE") {
      x_upper = nrow(ivout$ivtable) - r
      y_upper = max(ivout$ivtable[1:x_upper, 13], na.rm = T) * 
        1.25
      y_lower = min(ivout$ivtable[1:x_upper, 13], na.rm = T) * 
        1.25
      ch_woe = barplot(ivout$ivtable[1:x_upper, 13], names.arg = ivout$ivtable[1:x_upper, 
                                                                               1], axes = F, main = "Weight of Evidence", ylim = c(y_lower, 
                                                                                                                                   y_upper), col = gray.colors(length(unique(ivout$ivtable[1:x_upper, 
                                                                                                                                                                                           1]))))
      text(x = ch_woe, y = ivout$ivtable[1:x_upper, 13], label = round(ivout$ivtable[1:x_upper, 
                                                                                     13], 2), pos = 3, cex = 1)
      abline(h = 0)
      mtext(sub, 3)
    }
    else {
      return("Options are dist, goodrate, badrate, or WoE")
    }
  }

iv.num<-
  function (df, x, y, verbose = FALSE, rcontrol = NULL) 
  {
    if (verbose) 
      cat("  Building rpart model", sep = "\n")
    if (is.null(rcontrol)) {
      rcontrol <- rpart.control(cp = 0.001, minbucket = nrow(df)/10)
    }
    model <- rpart(data = df, formula = as.integer(df[, y]) ~ 
                     df[, x], control = rcontrol)
    if (verbose) 
      cat("  Model finished", sep = "\n")
    model_where <- data.frame(node_id = model$where, obs_id = as.numeric(names(model$where)), 
                              stringsAsFactors = F)
    model_frame <- data.frame(model$frame, tree_node = rownames(model$frame), 
                              node_id = row(model$frame["var"]))
    if (verbose) 
      cat("  Sending model to tree parser", sep = "\n")
    log <- capture.output({
      rpart.rules <- path.rpart(model, rownames(model$frame)[model$frame$var == 
                                                               "<leaf>"])
    })
    tree_rules <- iv.parse.rpart.rule(x, rpart.rules)
    if (verbose) 
      cat(paste("  Rules parsed:", nrow(tree_rules)), "  Mapping nodes to data", 
          sep = "\n")
    if (verbose) 
      cat("    SQL Merge", sep = "\n")
    t <- sqldf("select obs_id, tr.class_label as tmp_iv_calc_label\n              from \n                  model_where mw\n                  join model_frame mf using (node_id)        \n                  join tree_rules tr using (tree_node)")
    t$tmp_iv_calc_label <- factor(t$tmp_iv_calc_label)
    if (verbose) 
      cat("    DF Merge", sep = "\n")
    df <- merge(df, t["tmp_iv_calc_label"], by = 0, all = TRUE)
    if (verbose) 
      cat("  Calling iv.str for nodes", sep = "\n")
    iv_data <- iv.str(df, "tmp_iv_calc_label", y)
    if (verbose) 
      cat("  Formatting output", sep = "\n")
    iv_data$variable <- x
    sqldf("select iv.*, tr.sql || woe as sql from iv_data iv join tree_rules tr on (iv.class=tr.class_label) order by tr.min")
  }

iv.parse.rpart.rule <- function(x,rpart.rules) {
  
  out_rule <- data.frame(min=numeric(0),max=numeric(0),
                         min_comp=character(0),max_comp=character(0),
                         class_label=character(0),sql=character(0),
                         tree_node = character(0),
                         stringsAsFactors=F)
  
  for (i in seq_along(rpart.rules)) {
    
    t1 <- (gsub("df[, x]",fixed=TRUE,replacement="",x=rpart.rules[[i]]))
    t1
    ge <- gsub(pattern=">=",x=(t1[which(grepl("*>=[:digit:]*",x=t1,))]),fixed=T,replacement="")
    g  <- gsub(pattern=">" ,x=(t1[which(grepl("*> [:digit:]*",x=t1,))]),fixed=T,replacement="")
    l  <- gsub(pattern="<" ,x=(t1[which(grepl("*< [:digit:]*",x=t1,))]),fixed=T,replacement="")
    le <- gsub(pattern="<=",x=(t1[which(grepl("*<=[:digit:]*",x=t1,))]),fixed=T,replacement="")
    
    ge <- ifelse(length(ge)==0,NA,max(as.numeric(ge)))
    g  <- ifelse(length(g) ==0,NA,max(as.numeric(g)))
    le <- ifelse(length(le)==0,NA,min(as.numeric(le)))
    l  <- ifelse(length(l) ==0,NA,min(as.numeric(l)))
    
    out_rule[i,"min"] <- if (!(is.na(ge) && is.na(g))) max(ge,g,na.rm=T) else NA
    out_rule[i,"max"] <- if (!(is.na(le) && is.na(l))) max(le,l,na.rm=T) else NA
    out_rule[i,"min_comp"] <- ifelse(!is.na(ge),">=",ifelse(!is.na(g),">",""))
    out_rule[i,"max_comp"] <- ifelse(!is.na(le),"<=",ifelse(!is.na(l),"<",""))
    out_rule[i,"class_label"] <- paste0(
      ifelse(!is.na(ge),"<","("),
      ifelse(is.na(out_rule$min[i]),"",out_rule$min[i]),
      ";",
      ifelse(is.na(out_rule$max[i]),"",out_rule$max[i]),
      ifelse(!is.na(le),">",")")
    )
    out_rule[i,"sql"] <- paste0("when ",
                                ifelse(is.na(out_rule$min[i]),"",paste(x,out_rule$min_comp[i],out_rule$min[i])),
                                ifelse (!is.na(out_rule$min[i]) && !is.na(out_rule$max[i])," AND ", ""),
                                ifelse(is.na(out_rule$max[i]),"",paste(x,out_rule$max_comp[i],out_rule$max[i])),
                                " then ")
    out_rule[i,"tree_node"] <- names(rpart.rules)[i]
  }
  
  out_rule
}


replace.woe<-function (df, iv, verbose = FALSE) 
{
  iv_df <- rbind.fill(iv)
  for (n in iv) {
    variable_name <- n[1, 1]
    variable_name_woe <- paste(variable_name, "_woe", sep = "")
    if (verbose) {
      cat(paste0("Var Name: ", variable_name, "\n"))
      cat(paste0("WOE Name: ", variable_name_woe, "\n"))
    }
    if (!("sql" %in% colnames(n))) {
      sqlstr <- paste("select df.*, iv.woe as ", variable_name_woe, 
                      " from df join iv_df as iv on (df.", variable_name, 
                      " = iv.class and iv.variable ='", variable_name, 
                      "')", sep = "")
      df <- sqldf(sqlstr, drv = "SQLite")
    }
    else {
      sqlstr_woe <- ifelse(paste(n$sql, collapse = " ") == 
                             "when  then 0.0" || any(is.infinite(n$woe)), 
                           "0", paste("case ", paste(n$sql, collapse = " "), 
                                      "else 0 end"))
      sqlstr <- paste("select df.*,", sqlstr_woe, " as", 
                      variable_name_woe, "from df")
      df <- sqldf(sqlstr, drv = "SQLite")
    }
  }
  df
}