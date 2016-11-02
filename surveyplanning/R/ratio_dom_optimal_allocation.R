
ratio_dom_optimal_allocation <- function(H, Dom, poph, 
                                         Rh = NULL, deffh = NULL,
                                         se_max = 0.5, prop = 0.5,
                                         min_size = 3, step = 1, 
                                         dataset = NULL) {
  
  if (length(se_max) != 1 | any(!is.numeric(se_max) | se_max <= 0)) {
                  stop("'se_max' must be a numeric value larger than 0")  }
  if (length(prop) != 1 | any(!is.numeric(se_max) | prop < 0 | prop > 1 )) {
                  stop("'prop' must be a numeric value larger than 0 and smaller than 1")  }
  if( length(step) != 1 | !any(step > 0 | abs(step - round(step)) < .Machine$double.eps)) 
                  stop("'step' must be a integer value greater than 0")
  if( length(min_size) != 1 | !any(min_size > 0 | abs(min_size - round(min_size)) < .Machine$double.eps)) 
                  stop("'min_size' must be a integer value greater than 0")

  if (!is.null(dataset)) {
      dataset <- data.table(dataset)
      if (!is.null(Dom)) {
          if (min(Dom %in% names(dataset)) != 1) stop("'dom' does not exist in 'dataset'!")
          if (min(Dom %in% names(dataset)) == 1) Dom <- dataset[, Dom, with = FALSE] }
      if (!is.null(H)) {
          if (min(H %in% names(dataset)) != 1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset)) == 1) H <- dataset[, H, with = FALSE] }
      if(!is.null(poph)) {
        if (min(poph %in% names(dataset))!=1) stop("'poph' does not exist in 'dataset'!")
        if (min(poph %in% names(dataset))==1) poph <- dataset[, poph, with=FALSE] }
      if(!is.null(Rh)) {
          if (min(Rh %in% names(dataset))!=1) stop("'Rh' does not exist in 'dataset'!")
          if (min(Rh %in% names(dataset))==1) Rh <- dataset[, Rh, with=FALSE] }
      if(!is.null(deffh)) {
          if (min(deffh %in% names(dataset))!=1) stop("'deffh' does not exist in 'dataset'!")
          if (min(deffh %in% names(dataset))==1) deffh <- dataset[, deffh, with=FALSE] }
  }

  # poph
  poph <- data.table(poph)
  if (ncol(poph) != 1) stop("'poph' must be vector or 1 column data.frame, matrix, data.table")
  if (any(is.na(poph[[1]]))) stop("'poph' has unknown values")
  if (!is.numeric(poph[[1]])) stop("'poph' must be numerical")
  n <- nrow(poph)

  H <- data.table(H)
  if (nrow(H) != n) stop("'H' length must be equal with 'poph' row count")
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(names(H))) stop("'H' must be colnames")

  if (is.null(Rh)) Rh <- rep(1, n)
  Rh <- data.frame(Rh)
  if (nrow(Rh) != n) stop("'Rh' must be equal with 'poph' row count")
  if (ncol(Rh) != 1) stop("'Rh' must be vector or 1 column data.frame, matrix, data.table")
  Rh <- Rh[, 1]
  if (!is.numeric(Rh)) stop("'Rh' must be numerical")
  if (any(is.na(Rh))) stop("'Rh' has unknown values")

  # deffh
  if (is.null(deffh)) deffh <- rep(1, n)
  deffh <- data.table(deffh, check.names = TRUE)
  if (nrow(deffh) != n) stop("'deffh' length must be equal with 'poph' row count")
  if (ncol(deffh) != ncol(poph)) stop("'deffh' and 'poph' must be equal column count")
  if (any(is.na(deffh))) stop("'deffh' has unknown values")
  if (!all(sapply(deffh, is.numeric))) stop("'deffh' must be numeric values")
  if (is.null(names(deffh))) stop("'deffh' must be colnames")

  Dom <- data.table(Dom)
  if (any(duplicated(names(Dom)))) 
         stop("'Dom' are duplicate column names: ", 
               paste(names(Dom)[duplicated(names(Dom))], collapse = ","))
  if (nrow(Dom) != n) stop("'Dom' and 'poph' must be equal row count")
  if (any(is.na(Dom))) stop("'Dom' has unknown values")
  if (is.null(names(Dom))) stop("'Dom' must be colnames")
  Dom[, (names(Dom)) := lapply(.SD, as.character)]

  s2h <- NS <- domNS <- pop_Dom <- se <- n_neyman <- NULL
  sample_size <- nrh <- strata_var <- dom_var <- NULL
  sample_size_Dom <- pop <- . <- NULL

  nRh <- names(Rh)
  ndeffh <- names(Rh)
  nDom <- names(Dom)
  
  datah <- data.table(H, Dom, poph, Rh, deffh)
  datah[, s2h := ifelse(poph == 1, 0, poph / (poph - 1) * prop * (1 - prop))]
  
  datah[, NS := poph * sqrt(s2h)]
  datah[, domNS := sum(NS), by = nDom]
  datah[, pop_Dom := sum(poph), by = nDom]
    
  datah[, n := 0]
  datah[, se := 1]
   
  while (any(datah$se > se_max)){      
      datah[se > se_max, n := n + step]
      datah[se > se_max, n_neyman := ifelse(domNS != 0, n * (NS / domNS), 0)]     
      datah[se > se_max, sample_size := pmin(pmax(min_size, trunc(n_neyman)), poph)]     
      datah[se > se_max, nrh := round(sample_size * Rh)]
      datah[se > se_max, strata_var := poph ^ 2 * (1 - nrh / poph ) / nrh * s2h * deffh]
      datah[se > se_max, dom_var := sum(strata_var), by = nDom]
      datah[se > se_max, se := sqrt(dom_var) / pop_Dom]
   }

  aggr_Dom <- datah[, .(pop_Dom = mean(pop_Dom), sample_size_Dom = sum(sample_size)), keyby = nDom]
  aggr_Dom[, sample_size := sum(sample_size_Dom)][, pop := sum(pop_Dom)]
  
  list(datah = datah, aggr_Dom = aggr_Dom[])
  
 }