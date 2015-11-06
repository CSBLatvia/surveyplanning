dom_optimal_allocation <- function(id, Dom, H, Y, indicator, sup_w,
                                 sup_cv, min_size=3, correction_before=FALSE,
                                 dataset=NULL){

  if (!is.logical(correction_before)) stop("'corrected_before' must be the logical value")
  if (length(min_size) != 1 | any(!is.numeric(min_size) | min_size < 0)) {
          stop("'min_size' must be a numeric value larger than 0")  }

  if (!is.null(dataset)) {
      dataset <- data.table(dataset)
      if (!is.null(id)) {
          if (min(id %in% names(dataset)) != 1) stop("'indicator' does not exist in 'data'!")
          if (min(id %in% names(dataset)) == 1) indicator <- dataset[, indicator, with = FALSE] }
      if (!is.null(Dom)) {
          if (min(Dom %in% names(dataset)) != 1) stop("'dom' does not exist in 'data'!")
          if (min(Dom %in% names(dataset)) == 1) Dom <- dataset[, Dom, with = FALSE] }
      if (!is.null(H)) {
          if (min(H %in% names(dataset)) != 1) stop("'H' does not exist in 'data'!")
          if (min(H %in% names(dataset)) == 1) strata <- dataset[, H, with = FALSE] }
      if (!is.null(Y)) {
          if (min(Y %in% names(dataset)) != 1) stop("'Y' does not exist in 'data'!")
          if (min(Y %in% names(dataset)) == 1) Y <- dataset[, Y, with = FALSE] }
      if (!is.null(indicator)) {
          if (min(indicator %in% names(dataset)) != 1) stop("'indicator' does not exist in 'data'!")
          if (min(indicator %in% names(dataset)) == 1) indicator <- dataset[, indicator, with = FALSE] }
      if (!is.null(sup_cv)) {
          if (min(sup_cv %in% names(dataset)) != 1) stop("'sup_cv' does not exist in 'data'!")
          if (min(sup_cv %in% names(dataset)) == 1) sup_cv <- dataset[, sup_cv, with = FALSE] }
      if (!is.null(sup_w)) {
          if (min(sup_w %in% names(dataset)) != 1) stop("'indicator' does not exist in 'data'!")
          if (min(sup_w %in% names(dataset)) == 1) sup_w <- dataset[, sup_w, with = FALSE] }
   }

  # Y
  Y <- data.table(Y)
  if (ncol(Y) != 1) stop("'Y' must be vector or 1 column data.frame, matrix, data.table")
  if (!is.numeric(Y[[1]])) stop("'Y' must be numerical")
  if (any(is.na(Y[[1]]))) stop("'Y' has unknown values")

  n <- nrow(Y)

  # id
  id <- data.table(id)
  if (any(is.na(id))) stop("'id' has unknown values")
  if (ncol(id) != 1) stop("'id' must be 1 column data.frame, matrix, data.table")
  if (nrow(id) != n) stop("'id' must be equal with 'Y' row count")
  if (any(duplicated(id))) stop("'id' are duplicate values")
  if (is.null(names(id))) stop("'id' must be colnames")


  # H
  H <- data.table(H)
  if (nrow(H) != n) stop("'H' length must be equal with 'Y' row count")
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(names(H))) stop("'H' must be colnames")

  # indicator
  indicator <- data.table(indicator)
  if (any(is.na(indicator))) stop("'indicator' has unknown values")
  if (nrow(indicator) != n) stop("'indicator' length must be equal with 'Y' row count")
  if (ncol(indicator) != 1) stop("'indicator' must be vector or 1 column data.frame, matrix, data.table")
  if (!is.numeric(indicator[[1]])) stop("'indicator' must be numerical")
  if (any(is.na(indicator[[1]]))) stop("'indicator' has unknown values")
  indicator1 <- indicator[,.N, keyby = indicator][[1]]
  if (any(!(indicator1 %in% c(0,1)))) stop("'indicator' must be only two values - 0,1")


  # sup_w
  sup_w <- data.table(sup_w)
  if (nrow(sup_w) != n) stop("'sup_w' must be equal with 'Y' row count")
  if (ncol(sup_w) != 1) stop("'sup_w' must be vector or 1 column data.frame, matrix, data.table")
  if (!is.numeric(sup_w[[1]])) stop("'sup_w' must be numerical")
  if (any(is.na(sup_w[[1]]))) stop("'sup_w' has unknown values")

  # sup_cv
  sup_cv <- data.table(sup_cv)
  if (nrow(sup_cv) != n) stop("'sup_cv' must be equal with 'Y' row count")
  if (ncol(sup_cv) != 1) stop("'sup_cv' must be vector or 1 column data.frame, matrix, data.table")
  if (!is.numeric(sup_cv[[1]])) stop("'sup_cv' must be numerical")
  if (any(is.na(sup_cv[[1]]))) stop("'sup_cv' has unknown values")


  # Dom
  if (!is.null(Dom)) {
    Dom <- data.table(Dom)
    if (any(duplicated(names(Dom)))) 
           stop("'Dom' are duplicate column names: ", 
                 paste(names(Dom)[duplicated(names(Dom))], collapse = ","))
    if (nrow(Dom) != n) stop("'Dom' and 'Y' must be equal row count")
    if (any(is.na(Dom))) stop("'Dom' has unknown values")
    if (is.null(names(Dom))) stop("'Dom' must be colnames")
    Dom[, (names(Dom)):=lapply(.SD, as.character)]
  }

  r <- data.table(id, Dom, H, var, indicator, sup_w, sup_cv)
  sd_Y <- apj <- min_apj <- poph <- nh <- NULL
  samplsize <- izl_str <- var_est <- variance <- NULL
  cv <- design_weights <- sum_Y <- izl100 <- NULL

  id1 <- names(id)
  Dom_agg <- data.table(unique(Dom))
  dom1 <- names(Dom_agg)
  strata1 <- names(H)
  Y1 <- names(Y)
  indicator1 <- names(indicator)
  sup_w1 <- names(sup_w)
  . <- namesr <- names(r)

  for (j in 1:nrow(Dom_agg)) {
          D <- Dom_agg[j,][rep(1,nrow(Dom)),]
          d0 <- r[(rowSums(Dom == D) == ncol(Dom))]
          d1 <- d0[indicator == 0]

          #the part of domain, where select all units 100%
          n_100 <- d0[get(indicator1) == 1, .N]

          #the part, where create sample
          nd <- d0[get(indicator1) == 0, .N]

          if (nd != 0) {
                aa <- d1[, .(poph = .N,
                            sd_Y = sd(Y1, na.rm = TRUE)), keyby = strata1]
                aa[is.na(sd_Y), sd_Y:=  0]

                aa[, apj:=min_apj]
                aa[apj>poph, apj:= poph]
                A <- aa[, sum(apj)][1]:aa[, sum(poph)][1]
                l <- length(A)

                a1 <- d0[ , .(poph = .N,
                              sd_Y = sd(Y1, na.rm = TRUE),
                              sum_Y = sum(Y1, na.rm = TRUE),
                              izl_str = sum(1 - get(indicator1), na.rm = TRUE),
                              sup_w = mean(sup_w1, na.rm = TRUE),
                              sup_cv = mean(sup_w1, na.rm = TRUE)), keyby = strata1]
                a1[is.na(sd_Y), sd_Y:=  0]
                a1[, samplsize:=  poph-izl_str]

                aa[, apj:=min_size]
                aa[apj > poph, apj:=  poph]

                for (k in 1:l) {
                         aa[, nh:= round(A[k]*(poph*sd_Y/sum(poph*sd_Y)))]
                         t1 <- aa[, c(strata1, "nh"), with = FALSE]
                         a2 <- merge(a1, t1, all.x = TRUE)
                         a2[is.na(nh), nh:=0]
                         a2[, nh:=nh + samplsize]
                         a2[nh < min_size, nh:= min_apj]
                         a2[nh > poph, nh:= poph]

                         a2[(poph/nh > sup_w) & (correction_before), nh:= round(poph/sup_w)]
                         a2[(poph/nh > sup_w) & (correction_before), nh:= nh+1]

                         a2[, var_est:=poph^2/nh*(1-nh/poph)*sd_Y^2]

                         if (sum(a2[["sum_Y"]]) == 0 | sqrt(sum(a2[["var_est"]]))/sum(a2[["sum_Y"]]) < sup_cv/100) break
                    }
                d <- merge(d0, a2[, c(strata1, "poph", "nh"), with = FALSE], all = TRUE, by = strata1)
              } else {
                   a1 <- d0[, .(poph = .N, nh = .N), by = strata1]
                   d <- merge(d0, a1, keyby = strata1, all = TRUE, by = strata1)
           }
        r3 <- d[, c(namesr, "poph", "nh"), with = FALSE]
        setkeyv(r3, names(id))

        r3[(poph/nh > sup_w) & (!correction_before), nh:=round(poph/sup_w)]
        r3[(poph/nh > sup_w) & (!correction_before), nh:=nh+1]
     }

     a1 <- r3[, .(nh = mean(nh, na.rm = TRUE),
                  poph = .N,
                  sum_Y = sum(Y1, na.rm = TRUE),
                  variance = var(Y1, na.rm = TRUE)), keyby = c(strata1,  dom1)]
     a1[, var_est:=poph^2/nh*(1-nh/poph)*variance]
     a1[, cv:= 100*sqrt(variance)/sum_Y]

     a2 <- a1[, .(nh = sum(nh, na.rm = TRUE),
                  poph = sum(poph),
                  sum_Y = sum(sum_Y, na.rm = TRUE),
                  variance = sum(variance, na.rm = TRUE)), keyby = dom1]
     a2[, cv:=100*sqrt(variance)/sum_Y]

     a3 <- a2[, .(nh = sum(nh, na.rm = TRUE),
                  poph = sum(poph),
                  sum_Y = sum(sum_Y, na.rm = TRUE),
                  variance = sum(variance, na.rm = TRUE))]
     a3[, cv:=100*sqrt(variance)/sum_Y]

     r4 <- r3[, .(nh = mean(nh, na.rm = TRUE),
                  poph = .N), keyby = strata1]

     # Check is any strata, where nh>Nh
     test <- r4[nh > poph]
     if (nrow(test) == 0) test <- 0

     # sample size
     r4 <- data.table(r3)
     r4 <- rbind(r4[1], r4)
     r4[1,(c("index1", "indicator")):=0]

     apj_sum <- r4[indicator==0, .(izl100=sum(indicator, na.rm = TRUE)), keyby=c(strata1, dom1)]

     dom_strata_size <- r4[, lapply(.SD, mean, na.rm = TRUE), by=c(strata1, dom1), .SDcols=c("sup_w", "poph", "nh")]
     dom_strata_size <- merge(dom_strata_size, apj_sum, all.x =TRUE, by = c(strata1, dom1))
     dom_strata_size[is.na(izl100), izl100:=0]
     dom_strata_size[, design_weights:=poph/nh]
     dom_size <- dom_strata_size[, lapply(.SD, sum, na.rm = TRUE),
                                    keyby = dom1, .SDcols = c("poph", "nh", "izl100")]
     dom_size[, design_weights:=poph/nh]
     sample_siz <- dom_size[, lapply(.SD, sum), .SDcols=c("poph", "nh", "izl100")]

     return (list(data = r3,
                  nh_larger_then_Nh = test,
                  dom_strata_size = dom_strata_size,
                  dom_size = dom_size,
                  size = sample_siz,
                  dom_strata_expected_precision = a1,
                  dom_expected_precision = a2,
                  total_expected_precision = a3))
}
