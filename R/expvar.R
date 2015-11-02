expvar <- function(Yh, H, s2h, nh, poph, Rh = NULL, deffh = NULL, Dom = NULL,
                   dataset = NULL) {

  if(!is.null(dataset)) {
      dataset <- data.table(dataset)
      if (min(Yh %in% names(dataset))!=1) stop("'Yh' does not exist in 'dataset'!")
      if (min(Yh %in% names(dataset))==1)  Yh <- dataset[, Yh, with=FALSE]

      if(!is.null(H)) {
          if (min(H %in% names(dataset))!=1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset))==1) H <- dataset[, H, with=FALSE] }
      if(!is.null(s2h)) {
          if (min(s2h %in% names(dataset))!=1) stop("'s2h' does not exist in 'dataset'!")
          if (min(s2h %in% names(dataset))==1) s2h <- dataset[, s2h, with=FALSE]      }
      if(!is.null(nh)) {
          if (min(nh %in% names(dataset))!=1) stop("'nh' does not exist in 'dataset'!")
          if (min(nh %in% names(dataset))==1) nh <- dataset[, nh, with=FALSE] }
      if(!is.null(poph)) {
          if (min(poph %in% names(dataset))!=1) stop("'poph' does not exist in 'dataset'!")
          if (min(poph %in% names(dataset))==1) poph <- dataset[, poph, with=FALSE] }
      if(!is.null(Rh)) {
          if (min(Rh %in% names(dataset))!=1) stop("'Rh' does not exist in 'dataset'!")
          if (min(Rh %in% names(dataset))==1) Rh <- dataset[, Rh, with=FALSE] }
      if(!is.null(deffh)) {
          if (min(deffh %in% names(dataset))!=1) stop("'deffh' does not exist in 'dataset'!")
          if (min(deffh %in% names(dataset))==1) deffh <- dataset[, deffh, with=FALSE] }
      if (!is.null(Dom)) {
          if (min(Dom %in% names(dataset))!=1) stop("'Dom' does not exist in 'data'!")
          if (min(Dom %in% names(dataset))==1) Dom <- dataset[, Dom, with=FALSE]  }
    }

  # Yh
  Yh <- data.table(Yh, check.names=TRUE)
  n <- nrow(Yh)
  m <- ncol(Yh)
  if (any(is.na(Yh))) stop("'Yh' has unknown values")
  if (!all(sapply(Yh, is.numeric))) stop("'Yh' must be all numeric values")
  if (is.null(names(Yh))) stop("'Yh' must be colnames")
  Yh <- data.table(sapply(Yh, as.numeric))

  s2h <- data.table(s2h, check.names=TRUE)
  if (nrow(s2h) != n) stop("'s2h' length must be equal with 'Yh' row count")
  if (ncol(s2h) != m) stop("'s2h' and 'Yh' must be equal column count")
  if (any(is.na(s2h))) stop("'s2h' has unknown values")
  if (!all(sapply(s2h, is.numeric))) stop("'S2h' must be numeric values")
  if (is.null(names(s2h))) stop("'s2h' must be colnames")

  # H
  H <- data.table(H)
  if (nrow(H) != n) stop("'H' length must be equal with 'Yh' row count")
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(names(H))) stop("'H' must be colnames")

  # nh
  nh <- data.frame(nh)
  if (nrow(nh) != n) stop("'nh' must be equal with 'Yh' row count")
  if (ncol(nh) != 1) stop("'nh' must be vector or 1 column data.frame, matrix, data.table")
  nh <- nh[,1]
  if (!is.numeric(nh)) stop("'nh' must be numerical")
  if (any(is.na(nh))) stop("'nh' has unknown values")

  # poph
  poph <- data.frame(poph)
  if (nrow(poph) != n) stop("'poph' must be equal with 'Yh' row count")
  if (ncol(poph) != 1) stop("'poph' must be vector or 1 column data.frame, matrix, data.table")
  poph <- poph[,1]
  if (!is.numeric(poph)) stop("'poph' must be numerical")
  if (any(is.na(poph))) stop("'poph' has unknown values")

  # Rh
  if (is.null(Rh)) Rh <- rep(1, n)
  Rh <- data.frame(Rh)
  if (nrow(Rh) != n) stop("'Rh' must be equal with 'Yh' row count")
  if (ncol(Rh) != 1) stop("'Rh' must be vector or 1 column data.frame, matrix, data.table")
  Rh <- Rh[, 1]
  if (!is.numeric(Rh)) stop("'Rh' must be numerical")
  if (any(is.na(Rh))) stop("'Rh' has unknown values")

  if (!is.null(deffh)) {
    deffh <- data.table(deffh, check.names=TRUE)
    if (nrow(deffh) != n) stop("'deffh' length must be equal with 'Yh' row count")
    if (ncol(deffh) != m) stop("'deffh' and 'Yh' must be equal column count")
    if (any(is.na(deffh))) stop("'deffh' has unknown values")
    if (!all(sapply(deffh, is.numeric))) stop("'deffh' must be numeric values")
    if (is.null(names(deffh))) stop("'deffh' must be colnames")
   }

  # Dom
  if (!is.null(Dom)) {
    Dom <- data.table(Dom)
    if (any(duplicated(names(Dom))))
           stop("'Dom' are duplicate column names: ",
                 paste(names(Dom)[duplicated(names(Dom))], collapse = ","))
    if (nrow(Dom) != n) stop("'Dom' and 'Y' must be equal row count")
    if (any(is.na(Dom))) stop("'Dom' has unknown values")
    if (is.null(names(Dom))) stop("'Dom' must be colnames")
    Dom <- Dom[, lapply(.SD, as.character), .SDcols = names(Dom)]
  }

  variable <- nrh <- se <- cv <- estim <- NULL

  nh <- data.table(melt(data.table(H, nh), id=c(names(H))))
  nh[, variable:=NULL]
  setnames(nh, "value", "nh")
  setkeyv(nh, names(H))

  Rh <- data.table(melt(data.table(H, Rh), id=c(names(H))))
  Rh[, variable:=NULL]
  setnames(Rh, "value", "Rh")
  setkeyv(Rh, names(H))
  resulth <- merge(nh, Rh, all=TRUE)

  poph <- data.table(melt(data.table(H, poph), id=c(names(H))))
  poph[, variable:=NULL]
  setnames(poph, "value", "poph")
  setkeyv(poph, names(H))

  resulth <- merge(resulth, poph, all=TRUE)

  setnames(s2h, names(s2h), names(Yh))
  s2h <- data.table(melt(data.table(H, s2h), id=c(names(H))))
  setnames(s2h, "value", "s2h")
  setkeyv(s2h, c(names(H), "variable"))
  resulth <- merge(s2h, resulth, all=TRUE)
  setkeyv(resulth, c(names(H), "variable"))

  if (!is.null(deffh)) {
      setnames(deffh, names(deffh), names(Yh))
      deffh <- data.table(melt(data.table(H, deffh), id=c(names(H))))
      setnames(deffh, "value", "deffh")
      setkeyv(deffh, c(names(H), "variable"))
      resulth <- merge(deffh, resulth, all=TRUE)
  }

  if (is.null(deffh)) resulth[, deffh:=1]

  domH <- H
  if (!is.null(Dom)) domH <- data.table(Dom, domH)
  Yh <- data.table(melt(data.table(domH, Yh), id=c(names(domH))))
  setnames(Yh, "value", "estim")
  setkeyv(Yh, c(names(H), "variable"))
  resulth <- merge(Yh, resulth, all=TRUE)

  resulth[, nrh := round(nh * Rh)]
  resulth[nrh < 1, nrh:=1]
  resulth[, var := poph ^ 2 * (1 - nrh / poph) / nrh * s2h * deffh]
  resulth[!is.nan(var), se:=sqrt(var)]
  resulth[is.nan(var) | is.na(var), se := NA]
  resulth[, cv := 100 * se / estim]

  domH <- "variable"
  if (!is.null(Dom)) domH <- c(names(Dom), domH)
  result <- resulth[, lapply(.SD, sum, na.rm = TRUE), keyby = domH,
                    .SDcols = c("poph", "nh", "nrh", "estim", "var")]
  result[, se := sqrt(var)]
  result[, cv := 100 * se / estim]

  list(resultH = resulth,
       result = result)
}
