##################

optsize <- function(H, n, poph, s2h = NULL, Rh = NULL, dataset = NULL) {

  ### Checking
  if(any(abs(n - round(n)) >= .Machine$double.eps | n < 0)) stop("'n' must be a integer value greater than 0")

  if(!is.null(dataset)) {
      dataset <- data.table(dataset)
      if(!is.null(H)) {
          if (min(H %in% names(dataset))!=1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset))==1) H <- dataset[, H, with=FALSE] }
      if(!is.null(s2h)) {
          if (min(s2h %in% names(dataset))!=1) stop("'s2h' does not exist in 'dataset'!")
          if (min(s2h %in% names(dataset))==1) s2h <- dataset[, s2h, with=FALSE] }
      if(!is.null(poph)) {
          if (min(poph %in% names(dataset))!=1) stop("'poph' does not exist in 'dataset'!")
          if (min(poph %in% names(dataset))==1) poph <- dataset[, poph, with=FALSE] }
      if(!is.null(Rh)) {
          if (min(Rh %in% names(dataset))!=1) stop("'Rh' does not exist in 'dataset'!")
          if (min(Rh %in% names(dataset))==1) Rh <- dataset[, Rh, with=FALSE] }
    }

  # s2h
  if (is.null(s2h)) s2h <- rep(1, nrow(H))
  s2h <- data.table(s2h, check.names=TRUE)
  m <- ncol(s2h)
  if (any(is.na(s2h))) stop("'s2h' has unknown values")
  if (!all(sapply(s2h, is.numeric))) stop("'s2h' must be numeric values")
  if (is.null(names(s2h))) stop("'s2h' must be colnames")

  # H
  H <- data.table(H)
  if (nrow(H) != nrow(s2h)) stop("'H' length must be equal with 'S2h' row count")
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(names(H))) stop("'H' must be colnames")

  # poph
  poph <- data.frame(poph)
  if (nrow(poph) != nrow(s2h)) stop("'poph' must be equal with 's2h' row count")
  if (ncol(poph) != 1) stop("'poph' must be vector or 1 column data.frame, matrix, data.table")
  poph <- poph[,1]
  if (!is.numeric(poph)) stop("'poph' must be numerical")
  if (any(is.na(poph))) stop("'poph' has unknown values")

  # Rh
  if (is.null(Rh)) Rh <- rep(1, nrow(s2h))
  Rh <- data.frame(Rh)
  if (nrow(Rh) != nrow(s2h)) stop("'Rh' must be equal with 's2h' row count")
  if (ncol(Rh) != 1) stop("'Rh' must be vector or 1 column data.frame, matrix, data.table")
  Rh <- Rh[, 1]
  if (!is.numeric(Rh)) stop("'Rh' must be numerical")
  if (any(is.na(Rh))) stop("'Rh' has unknown values")

  variable <- pnh <- nh <- NULL

  Rh <- melt(data.table(H, Rh), id=c(names(H)))
  Rh[, variable := NULL]
  setnames(Rh, "value", "Rh")
  setkeyv(Rh, names(H))

  poph <- melt(data.table(H, poph), id=c(names(H)))
  poph[, variable:=NULL]
  setnames(poph, "value", "poph")
  setkeyv(poph, names(H))

  resulth <- merge(Rh, poph, all=TRUE)

  s2h <- melt(data.table(H, s2h), id=c(names(H)))
  setnames(s2h, "value", "s2h")
  setkeyv(s2h, c(names(H), "variable"))
  setkeyv(resulth, c(names(H)))
  resulth <- merge(s2h, resulth, all=TRUE)

  resulth[, pnh := poph * sqrt(s2h / Rh)]
  resulth[, nh := n * pnh / sum(pnh), by = "variable"]
  resulth[, pnh:=NULL]
  return(resulth)
}
