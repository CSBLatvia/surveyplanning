recode.NA <- function(DT, cols = seq_len(ncol(DT)), fixed_value = 0) {
  if (!is.numeric(fixed_value)) stop("'fixed_value' must be numeric")

  for (j in cols) if (is.numeric(DT[[j]]))
    set(DT, which(is.na(DT[[j]])), j, ifelse(is.integer(DT[[j]]),
                                      as.integer(fixed_value), as.numeric(fixed_value)))
}