#' Optimal sample size allocation
#'
#' The function computes optimal sample size allocation over strata and domain for population.
#'
#' @param id Variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, values are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.
#' @param H The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Y Variable of interest. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param Rh The expected response rate in each stratum (optional). If not defined, it is assumed to be 1 in each stratum (full-response). Object convertible to one-column \code{data.table}, variable name as character, or column number.
#' @param deffh The expected design effect for the estimate of variable (optional). If not defined, it is assumed to be 1 for each variable in each stratum. If is defined, then variables is defined the same arrangement as \code{Yh}. Object convertible to \code{data.table}, variable name as character vector, or column numbers.
#' @param indicator Variable for detection fully surveyed units. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param sup_w Variable for weight limit in domain of stratum. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param sup_cv Variable for maximum coeficient of variation (CV) in percentage for domain. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param correction_before by default FALSE; correction of sample size is made before ending, if true, correction of sample size is made at the end.
#' @param min_size A numeric value for sample size.
#' @param dataset Optional survey data object convertible to \code{data.table} with one row for each stratum.
#'
#' @return A list with eights data objects:\cr
#' \item{data}{An object as \code{data.table}, with variables:
#'   \code{id} - variable with unit ID codes, \cr
#'   \code{Dom} - optional variables used to define population domains, \cr
#'   \code{H} - the unit stratum variable, \cr
#'   \code{Y} - variable of interest, \cr
#'   \code{Rh} - the expected response rate in each stratum, \cr
#'   \code{deffh} - the expected design effect, \cr
#'   \code{indicator} - variable for full surveys, \cr
#'   \code{sup_w} - variable for weight limit in domain of stratum, \cr
#'   \code{sup_cv} - Variable for maximum coeficient of variation, \cr
#'   \code{poph} - population size, \cr
#'   \code{nh} - sample size . }
#'
#'\item{nh_larger_then_Nh}{An object as \code{data.table}, with variables: \cr
#'   \code{H} - the stratum variable, \cr
#'   \code{nh} - sample size,
#'   \code{poph} - population size.}
#'
#'\item{dom_strata_size}{An object as \code{data.table}, with variables: \cr
#'   \code{H} - the unit stratum variable, \cr
#'   \code{Dom} - optional variables used to define population domains, \cr
#'   \code{sup_w} - variable for weight limit in domain of stratum, \cr
#'   \code{poph} - population size, \cr
#'   \code{nh} - sample size, \cr
#'   \code{sample100} - sample size for fully surveyed units, \cr
#'   \code{design_weights} - design weigts. }
#'
#'\item{dom_size}{An object as \code{data.table}, with variables: \cr
#'   \code{Dom} - optional variables used to define population domains, \cr
#'   \code{poph} - population size, \cr
#'   \code{nh} - sample size, \cr
#'   \code{sample100} - sample size for fully surveyed units, \cr
#'   \code{design_weights} - design weigts. }
#'
#'\item{size}{An object as \code{data.table}, with variables: \cr
#'   \code{poph} - population size, \cr
#'   \code{nh} - sample size, \cr
#'   \code{sample100} - sample size for fully surveyed units. }
#'
#'\item{dom_strata_expected_precision}{An object as \code{data.table}, with variables: \cr
#'   \code{H} - stratum, \cr
#'   \code{variable} - the name of variable of interest, \cr
#'   \code{estim} - total value, \cr
#'   \code{deffh} - the expected design effect, \cr
#'   \code{s2h} - population variance \eqn{S^2}, \cr
#'   \code{nh} - sample size, \cr
#'   \code{Rh} - the expected response rate, \cr
#'   \code{deffh} - the expected design effect, \cr
#'   \code{poph} - population size, \cr
#'   \code{nrh} - expected number of respondents, \cr
#'   \code{var} - expected variance, \cr
#'   \code{se} - expected standard error, \cr
#'   \code{cv} - expected coeficient of variance.}
#'
#'\item{dom_expected_precision}{An object as \code{data.table}, with variables: \cr
#'   \code{Dom} - domain, \cr
#'   \code{variable} - the name of variable of interest, \cr
#'   \code{poph} - the population size, \cr
#'   \code{nh} - sample size, \cr
#'   \code{nrh} - expected number of respondents, \cr
#'   \code{estim} - total value, \cr
#'   \code{var} - the expected variance, \cr
#'   \code{se} - the expected standart error, \cr
#'   \code{cv} - the expected coeficient of variance.}
#'
#'\item{ total_expected_precision}{An object as \code{data.table}, with variables: \cr
#'   \code{variable} - the name of variable of interest, \cr
#'   \code{poph} - the population size, \cr
#'   \code{nh} - sample size, \cr
#'   \code{nrh} - expected number of respondents, \cr
#'   \code{estim} - total value, \cr
#'   \code{var} - the expected variance, \cr
#'   \code{se} - the expected standart error, \cr
#'   \code{cv} - the expected coeficient of variance.}
#'
#' @seealso \code{\link{expsize}}, \code{\link{optsize}}, \code{\link{prop_dom_optimal_allocation}}
#' @keywords surveysampling
#'
#' @examples
#' library(laeken)
#' library(data.table)
#' data(ses)
#' data <- data.table(ses)
#' data[, H := paste(location, NACE1, size, sep = "_")]
#' data[, id := .I]
#' data[, full := 0]
#' data[, sup_cv := 10]
#' data[, sup_w := 20]
#' #vars <- dom_optimal_allocation(id = "id", dom = "sex",
#' #                                H = "H", Y = "earnings",
#' #                                indicator = "full",
#' #                                sup_w = "sup_w",
#' #                                sup_cv = "sup_cv",
#' #                                min_size = 3,
#' #                                correction_before = FALSE,
#' #                                dataset = data)
#' #                                dataset=data)
#' #vars
#' @import data.table laeken
#' @export dom_optimal_allocation

dom_optimal_allocation <- function(id, Dom, H, Y, Rh = NULL,
                                   deffh = NULL, indicator,
                                   sup_w, sup_cv, min_size = 3,
                                   correction_before = FALSE,
                                   dataset = NULL){

  if (!any(is.logical(correction_before)) | length(correction_before) != 1)
                            stop("'corrected_before' must be the logical value")
  if( length(min_size)!=1 | !any(min_size > 0 | abs(min_size - round(min_size)) < .Machine$double.eps))
                            stop("'min_size' must be a integer value greater than 0")

  if (!is.null(dataset)) {
      dataset <- data.table(dataset)
      if (!is.null(id)) {
          if (min(id %in% names(dataset)) != 1) stop("'id' does not exist in 'dataset'!")
          if (min(id %in% names(dataset)) == 1) id <- dataset[, id, with = FALSE] }
      if (!is.null(Dom)) {
          if (min(Dom %in% names(dataset)) != 1) stop("'dom' does not exist in 'dataset'!")
          if (min(Dom %in% names(dataset)) == 1) Dom <- dataset[, Dom, with = FALSE] }
      if (!is.null(H)) {
          if (min(H %in% names(dataset)) != 1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset)) == 1) H <- dataset[, H, with = FALSE] }
      if (!is.null(Y)) {
          if (min(Y %in% names(dataset)) != 1) stop("'Y' does not exist in 'dataset'!")
          if (min(Y %in% names(dataset)) == 1) Y <- dataset[, Y, with = FALSE] }
      if(!is.null(Rh)) {
          if (min(Rh %in% names(dataset))!=1) stop("'Rh' does not exist in 'dataset'!")
          if (min(Rh %in% names(dataset))==1) Rh <- dataset[, Rh, with=FALSE] }
      if(!is.null(deffh)) {
          if (min(deffh %in% names(dataset))!=1) stop("'deffh' does not exist in 'dataset'!")
          if (min(deffh %in% names(dataset))==1) deffh <- dataset[, deffh, with=FALSE] }
      if (!is.null(indicator)) {
          if (min(indicator %in% names(dataset)) != 1) stop("'indicator' does not exist in 'dataset'!")
          if (min(indicator %in% names(dataset)) == 1) indicator <- dataset[, indicator, with = FALSE] }
      if (!is.null(sup_cv)) {
          if (min(sup_cv %in% names(dataset)) != 1) stop("'sup_cv' does not exist in 'dataset'!")
          if (min(sup_cv %in% names(dataset)) == 1) sup_cv <- dataset[, sup_cv, with = FALSE] }
      if (!is.null(sup_w)) {
          if (min(sup_w %in% names(dataset)) != 1) stop("'sup_w' does not exist in 'dataset'!")
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
  indicator1 <- indicator[, .N, keyby = indicator][[1]]
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

  # Rh
  if (is.null(Rh)) Rh <- rep(1, n)
  Rh <- data.table(Rh)
  if (nrow(Rh) != n) stop("'Rh' must be equal with 'Yh' row count")
  if (ncol(Rh) != 1) stop("'Rh' must be vector or 1 column data.frame, matrix, data.table")
  if (!is.numeric(Rh[[1]])) stop("'Rh' must be numerical")
  if (any(is.na(Rh[[1]]))) stop("'Rh' has unknown values")

  # deffh
  if (is.null(deffh)) deffh <- rep(1, n)
  deffh <- data.table(deffh, check.names = TRUE)
  if (nrow(deffh) != n) stop("'deffh' length must be equal with 'Yh' row count")
  if (ncol(deffh) != ncol(Y)) stop("'deffh' and 'Y' must be equal column count")
  if (any(is.na(deffh))) stop("'deffh' has unknown values")
  if (!all(sapply(deffh, is.numeric))) stop("'deffh' must be numeric values")
  if (is.null(names(deffh))) stop("'deffh' must be colnames")

  # Dom
  if (!is.null(Dom)) {
    Dom <- data.table(Dom)
    if (any(duplicated(names(Dom))))
           stop("'Dom' are duplicate column names: ",
                 paste(names(Dom)[duplicated(names(Dom))], collapse = ","))
    if (nrow(Dom) != n) stop("'Dom' and 'Y' must be equal row count")
    if (any(is.na(Dom))) stop("'Dom' has unknown values")
    if (is.null(names(Dom))) stop("'Dom' must be colnames")
    Dom[, (names(Dom)):= lapply(.SD, as.character)]
  }

  poph_sample <- sample100 <- var_est <- s2_Y <- NULL
  apj <- min_apj <- poph <- nh <- index__1 <- NULL
  . <- cv_check <- design_weights <- sum_Y <- NULL
  cv <- pnh <- vars <- sum_apj <- sum_poph <- NULL

  if (is.null(Dom)) Dom <- data.table(dom = rep(1, n))

  r <- data.table(id, Dom, H, Y, Rh, deffh, indicator, sup_w, sup_cv)

  id1 <- names(id)
  Dom_agg <- data.table(unique(Dom))
  dom1 <- names(Dom_agg)
  strata1 <- names(H)
  Y1 <- names(Y)
  Rh1 <- names(Rh)
  deffh1 <- names(deffh)
  indicator1 <- names(indicator)
  sup_w1 <- names(sup_w)
  sup_cv1 <- names(sup_cv)

  aa <- r[ , .(poph = .N,
               s2_Y = var(get(Y1), na.rm = TRUE),
               sum_Y = sum(as.numeric(get(Y1)), na.rm = TRUE),
               poph_sample = sum(1 - get(indicator1), na.rm = TRUE),
               Rh = mean(get(Rh1), na.rm = TRUE),
               deffh = mean(get(deffh1), na.rm = TRUE),
               sup_w = mean(get(sup_w1), na.rm = TRUE),
               sup_cv = mean(get(sup_cv1), na.rm = TRUE)),
               keyby = c(dom1, strata1)]
  setnames(aa, c("Rh"), c(Rh1))
  aa[is.na(s2_Y), s2_Y := 0]
  aa[, poph := as.numeric(poph)]
  aa[, apj := as.numeric(min_size)]
  aa[, poph_sample := as.numeric(poph_sample)]
  aa[apj > poph_sample, apj := poph_sample]
  aa[, sample100 := poph - poph_sample]
  aa[poph_sample != 0, sum_apj := as.integer(sum(apj)) - 1, by = dom1]

  a1 <- copy(aa)
  a1[, cv := 1000]
  a1[, nh := sample100]

  while (nrow(a1[cv > sup_cv]) > 0){
         a1[(cv > sup_cv) & (poph_sample != 0), sum_apj := sum_apj + 1]
         a1[, (c("nh", "cv")) := NULL]
         a1[poph_sample != 0, pnh := poph * sqrt(s2_Y * deffh / Rh)]
         a1[poph_sample != 0, nh := sum_apj * pnh / sum(pnh), by = dom1]

         a1[is.na(nh), nh := 0]
         a1[, nh := round(nh)]
         a1[, nh := nh + sample100]
         a1[nh < min_size, nh := as.numeric(min_size)]
         a1[nh > poph, nh := poph]
         a1[(poph/nh > sup_w) & (correction_before), nh := round(poph / sup_w)]
         a1[(poph/nh > sup_w) & (correction_before), nh := nh + 1]

         a1[, vars := expvarh(s2h = get("s2_Y"), nh = get("nh"), poph = get("poph"),
                              Rh = get("Rh"), deffh = get("deffh"))]
         a1[, cv := sqrt(sum(vars))/sum(sum_Y) * 100, by = dom1]
         a1[, vars := NULL]
      }

  d <- merge(r, a1[, c(dom1, strata1, "poph", "nh"), with = FALSE], all = TRUE, by = c(dom1, strata1))
  r3 <- d[, c(names(r), "poph", "nh"), with = FALSE]
  setkeyv(r3, names(id))

  r3[(poph/nh > get(sup_w1)) & (!correction_before), nh:=round(poph/get(sup_w1))]
  r3[(poph/nh > get(sup_w1)) & (!correction_before), nh:=nh + 1]

  a1 <- r3[, .(nh = mean(nh, na.rm = TRUE),
               poph = .N,
               Rh = mean(get(Rh1), na.rm = TRUE),
               deffh = mean(get(deffh1), na.rm = TRUE),
               sum_Y = sum(as.numeric(get(Y1)), na.rm = TRUE),
               s2_Y = var(get(Y1), na.rm = TRUE)), keyby = c(strata1,  dom1)]
  a1[is.na(sum_Y), sum_Y:=0]
  a1[is.na(s2_Y), s2_Y:=0]
  setnames(a1, c("sum_Y", "Rh", "deffh"), c(Y1, Rh1, deffh1))

  a2 <- expvar(Yh = Y1, H = strata1,
               s2h = "s2_Y", nh = "nh",
               poph = "poph", Rh= Rh1,
               deffh = deffh1, Dom = dom1,
               dataset = a1)
  a1 <- NULL

  r4 <- r3[, .(nh = mean(nh, na.rm = TRUE),
               poph = .N), keyby = strata1]

  # Check is any strata, where nh>Nh
  test <- r4[nh > poph]
  if (nrow(test) == 0) test <- 0

  # sample size
  r4 <- copy(r3)
  r4[, index__1 := 1]
  r4 <- rbind(r4[1], r4)
  r4[1, (indicator1) := 1]
  r4[1, ("index__1") := 0]

  apj_sum <- r4[get(indicator1) == 1, .(sample100 = sum(index__1, na.rm = TRUE)), keyby = c(strata1, dom1)]

  ds <- c(strata1, dom1)
  dom_strata_size <- r4[, lapply(.SD, mean, na.rm = TRUE), by = c(strata1, dom1), .SDcols = c(sup_w1, "poph", "nh")]
  dom_strata_size <- merge(dom_strata_size, apj_sum, all.x = TRUE, by = c(strata1, dom1))
  dom_strata_size[is.na(sample100), sample100 := 0]
  dom_strata_size[, design_weights := poph / nh]
  dom_size <- dom_strata_size[, lapply(.SD, sum, na.rm = TRUE),
                                keyby = dom1, .SDcols = c("poph", "nh", "sample100")]
  dom_size[, design_weights := poph / nh]
  sample_siz <- dom_size[, lapply(.SD, sum), .SDcols=c("poph", "nh", "sample100")]

  return (list(data = r3,
               nh_larger_then_Nh = test,
               dom_strata_size = dom_strata_size,
               dom_size = dom_size,
               size = sample_siz,
               dom_strata_expected_precision = a2$resultDomH,
               dom_expected_precision = a2$resultDom,
               total_expected_precision = a2$result))
 }

