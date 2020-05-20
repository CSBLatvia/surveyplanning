#' Optimal sample size allocation for proportion
#'
#' @description The function computes optimal sample size allocation over strata and domain for proportion.
#'
#' @param H The stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom}{Variables used to define population domains. An object convertible to \code{data.table} or variable names as character vector, column numbers.
#' @param pop}{The population size in each stratum.
#' @param R}{The expected response rate in each stratum (optional). If not defined, it is assumed to be 1 in each stratum (full-response). Object convertible to one-column \code{data.table}, variable name as character, or column number.
#' @param deff}{The expected design effect for the estimate of variable (optional). If not defined, it is assumed to be 1 for each variable in each stratum. If is defined, then variables is defined the same arrangement as \code{Yh}. Object convertible to \code{data.table}, variable name as character vector, or column numbers.
#' @param se_max}{Variable for maximum standarterror (se) in domain.
#' @param prop}{The excepted ratio proportion.
#' @param min_size}{A numeric value for minimal sample size.
#' @param step}{A value for pace.
#' @param unit_level}{A logical value, if dataset is prepared for unit level then value TRUE, othercase FALSE.
#' @param dataset}{Optional agrregated survey data object convertible to \code{data.table} with one row for each stratum.
#'
#' @return  A list with two data objects:
#' \item{datah}{An object as \code{data.table}, with variables: \cr
#'   \code{H} - the unit stratum variable, \cr
#'   \code{Dom} - variables used to define population domains, \cr
#'   \code{poph} - the population size in each stratum, \cr
#'   \code{Rh} - the expected response rate in each stratum, \cr
#'   \code{deffh} - the expected design effect, \cr
#'   \code{s2h} - variance in domain of stratum, \cr
#'   \code{sup_cv} - Variable for maximum coeficient of variation, \cr
#'   \code{poph} - population size, \cr
#'   \code{nh} - sample size .}

#'\item{aggr_Dom}{An object as \code{data.table}, with variables: \cr
#'   \code{Dom} - optional variables used to define population domains, \cr
#'   \code{pop_Dom} - population size, \cr
#'   \code{sample_size_Dom} - optional variables used to define population domains, \cr
#'   \code{sample_size} - optional variables used to define population domains, \cr
#'   \code{pop} - sample size}
#'
#'
#' @seealso \code{\link{expsize}}, \code{\link{optsize}}, \code{\link{dom_optimal_allocation}}
#'
#' @keywords surveysampling

#' @examples
#' library("data.table")
#' library("laeken")
#' data("eusilc")
#' eusilc <- data.table(eusilc)
#' dataset <- eusilc[, .(poph = sum(db090)), by = c("db040")]
#' dataset[, dom := "1"]
#' res <- prop_dom_optimal_allocation(H = "db040", Dom = "dom",
#'                                    pop = "poph", R = NULL,
#'                                    deff = NULL, se_max = 0.5,
#'                                    prop = 0.5, min_size = 3,
#'                                    step = 1, unit_level = FALSE,
#'                                    dataset = dataset)
#'
#' @import data.table laeken stats
#' @export prop_dom_optimal_allocation
#'
prop_dom_optimal_allocation <- function(H, Dom, pop = NULL,
                                        R = NULL, deff = NULL,
                                        se_max = 0.5, prop = 0.5,
                                        min_size = 3, step = 1,
                                        unit_level = TRUE,
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
      if(!is.null(pop)) {
        if (min(pop %in% names(dataset)) != 1) stop("'pop' does not exist in 'dataset'!")
        if (min(pop %in% names(dataset)) == 1) pop <- dataset[, pop, with = FALSE] }
      if(!is.null(R)) {
          if (min(R %in% names(dataset)) != 1) stop("'R' does not exist in 'dataset'!")
          if (min(R %in% names(dataset)) == 1) R <- dataset[, R, with = FALSE] }
      if(!is.null(deff)) {
          if (min(deff %in% names(dataset)) != 1) stop("'deff' does not exist in 'dataset'!")
          if (min(deff %in% names(dataset)) == 1) deff <- dataset[, deff, with = FALSE] }
  }

  H <- data.table(H)
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(names(H))) stop("'H' must be colnames")
  n <- nrow(H)

  if (is.null(R)) R <- rep(1, n)
  R <- data.frame(R)
  if (anyNA(R)) stop("'R' has unknown values")
  if (nrow(R) != n) stop("'R' and 'H' must be equal row count")
  if (ncol(R) != 1) stop("'R' must be vector or 1 column data.frame, matrix, data.table")
  R <- R[, 1]
  if (!is.numeric(R)) stop("'R' must be numerical")

  # deff
  if (is.null(deff)) deff <- rep(1, n)
  deff <- data.table(deff, check.names = TRUE)
  if (!all(sapply(deff, is.numeric))) stop("'deff' must be numeric values")
  if (ncol(deff) != 1) stop("'deff' must be vector or 1 column data.frame, matrix, data.table")
  if (nrow(deff) != n) stop("'deff' and 'H' must be equal row count")
  if (any(is.na(deff))) stop("'deff' has unknown values")
  deff <- deff[, 1]

  if (is.null(Dom)) stop("'Dom' must be defined")
  Dom <- data.table(Dom)
  if (any(duplicated(names(Dom))))
         stop("'Dom' are duplicate column names: ",
               paste(names(Dom)[duplicated(names(Dom))], collapse = ","))
  if (nrow(Dom) != n) stop("'Dom' and 'H' must be equal row count")
  if (any(is.na(Dom))) stop("'Dom' has unknown values")
  if (is.null(names(Dom))) stop("'Dom' must be colnames")
  Dom[, (names(Dom)) := lapply(.SD, as.character)]

  datah <- data.table(H, Dom)

  # pop
  if (unit_level) { if (!is.null(pop)) stop("'pop' must be NULL")
               } else {pop <- data.table(pop)
                       if (ncol(pop) != 1) stop("'pop' must be vector or 1 column data.frame, matrix, data.table")
                       if (nrow(pop) != n) stop("'pop' and 'H' must be equal row count")
                       if (any(is.na(pop[[1]]))) stop("'pop' has unknown values")
                       if (!is.numeric(pop[[1]])) stop("'pop' must be numerical")
                       pop <- pop[[1]]
                      }


  s2h <- NS <- domNS <- pop_Dom <- se <- n_neyman <- NULL
  sample_size <- nrh <- strata_var <- dom_var <- NULL
  sample_size_Dom <- aggr_tot <- . <- NULL

  nDom <- names(Dom)

  datah <- data.table(datah, R, deff)
  if (unit_level) {
            datah <- datah[, .(deff = mean(deff), R = mean(R), pop = .N), by = c(names(H), nDom)]
                } else datah <- data.table(datah, pop)

  datah[, pop_Dom := sum(pop), by = nDom]

  datah[, s2h := ifelse(pop == 1, 0, pop / (pop - 1) * prop * (1 - prop))]

  datah[, NS := pop * sqrt(s2h)]
  datah[, domNS := sum(NS), by = nDom]

  datah[, n := 0]
  datah[, se := 1]

  while (any(datah$se > se_max)){
      datah[se > se_max, n := n + step]
      datah[se > se_max, n_neyman := ifelse(domNS != 0, n * (NS / domNS), 0)]
      datah[se > se_max, sample_size := pmin(pmax(min_size, trunc(n_neyman)), pop)]
      datah[se > se_max, nrh := round(sample_size * R)]
      datah[se > se_max, strata_var := pop ^ 2 * (1 - nrh / pop ) / nrh * s2h * deff]
      datah[se > se_max, dom_var := sum(strata_var), by = nDom]
      datah[se > se_max, se := sqrt(dom_var) / pop_Dom]
   }

  aggr_Dom <- datah[, .(pop_Dom = mean(pop),
                        sample_size_Dom = sum(sample_size)), keyby = c(names(H), nDom)]
  aggr_tot <- aggr_Dom[, .(pop_size = sum(pop_Dom),
                           sample_size = sum(sample_size_Dom))]

  list(datah = datah[], aggr_Dom = aggr_Dom[], aggr_tot = aggr_tot)

 }