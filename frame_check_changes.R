#frame_old - vecais râmis
#frame_now - jaunais râmis
#new_year - apsekojuma gads, kuram sagatavots jaunais râmis
#previous_year -apsekojuma gads, kuram sagatavots vecais râmis
#variable_names - datu mainîgo nosaukumi, kam tiek aprçíinâtas izmaiòas starp gadiem!
#id_name - ID nosaukums

frame_check_changes <- function(frame_old, frame_now, new_year, previous_year, variables_names = NULL, domain = NULL, id_name){
     setnames(frame_old, names(frame_old), tolower(names(frame_old)))
     id_name <- tolower(id_name)

     frame_now <- data.table(data.frame(frame_now))
     setnames(frame_now, names(frame_now), tolower(names(frame_now)))

     if (!(id_name %in% names(frame_now))) stop("'id_name' not exist in frame_now")
     if (!(id_name %in% names(frame_old))) stop("'id_name' not exist in frame_old")

     checks <- frame_now[,.N, keyby = id_name]
     checks <- checks[N > 1]

     frame_old[, in_frame := 1]
     setnames(frame_old, names(frame_old), paste0(names(frame_old),"_", previous_year))
     setnames(frame_old, paste0(id_name, "_", previous_year), id_name)
     frame_old[, (id_name) := as.character(get(id_name))]

     frame_now[, in_frame := 1]
     setnames(frame_now, names(frame_now), paste0(names(frame_now),"_", new_year))
     setnames(frame_now, paste0(id_name, "_", new_year), id_name)
     frame_now[, (id_name) := as.character(get(id_name))]

     changes <- merge(frame_now, frame_old, all = TRUE, by = id_name)
     recode.NA(changes, paste0("in_frame_", c(previous_year, new_year)))
     frame_old <- frame_now <- NULL
     sar2 <- sar <- paste0("in_frame_", c(new_year, previous_year))
     sar3 <- doms <- NULL
     if (!is.null(domain)) {sar2 <- c(paste0(c("in_frame_", paste0(domain, "_")), new_year),
                                      paste0(c("in_frame_", paste0(domain, "_")), previous_year))
                            sar3 <- paste0(domain, "_", c(new_year, previous_year)) }

     if (!is.null(variables_names)) sar <- c(paste0(variables_names,"_", new_year), sar,
                                             paste0(variables_names,"_", previous_year))

     sar_domains <- changes[, .(count = .N), keyby = sar2]

     if (!is.null(sar3)) doms <- changes[, .(count = .N), keyby = sar3]

     calcs <- changes[, lapply(.SD, sum, na.rm = TRUE), .SDcols = sar[sar %in% names(changes)]]
     calcs[, tnws := 1]
     calcs <- melt(calcs, id.vars = c("tnws"))
     calcs[, variable := as.character(variable)]
     calcs[, year := substring(variable, nchar(variable) - 3, nchar(variable))]
     calcs[, name := substring(variable, 1, nchar(variable) - 5)]
     calcs <- data.table(dcast(calcs, name ~ year, sum, value.var = "value"), check.names = TRUE)
     setnames(calcs, names(calcs), tolower(names(calcs)))

     setkeyv(calcs, "name")
     calcs[, izmainas := get(paste0("x", new_year)) / get(paste0("x", previous_year)) * 100 - 100]
     return(list(checking_id = checks,
                 frame_domains = sar_domains[],
                 domains = doms[],
                 variable_calc = calcs[]))
  }
