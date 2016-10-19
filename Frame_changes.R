#frame_old - vecais râmis
#frame_now - jaunais râmis
#new_year - apsekojuma gads, kuram sagatavots jaunais râmis
#previous_year -apsekojuma gads, kuram sagatavots vecais râmis
#previous_year -apsekojuma gads, kuram sagatavots vecais râmis
#nosauk - datu mainîgo nosaukumi, kam tiek aprçíinâtas izmaiòas starp gadiem!
#id - ID nosaukums

izmainas_rami <- function(frame_old, frame_now, new_year, previous_year, nosauk, id){
     setnames(ramis_vecais, names(frame_old), tolower(names(frame_old)))
     frame_old[, ramis:=1]
     frame_old[, der:=1]
     setnames(frame_old, names(frame_old), paste0(names(frame_old),"_", previous_year))
     setnames(frame_old, paste0(id, "_", previous_year), id)

     frame_now <- data.table(data.frame(frame_now))
     frame_now[, paste0("ramis_", new_year):=1]

     setnames(frame_now, "der", paste0("der_", new_year))
     setnames(frame_now, "pazime_21ls", paste0("pazime_21ls_", new_year))

     izmainas <- merge(frame_now, frame_old, all=TRUE, by="lsn")
     recode.NA(izmainas, paste0("ramis_", c(previous_year, new_year)))
     frame_old <- frame_now <- NULL
     sar <- c(nosauk, paste0("ramis_", c(gads, previous_year)),
                                  paste0(nosauk,"_", previous_year))

     skaits <- izmainas[, lapply(.SD, sum, na.rm=TRUE), .SDcols=sar[sar %in% names(izmainas)]]
     setnames(skaits, paste0("ramis_", new_year), "ramis")
     skaits <- data.table(name=names(skaits), t(skaits))
     skaits[substr(name, nchar(name)-3, nchar(name))==previous_year, year:=previous_year]
     skaits[is.na(year), year:=new_year]

     skaits[substr(name, nchar(name)-3, nchar(name))==previous_year, name:=substr(name, 1, nchar(name)-5)]
     skaits <- data.table(dcast(skaits, name ~ year, sum, value.var="V1"), check.names=TRUE)
     setnames(skaits, names(skaits), tolower(names(skaits)))

     setkeyv(skaits, "name")
     skaits[, izmainas:=get(paste0("x", new_year))/get(paste0("x", previous_year))*100-100]
     skaits[]
  }
