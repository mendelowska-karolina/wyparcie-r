# Male sieci budowane recznie do przypadkow analitycznych.
#
# Publikacja docelowa "p" z roku 2010, zrodla z roku 2009, prace cytujace
# z roku 2011 (wewnatrz okna 2011-2015).

siec_reczna <- function(krawedzie, lata = NULL) {
  krawedzie <- as.data.frame(krawedzie, stringsAsFactors = FALSE)
  names(krawedzie) <- c("cytujacy", "cytowany")
  id <- unique(c("p", krawedzie$cytujacy, krawedzie$cytowany))
  rok <- ifelse(id == "p", 2010, ifelse(grepl("^z", id), 2009, 2011))
  publikacje <- data.frame(id = id, rok = rok, stringsAsFactors = FALSE)
  if (!is.null(lata)) {
    publikacje$rok[match(names(lata), publikacje$id)] <- lata
  }
  przygotuj_siec(krawedzie, publikacje)
}

krawedz <- function(...) {
  pary <- c(...)
  matrix(pary, ncol = 2, byrow = TRUE)
}
