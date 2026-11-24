# Zbiory cytowan dla jednej publikacji.
#
# Dla publikacji p z rokiem y i okna o dlugosci w:
#   F   - prace cytujace p, opublikowane w latach y + 1, ..., y + w,
#   B_i - prace cytujace i-te zrodlo p, opublikowane w tych samych latach,
#   B   - suma zbiorow B_i; praca cytujaca kilka zrodel nalezy do B raz.
# Prace cytujace bez roku nie naleza do zadnego okna; ich liczba trafia do
# diagnostyki. Publikacja p cytuje wlasne zrodla, ale jej rok lezy poza
# oknem, wiec nie nalezy do B.

w_oknie <- function(indeksy, rok, poczatek, koniec) {
  r <- rok[indeksy]
  indeksy[!is.na(r) & r >= poczatek & r <= koniec]
}

zbiory_dla_celu <- function(siec, p, okno) {
  y <- siec$rok[p]
  zrodla <- siec$zrodla[[p]]
  if (is.na(y)) {
    return(list(rok = y, F = integer(0), B_i = list(), zrodla = zrodla,
                bez_roku = NA_integer_))
  }
  poczatek <- y + 1
  koniec <- y + okno
  wszystkie_f <- siec$cytujace[[p]]
  f <- w_oknie(wszystkie_f, siec$rok, poczatek, koniec)
  B_i <- lapply(siec$cytujace[zrodla], w_oknie, rok = siec$rok,
                poczatek = poczatek, koniec = koniec)
  wszystkie_b <- unlist(siec$cytujace[zrodla], use.names = FALSE)
  list(
    rok = y,
    F = f,
    B_i = B_i,
    zrodla = zrodla,
    bez_roku = sum(is.na(siec$rok[c(wszystkie_f, wszystkie_b)]))
  )
}

# Licznosci kategorii z artykulu Lin, Li i Wu (2026):
#   N_i = |F \ B|, N_j = |F n B|, N_k = |B \ F|.
licznosci <- function(zbiory) {
  f <- zbiory$F
  b <- unique(unlist(zbiory$B_i, use.names = FALSE))
  c(
    Ni = length(setdiff(f, b)),
    Nj = length(intersect(f, b)),
    Nk = length(setdiff(b, f)),
    B = length(b),
    Cmax = if (length(zbiory$B_i)) max(lengths(zbiory$B_i)) else NA_real_,
    liczba_zrodel = length(zbiory$zrodla)
  )
}
