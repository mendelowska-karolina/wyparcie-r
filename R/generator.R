#' Generowanie sieci cytowan o znanej strukturze
#'
#' Procedura tworzy siec z dwoch czesci: siec bazowa o realistycznym,
#' skosnym rozkladzie cytowan oraz rozlaczne skladowe, w ktorych licznosci
#' \eqn{N_i}, \eqn{N_j}, \eqn{N_k} i liczba cytowan najczesciej cytowanego
#' zrodla \eqn{C_{\max}} sa znane z konstrukcji.
#'
#' \strong{Siec bazowa.} Publikacja \eqn{i = 1, \ldots, n} ma rok
#' \eqn{2000 + \lfloor (i - 1) / 50 \rfloor}. Kazda publikacja od roku 2001
#' wybiera \code{liczba_zrodel} zrodel sposrod prac z lat wczesniejszych,
#' po jednym, bez zwracania. Z prawdopodobienstwem \code{kopiowanie} (gdy ma
#' juz co najmniej jedno zrodlo) wybiera losowo jedno ze zrodel juz wybranej
#' pracy. W przeciwnym razie wybiera prace \eqn{j} z prawdopodobienstwem
#' proporcjonalnym do
#' \deqn{(1 + c_j) \exp\left(-\frac{a_{ij} - 1}{\tau}\right),}{
#'   (1 + c_j) * exp(-(a_ij - 1) / tau),}
#' gdzie \eqn{c_j} jest dotychczasowa liczba cytowan pracy \eqn{j},
#' \eqn{a_{ij}} roznica lat publikacji, a \eqn{\tau} parametrem
#' \code{starzenie}. Pierwszy czynnik daje kumulacje cytowan, drugi starzenie
#' literatury, a kopiowanie zrodel wspolcytowanie publikacji z jej zrodlami.
#'
#' \strong{Skladowe kontrolne.} Kazda skladowa ma publikacje docelowa z roku
#' 2015, \eqn{n_z} zrodel z lat 2010-2014 oraz prace cytujace z lat
#' 2016-2020: \eqn{N_i} cytuje tylko publikacje docelowa, \eqn{N_j}
#' publikacje docelowa i jedno zrodlo, \eqn{N_k} tylko jedno zrodlo. Liczby
#' cytowan zrodel sa proporcjonalne do \eqn{1/r} (rozklad Zipfa, \eqn{r}
#' to ranga zrodla), zaokraglone metoda najwiekszych reszt, wiec
#' \eqn{C_{\max}} jest znane. Skladowe nie lacza sie z siecia bazowa ani ze
#' soba.
#'
#' \strong{Warianty obserwacji.} Na pelnej sieci powstaja trzy warianty
#' niekompletnosci: losowe usuniecie 5\% i 20\% krawedzi oraz odciecie
#' cytowan pozniejszych niz trzy lata po publikacji cytowanej pracy.
#'
#' Kolejnosc losowania jest ustalona: najpierw siec bazowa (publikacje po
#' kolei), nastepnie krawedzie usuwane w wariantach 5\% i 20\%.
#'
#' @param n Liczba publikacji sieci bazowej.
#' @param ziarno Ziarno generatora liczb losowych. Stan generatora w sesji
#'   uzytkownika zostaje przywrocony po zakonczeniu losowania.
#' @param liczba_zrodel Liczba zrodel kazdej publikacji sieci bazowej.
#' @param kopiowanie Prawdopodobienstwo wyboru zrodla sposrod zrodel juz
#'   wybranej pracy.
#' @param starzenie Parametr \eqn{\tau} wygasania atrakcyjnosci starszych
#'   prac.
#' @param skladowe Ramka danych z kolumnami \code{Ni}, \code{Nj}, \code{Nk},
#'   \code{liczba_zrodel} opisujaca skladowe kontrolne; domyslnie siatka 20
#'   skladowych z \code{skladowe_kontrolne()}.
#'
#' @return Lista ze skladnikami:
#'   \describe{
#'     \item{publikacje}{ramka danych z kolumnami \code{id}, \code{rok}}
#'     \item{krawedzie}{pelna siec: ramka danych z kolumnami \code{cytujacy},
#'       \code{cytowany}}
#'     \item{prawda}{tabela skladowych kontrolnych z licznosciami i
#'       wskaznikami wynikajacymi z konstrukcji (okno 5 lat)}
#'     \item{warianty}{lista tabel krawedzi: \code{usuniete_5},
#'       \code{usuniete_20}, \code{obciete_3}}
#'     \item{parametry}{lista argumentow wywolania}
#'   }
#'
#' @examples
#' siec <- generuj_siec(n = 200)
#' head(siec$prawda)
#' nrow(siec$krawedzie)
#'
#' @export
generuj_siec <- function(n = 1000, ziarno = 202704, liczba_zrodel = 10,
                         kopiowanie = 0.3, starzenie = 3,
                         skladowe = skladowe_kontrolne()) {
  # TODO: kontrola argumentow (dodatnie liczby calkowite, prawdopodobienstwo
  #   w [0, 1], dodatnie `starzenie`, poprawna tabela `skladowe`).
  withr::with_seed(ziarno, {
    bazowa <- siec_bazowa(n, liczba_zrodel, kopiowanie, starzenie)
    kontrolne <- siec_skladowych(skladowe)
    publikacje <- rbind(bazowa$publikacje, kontrolne$publikacje)
    krawedzie <- rbind(bazowa$krawedzie, kontrolne$krawedzie)
    m <- nrow(krawedzie)
    usuniete_5 <- sample.int(m, round(0.05 * m))
    usuniete_20 <- sample.int(m, round(0.20 * m))
  })

  rok_cytujacego <- publikacje$rok[match(krawedzie$cytujacy, publikacje$id)]
  rok_cytowanego <- publikacje$rok[match(krawedzie$cytowany, publikacje$id)]
  obciete <- rok_cytujacego - rok_cytowanego > 3

  list(
    publikacje = publikacje,
    krawedzie = krawedzie,
    prawda = kontrolne$prawda,
    warianty = list(
      usuniete_5 = krawedzie[-usuniete_5, , drop = FALSE],
      usuniete_20 = krawedzie[-usuniete_20, , drop = FALSE],
      obciete_3 = krawedzie[!obciete, , drop = FALSE]
    ),
    parametry = list(n = n, ziarno = ziarno, liczba_zrodel = liczba_zrodel,
                     kopiowanie = kopiowanie, starzenie = starzenie)
  )
}


#' @rdname generuj_siec
#' @return \code{skladowe_kontrolne()} zwraca domyslna siatke 20 skladowych:
#'   ramke danych z kolumnami \code{Ni}, \code{Nj}, \code{Nk},
#'   \code{liczba_zrodel}. Siatka obejmuje przypadki z \eqn{b_p < 1},
#'   \eqn{b_p = 1} i \eqn{b_p > 1}, liczby zrodel 3, 10 i 30 oraz przypadki
#'   brzegowe: brak cytowan publikacji przy cytowanych zrodlach i brak prac
#'   cytujacych zrodla.
#' @export
skladowe_kontrolne <- function() {
  data.frame(
    Ni = c(10, 0, 5, 1, 20, 6, 1, 0, 2, 8, 15, 1, 4, 30, 2, 0, 10, 5, 12, 3),
    Nj = c(0, 10, 5, 0, 0, 6, 1, 0, 3, 2, 5, 4, 0, 10, 8, 5, 10, 2, 0, 1),
    Nk = c(10, 10, 90, 1, 0, 0, 200, 12, 15, 40, 60, 5, 2, 200, 5, 25, 30, 150,
           9, 7),
    liczba_zrodel = c(3, 3, 10, 3, 3, 10, 3, 3, 10, 10, 30, 3, 10, 30, 3, 30,
                      10, 30, 3, 10)
  )
}


# Siec bazowa: kumulacja cytowan, starzenie i kopiowanie zrodel.
siec_bazowa <- function(n, liczba_zrodel, kopiowanie, starzenie) {
  rok <- 2000 + (seq_len(n) - 1L) %/% 50L
  cytowania <- integer(n)
  zrodla <- vector("list", n)
  for (i in seq_len(n)) {
    pula <- which(rok < rok[i])
    if (!length(pula)) next
    k <- min(liczba_zrodel, length(pula))
    wybrane <- integer(0)
    while (length(wybrane) < k) {
      kandydaci <- integer(0)
      if (length(wybrane) && stats::runif(1) < kopiowanie) {
        kandydaci <- setdiff(unlist(zrodla[wybrane]), wybrane)
      }
      if (length(kandydaci)) {
        nowe <- kandydaci[sample.int(length(kandydaci), 1L)]
      } else {
        dostepne <- setdiff(pula, wybrane)
        waga <- (1 + cytowania[dostepne]) *
          exp(-(rok[i] - rok[dostepne] - 1) / starzenie)
        nowe <- dostepne[sample.int(length(dostepne), 1L, prob = waga)]
      }
      wybrane <- c(wybrane, nowe)
    }
    zrodla[[i]] <- wybrane
    cytowania[wybrane] <- cytowania[wybrane] + 1L
  }
  id <- sprintf("b%04d", seq_len(n))
  list(
    publikacje = data.frame(id = id, rok = rok, stringsAsFactors = FALSE),
    krawedzie = data.frame(
      cytujacy = rep(id, lengths(zrodla)),
      cytowany = id[unlist(zrodla)],
      stringsAsFactors = FALSE
    )
  )
}


# Rozdzial liczby calkowitej proporcjonalnie do wag metoda najwiekszych reszt.
rozdziel <- function(calosc, wagi) {
  dokladne <- calosc * wagi / sum(wagi)
  wynik <- floor(dokladne)
  reszta <- calosc - sum(wynik)
  if (reszta > 0) {
    kolejnosc <- order(dokladne - wynik, decreasing = TRUE)
    wynik[kolejnosc[seq_len(reszta)]] <- wynik[kolejnosc[seq_len(reszta)]] + 1
  }
  wynik
}


# Skladowe kontrolne o licznosciach znanych z konstrukcji.
siec_skladowych <- function(skladowe) {
  publikacje <- list()
  krawedzie <- list()
  prawda <- vector("list", nrow(skladowe))
  for (s in seq_len(nrow(skladowe))) {
    Ni <- skladowe$Ni[s]
    Nj <- skladowe$Nj[s]
    Nk <- skladowe$Nk[s]
    nz <- skladowe$liczba_zrodel[s]
    prefiks <- sprintf("k%02d_", s)
    cel <- paste0(prefiks, "cel")
    zrodla <- sprintf("%sz%02d", prefiks, seq_len(nz))
    cytujace <- sprintf("%sc%03d", prefiks, seq_len(Ni + Nj + Nk))

    # Cytowania zrodel: rozklad Zipfa na prace z B (Nj + Nk prac).
    na_zrodlo <- rozdziel(Nj + Nk, 1 / seq_len(nz))
    przydzial <- rep(seq_len(nz), times = na_zrodlo)
    cytujace_b <- cytujace[Ni + seq_len(Nj + Nk)]
    cytujace_f <- cytujace[seq_len(Ni + Nj)]

    publikacje[[s]] <- data.frame(
      id = c(cel, zrodla, cytujace),
      rok = c(2015, 2014 - (seq_len(nz) - 1) %% 5,
              2016 + (seq_along(cytujace) - 1) %% 5),
      stringsAsFactors = FALSE
    )
    krawedzie[[s]] <- data.frame(
      cytujacy = c(rep(cel, nz), cytujace_f, cytujace_b),
      cytowany = c(zrodla, rep(cel, Ni + Nj), zrodla[przydzial]),
      stringsAsFactors = FALSE
    )
    Cp <- Ni + Nj
    Cmax <- max(na_zrodlo)
    prawda[[s]] <- data.frame(
      id = cel, Ni = Ni, Nj = Nj, Nk = Nk, Cp = Cp, Cmax = Cmax,
      D = if (Ni + Nj + Nk > 0) (Ni - Nj) / (Ni + Nj + Nk) else NA_real_,
      d = if (Cp > 0) (Ni - Nj) / Cp else NA_real_,
      Rk = if (Cp > 0) Nk / Cp else NA_real_,
      bp = if (Cp > 0) Cmax / Cp else NA_real_,
      liczba_zrodel = nz,
      stringsAsFactors = FALSE
    )
  }
  list(
    publikacje = do.call(rbind, publikacje),
    krawedzie = do.call(rbind, krawedzie),
    prawda = do.call(rbind, prawda)
  )
}
