#' Indeks przelomowosci i jego skladowe
#'
#' Funkcja wyznacza dla kazdej publikacji docelowej licznosci kategorii
#' cytujacych, klasyczny indeks przelomowosci \eqn{D_p}, bilans
#' bezposrednio cytujacych \eqn{d_p}, stosunek \eqn{R_k} oraz czynnik
#' \eqn{b_p} zwiazany z najczesciej cytowanym zrodlem (Lin, Li, Wu, 2026).
#'
#' Dla publikacji \eqn{p} z rokiem \eqn{y} i okna o dlugosci \eqn{w}
#' zbior \eqn{F} obejmuje prace cytujace \eqn{p}, a zbior \eqn{B} prace
#' cytujace co najmniej jedno zrodlo \eqn{p}; oba zbiory dotycza prac
#' opublikowanych w latach od \eqn{y + 1} do \eqn{y + w}. Licznosci wynosza
#' \eqn{N_i = |F \setminus B|}, \eqn{N_j = |F \cap B|},
#' \eqn{N_k = |B \setminus F|}, a wskazniki
#' \deqn{D_p = \frac{N_i - N_j}{N_i + N_j + N_k}, \quad
#'   d_p = \frac{N_i - N_j}{N_i + N_j}, \quad
#'   R_k = \frac{N_k}{N_i + N_j}, \quad b_p = \frac{C_{\max}}{C_p}.}{
#'   D = (Ni - Nj) / (Ni + Nj + Nk), d = (Ni - Nj) / (Ni + Nj),
#'   Rk = Nk / (Ni + Nj), bp = Cmax / Cp.}
#' \eqn{C_p = N_i + N_j} jest liczba cytowan publikacji, a \eqn{C_{\max}}
#' najwieksza liczba cytowan pojedynczego zrodla w tym samym oknie. Przy
#' dodatnich mianownikach zachodzi tozsamosc \eqn{D_p = d_p / (1 + R_k)}.
#'
#' Status wyniku:
#' \describe{
#'   \item{\code{zdefiniowany}}{wszystkie wskazniki okreslone}
#'   \item{\code{brak_cytowan}}{\eqn{C_p = 0} przy \eqn{N_k > 0}: \eqn{D = 0},
#'     pozostale wskazniki \code{NA}}
#'   \item{\code{zerowy_mianownik}}{\eqn{N_i + N_j + N_k = 0}: \eqn{D}
#'     nieokreslony}
#'   \item{\code{brak_zrodel}}{publikacja bez zrodel: \eqn{b_p} nieokreslony,
#'     a \eqn{D = 1} przy \eqn{C_p > 0} nie swiadczy o przelomie}
#'   \item{\code{niekompletne_zrodla}}{czesc zrodel nie ma rekordu w sieci,
#'     wiec ich cytowania sa nieznane}
#'   \item{\code{brak_roku}, \code{brak_rekordu}}{publikacji nie da sie
#'     przypisac do okna}
#' }
#'
#' @param siec Obiekt klasy \code{siec_cytowan}, wynik
#'   \code{\link{przygotuj_siec}}.
#' @param cele Wektor identyfikatorow publikacji docelowych.
#' @param okno Dlugosc okna przyszlych cytowan w latach.
#'
#' @return Obiekt klasy \code{wyparcie_cytowan}, lista ze skladnikami:
#'   \describe{
#'     \item{tabela}{ramka danych z kolumnami \code{id}, \code{Ni},
#'       \code{Nj}, \code{Nk}, \code{Cp}, \code{Cmax}, \code{D}, \code{d},
#'       \code{Rk}, \code{bp}, \code{liczba_zrodel}, \code{status}}
#'     \item{parametry}{lista z dlugoscia okna}
#'     \item{diagnostyka}{liczba prac cytujacych bez roku, liczebnosci
#'       statusow, najwieksze odchylenie od tozsamosci
#'       \eqn{D = d / (1 + R_k)}}
#'   }
#'
#' @references Lin, Y., Li, L., Wu, L. (2026). The disruption index measures
#'   displacement between a paper and its most cited reference. Quantitative
#'   Science Studies, 7, 229--239. \doi{10.1162/QSS.a.409}
#'
#' @examples
#' siec <- przygotuj_siec(krawedzie_przyklad, publikacje_przyklad)
#' wynik <- oblicz_wyparcie(siec, cele = c("k01_cel", "k02_cel", "k03_cel"))
#' wynik
#' summary(wynik)
#'
#' @export
oblicz_wyparcie <- function(siec, cele, okno = 5) {
  # TODO: kontrola argumentow: `siec` klasy siec_cytowan, `cele` jako
  #   niepusty wektor znakowy, `okno` jako dodatnia liczba calkowita
  #   (komunikat "Okno musi byc dodatnia liczba calkowita").
  # TODO: wariant blokowy dla wielu celow (np. macierz rzadka cytowan
  #   z pakietu Matrix) sprawdzony wzgledem tej funkcji, z pomiarem czasu
  #   i pamieci na sieci z eksperymentu.
  cele <- as.character(cele)
  indeksy <- match(cele, siec$id)

  wiersze <- lapply(seq_along(cele), function(i) {
    p <- indeksy[i]
    if (is.na(p)) {
      return(wiersz_wyniku(NA, NA, NA, NA, NA, NA, "brak_rekordu", 0))
    }
    zbiory <- zbiory_dla_celu(siec, p, okno)
    if (is.na(zbiory$rok)) {
      return(wiersz_wyniku(NA, NA, NA, NA, NA, length(zbiory$zrodla),
                           "brak_roku", 0))
    }
    l <- licznosci(zbiory)
    status <- status_wyniku(l, sum(is.na(siec$rok[zbiory$zrodla])))
    wiersz_wyniku(l[["Ni"]], l[["Nj"]], l[["Nk"]], l[["B"]], l[["Cmax"]],
                  l[["liczba_zrodel"]], status, zbiory$bez_roku)
  })
  wiersze <- do.call(rbind, wiersze)

  tabela <- data.frame(
    id = cele,
    Ni = as.integer(wiersze$Ni),
    Nj = as.integer(wiersze$Nj),
    Nk = as.integer(wiersze$Nk),
    Cp = as.integer(wiersze$Ni + wiersze$Nj),
    Cmax = as.integer(wiersze$Cmax),
    stringsAsFactors = FALSE
  )
  mianownik <- tabela$Ni + tabela$Nj + tabela$Nk
  tabela$D <- ifelse(!is.na(mianownik) & mianownik > 0,
                     (tabela$Ni - tabela$Nj) / mianownik, NA_real_)
  cytowana <- !is.na(tabela$Cp) & tabela$Cp > 0
  tabela$d <- ifelse(cytowana, (tabela$Ni - tabela$Nj) / tabela$Cp, NA_real_)
  tabela$Rk <- ifelse(cytowana, tabela$Nk / tabela$Cp, NA_real_)
  tabela$bp <- ifelse(cytowana & wiersze$liczba_zrodel > 0,
                      tabela$Cmax / tabela$Cp, NA_real_)
  tabela$liczba_zrodel <- as.integer(wiersze$liczba_zrodel)
  tabela$status <- wiersze$status

  odchylenie <- abs(tabela$D - tabela$d / (1 + tabela$Rk))
  structure(
    list(
      tabela = tabela,
      parametry = list(okno = okno),
      diagnostyka = list(
        cytujace_bez_roku = sum(wiersze$bez_roku, na.rm = TRUE),
        statusy = table(factor(tabela$status, levels = STATUSY)),
        odchylenie_tozsamosci = if (any(!is.na(odchylenie)))
          max(odchylenie, na.rm = TRUE) else NA_real_
      )
    ),
    class = "wyparcie_cytowan"
  )
}

STATUSY <- c("zdefiniowany", "brak_cytowan", "zerowy_mianownik",
             "brak_zrodel", "niekompletne_zrodla", "brak_roku",
             "brak_rekordu")

wiersz_wyniku <- function(Ni, Nj, Nk, B, Cmax, liczba_zrodel, status,
                          bez_roku) {
  data.frame(Ni = Ni, Nj = Nj, Nk = Nk, B = B, Cmax = Cmax,
             liczba_zrodel = liczba_zrodel, status = status,
             bez_roku = bez_roku, stringsAsFactors = FALSE)
}

status_wyniku <- function(l, zrodla_bez_rekordu) {
  if (l[["liczba_zrodel"]] == 0) {
    "brak_zrodel"
  } else if (zrodla_bez_rekordu > 0) {
    "niekompletne_zrodla"
  } else if (l[["Ni"]] + l[["Nj"]] + l[["Nk"]] == 0) {
    "zerowy_mianownik"
  } else if (l[["Ni"]] + l[["Nj"]] == 0) {
    "brak_cytowan"
  } else {
    "zdefiniowany"
  }
}


#' Porownanie okien cytowan
#'
#' Funkcja liczy wskazniki z \code{\link{oblicz_wyparcie}} dla kilku dlugosci
#' okna i zestawia je w jednej tabeli. Porownanie okien jest analiza
#' wrazliwosci: ta sama publikacja moze zmienic znak \eqn{D_p}, gdy
#' cytowania zrodel narastaja szybciej niz cytowania publikacji.
#'
#' @inheritParams oblicz_wyparcie
#' @param okna Wektor dlugosci okien w latach.
#'
#' @return Obiekt klasy \code{porownanie_okien}, lista ze skladnikami
#'   \code{tabela} (tabela wyniku \code{\link{oblicz_wyparcie}} z dodatkowa
#'   kolumna \code{okno}) i \code{parametry}.
#'
#' @examples
#' siec <- przygotuj_siec(krawedzie_przyklad, publikacje_przyklad)
#' porownaj_okna(siec, cele = c("k01_cel", "k03_cel"), okna = c(3, 5))
#'
#' @export
porownaj_okna <- function(siec, cele, okna = c(3, 5, 10)) {
  # TODO: kontrola `okna` (niepusty wektor dodatnich liczb calkowitych).
  tabele <- lapply(okna, function(w) {
    tabela <- oblicz_wyparcie(siec, cele, okno = w)$tabela
    cbind(okno = w, tabela)
  })
  structure(
    list(tabela = do.call(rbind, tabele),
         parametry = list(okna = okna)),
    class = "porownanie_okien"
  )
}
