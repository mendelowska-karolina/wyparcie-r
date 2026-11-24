#' Metody dla wyniku indeksu przelomowosci
#'
#' Wypisanie, podsumowanie i wykres obiektu klasy \code{wyparcie_cytowan}.
#' Podsumowanie zestawia statusy, rozklady wskaznikow oraz udzialy publikacji
#' z ujemnym i dodatnim bilansem \eqn{d_p}. Wykres pokazuje zaleznosc
#' \eqn{D_p} od czynnika \eqn{b_p} na skali logarytmicznej, z kolorem
#' wedlug znaku \eqn{d_p}: przy duzym \eqn{b_p} wartosc \eqn{D_p} zbliza sie
#' do zera niezaleznie od bilansu bezposrednio cytujacych.
#'
#' @param x,object Obiekt klasy \code{wyparcie_cytowan} albo
#'   \code{summary.wyparcie_cytowan}.
#' @param cyfry Liczba cyfr znaczacych w wydruku.
#' @param ... Argumenty przekazywane dalej; w \code{plot()} do
#'   \code{\link[graphics]{plot.default}}.
#'
#' @return \code{print()} niewidocznie zwraca \code{x}. \code{summary()}
#'   zwraca obiekt klasy \code{summary.wyparcie_cytowan}. \code{plot()}
#'   niewidocznie zwraca ramke danych z punktami wykresu.
#'
#' @examples
#' siec <- przygotuj_siec(krawedzie_przyklad, publikacje_przyklad)
#' cele <- sprintf("k%02d_cel", 1:20)
#' wynik <- oblicz_wyparcie(siec, cele)
#' print(wynik)
#' summary(wynik)
#' plot(wynik)
#'
#' @name metody_wyparcie_cytowan
NULL

#' @rdname metody_wyparcie_cytowan
#' @export
print.wyparcie_cytowan <- function(x, cyfry = 3, ...) {
  tabela <- x$tabela
  cat(sprintf("Indeks przelomowosci, okno %d lat, publikacje: %d\n\n",
              as.integer(x$parametry$okno), nrow(tabela)))
  liczbowe <- c("D", "d", "Rk", "bp")
  tabela[liczbowe] <- lapply(tabela[liczbowe], signif, digits = cyfry)
  print(utils::head(tabela, 10), row.names = FALSE)
  if (nrow(tabela) > 10) {
    cat(sprintf("... oraz %d kolejnych publikacji\n", nrow(tabela) - 10))
  }
  invisible(x)
}

#' @rdname metody_wyparcie_cytowan
#' @export
summary.wyparcie_cytowan <- function(object, ...) {
  tabela <- object$tabela
  okreslone <- !is.na(tabela$d)
  rozklady <- sapply(tabela[c("D", "d", "Rk", "bp")], function(w) {
    w <- w[!is.na(w)]
    if (!length(w)) return(rep(NA_real_, 5))
    stats::quantile(w, c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
  })
  rownames(rozklady) <- c("min", "q25", "mediana", "q75", "max")
  structure(
    list(
      liczba_publikacji = nrow(tabela),
      statusy = object$diagnostyka$statusy,
      rozklady = rozklady,
      udzial_d_ujemny = if (any(okreslone)) mean(tabela$d[okreslone] < 0)
                        else NA_real_,
      udzial_d_dodatni = if (any(okreslone)) mean(tabela$d[okreslone] > 0)
                         else NA_real_,
      odchylenie_tozsamosci = object$diagnostyka$odchylenie_tozsamosci,
      parametry = object$parametry
    ),
    class = "summary.wyparcie_cytowan"
  )
}

#' @rdname metody_wyparcie_cytowan
#' @export
print.summary.wyparcie_cytowan <- function(x, cyfry = 3, ...) {
  cat(sprintf("Indeks przelomowosci, okno %d lat, publikacje: %d\n\n",
              as.integer(x$parametry$okno), x$liczba_publikacji))
  cat("Statusy:\n")
  statusy <- x$statusy[x$statusy > 0]
  print(data.frame(status = names(statusy), liczba = as.integer(statusy)),
        row.names = FALSE)
  cat("\nRozklady wskaznikow (wartosci okreslone):\n")
  print(signif(x$rozklady, cyfry))
  cat(sprintf("\nUdzial d < 0: %s, udzial d > 0: %s\n",
              format(x$udzial_d_ujemny, digits = cyfry),
              format(x$udzial_d_dodatni, digits = cyfry)))
  cat(sprintf("Najwieksze odchylenie od D = d / (1 + Rk): %s\n",
              format(x$odchylenie_tozsamosci, digits = 2)))
  invisible(x)
}

#' @rdname metody_wyparcie_cytowan
#' @export
plot.wyparcie_cytowan <- function(x, ...) {
  tabela <- x$tabela
  punkty <- tabela[!is.na(tabela$bp) & tabela$bp > 0 & !is.na(tabela$D),
                   c("id", "bp", "D", "d")]
  kolor <- ifelse(punkty$d < 0, "#B2182B",
                  ifelse(punkty$d > 0, "#2E5E9E", "grey50"))
  graphics::plot(
    punkty$bp, punkty$D, log = "x", pch = 19, col = kolor,
    ylim = c(-1, 1), las = 1,
    xlab = "b_p = C_max / C_p (skala logarytmiczna)",
    ylab = "D_p",
    main = sprintf("Indeks przelomowosci, okno %d lat",
                   as.integer(x$parametry$okno)),
    ...
  )
  graphics::abline(h = 0, col = "grey70", lty = 2)
  graphics::abline(v = 1, col = "grey70", lty = 3)
  graphics::legend("topright", pch = 19, col = c("#2E5E9E", "#B2182B", "grey50"),
                   legend = c("d > 0", "d < 0", "d = 0"), bty = "n")
  invisible(punkty)
}


#' Metody dla porownania okien
#'
#' Wypisanie i wykres obiektu klasy \code{porownanie_okien}. Wykres pokazuje
#' \eqn{D_p} kazdej publikacji w kolejnych oknach.
#'
#' @param x Obiekt klasy \code{porownanie_okien}.
#' @param cyfry Liczba cyfr znaczacych w wydruku.
#' @param ... Argumenty przekazywane dalej; w \code{plot()} do
#'   \code{\link[graphics]{matplot}}.
#'
#' @return \code{print()} niewidocznie zwraca \code{x}. \code{plot()}
#'   niewidocznie zwraca macierz \eqn{D_p} (wiersze: okna, kolumny:
#'   publikacje).
#'
#' @examples
#' siec <- przygotuj_siec(krawedzie_przyklad, publikacje_przyklad)
#' okna <- porownaj_okna(siec, cele = c("k01_cel", "k03_cel"), okna = c(3, 5))
#' print(okna)
#' plot(okna)
#'
#' @name metody_porownanie_okien
NULL

#' @rdname metody_porownanie_okien
#' @export
print.porownanie_okien <- function(x, cyfry = 3, ...) {
  tabela <- x$tabela[c("okno", "id", "Ni", "Nj", "Nk", "D", "d", "status")]
  tabela[c("D", "d")] <- lapply(tabela[c("D", "d")], signif, digits = cyfry)
  cat(sprintf("Porownanie okien cytowan: %s lat\n\n",
              paste(x$parametry$okna, collapse = ", ")))
  print(tabela, row.names = FALSE)
  invisible(x)
}

#' @rdname metody_porownanie_okien
#' @export
plot.porownanie_okien <- function(x, ...) {
  macierz <- tapply(x$tabela$D, list(x$tabela$okno, x$tabela$id), identity)
  okna <- as.numeric(rownames(macierz))
  graphics::matplot(
    okna, macierz, type = "b", pch = 19, lty = 1,
    col = grDevices::hcl.colors(ncol(macierz), "Dark 3"),
    ylim = c(-1, 1), las = 1, xaxt = "n",
    xlab = "Dlugosc okna (lata)", ylab = "D_p",
    main = "Indeks przelomowosci w kolejnych oknach", ...
  )
  graphics::axis(1, at = okna)
  graphics::abline(h = 0, col = "grey70", lty = 2)
  invisible(macierz)
}
