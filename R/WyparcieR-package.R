#' WyparcieR: indeks przelomowosci publikacji jako miara wyparcia
#'
#' Pakiet wyznacza klasyczny indeks przelomowosci \eqn{D_p} wraz ze
#' skladowymi opisanymi przez Lin, Li i Wu (2026): licznosciami \eqn{N_i},
#' \eqn{N_j}, \eqn{N_k}, bilansem bezposrednio cytujacych \eqn{d_p},
#' stosunkiem \eqn{R_k} i czynnikiem \eqn{b_p} zwiazanym z najczesciej
#' cytowanym zrodlem. Skladowe pozwalaja ustalic, czy zmiana indeksu wynika
#' z cytowan publikacji, czy z popularnosci jej zrodel.
#'
#' Sciezka analizy:
#' \itemize{
#'   \item \code{\link{przygotuj_siec}} zamienia tabele krawedzi i publikacji
#'     w siec cytowan,
#'   \item \code{\link{oblicz_wyparcie}} liczy wskazniki dla publikacji
#'     docelowych w ustalonym oknie,
#'   \item \code{\link{porownaj_okna}} zestawia wyniki dla kilku okien,
#'   \item \code{\link{generuj_siec}} tworzy siec o znanej strukturze.
#' }
#'
#' @references Lin, Y., Li, L., Wu, L. (2026). The disruption index measures
#'   displacement between a paper and its most cited reference. Quantitative
#'   Science Studies, 7, 229--239. \doi{10.1162/QSS.a.409}
#'
#'   Funk, R. J., Owen-Smith, J. (2017). A dynamic network measure of
#'   technological change. Management Science, 63(3), 791--817.
#'   \doi{10.1287/mnsc.2015.2366}
#'
#' @keywords internal
"_PACKAGE"
