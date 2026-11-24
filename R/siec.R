#' Przygotowanie sieci cytowan
#'
#' Funkcja zamienia tabele krawedzi i tabele publikacji w obiekt sieci
#' gotowy do liczenia indeksow przelomowosci. Krawedz \code{cytujacy ->
#' cytowany} oznacza, ze praca \code{cytujacy} odwoluje sie do pracy
#' \code{cytowany}. Powtorzone krawedzie zostaja usuniete, a ich liczba
#' trafia do diagnostyki. Krawedz wskazujaca te sama publikacje zatrzymuje
#' wykonanie.
#'
#' Publikacja wystepujaca w krawedziach, a nieobecna w tabeli publikacji, nie
#' ma roku. Taka praca nie moze zostac przypisana do okna cytowan, a zrodlo bez
#' rekordu oznacza niepelna wiedze o jego cytowaniach. Oba przypadki sa
#' raportowane w diagnostyce i w statusie wyniku \code{\link{oblicz_wyparcie}}.
#'
#' @param krawedzie Ramka danych z kolumnami \code{cytujacy} i
#'   \code{cytowany} (identyfikatory publikacji).
#' @param publikacje Ramka danych z kolumnami \code{id} i \code{rok}.
#'
#' @return Obiekt klasy \code{siec_cytowan}, lista ze skladnikami:
#'   \describe{
#'     \item{krawedzie}{ramka danych krawedzi bez powtorzen}
#'     \item{publikacje}{ramka danych publikacji}
#'     \item{id}{wektor identyfikatorow wszystkich prac w sieci}
#'     \item{rok}{wektor lat publikacji (\code{NA} dla prac bez rekordu)}
#'     \item{zrodla}{lista: dla kazdej pracy indeksy prac, ktore cytuje}
#'     \item{cytujace}{lista: dla kazdej pracy indeksy prac, ktore ja cytuja}
#'     \item{diagnostyka}{liczba usunietych powtorzen, liczba krawedzi
#'       i publikacji, identyfikatory prac bez rekordu}
#'   }
#'
#' @examples
#' krawedzie <- data.frame(cytujacy = c("c1", "c2", "c2", "c3"),
#'                         cytowany = c("p", "p", "z", "z"))
#' publikacje <- data.frame(id = c("z", "p", "c1", "c2", "c3"),
#'                          rok = c(2009, 2010, 2011, 2012, 2012))
#' przygotuj_siec(krawedzie, publikacje)
#'
#' @export
przygotuj_siec <- function(krawedzie, publikacje) {
  # TODO: kontrola wejscia zgodna z kontraktem bledow (SPEC.md):
  #   - ramki danych z kolumnami `cytujacy`, `cytowany` oraz `id`, `rok`,
  #   - niepuste identyfikatory bez NA,
  #   - jednoznaczne `id`, komunikat
  #     "Identyfikatory publikacji musza byc jednoznaczne",
  #   - `rok` liczbowy, bez NA dla prac potrzebnych do okna, komunikat
  #     "Brak metadanych potrzebnych do okna cytowan".
  #   Komunikaty bez znakow diakrytycznych, stop(..., call. = FALSE).
  #   Testy: tests/testthat/test-kontrakt-bledow.R (obecnie pomijane).
  if (!is.data.frame(krawedzie) || !is.data.frame(publikacje)) {
    stop("Argumenty `krawedzie` i `publikacje` musza byc ramkami danych.",
         call. = FALSE)
  }
  cytujacy <- as.character(krawedzie$cytujacy)
  cytowany <- as.character(krawedzie$cytowany)
  if (any(cytujacy == cytowany)) {
    stop("Krawedz nie moze wskazywac tej samej publikacji.", call. = FALSE)
  }

  klucz <- paste(cytujacy, cytowany, sep = "\r")
  powtorzone <- duplicated(klucz)
  cytujacy <- cytujacy[!powtorzone]
  cytowany <- cytowany[!powtorzone]

  id_publikacji <- as.character(publikacje$id)
  id <- unique(c(id_publikacji, cytujacy, cytowany))
  rok <- publikacje$rok[match(id, id_publikacji)]
  bez_rekordu <- setdiff(id, id_publikacji)

  i_cytujacy <- match(cytujacy, id)
  i_cytowany <- match(cytowany, id)
  poziomy <- factor(seq_along(id))
  zrodla <- unname(split(i_cytowany, factor(i_cytujacy, levels = poziomy)))
  cytujace <- unname(split(i_cytujacy, factor(i_cytowany, levels = poziomy)))

  structure(
    list(
      krawedzie = data.frame(cytujacy = cytujacy, cytowany = cytowany,
                             stringsAsFactors = FALSE),
      publikacje = data.frame(id = id_publikacji, rok = publikacje$rok,
                              stringsAsFactors = FALSE),
      id = id,
      rok = rok,
      zrodla = zrodla,
      cytujace = cytujace,
      diagnostyka = list(
        usuniete_powtorzenia = sum(powtorzone),
        liczba_krawedzi = length(cytujacy),
        liczba_publikacji = length(id_publikacji),
        bez_rekordu = bez_rekordu
      )
    ),
    class = "siec_cytowan"
  )
}


#' @rdname przygotuj_siec
#' @param x Obiekt klasy \code{siec_cytowan}.
#' @param ... Argumenty przekazywane dalej, obecnie nieuzywane.
#' @export
print.siec_cytowan <- function(x, ...) {
  d <- x$diagnostyka
  cat("Siec cytowan\n")
  cat(sprintf("  publikacje z rekordem:   %d\n", d$liczba_publikacji))
  cat(sprintf("  krawedzie:               %d\n", d$liczba_krawedzi))
  cat(sprintf("  usuniete powtorzenia:    %d\n", d$usuniete_powtorzenia))
  cat(sprintf("  prace bez rekordu:       %d\n", length(d$bez_rekordu)))
  if (all(!is.na(x$rok))) {
    cat(sprintf("  lata:                    %d-%d\n",
                as.integer(min(x$rok)), as.integer(max(x$rok))))
  }
  invisible(x)
}
