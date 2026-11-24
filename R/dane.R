#' Przykladowe publikacje sieci cytowan
#'
#' Publikacje sieci wygenerowanej funkcja \code{\link{generuj_siec}} w
#' skrypcie \code{data-raw/generuj_dane.R}: 1000 publikacji sieci bazowej
#' z lat 2000-2019 oraz 20 skladowych kontrolnych o znanych licznosciach,
#' z publikacjami docelowymi \code{k01_cel}, ..., \code{k20_cel} z roku 2015.
#'
#' @format Ramka danych z kolumnami:
#' \describe{
#'   \item{id}{identyfikator publikacji}
#'   \item{rok}{rok publikacji}
#' }
#'
#' @source Dane symulowane, \code{data-raw/generuj_dane.R}.
#'
#' @examples
#' table(publikacje_przyklad$rok)
"publikacje_przyklad"


#' Przykladowe krawedzie sieci cytowan
#'
#' Krawedzie sieci opisanej w \code{\link{publikacje_przyklad}}. Krawedz
#' \code{cytujacy -> cytowany} oznacza odwolanie publikacji \code{cytujacy}
#' do publikacji \code{cytowany}.
#'
#' @format Ramka danych z kolumnami:
#' \describe{
#'   \item{cytujacy}{identyfikator publikacji cytujacej}
#'   \item{cytowany}{identyfikator publikacji cytowanej}
#' }
#'
#' @source Dane symulowane, \code{data-raw/generuj_dane.R}.
#'
#' @examples
#' head(krawedzie_przyklad)
"krawedzie_przyklad"
