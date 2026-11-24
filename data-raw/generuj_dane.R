# Zbiory przykladowe pakietu.
#
# Skrypt nie wchodzi w sklad zbudowanego pakietu; zapisuje zbiory do katalogu
# data/. Uruchomienie z katalogu glownego pakietu:
#     Rscript -e "devtools::load_all(); source('data-raw/generuj_dane.R')"
#
# Zbiory powstaja ta sama procedura co dane eksperymentu (generuj_siec),
# w tej samej skali: 1000 publikacji sieci bazowej i 20 skladowych
# kontrolnych o licznosciach znanych z konstrukcji.

siec <- generuj_siec(n = 1000, ziarno = 202704)

publikacje_przyklad <- siec$publikacje
krawedzie_przyklad <- siec$krawedzie

stopifnot(
  !anyDuplicated(publikacje_przyklad$id),
  !anyDuplicated(krawedzie_przyklad),
  all(krawedzie_przyklad$cytujacy != krawedzie_przyklad$cytowany),
  all(c(krawedzie_przyklad$cytujacy, krawedzie_przyklad$cytowany) %in%
        publikacje_przyklad$id),
  sum(grepl("_cel$", publikacje_przyklad$id)) == 20
)

dir.create("data", showWarnings = FALSE)
save(publikacje_przyklad, file = "data/publikacje_przyklad.rda",
     compress = "xz", version = 3)
save(krawedzie_przyklad, file = "data/krawedzie_przyklad.rda",
     compress = "xz", version = 3)

# TODO: osobny skrypt pobrania danych OpenAlex (CC0) dla studium przypadku:
#   20 publikacji z 2015 r. z jednego podobszaru badan nad tozsamoscia
#   plciowa i seksualnoscia, ich zrodla oraz prace z lat 2016-2020 cytujace
#   publikacje lub choc jedno zrodlo (bez ograniczenia do podobszaru).
#   Zapis surowych odpowiedzi, daty i parametrow pobrania; nieobecny rekord
#   lub niedostepna lista zrodel jako brak informacji. Pobranie poza
#   funkcjami pakietu i testami.
