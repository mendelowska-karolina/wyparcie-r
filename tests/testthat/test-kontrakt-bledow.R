# Warstwa trzecia: dane, przy ktorych metoda nie ma sensu.
#
# Testy opisuja kontrakt bledow z briefu tematu. Oczekiwanym wynikiem jest
# zatrzymanie wykonania z komunikatem, a nie liczba. Walidacja wejscia nie
# jest jeszcze w pelni zaimplementowana, wiec czesc testow jest pomijana;
# po implementacji kontroli w R/siec.R (przygotuj_siec) i w R/wyparcie.R
# (oblicz_wyparcie, porownaj_okna) nalezy usunac wywolania pomin().

pomin <- function() {
  skip("Walidacja wejscia do implementacji (kontrakt bledow, SPEC.md).")
}

publikacje_ok <- data.frame(id = c("p", "z", "a"), rok = c(2010, 2009, 2011))
krawedzie_ok <- data.frame(cytujacy = c("p", "a"), cytowany = c("z", "p"))

test_that("krawedz wskazujaca te sama publikacje zatrzymuje wykonanie", {
  krawedzie <- rbind(krawedzie_ok, data.frame(cytujacy = "a", cytowany = "a"))
  expect_error(przygotuj_siec(krawedzie, publikacje_ok),
               "Krawedz nie moze wskazywac tej samej publikacji")
})

test_that("powtorzony identyfikator publikacji zatrzymuje wykonanie", {
  pomin()
  publikacje <- rbind(publikacje_ok, data.frame(id = "a", rok = 2012))
  expect_error(przygotuj_siec(krawedzie_ok, publikacje),
               "Identyfikatory publikacji musza byc jednoznaczne")
})

test_that("brak roku publikacji docelowej zatrzymuje wykonanie", {
  pomin()
  publikacje <- publikacje_ok
  publikacje$rok[1] <- NA
  expect_error(oblicz_wyparcie(przygotuj_siec(krawedzie_ok, publikacje), "p"),
               "Brak metadanych potrzebnych do okna cytowan")
})

test_that("niepoprawne okno zatrzymuje wykonanie", {
  pomin()
  s <- przygotuj_siec(krawedzie_ok, publikacje_ok)
  expect_error(oblicz_wyparcie(s, "p", okno = 0),
               "Okno musi byc dodatnia liczba calkowita")
  expect_error(oblicz_wyparcie(s, "p", okno = 2.5),
               "Okno musi byc dodatnia liczba calkowita")
  expect_error(porownaj_okna(s, "p", okna = c(3, -1)),
               "Okno musi byc dodatnia liczba calkowita")
})

test_that("brakujace kolumny lub puste identyfikatory zatrzymuja wykonanie", {
  pomin()
  expect_error(przygotuj_siec(krawedzie_ok[, 1, drop = FALSE], publikacje_ok),
               "cytowany")
  krawedzie <- krawedzie_ok
  krawedzie$cytujacy[1] <- ""
  expect_error(przygotuj_siec(krawedzie, publikacje_ok), "identyfikator")
})
