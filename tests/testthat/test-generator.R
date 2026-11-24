# Procedura generowania danych o znanej strukturze: licznosci skladowych
# kontrolnych sa prawda konstrukcyjna, a siec bazowa ma realistyczny wzorzec
# cytowan.

test_that("generator jest powtarzalny i nie zmienia stanu generatora", {
  set.seed(1)
  przed <- .Random.seed
  a <- generuj_siec(n = 150)
  expect_identical(.Random.seed, przed)
  b <- generuj_siec(n = 150)
  expect_identical(a, b)
})

test_that("skladowe kontrolne sa odtwarzane dokladnie", {
  g <- generuj_siec(n = 150)
  s <- przygotuj_siec(g$krawedzie, g$publikacje)
  t <- oblicz_wyparcie(s, g$prawda$id, okno = 5)$tabela
  expect_equal(t[c("Ni", "Nj", "Nk", "Cp", "Cmax", "liczba_zrodel")],
               g$prawda[c("Ni", "Nj", "Nk", "Cp", "Cmax", "liczba_zrodel")],
               ignore_attr = TRUE)
  expect_equal(t$D, g$prawda$D)
  expect_equal(t$bp, g$prawda$bp)
})

test_that("siatka skladowych obejmuje bp < 1, bp = 1 i bp > 1", {
  p <- generuj_siec(n = 50)$prawda
  expect_true(any(p$bp < 1, na.rm = TRUE))
  expect_true(any(p$bp == 1, na.rm = TRUE))
  expect_true(any(p$bp > 10, na.rm = TRUE))
  expect_setequal(unique(p$liczba_zrodel), c(3, 10, 30))
})

test_that("warianty niekompletnosci maja zadany rozmiar", {
  g <- generuj_siec(n = 300)
  m <- nrow(g$krawedzie)
  expect_equal(nrow(g$warianty$usuniete_5), m - round(0.05 * m))
  expect_equal(nrow(g$warianty$usuniete_20), m - round(0.20 * m))
  obciete <- g$warianty$obciete_3
  rok <- stats::setNames(g$publikacje$rok, g$publikacje$id)
  expect_true(all(rok[obciete$cytujacy] - rok[obciete$cytowany] <= 3))
})

test_that("siec bazowa ma czesciej ujemny niz dodatni bilans d", {
  # Lin, Li i Wu (2026) raportuja d < 0 dla 65% i d > 0 dla 32% prac.
  # Kopiowanie zrodel daje wspolcytowanie publikacji z jej zrodlami.
  g <- generuj_siec()
  s <- przygotuj_siec(g$krawedzie, g$publikacje)
  cele <- g$publikacje$id[grepl("^b", g$publikacje$id) &
                            g$publikacje$rok %in% 2001:2014]
  t <- oblicz_wyparcie(s, cele, okno = 5)$tabela
  ok <- !is.na(t$d)
  expect_gt(mean(t$d[ok] < 0), mean(t$d[ok] > 0))
  expect_gt(mean(t$bp[ok] > 1), 0.9)
})
