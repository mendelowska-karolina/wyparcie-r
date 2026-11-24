# Warstwa druga: wlasnosci, ktore musza zachodzic niezaleznie od danych.

siec_testowa <- local({
  g <- generuj_siec(n = 400, ziarno = 11)
  list(g = g, s = przygotuj_siec(g$krawedzie, g$publikacje))
})
cele_testowe <- siec_testowa$g$publikacje$id[
  siec_testowa$g$publikacje$rok %in% 2001:2003 |
    grepl("_cel$", siec_testowa$g$publikacje$id)
]
wynik_testowy <- oblicz_wyparcie(siec_testowa$s, cele_testowe, okno = 5)

# Niezalezna, najprostsza wersja obliczenia: petla po krawedziach.
licznosci_naiwne <- function(krawedzie, publikacje, p, okno) {
  rok <- stats::setNames(publikacje$rok, publikacje$id)
  y <- rok[[p]]
  w_oknie <- function(x) x[!is.na(rok[x]) & rok[x] >= y + 1 & rok[x] <= y + okno]
  f <- w_oknie(unique(krawedzie$cytujacy[krawedzie$cytowany == p]))
  zrodla <- unique(krawedzie$cytowany[krawedzie$cytujacy == p])
  b <- character(0)
  cmax <- 0
  for (z in zrodla) {
    bz <- w_oknie(unique(krawedzie$cytujacy[krawedzie$cytowany == z]))
    b <- union(b, bz)
    cmax <- max(cmax, length(bz))
  }
  c(Ni = sum(!f %in% b), Nj = sum(f %in% b), Nk = sum(!b %in% f), Cmax = cmax)
}

test_that("licznosci zgadzaja sie z niezalezna petla po krawedziach", {
  g <- siec_testowa$g
  t <- wynik_testowy$tabela
  wybrane <- c(1, 5, 25, 60, which(grepl("_cel$", t$id))[c(1, 7, 14)])
  for (i in wybrane) {
    ref <- licznosci_naiwne(g$krawedzie, g$publikacje, t$id[i], 5)
    expect_equal(unname(c(t$Ni[i], t$Nj[i], t$Nk[i], t$Cmax[i])),
                 unname(ref), info = t$id[i])
  }
})

test_that("tozsamosc D = d / (1 + Rk) zachodzi dokladnie", {
  t <- wynik_testowy$tabela
  ok <- !is.na(t$d)
  expect_true(sum(ok) > 50)
  expect_equal(t$D[ok], t$d[ok] / (1 + t$Rk[ok]), tolerance = 1e-12)
})

test_that("wskazniki naleza do swoich zakresow", {
  t <- wynik_testowy$tabela
  expect_true(all(t$D >= -1 & t$D <= 1, na.rm = TRUE))
  expect_true(all(t$d >= -1 & t$d <= 1, na.rm = TRUE))
  expect_true(all(t$Rk >= 0, na.rm = TRUE))
  expect_true(all(t$bp >= 0, na.rm = TRUE))
  expect_true(all(t$Ni >= 0 & t$Nj >= 0 & t$Nk >= 0))
  expect_equal(t$Cp, t$Ni + t$Nj)
})

test_that("Cmax jest ograniczone przez rozmiar B i liczbe zrodel", {
  # Kazda praca z B cytuje co najmniej jedno zrodlo, wiec
  # ceiling(|B| / liczba_zrodel) <= Cmax <= |B|.
  t <- wynik_testowy$tabela
  t <- t[t$liczba_zrodel > 0, ]
  b <- t$Nj + t$Nk
  expect_true(all(t$Cmax <= b))
  expect_true(all(t$Cmax >= ceiling(b / t$liczba_zrodel)))
})

test_that("bp i Rk spelniaja nierownosci wynikajace z definicji", {
  # Cmax <= |B| = Nj + Nk <= Cp + Nk, wiec bp <= 1 + Rk;
  # Nk <= |B| <= liczba_zrodel * Cmax, wiec Rk <= liczba_zrodel * bp.
  t <- wynik_testowy$tabela
  ok <- !is.na(t$bp)
  expect_true(all(t$bp[ok] <= 1 + t$Rk[ok] + 1e-12))
  expect_true(all(t$Rk[ok] <= t$liczba_zrodel[ok] * t$bp[ok] + 1e-12))
})

test_that("wynik nie zalezy od kolejnosci wierszy wejscia", {
  g <- siec_testowa$g
  withr::local_seed(3)
  s2 <- przygotuj_siec(g$krawedzie[sample(nrow(g$krawedzie)), ],
                       g$publikacje[sample(nrow(g$publikacje)), ])
  t2 <- oblicz_wyparcie(s2, cele_testowe, okno = 5)$tabela
  expect_equal(t2, wynik_testowy$tabela)
})

test_that("porownanie okien zgadza sie z obliczeniem dla pojedynczego okna", {
  s <- siec_testowa$s
  cele <- c("k01_cel", "k03_cel", "k14_cel")
  okna <- porownaj_okna(s, cele, okna = c(3, 5))
  w3 <- oblicz_wyparcie(s, cele, okno = 3)$tabela
  expect_equal(okna$tabela$D[okna$tabela$okno == 3], w3$D)
  # Prace cytujace skladowych maja lata 2016-2020 cyklicznie, wiec okno
  # 3 lat obejmuje ich czesc, a okno 5 lat wszystkie.
  expect_true(all(okna$tabela$Cp[okna$tabela$okno == 3] <=
                    okna$tabela$Cp[okna$tabela$okno == 5]))
})
