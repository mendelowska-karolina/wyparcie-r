# Warstwa pierwsza: przypadki o wyniku policzonym recznie ze wzorow
# D = (Ni - Nj) / (Ni + Nj + Nk), d = (Ni - Nj) / (Ni + Nj),
# Rk = Nk / (Ni + Nj), bp = Cmax / Cp, Cp = Ni + Nj.

test_that("(Ni, Nj, Nk) = (1, 0, 1)", {
  # a cytuje tylko p, b cytuje tylko zrodlo z:
  # F = {a}, B = {b}; Ni = 1, Nj = 0, Nk = 1
  # D = 1 / 2 = 0,5; d = 1 / 1 = 1; Rk = 1 / 1 = 1; Cmax = 1, bp = 1
  s <- siec_reczna(krawedz("p", "z", "a", "p", "b", "z"))
  t <- oblicz_wyparcie(s, "p")$tabela
  expect_equal(c(t$Ni, t$Nj, t$Nk), c(1, 0, 1))
  expect_equal(t$D, 0.5)
  expect_equal(t$d, 1)
  expect_equal(t$Rk, 1)
  expect_equal(t$bp, 1)
  expect_identical(t$status, "zdefiniowany")
})

test_that("(0, 0, 1) daje D = 0 i nieokreslone d, Rk, bp", {
  # Tylko b cytuje zrodlo: Ni + Nj + Nk = 1 > 0, wiec D = 0 / 1 = 0,
  # ale Cp = 0, wiec d, Rk i bp nie istnieja.
  s <- siec_reczna(krawedz("p", "z", "b", "z"))
  t <- oblicz_wyparcie(s, "p")$tabela
  expect_equal(t$D, 0)
  expect_true(all(is.na(c(t$d, t$Rk, t$bp))))
  expect_identical(t$status, "brak_cytowan")
})

test_that("(2, 1, 3) z Cmax = 4", {
  # F = {a1, a2, c}; c cytuje takze z1, wiec c nalezy do F n B.
  # B = {c, e1, e2, e3}; z1 cytuja c, e1, e2, e3 (4 prace), z2 tylko e3.
  # Ni = 2, Nj = 1, Nk = 3; Cp = 3; Cmax = 4 (rowne |B|, maksimum mozliwe)
  # D = (2 - 1) / 6 = 1/6; d = 1/3; Rk = 3/3 = 1; bp = 4/3
  s <- siec_reczna(krawedz(
    "p", "z1", "p", "z2",
    "a1", "p", "a2", "p", "c", "p", "c", "z1",
    "e1", "z1", "e2", "z1", "e3", "z1", "e3", "z2"
  ))
  t <- oblicz_wyparcie(s, "p")$tabela
  expect_equal(c(t$Ni, t$Nj, t$Nk, t$Cp, t$Cmax), c(2, 1, 3, 3, 4))
  expect_equal(t$D, 1 / 6)
  expect_equal(t$d, 1 / 3)
  expect_equal(t$Rk, 1)
  expect_equal(t$bp, 4 / 3)
})

test_that("(0, 0, 0) daje nieokreslony D", {
  s <- siec_reczna(krawedz("p", "z"))
  t <- oblicz_wyparcie(s, "p")$tabela
  expect_true(is.na(t$D))
  expect_identical(t$status, "zerowy_mianownik")
})

test_that("publikacja bez zrodel ma D = 1 i status brak_zrodel", {
  # Ni = 2 (a, b), B pusty: D = 2 / 2 = 1, bp nieokreslony
  s <- siec_reczna(krawedz("a", "p", "b", "p"))
  t <- oblicz_wyparcie(s, "p")$tabela
  expect_equal(t$D, 1)
  expect_true(is.na(t$bp))
  expect_identical(t$status, "brak_zrodel")
})

test_that("praca cytujaca kilka zrodel nalezy do B raz", {
  # e cytuje z1, z2 i z3: |B| = 1, Nk = 1, Cmax = 1
  s <- siec_reczna(krawedz("p", "z1", "p", "z2", "p", "z3",
                           "a", "p", "e", "z1", "e", "z2", "e", "z3"))
  t <- oblicz_wyparcie(s, "p")$tabela
  expect_equal(c(t$Ni, t$Nj, t$Nk, t$Cmax), c(1, 0, 1, 1))
  expect_equal(t$D, 0.5)
})

test_that("okno obejmuje lata od rok + 1 do rok + okno", {
  # a z roku publikacji (2010) i d z 2016 sa poza oknem 2011-2015
  s <- siec_reczna(krawedz("p", "z", "a", "p", "b", "p", "c", "p", "d", "p"),
                   lata = c(a = 2010, b = 2011, c = 2015, d = 2016))
  expect_equal(oblicz_wyparcie(s, "p", okno = 5)$tabela$Ni, 2)
  expect_equal(oblicz_wyparcie(s, "p", okno = 6)$tabela$Ni, 3)
  expect_equal(oblicz_wyparcie(s, "p", okno = 1)$tabela$Ni, 1)
})

test_that("powtorzone krawedzie sa usuwane i liczone", {
  s <- siec_reczna(krawedz("p", "z", "a", "p", "a", "p", "b", "z", "b", "z"))
  expect_equal(s$diagnostyka$usuniete_powtorzenia, 2)
  t <- oblicz_wyparcie(s, "p")$tabela
  expect_equal(c(t$Ni, t$Nj, t$Nk), c(1, 0, 1))
})

test_that("zrodlo bez rekordu oznacza niekompletna siec", {
  krawedzie <- data.frame(cytujacy = c("p", "p", "a", "b"),
                          cytowany = c("z1", "z2", "p", "z1"))
  publikacje <- data.frame(id = c("p", "z1", "a", "b"),
                           rok = c(2010, 2009, 2011, 2011))
  s <- przygotuj_siec(krawedzie, publikacje)
  expect_equal(s$diagnostyka$bez_rekordu, "z2")
  t <- oblicz_wyparcie(s, "p")$tabela
  expect_identical(t$status, "niekompletne_zrodla")
})

test_that("praca cytujaca bez roku jest pomijana i raportowana", {
  krawedzie <- data.frame(cytujacy = c("p", "a", "x"),
                          cytowany = c("z", "p", "p"))
  publikacje <- data.frame(id = c("p", "z", "a"), rok = c(2010, 2009, 2011))
  w <- oblicz_wyparcie(przygotuj_siec(krawedzie, publikacje), "p")
  expect_equal(w$tabela$Ni, 1)
  expect_equal(w$diagnostyka$cytujace_bez_roku, 1)
})

test_that("publikacja spoza sieci ma status brak_rekordu", {
  s <- siec_reczna(krawedz("p", "z", "a", "p"))
  t <- oblicz_wyparcie(s, c("p", "nieznana"))$tabela
  expect_identical(t$status, c("zdefiniowany", "brak_rekordu"))
  expect_true(is.na(t$D[2]))
})
