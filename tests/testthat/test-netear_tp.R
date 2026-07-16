# netear_tp() test
#
# Fija el comportamiento validado el 2026-07-16 contra cifras oficiales
# reales (ver el comentario junto a netear_tp() en R/analisis_ppto.R y
# NEWS.md 0.1.1 para la metodología completa). No es solo un test de forma:
# los montos usados aquí replican los casos concretos que se probaron
# contra PEF 2022, PEF 2026 y Cuenta Pública 2022/2025.

fila_base <- function(id_ramo, id_partida_especifica, monto_aprobado,
                      id_capitulo = NA, id_ur = "1", desc_ur = "Otra",
                      id_modalidad = "E", id_pp = "1") {
  tibble::tibble(
    id_ramo = id_ramo,
    id_partida_especifica = id_partida_especifica,
    id_capitulo = if (is.na(id_capitulo))
      as.numeric(substr(id_partida_especifica, 1, 1)) * 1000
    else id_capitulo,
    id_ur = id_ur,
    desc_ur = desc_ur,
    id_modalidad = id_modalidad,
    id_pp = id_pp,
    desc_ramo = "Ramo de prueba",
    monto_aprobado = monto_aprobado
  )
}

test_that("netea las 8 partidas de aportaciones ISSSTE/cesantia salvo en ramo 51", {
  datos <- dplyr::bind_rows(
    fila_base("19", "14101", 100),  # se debe netear
    fila_base("19", "16107", 100),  # se debe netear
    fila_base("51", "14101", 100),  # NO se debe netear: excepcion ramo 51
    fila_base("51", "16107", 100)   # NO se debe netear: excepcion ramo 51
  )

  resultado <- netear_tp(datos)
  neteados <- resultado[resultado$desc_ramo == "Neteo", ]

  # deben existir exactamente 2 filas duplicadas de Neteo (las de ramo 19)
  expect_equal(nrow(neteados), 2)
  expect_true(all(neteados$id_ramo == "19"))
  # las de ramo 51 no generan fila de Neteo
  expect_false(any(resultado$id_ramo == "51" & resultado$desc_ramo == "Neteo"))
})

test_that("la excepcion de ramo 51 aplica a las 8 partidas por igual (no solo a 16107)", {
  # Esta es la discrepancia central resuelta el 2026-07-16: una
  # reimplementacion externa solo eximia a la partida 16107 en ramo 51,
  # lo que sub-neteaba (sobre-restaba) el total frente a la cifra oficial
  # decretada en PEF 2022 y PEF 2026 (ver NEWS.md 0.1.1).
  partidas_isste <- c("14101", "14105", "16104", "16107",
                      "83102", "83110", "83113", "83116")
  datos <- dplyr::bind_rows(
    lapply(partidas_isste, fila_base, id_ramo = "51", monto_aprobado = 100)
  )

  resultado <- netear_tp(datos)

  # ninguna de las 8 debe generar fila de Neteo cuando id_ramo == 51
  expect_equal(sum(resultado$desc_ramo == "Neteo"), 0)
})

test_that("no agrega una regla para la partida 45203 (confirmado innecesaria)", {
  # Comprobado el 2026-07-16 contra PEF 2022, PEF 2026, Cuenta Publica
  # 2022 y Cuenta Publica 2025: agregar esta regla nunca cambio ningun
  # resultado, porque esas filas ya caen en la regla de capitulo 4000 +
  # ramo 19 + UR ISSSTE/IMSS cuando de verdad son una transferencia
  # interna. Este test fija que netear_tp() no trata 45203 como un caso
  # especial fuera de esa regla existente.
  datos <- fila_base("19", "45203", 100, id_capitulo = 4000,
                     id_ur = "1", desc_ur = "Otra dependencia de ramo 19")

  resultado <- netear_tp(datos)

  expect_equal(sum(resultado$desc_ramo == "Neteo"), 0)
})

test_that("netea capitulo 4000 + ramo 19 + UR ISSSTE/IMSS", {
  datos <- dplyr::bind_rows(
    fila_base("19", "45203", 100, id_capitulo = 4000, id_ur = "GYR"),
    fila_base("19", "44101", 50, id_capitulo = 4000,
             desc_ur = "Instituto Mexicano del Seguro Social")
  )

  resultado <- netear_tp(datos)

  expect_equal(sum(resultado$desc_ramo == "Neteo"), 2)
})

test_that("netea ramo 23 + modalidad U + programa 129", {
  datos <- fila_base("23", "99999", 100, id_capitulo = 9000,
                     id_modalidad = "U", id_pp = "129")

  resultado <- netear_tp(datos)

  expect_equal(sum(resultado$desc_ramo == "Neteo"), 1)
})
