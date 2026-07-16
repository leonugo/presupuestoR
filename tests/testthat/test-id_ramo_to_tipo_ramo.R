# id_ramo_to_tipo_ramo() test

test_that("Ramo 56 (IMSS-Bienestar) se clasifica junto a IMSS/ISSSTE", {
  # Ramo 56 se formalizó a partir del ciclo 2026. Antes del fork,
  # id_ramo_to_tipo_ramo() lo dejaba como NA porque no aparecía en
  # ninguna de las categorías (ver NEWS.md 0.1.1).
  expect_equal(id_ramo_to_tipo_ramo("56"),
               "Entidades sujetas a control presupuestario directo")
  expect_equal(id_ramo_to_tipo_ramo(56),
               "Entidades sujetas a control presupuestario directo")
  # Debe seguir clasificando igual que antes a IMSS y ISSSTE.
  expect_equal(id_ramo_to_tipo_ramo("50"),
               "Entidades sujetas a control presupuestario directo")
  expect_equal(id_ramo_to_tipo_ramo("51"),
               "Entidades sujetas a control presupuestario directo")
})

test_that("id_ramo_to_tipo_ramo() regresa NA para ramos no clasificados", {
  expect_true(is.na(id_ramo_to_tipo_ramo("999")))
})

test_that("Ramos 54 (Mujeres) y 55 (ATDT) se clasifican como Ramos administrativos", {
  # Ambos se crearon con la reforma a la Ley Organica de la Administracion
  # Publica Federal (DOF 28-nov-2024), vigente desde el ciclo 2025-2026.
  # Antes del fork quedaban NA -- ver NEWS.md.
  expect_equal(id_ramo_to_tipo_ramo("54"), "Ramos administrativos")
  expect_equal(id_ramo_to_tipo_ramo("55"), "Ramos administrativos")
  expect_equal(id_ramo_to_tipo_ramo(54), "Ramos administrativos")
  expect_equal(id_ramo_to_tipo_ramo(55), "Ramos administrativos")
})
