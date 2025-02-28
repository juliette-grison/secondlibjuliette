test_that("creer_commune retourne un objet de classe 'commune'", {
  df <- data.frame(
    Libellé.de.la.commune = "Paris",
    Libellé.du.département = "Paris"
  )

  commune <- creer_commune(df)
  expect_s3_class(commune, "commune")
})

test_that("creer_commune génère une erreur si la colonne 'Libellé.de.la.commune' est absente", {
  df <- data.frame(Libellé.du.département = "Paris")
  expect_error(creer_commune(df), "Le data.frame doit contenir les colonnes 'Libellé.de.la.commune' et 'Libellé.du.département'.")
})

