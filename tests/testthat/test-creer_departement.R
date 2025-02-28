test_that("creer_departement retourne un objet de classe 'departement'", {
  df <- data.frame(
    Libellé.du.département = "Gironde",
    Libellé.de.la.commune = c("Bordeaux", "Pessac")
  )

  departement <- creer_departement(df)
  expect_s3_class(departement, "departement")
})

test_that("creer_departement génère une erreur si 'Libellé.du.département' est absent", {
  df <- data.frame(Libellé.de.la.commune = c("Bordeaux", "Pessac"))
  expect_error(creer_departement(df), "Le data.frame doit contenir les colonnes 'Libellé.du.département' et 'Libellé.de.la.commune'.")
})
