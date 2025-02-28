library(dplyr)
library(ggplot2)

test_that("plot.departement génère un ggplot valide", {
  df <- data.frame(
    Libellé.du.département = "Gironde",
    Libellé.de.la.commune = c("Bordeaux", "Pessac"),
    Code.de.la.catégorie.socio.professionnelle = c("A", "B", "A", "C", "B", "A")
  )

  departement <- creer_departement(df)
  p <- plot.departement(departement)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("plot.departement génère une erreur si l'objet n'est pas de classe 'departement'", {
  df <- data.frame(Libellé.du.département = "Gironde")
  expect_error(plot.departement(df), "L'objet fourni n'est pas un département")
})
