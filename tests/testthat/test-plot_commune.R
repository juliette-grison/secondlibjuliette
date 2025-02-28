library(dplyr)
library(ggplot2)

test_that("plot.commune génère un ggplot valide", {
  df <- data.frame(
    Libellé.de.la.commune = "Paris",
    Libellé.du.département = "Paris",
    Code.de.la.catégorie.socio.professionnelle = c("A", "B", "A")
  )

  commune <- creer_commune(df)
  p <- plot.commune(commune)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("plot.commune génère une erreur si l'objet n'est pas de classe 'commune'", {
  df <- data.frame(Libellé.de.la.commune = "Paris")
  expect_error(plot.commune(df), "L'objet fourni n'est pas une commune")
})
