# Création d'un jeu de données pour les tests
df <- data.frame(
  "Libellé.de.la.commune" = rep("Paris", 5),
  "Nom.de.l.élu" = c("Dupont", "Martin", "Bernard", "Durand", "Petit"),
  "Prénom.de.l.élu" = c("Jean", "Marie", "Paul", "Sophie", "Claire"),
  "Date.de.naissance" = as.Date(c("1960-05-12", "1980-07-24", "1975-03-30", "1990-12-10", "1985-06-15"))
)
class(df) <- c("commune", "data.frame")

test_that("summary.commune() renvoie une erreur si l'objet n'est pas de classe commune", {
  df_invalide <- df
  class(df_invalide) <- "data.frame" # Retirer la classe "commune"
  expect_error(summary.commune(df_invalide), "L'objet fourni n'est pas une commune")
})

test_that("summary.commune() renvoie une erreur si plusieurs communes sont présentes", {
  df_multiple_communes <- df
  df_multiple_communes$Libellé.de.la.commune <- c("Paris", "Lyon", "Marseille", "Bordeaux", "Nice")
  expect_error(summary.commune(df_multiple_communes), "Le dataframe contient plusieurs communes")
})

