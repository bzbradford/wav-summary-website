# remove site folder
unlink("site", recursive = TRUE)

# source setup scripts
list.files(".", pattern = "^setup_.+\\.R$", full.names = TRUE) |>
  purrr::walk(source)

# generate site
list.files(".", pattern = "\\.qmd$", full.names = TRUE) |>
  purrr::walk(quarto::quarto_render)
