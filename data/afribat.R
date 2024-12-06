delayedAssign("afribats_df", local({
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(afribat:::afribats_df)
  } else {
    afribat:::afribats_df
  }
}))


# https://www.mm218.dev/posts/2022-12-01-sf-in-packages/ see Update section
delayedAssign("afribats_sf", local({
  try(
    sf::read_sf(
      system.file("extdata/afribats_sf.gpkg", package = "afribat")
    ),
    silent = TRUE
  )
}))
