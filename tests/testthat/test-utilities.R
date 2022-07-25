test_that("parse_content_type", {
  p <- c("type", "name", "charset")

  a <- parse_content_type("text/plain; name=\"myfile.txt\";charset=UTF-8")
  e <- list(
    "type" = "text/plain",
    "name" = "myfile.txt",
    "charset" = "UTF-8"
  )
  expect_equal(a[p], e[p])

  a <- parse_content_type("image/png; name=\"myfile.png\"")
  e <- list(
    "type" = "image/png",
    "name" = "myfile.png",
    "charset" = NA_character_
  )
  expect_equal(a[p], e[p])
})
