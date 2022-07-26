test_that("can parse mime text/plain", {
  a <- parse_content_type("text/plain; name=\"myfile.txt\";charset=UTF-8")
  e <- list(
    "type" = "text/plain",
    "name" = "myfile.txt",
    "charset" = "UTF-8"
  )
  p <- c("type", "name", "charset")
  expect_equal(a[p], e[p])
})

test_that("can parse mime image/png", {
  a <- parse_content_type("image/png; name=\"myfile.png\"")
  e <- list(
    "type" = "image/png",
    "name" = "myfile.png"
  )
  p <- c("type", "name")
  expect_equal(a[p], e[p])
})
