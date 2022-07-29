render_to_docs <-
  function(x,
           output_format = "rmarkdown::html_document",
           output_dir = "docs") {
    rmarkdown::render(
      x,
      output_format = output_format,
      output_dir = output_dir
    )
  }

render_to_docs("docs_src/index.Rmd")
render_to_docs("vignettes/redcap_project_migration_app.Rmd")
render_to_docs("vignettes/redcap_project_migration_app2.Rmd")
