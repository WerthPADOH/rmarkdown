context("formats")

test_that("formats successfully produce a document", {

  skip_on_cran()

  testFormat <- function(output_format, df_print = NULL) {
    output_file <- I(tempfile())
    render("test-formats.Rmd",
           output_format = output_format,
           output_file = output_file,
          # output_options = ifelse(is.null(df_print), NULL, list(df_print = df_print)),
           quiet = TRUE)
    expect_true(file.exists(output_file))
    output_file
  }

  testFormat(html_document(), df_print = "kable")
  testFormat(html_notebook())
  testFormat(html_fragment(), df_print = "tibble")
  testFormat(html_vignette(), df_print = "tibble")
  testFormat(ioslides_presentation(), df_print = "kable")
  testFormat(slidy_presentation(), df_print = "kable")
  testFormat(md_document(), df_print = "kable")
  testFormat(pdf_document(), df_print = "kable")
  testFormat(beamer_presentation(), df_print = "kable")
  testFormat(word_document(), df_print = "kable")
  testFormat(html_vignette())

  if (requireNamespace("tufte", quietly = TRUE))
    testFormat(tufte_handout())
})

test_that("documents with spaces in names can be rendered", {

  skip_on_cran()

  # get path to notebook
  rmd_path <- "resources/empty.Rmd"

  # attempt to write to directory with spaces
  output_file <- "directory with spaces/r output.nb.html"
  dir.create(dirname(output_file))
  on.exit(unlink("directory with spaces", recursive = TRUE), add = TRUE)

  # generate copy with space in name
  with_spaces <- "directory with spaces/no content.Rmd"
  file.copy(rmd_path, with_spaces)

  output <- rmarkdown::render(with_spaces,
                              output_format = "html_notebook",
                              output_file = output_file,
                              quiet = TRUE)

  expect_true(file.exists(output))

})

test_that(
  "setting theme, highlight or fig_retina yields an error on html_vignette",
  {

  skip_on_cran()

  testFormat <- function(output_format) {
    output_file <- tempfile()
    expect_error(
      render("test-formats.Rmd",
           output_format = output_format,
           output_file = output_file,
           quiet = TRUE)
    )
  }

  testFormat(html_vignette(theme = "z"))
  testFormat(html_vignette(highlight = "z"))
  testFormat(html_vignette(fig_retina = 2))

})

test_that("page_break inserts the right page break for the output type", {
  rmd_lines <- c(
    readLines("test-formats.Rmd"),
    "`r rmarkdown::page_break()`",
    "PAGEBREAK ABOVE AND BELOW",
    "`r rmarkdown::page_break()`"
  )
  broken_rmd <- tempfile()
  writeLines(rmd_lines, broken_rmd)

  # check_fun: function returning TRUE if the rendered document has a page with
  #   just the phrase:
  #     PAGEBREAK ABOVE AND BELOW
  #   If it doesn't, returns FALSE.
  test_page_break <- function(output_format, check_fun) {
    output_file <- render(
      input = broken_rmd,
      output_format = output_format,
      output_file = I(tempfile()),
      quiet = TRUE
    )
    on.exit(file.remove(output_file))
    expect_true(check_fun(output_file))
    output_file
  }

  test_page_break(pdf_document(), function(path) {
    pages <- pdftools::pdf_text(path)
    stripped <- trimws(pages)
    sum(stripped == "PAGEBREAK ABOVE AND BELOW") == 1
  })

  # test_page_break(word_document(), function(path) {
  #
  # })
})
