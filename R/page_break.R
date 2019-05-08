#' @importFrom knitr is_latex_output is_html_output opts_knit
#' @export
page_break <- function(format = NULL) {
  if (is.null(format)) {
    format <- if (is_latex_output()) {
      "latex"
    } else {
      opts_knit$get("rmarkdown.pandoc.to")
    }
  }
  switch(format,
    latex = "\\newpage",
    docx = '```{=openxml}\n<w:p><w:r><w:br w:type="page"/></w:r></w:p>\n```',
    ""
  )
}
