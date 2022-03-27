#' @export

knit_chisq <- function(data, x, y, x_name = NULL, y_name = NULL) {

  if (is.null(x_name)) {
    x_name <- x
  }

  if (is.null(y_name)) {
    y_name <- y
  }

  cat(
    asis_output(
      knit_child(input = system.file("templates/template_knit_chisq.Rmd", package = "dredge"),
                 envir = environment(),
                 quiet = TRUE)
      )
    )

}

#' @export


print_chisq_text <- function(data, x, y, var1_name, var2_name, decimal_separator = ".") {
  chi.obj <- chisq.test(table(data[,x],data[,y]))
  chi.statistic <- gsub("\\.",decimal_separator,force_decimals(chi.obj$statistic))

  chi_rs <- glue::glue("$\\chi^2({chi.obj$parameter[1]}) = {chi.statistic}$, {format_p(chi.obj$p.value)}")

  if (chi.obj$p.value < 0.05) {
    if (decimal_separator == ",") {
      glue::glue("Atlikus Chi-kvadrato testą rasta statistiškai reikšminga asociacija tarp {var1_name} ir {var2_name} ({chi_rs}).")
    } else {
      glue::glue("Pearson's $\\chi^2$ test showed a statistically significant association between between {var1_name} and {var2_name} ({chi_rs}).")
    }

  } else {
    if (decimal_separator == ",") {
      glue::glue("Atlikus Chi-kvadrato testą statistiškai reikšminga asociacija tarp {var1_name} ir {var2_name} nebuvo nustatyta ({chi_rs}).")
    } else {
      glue::glue("Pearson's $\\chi^2$ test did not show an association between {var1_name} and {var2_name} ({chi_rs}).")
    }

  }
}
