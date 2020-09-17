# YAML --------------------------------------------------------------------

#' yaml_today
#'
#' A truly lazy way to insert today's date into an Rmarkdown document.
#' To use, write date: nicethings::yaml_today() in the YAML header of an
#' Rmarkdown document.
#' Pinched from https://stackoverflow.com/a/25389694 and {nicethings}
#' @examples
#' \dontrun{
#'  # An example yaml header would go
#'  date: '`r nicethings::yaml_today()`'
#' }
#' @export

yaml_today <- function(){
  return(format(Sys.Date(), "%d %B %Y"))
}

#' Set-up repo for paper
#'
#' @param paper_name Document name
#' @param paper_type Future proofing for different paper types
#' @param csl Type of Citation Style Language you wish to use
#'
#' @return
#' @export
aca_setup_paper <- function(paper_name = "manuscript",
                            paper_type = "normal",
                            csl = "nature") {

  usethis::use_directory("report")
  usethis::use_directory("data")
  usethis::use_directory("R")

  lib_text <- c("# pkgs: start","# pkgs: end")

  writeLines(lib_text,file.path("R","0_library.R"))

  aca_use_github("mcguinlu","mal")

  usethis::use_template("manuscript-standard.Rmd",
                        save_as = file.path("report",paste0(paper_name,".Rmd")),
                        package = "mal",
                        data = list(citation_style = csl))


  file.copy(find_word_template(), here::here("report",
                                             "word-styles-reference-01.docx"))

  file.create(file.path("report","references.bib"))

  aca_citation_format(csl)

}


#' Add package to the library fi
#'
#' @param package
#'
#' @return
#' @export
#'
#' @examples
aca_use_package <- function(package) {

  path <- here::here("R","0_library.R")
  pkg_start <- "# pkgs: start"
  pkg_end <- "# pkgs: end"

  changed <- usethis:::block_append(
    desc = glue::glue("{package}"),
    value = glue::glue("if (!require(\"{package}\")) install.packages(\"{package}\"); library({package})"),
    path = path,
    block_start = pkg_start,
    block_end = pkg_end
  )

  invisible(changed)
}


#' Add GitHub package to 0_library.R
#'
#' @param user GitHub username
#' @param package Package name
#'
#' @return
#' @export
#'
#' @examples
aca_use_github <- function(user, package) {

  path <- here::here("R","0_library.R")
  pkg_start <- "# pkgs: start"
  pkg_end <- "# pkgs: end"

  aca_use_package("devtools")

  changed <- usethis:::block_append(
    desc = glue::glue("{package}"),
    value = glue::glue("if (!require(\"{package}\")) devtools::install_github(\"{user}/{package}\"); library({package})"),
    path = path,
    block_start = pkg_start,
    block_end = pkg_end
  )

  invisible(changed)
}

#' Add Citation Style Language (CSL) file to directory
#'
#' @param citation_style Name of the CSL you wish to use. Taken from
#'   https://github.com/citation-style-language/styles
#' @param dir Directory to add CSL file to
#'
#' @return
#' @export

aca_citation_format <- function(citation_style = "nature", dir = "report") {

  url <- paste0("https://raw.githubusercontent.com/citation-style-language/",
                "styles/master/",
                citation_style,
                ".csl")

  csl <- readLines(url)

  writeLines(csl,here::here("report",paste0(citation_style,".csl")))
}


#' Formats estimates and 95\% confidence intervals for nice printing.
#'
#' Rounds estimates to 1 decimal place and copies similarly formatted confidence
#' intervals inside brackets. If you want a \strong{really} good inline output
#' from a regression model, see Benjamin Nutter's
#' \code{\link[pixiedust]{dust_inline}} from his package
#' \href{https://github.com/nutterb/pixiedust}{pixiedust}
#' @param estimate An estimate such as a rate ratio
#' @param lci The lower confidence interval
#' @param uci The upper confidence interval
#' @return A string in format d.d (95\% CI: d.d-d.d)
#' @examples
#' nice_estimate(100.111, 90.0, 110.000002)
#' nice_estimate(0.9, 0.8001, 0.95)
#' @export
mal_estimate <- function(estimate, lci, uci, type = "OR"){

  if (type != "") {
    type <- paste0(type, ": ")
  }

  if(estimate > uci | estimate < lci){
    stop("Estimate outside CI bounds")
  }

  if(lci > uci){
    stop("Lower CI is greater than upper CI")
  }

  estimate <- stringr::str_trim(sprintf("%7.1f", estimate))
  lci <- stringr::str_trim(sprintf("%7.1f", lci))
  uci <- stringr::str_trim(sprintf("%7.1f", uci))
  z <- paste0(type, estimate, " (95% CI:", lci, "-", uci, ")" )
  return(z)
}

#' Create a formatted table using common defaults
#'
#' @param data Data to present
#' @param fontsize Fontsize
#' @param autofit Autofit?
#' @param bold_first_column Whether or not to bold first column.
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr '%>%'
aca_simple_table <-
  function(data,
           fontsize = 7,
           autofit = TRUE,
           bold_first_column = TRUE) {

  ft <- flextable::flextable(data) %>%
  flextable::bg(bg = "#A6A6A6", part = "header") %>%
  flextable::bold(part = "header") %>%
  flextable::align(align = "center", part = "all" ) %>%
  flextable::bg(i = ~ seq(from = 1, to = nrow(data)) %% 2 == 0, bg = "#DDDDDD", part = "body")  %>%
  flextable::fontsize(size = fontsize, part = "all")

  if (autofit == TRUE) {
  ft <- flextable::set_table_properties(ft, layout = "autofit")
  }

  if (bold_first_column == TRUE) {
  ft <- flextable::bold(ft, j = 1, part = "body")
  }

return(ft)

}


#' Set-up common GitHub Actions
#'
#' @export

setup_gha <- function(){
  usethis::use_readme_rmd()
  usethis::use_lifecycle_badge("experimental")
  usethis::use_github_action_check_standard()
  usethis::use_github_action("pkgdown")
  usethis::use_github_action("test-coverage")
  rmarkdown::render("README.Rmd", output_format = "github_document")
  usethis::use_pkgdown()
}
