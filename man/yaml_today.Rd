% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmarkdown.R
\name{yaml_today}
\alias{yaml_today}
\title{yaml_today}
\usage{
yaml_today()
}
\description{
A truly lazy way to insert today's date into an Rmarkdown document.
To use, write date: nicethings::yaml_today() in the YAML header of an
Rmarkdown document.
Pinched from https://stackoverflow.com/a/25389694 and {nicethings}
}
\examples{
\dontrun{
 # An example yaml header would go
 date: '`r nicethings::yaml_today()`'
}
}
