% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/implement_pond.R
\name{update_res_res_pond}
\alias{update_res_res_pond}
\title{Update the reservoir.res input table.}
\usage{
update_res_res_pond(res_res, res_res_pnd, hru_id, label)
}
\arguments{
\item{res_res}{reservoir.res input table}

\item{res_res_pnd}{table with rel, sed, and nut pointers for added pond.}

\item{hru_id}{ID of the HRU which is replaced by a pond.}

\item{label}{Either 'pnd' for pond or 'cwl' for constructed wetlands.}
}
\value{
Updated reservoir.res input table.
}
\description{
Add a new line in the reservoir.res table with the ID and name of the new
reservoir. Only the hydrology column \code{hyd} and the ID are updated. The other
pointers are set to the default entries (currently the case for all
reservoirs).
}
\keyword{internal}
