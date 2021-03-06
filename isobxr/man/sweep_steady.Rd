% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_sweepers.R
\name{sweep_steady}
\alias{sweep_steady}
\title{Sweep the space of two parameters at the final state of a run}
\usage{
sweep_steady(
  workdir,
  SERIES_ID,
  time_units,
  EXPLO_MASTER,
  EXPLO_AXIS_1,
  EXPLO_AXIS_2,
  to_STD_DIGEST_CSVs = FALSE
)
}
\arguments{
\item{workdir}{Working directory of \strong{\emph{0_ISOBXR_MASTER.xlsx}} master file, \cr
of the steady sweep master file (e.g., \strong{\emph{0_EXPLO_STEADY_MASTER.xlsx}}) \cr
and where output files will be stored. \cr
(character string)}

\item{SERIES_ID}{Name of the sweep series the run belongs to. \cr
It determines the folder in which the output files will be stored for this sweep run.\cr
A sweep run number is automatically linked to it,
and subsequent sweep runs can not overwrite a previous one.\cr
(character string)}

\item{time_units}{Vector defining the initial time unit
(identical to unit used in fluxes), \cr
followed by the time unit used for the graphical output.\cr
Character string, to be selected  amongst the following:\cr
\emph{micros, ms, s, min, h, d, wk, mo, yr, kyr, Myr, Gyr}\cr
e.g.,  c("d", "yr") to convert days into years}

\item{EXPLO_MASTER}{Name of the steady sweep master file (e.g., \strong{\emph{0_EXPLO_STEADY_MASTER.xlsx}}),
defining the steady sweep run scenario. \cr
(character string)}

\item{EXPLO_AXIS_1}{Set of values of sweeping parameter 1. \cr
See Vignette for further details.}

\item{EXPLO_AXIS_2}{Set of values of sweeping parameter 2. \cr
See Vignette for further details.}

\item{to_STD_DIGEST_CSVs}{\emph{OPTIONAL} \cr
Logical value. \cr
Exports all global csv outputs to \strong{\emph{0_STD_DIGEST}} folder (full and final evD and evS) if TRUE. \cr
Default is FALSE.}
}
\value{
Creates and stores all outputs in a dedicated sweep steady SERIES directory
located in working directory, with the following name structure: \cr
\strong{\emph{4_STD + SERIES_ID + YYY}}, where YYY is a sweep steady run number automically set between 001 and 999. \cr
No overwriting of previous sweep steady run runs is possible.

\enumerate{
\item Calculates the number of single runs the sweeping will require depending on the sweeped parameters.
\item Asks the user confirmation to run \code{\link{sweep_steady}},
as the run calculation time depends on the number of successive sweeping runs.
\item Writes the set of inputs and outputs for the single initial run only with the following format: \cr
\strong{\emph{STD + SERIES_ID + YYY + 0001 + IN.Rda}} \cr
\strong{\emph{STD + SERIES_ID + YYY + 0001 + OUT.Rda}} \cr
(see \code{\link{run_isobxr}} documentation).

\item Writes summarized results in the  \strong{\emph{0_STD_DIGEST}} folder:
\enumerate{
\item Archived LOG file of local sweep steady run. \cr
(file name structure:  \strong{\emph{STD + SERIES_ID + YYY + _LOG.csv}})
\item Archived sweep steady master file. \cr
(file name structure:  \strong{\emph{STD + SERIES_ID + YYY + _MASTER.xlsx}})
\item Dataset of temporal evolution of delta values (evD) in all boxes over the \emph{n} runs that constitute the sweep steady run. \cr
(file name structure: \strong{\emph{STD + SERIES_ID + YYY + evD.RDS}})
\item Dataset of temporal evolution of box sizes (evS, masses of X) in all boxes over the \emph{n} runs that constitute the sweep steady run. \cr
(file name structure: \strong{\emph{STD + SERIES_ID + YYY + evS.RDS}})
\item Dataset of final state of delta values (evD) in all boxes over the \emph{n} runs that constitute the sweep steady run. \cr
(file name structure: \strong{\emph{STD + SERIES_ID + YYY + evD_final.RDS}})
\item Dataset of final state of box sizes (evS, masses of X) in all boxes over the \emph{n} runs that constitute the sweep steady run. \cr
(file name structure: \strong{\emph{STD + SERIES_ID + YYY + evS_final.RDS}})
}
}
}
\description{
A function to assess the influence of two parameters (varying
over a range of values) on the final state of a given model.
}
\section{Optional output}{

\enumerate{
\item If to_STD_DIGEST_CSVs = TRUE \cr
In the \strong{\emph{0_STD_DIGEST}} folder,
edits csv versions of the sweep steady run datasets (full and final evD and evS) with the following name structures:
\enumerate{
\item \strong{\emph{STD + SERIES_ID + YYY + evD.csv}}
\item \strong{\emph{STD + SERIES_ID + YYY + evS.csv}}
\item \strong{\emph{STD + SERIES_ID + YYY + evD_final.csv}}
\item \strong{\emph{STD + SERIES_ID + YYY + evS_final.csv}}
}
}
}

