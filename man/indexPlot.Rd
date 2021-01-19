\name{indexPlot}
\alias{indexPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
indexPlot
}
\description{
Plots the sorting map. Each dot in the plot corresponds to the physical X, Y position of the microplate used for the sorting.
The size of each dot is proportional to the number of events sorted at that position/well.
}
\usage{
indexPlot(ff, n_col=NA, n_row=NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ff}{
flowFrame object, arlready indexed by \code{\link{astrioIndex}} or \code{\link{astrioIndex_fcs}}
}
  \item{n_col}{
  Optional, default is \code{NA}. Sets the number of columns of the microplate used for cell sorting. If \code{NA}, \code{n_col} is set as the higest X-index value.
  }
  \item{n_row}{
  Optional, default is \code{NA}. Sets the number of rows of the microplate used for cell sorting. If \code{NA}, \code{n_row} is set as the higest Y-index value.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a \code{ggplot} object.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Marco De Zuani, PhD
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Functions \code{\link{astrioIndex}} and \code{\link{astrioIndex_fcs}}.
}
\examples{
## Plots the sorting map of the "fframe" \code{flowframe} on a 96 well microplate.
index_plot(ff = fframe, n_col = 12, n_row = 8)

