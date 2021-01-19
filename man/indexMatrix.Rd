\name{indexMatrix}
\alias{indexMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
indexMatrix
}
\description{
Returns the sorting map of a indexed \code{flowframe} as a matrix.
}
\usage{
indexMatrix(ff, n_col=NA, n_row=NA)
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
Returns a matrix containing the sorting map of a indexed \code{flowframe}.
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
## Ger
}
