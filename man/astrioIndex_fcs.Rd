\name{astrioIndex_fcs}
\alias{astrioIndex_fcs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
astrioIndex_fcs
}
\description{
A simple tool to convert "Sort parameter" of Beckman Coulter Astrios sorters to X-sort and Y-sort parameters. Requires as input an fcs file.
}
\usage{
astrioIndex_fcs(fcs, out_file = TRUE, out_name="Indexed_file.fcs")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fcs}{
  Name (and location) of the fcs file to be indexed.
  }
  \item{out_file}{
  Bolean indicating whether to export the indexed FlowFrame to a new .fcs file. Default is TRUE.
  }
  \item{out_name}{
  Locatiion and name for the file to be exported. By default it exports in the working directory as "Indexed_file.fcs"
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
Function \code{\link{astrioIndex}}
}
\examples{
## Index an external .fcs file and save the new indexed .fcs file
astrioIndex_fcs(fcs = "/data/my_file.fcs",
                out_file = TRUE,
                out_name = "/data/my_file_indexed.fcs")
}
