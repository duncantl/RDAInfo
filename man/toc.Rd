\name{toc}
\alias{toc}
\alias{as.data.frame.RDAToc}
\alias{print.RDAToc}

\title{Table of Contents for an RDA file/stream}
\description{
  This reads an R data (RDA) connection and identifies
  the top-level variables it contains.
  It returns a description
  for each of these variables, giving
  the R type, class, length, names.

  It avoids deserializing/creating the objects as much as possible.
  This is useful when we are dealing with large vectors, data.frames and R objects in general.
}
\usage{
toc(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{the name of the RDA file, or a connection}
}
\value{
  An object of class \code{RDAToc}.
  This is a list with as many elements as there are top-level
  variables/objects in the RDA file.
  Each element is a 1-row data.frame describing that object.
  Each of these data.frames may have different columns as there are different
  characteristics described for different types.

  The individual data.frames can be combined with the as.data.frame
  method
}
\references{
  R Internals manual
}
\author{
Duncan Temple Lang
}

\seealso{

}
\examples{

}
\keyword{IO}
\concept{deserialization}
