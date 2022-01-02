\name{Load}
\alias{Load}
\title{A layer above the load() function for deserializing RDA files}
\description{
  This function restores a user-specified subset of the variables
  in an RDA file.
  It also allows one to remap the variables names in the RDA
  file to different variables names when restoring the objects.
  By default, it avoids overwriting existing variables.
  It returns the names of the loaded variables
  without making them invisible().

  This is a drop-in replacement for \code{\link[base]{load}},
  with the first three arguments being passed directly to \code{\link[base]{load}}.
  
  This version currently restores all the objects in the RDA file into
  a temporary environment and then assigning only the ones of interest to the
  target environment, discarding the other variables.
  So this is a helper function. The Table-of-Contents (RDAToc) approach
  allows us to restore just the variables of interest, but with
  a caveat of not yet fully handling reference objects in the RDA.
}
\usage{
Load(file, envir = parent.frame(), verbose = FALSE, vars = character(), .noOverwrite = TRUE)
}
\arguments{
  \item{file}{ the path to the RDA file, or a connection}
  \item{envir}{the environment into which the objects will be assigned/loaded}
  \item{verbose}{should item names be printed during loading?}
  \item{vars}{a character vector of variable names identifying the variables in the RDA file to restore/load.
    The elements of this character vector can have a name, e.g., \code{c(x = "a")}
    and the name specifies the new/target variable name to which the value will be assigned.
    In other words, the values in \code{vars} identify the variables in the RDA file;
    any names on this vector specify the target variables in \code{envir} to which they will be assigned.
  }
%  \item{\dots}{ name=value pairs which   allows the  caller to specify \code{vars} without the \code{c()}.  May be removed}
}
  \item{.noOverwrite}{a logical value that controls whether to overwrite any existing variables in
  \code{envir}.}
}
\details{
This current calls \code{load} and then copies the subset of objects to \code{envir}.
}
\value{
  a character vector  which is the subset of the combination of \code{vars} and \dots
  for which the assignment took place.
}

\author{
Duncan Temple Lang
}

\seealso{
\code{\link[base]{load}}
}
\examples{

  #  Create a simple RDA file.
  rda = tempfile()
  a = 1:10
  b = letters[1:5]
  d = mtcars
  save(a, b, d, file = rda)


  # load a and b into a new environment and also ask for non-existent f
  e = new.env()
  Load(rda, e, vars = c("a", xyz = "b", "f"))
  # result shows a and xyz=b, but no f.

  # do it again and we won't overwrite
  Load(rda, e, vars = c("a", xyz = "b"))  
}
\keyword{IO}
