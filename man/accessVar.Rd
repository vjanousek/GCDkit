\name{accessVar}
\title{Accessing data in memory of R}
\alias{accessVar}


\description{
    Loads data already present in memory of R into GCDkit.
}

\usage{
accessVar(var=NULL,GUI=FALSE)
}

\arguments{
    \item{var}{a text string specifying the variable to be accessed}
    \item{GUI}{logical; is the function called from GUI (or from the command line)?}
}

\details{
    This function makes possible to access a variable, already present in R, most
    importantly the sample data sets.
    Firstly these need to be made available using the command \code{data}.
}

\value{
    \item{WR}{numeric matrix: all numeric data}
    \item{labels}{data frame:  all at least partly character fields;
    \code{labels$Symbol} contains plotting symbols and
    \code{labels$Colour} the plotting colours}

    The function prints a short summary about the attached data.
    It also loads and executes the Plugins, i.e. all the R code that
    is currently stored in the subdirectory '\code{\\Plugin}'.
}

\examples{
data(swiss)
accessVar("swiss")
binary("Catholic","Education")

data(sazava)
accessVar("sazava")
binary("SiO2","Ba")
}
\keyword{manip}
\concept{Menu: GCDkit}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
