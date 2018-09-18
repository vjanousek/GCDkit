\name{info}
\title{Info on datafile}
\alias{info}

\description{
Prints information about the current dataset (and its selected subset, if applicable).
}

\usage{
info()
}


\details{

This function prints comprehensive information about the current dataset. 
For each of the labels, individual levels and their frequencies are given. 
The number of numeric columns is printed, and for each of the variables number 
of available values. Moreover, the information concerning the total number of 
samples, the names of the samples in the selected subset (or all samples if none 
is defined) and the current grouping are shown.  
}

\value{
None
}

\keyword{misc}
\concept{Print}
\concept{Menu: GCDkit}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
