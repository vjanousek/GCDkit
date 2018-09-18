\name{purgeDatasets}
\alias{purgeDatasets}

\title{
    Removing stored datasets from the memory
}

\description{
    Removes all the stored datasets (apart from the current one) in order to save memory. 
}

\usage{
    purgeDatasets(GUI=FALSE)
}

\arguments{
  \item{GUI}{logical; is the function called from GUI?}
}

\details{
    This function removes all older datasets, regardless whether stored automatically by 
    the functions '\code{\link{loadData}}' or '\code{\link{accessVar}}', as well as on 
    demand by '\code{\link{pokeDataset}}'. 
    
    Only the most recent copy of the current dataset is preserved (i.e. the last
    item within the list '\code{WRCube}').
}

\section{Warning}{
    If not called from a GUI, no warning is issued and all but the current dataset 
    are deleted immediately. 
}

\value{
    None.
}

\seealso{
  '\code{\link{pokeDataset}}' '\code{\link{peekDataset}}' '\code{\link{selectDataset}}'
}

\keyword{file}
\concept{Switching datasets}
\concept{Menu: GCDkit}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
