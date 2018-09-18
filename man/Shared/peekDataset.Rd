\name{peekDataset}
\alias{peekDataset}
\alias{selectDataset}

\title{
    Retrieving previous dataset stored in memory
}

\description{
    Both functions restore the previously stored dataset and make it current. 
}

\usage{
    peekDataset(which.dataset=NULL)
    selectDataset()
}

\arguments{
  \item{which.dataset}{numeric or character; a sequence number or name of the stored dataset.}
}

\details{
    The function '\code{peekDataset}' restores a dataset saved previously into memory by 
    the function '\code{\link{pokeDataset}}'. This means that it assigns 
    all global variables specified by individual items of the list '\code{WRCube}'. 
    
    These typically are: '\code{WR}', '\code{WRanh}', '\code{milli}',  
    '\code{labels}',  '\code{filename}', '\code{groups}' and '\code{grouping}'.
    
    The function '\code{selectDataset}' provides a graphical interface to 
    '\code{\link{peekDataset}}', i.e. shows a list box filled by the names of datasets
    currently stored in the memory.
}

\value{
    None. But several global variables, among others '\code{WR}', '\code{WRanh}', 
    '\code{milli}' and '\code{labels}', are affected. 
    The name of the current dataset is stored in '\code{dataset.name}'. 
}

\seealso{
  '\code{\link{pokeDataset}}' '\code{\link{purgeDatasets}}'
}

\examples{
data(sazava)
accessVar("sazava")
# stored as sazava in WRCube

assignColVar("MgO","blues")
assign1symb(15)
# store a new copy in the WRCube
pokeDataset("coloured sazava")
    
data(swiss)
accessVar("swiss")
# stored as swiss in WRCube
    
peekDataset("sazava")
binary("SiO2","Ba")
        
peekDataset("coloured sazava")
binary("SiO2","Ba")
    
peekDataset("swiss")
binary("Catholic","Education",pch=15,col="darkgreen")

peekDataset(2)
binary("SiO2","Sr")
}

\keyword{file}
\concept{File}
\concept{Switching datasets}
\concept{Menu: GCDkit}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

