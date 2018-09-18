\name{pokeDataset}
\alias{pokeDataset}

\title{
    Storing a dataset into memory for later use
}

\description{
    Saves the current dataset into memory so that it can be later re-stored. 
}

\usage{
pokeDataset(which.dataset=NULL, 
    par.list="WR,WRanh,milli,labels,filename,groups,grouping,init,age",
    overwrite.warn=TRUE)
}

\arguments{
  \item{which.dataset}{character; a name of the stored dataset.}
  \item{par.list}{list of global variables to be stored.}
  \item{overwrite.warn}{logical, warn if a dataset is going to be rewritten in '\code{WRCube}'. See Details.}
}

\details{
    This function stores  the global variables specified by \code{par.list}, typically 
    '\code{WR}', '\code{WRanh}', '\code{milli}' '\code{labels}', 
    '\code{filename}', '\code{groups}' and '\code{grouping}'  
    into the list '\code{WRCube}'. 
    
    If no \code{which.dataset} is provided upon the call, it can be typed in or
    selected from the list of existing datasets. 
    
    Please note that '\code{\link{pokeDataset}}' is also invoked when a new dataset 
    is loaded into memory using the functions '\code{\link{loadData}}' 
    or  '\code{\link{accessVar}}'. In the former case it is stored under 
    the name of the file, in the latter under the variable name. 
    If such a name already exists in '\code{WRCube}', a time stamp is attached. 
    
    For restoring the stored variables serve functions '\code{\link{peekDataset}}' and 
    '\code{\link{selectDataset}}'. The function '\code{\link{purgeDatasets}}' removes 
    all older datasets, apart from the most recent copy of the current one.     
}

\value{
    None.
}

\section{Warning}{
    If not called from a GUI, no warning is issued upon rewriting the existing dataset. 
}

\seealso{
  '\code{\link{peekDataset}}' '\code{\link{selectDataset}}' '\code{\link{purgeDatasets}}'
  '\code{\link{loadData}}' '\code{\link{accessVar}}'
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
}

\keyword{file}
\concept{File}
\concept{Switching datasets}
\concept{Menu: GCDkit}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
