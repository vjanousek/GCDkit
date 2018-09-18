\name{selectSubset}
\title{Select subset}
\alias{selectSamples}
\alias{selectSubset}

\description{
Selects samples corresponding to given criteria.
}

\usage{
selectSubset(what=NULL,where=cbind(labels,WR),save=TRUE,multiple=TRUE,
text="Press ENTER for all samples, or specify search pattern \n by sample name, range or Boolean condition",
range=FALSE,GUI=FALSE, all.nomatch=TRUE)

selectSamples(what=NULL, print=TRUE, multiple=TRUE, text=NULL)
}

\arguments{
  \item{what}{search pattern}  
  \item{where}{data to be searched}
  \item{save}{should the newly selected subset replace the data in memory, i.e. '\code{labels}' and '\code{WR}'}
  \item{multiple}{logical, can be multiple items selected?}
  \item{text}{text prompt}
  \item{range}{logical: is the search pattern to be interpreted as a range of samples?}
  \item{GUI}{logical: is the function called from within GUI?}
  \item{all.nomatch}{logical: return all samples when there is no match?}
  \item{print}{logical: should be the chosen samples ID printed?}
  
}


\details{
The function '\code{selectSubset}' has two purposes.
 
1. If '\code{save=TRUE}', it is a core function used in selecting subsets of the 
current data set by ranges (see \code{\link{subsetRange}}) or Boolean conditions 
(see \code{\link{subsetBoolean}}). 

2. If \code{save=FALSE}, no permanent subsetting takes place. This is useful for 
temporary selections of the data, e.g. in determining which samples are to be 
plotted on a diagram. 

In this case, the samples can be selected based on combination of three searching 
mechanisms. The search pattern is first tested whether it obeys a syntax of a 
valid \code{\link{regular expression}} that could be interpreted as a query 
directed to the sample name(s). 

If not, the syntax of the search pattern is assumed to correspond to a selection 
of sample sequence numbers. 

At the last resort, the search pattern is interpreted as a Boolean condition 
that may employ most of the comparison operators common in R, i.e.  < (lower 
than), > (greater than), <= (lower or equal to), >= (greater or equal to), = or 
== (equal to), != (not equal to). The character strings should be quoted. 
Regular expressions can be employed to search the textual labels.

The conditions can be combined together by logical \code{and, or} and brackets.

Logical \code{and} can be expressed as \code{.and. .AND. &}
  
Logical \code{or} can be expressed as \code{.or. .OR. |}


The function '\code{selectSamples}' is a front-end to '\code{selectSubset}'.  
}


\section{Warning}{
So far only names of existing numeric data columns and not formulae involving 
these can be handled.
}

\value{
If '\code{save=TRUE}', the function overwrites the data frame '\code{labels}' and 
numeric matrix '\code{WR}' by subset that fulfills the search criteria. 
Otherwise names of samples fulfilling the given criteria are returned.
}

\seealso{
\code{\link{regex}}, \code{\link{selectByLabel}} and \code{\link{selectAll}}
}

\examples{
# permanent selection, the variables 'WR' and 'labels' affected
selectSubset("SiO2>70") 

# back to the complete, originally loaded dataset
selectAll() 

# both expressions below return only sample names of analyses fulfilling 
# the given criteria, variables 'WR' and 'labels' NOT affected
selectSamples("SiO2<70&MgO>5")
selectSubset("SiO2<70&MgO>5",save=FALSE)  

\dontrun{
#EXAMPLES OF SEARCHING PATTERNS
# Searching by sample name

The sample names are: Bl-1, Bl-3, Koz-1, Koz-2, 
Koz-5, Koz-11, KozD-1, Ri-1.

oz-[1-3]    
# Samples Koz-1, Koz-2, Koz-11

oz-|Bl- 
# Samples Bl-1, Bl-2, Bl-3, Koz-1, Koz-2, Koz-5, Koz-11

# Searching by range

1:5
# First to fifth samples in the data set

1,10
# First and tenth samples

1:5, 10:11, 25  
# Samples number 1,  2, ...5, 10, 11, 25

# Searching by Boolean
######################

Intrusion="Rum"
# Finds all analyses from Rum

Intrusion="Rum".and.SiO2>65
Intrusion="Rum".AND.SiO2>65
Intrusion="Rum"&SiO2>65
# All analyses from Rum with silica greater than 65 
# (all three expressions are equivalent) 


MgO>10&(Locality="Skye"|Locality="Islay")
# All analyses from Skye or Islay with MgO greater than 10

Locality="^S"
# All analyses from any locality whose name starts with capital S
}
}

\keyword{manip}
\keyword{database}
\concept{Subsetting}
\concept{Edit}
\concept{Menu: Data handling}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
