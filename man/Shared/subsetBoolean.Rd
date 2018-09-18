\name{Boolean conditions}
\title{Select subset by Boolean condition}
\alias{subsetBoolean}

\description{
Selecting subsets of the current dataset using Boolean conditions that can query 
both numeric fields and labels. Regular expressions can be employed to search 
the labels.
}

\details{
The menu item '\code{Select subset by Boolean}', connected to the function 
\code{\link{selectSubset}}, enables the user to query by any combination of the 
numeric columns and labels in the whole dataset. The current data will be 
replaced by its newly chosen subset.

First, the user is prompted to enter a search pattern which can contain 
conditions that may employ most of the comparison operators common in R, i.e.  
< (lower than), > (greater than), <= (lower or equal to), >= (greater or equal 
to), = or == (equal to), != (not equal to). The character strings should be 
quoted. The conditions can be combined together by logical \code{and, or} and 
brackets.

Logical \code{and} can be expressed as \code{.and. .AND. &}
  
Logical \code{or} can be expressed as \code{.or. .OR. |}    

Please note that at the moment no extra spaces can be handled (apart from in 
quoted character strings).
}

\value{
Overwrites the data frame '\code{labels}' and numeric matrix '\code{WR}' by 
subset that fulfills the search criteria. 
}

\examples{
\dontrun{
# Valid search patterns

Intrusion="Rum"
# Finds all analyses from Rum

Intrusion="Rum".and.SiO2>65
Intrusion="Rum".AND.SiO2>65
Intrusion="Rum"&SiO2>65
# All analyses from Rum with silica greater than 65 
# (all three expressions are equivalent) 


MgO>10&(Locality="Skye"|Locality="Islay")
# All analyses from Skye or Islay with MgO greater than 10

MgO>=10&(Locality!="Skye"&Locality!="Islay")
# All analyses from any locality except Skye and Islay with MgO greater 
# or equal to 10

Locality="^S"
# All analyses from any locality whose name starts with capital S
}
}

\keyword{manip}
\keyword{database}
\concept{Subsetting}
\concept{Edit}
\concept{Menu: Data handling}

\seealso{
\code{\link{regular.expressions}} \code{\link{regex}}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
