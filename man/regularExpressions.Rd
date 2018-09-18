\name{Regular expressions}
\title{Implementation of regular expressions in GCDkit}
\alias{regular expressions}
\alias{regular expression}
\alias{regular.expression}
\alias{regular.expressions}

\description{Implementation of regular expressions in the searching patterns.}

\details{
Many enquiries in the GCDkit employ regular expressions. This is a quite powerful 
searching mechanism more familiar to people working in Unix. Put in simple terms, 
most characters, including all letters and digits, are regular expressions that 
match themselves. However, metacharacters with a special meaning ('?' '+' '\{' 
'\}' '|' '(' ')') must be preceded by a backslash. 

\tabular{ll}{
\bold{Regular expression     } \tab \bold{Matches}\cr
. \tab Any character\cr
\code{^} \tab Beginning of the expression\cr
\$ \tab End of the expression\cr
[ ]  \tab Any of the characters given in square brackets\cr
[m-n]\tab Any character in the range given by \code{m} and \code{n}\cr
}

A subexpression is a regular expression enclosed in '\\(' and '\\)'. 
Two such subexpressions may be joined by the infix operator '|' (logical or); 
the resulting regular expression matches any string matching either of them. 
For instance:

\code{\\(South\\)|\\(North\\)Uist}

yields both

\code{South Uist} and \code{North Uist}.

A regular expression may be followed by one of several repetition operators:
 
\tabular{ll}{
\bold{Repetition operator} \tab \bold{The preceding item will be matched}\cr
?   \tab At most once (i.e. is optional)\cr
\code{*}   \tab Zero or more times \cr
+   \tab One or more times \cr
\{n\} \tab Exactly \code{n} times \cr
\{n,\}  \tab At least n times \cr
\{n,m\}   \tab At least \code{n} times, but not more than \code{m} times \cr 
}
}

\examples{
\dontrun{
# Subset by label
The searched field corresponds to localities with the following levels: 
Mull, Rum, Skye, Coll, Colonsay, Hoy, Westray, Sanday, 
Stronsay, Tiree, Islay

Search pattern = ol       
Coll, Colonsay

Search pattern = n.a      
Colonsay, Sanday, Stronsay

Search pattern = ^S       
Skye, Sanday, Stronsay

Search pattern = e$       
Skye, Tiree

Search pattern = [ds]ay   
Colonsay, Sanday, Stronsay

Search pattern = [p-s]ay
Colonsay, Westray, Stronsay

Search pattern = ol|oy    
Coll, Colonsay, Hoy

Search pattern = l\{2\}   
Mull, Coll


# Subset by sample name
The sample names are: Bl-1, Bl-3, Koz-1, Koz-2, Koz-5, Koz-11, 
KozD-1, Ri-1.

Search pattern = oz-[1-3]    
Koz-1, Koz-2, Koz-11

Search pattern = oz-|Bl- 
Bl-1, Bl-2, Bl-3, Koz-1, Koz-2, Koz-5, Koz-11
}
}

\keyword{database}
\concept{Edit}

\seealso{
\code{\link{regex}}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
