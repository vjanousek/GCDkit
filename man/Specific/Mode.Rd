\name{Mode}
\title{Approximating the mode by least-squares method}
\alias{Mode}
\alias{ModeC}
\alias{ModeMain}
\alias{ModeAll}
\alias{WRComp}

\description{
The functions '\code{Mode}' and '\code{ModeC}' calculate the best approximations of the 
mode given major-\cr
element compositions of the rock and its main 
mineral constituents. Function '\code{WRComp}' does the 
opposite, i.e. yields the whole-rock composition given 
the chemistry of individual minerals and their modal proportions.
}

\usage{
ModeMain(WR,sample.id="",select.oxides=TRUE,select.minerals=TRUE)
Mode(rock, mins,sample.id="")
ModeC(rock, mins,sample.id="")
ModeAll(WR)
WRComp(mins, f)
}

\arguments{
\item{WR}{a numerical matrix; the whole-rock data to be normalized.}
\item{rock}{whole-rock composition of the given sample.}
\item{sample.id}{(optional) sample name.}
\item{select.oxides}{(logical) should be selected oxides used for calculation?}
\item{select.minerals}{(logical) should be selected minerals used for calculation?}
\item{mins}{composition of its main rock-forming minerals.}
\item{f}{their modal proportions.}
}

\details{
'\code{Mode}' uses unconstrained least-squares method taking advantage of the 
standard R function\cr
'\code{lsfit(mins,rock,intercept=F)}'. It produces results that generally 
do not sum up to 100 \% due to the presence of elements not used in 
calculation (such as water), and, or, analytical noise. 

'\code{ModeC}' is the constrained variation whose output ought to sum up to 100 \% 
by definition \emph{(Albarede 1995)}. As such it seems to be more appropriate in 
most applications. 

In both cases, the printed output involves the input data, calculated modal 
proportions of the individual minerals, the calculated composition of the rock 
(using the auxiliary function '\code{WRComp}') and differences between the 
approximated and the real data (residuals). 

The sum of squared residuals is a measure of fit (as a rough guide it should be 
less than ca. 1).  

The mineral compositions are provided by a tab-delimited ASCII file, whose first 
row contains the names of the determined oxides, the following ones start with 
the mineral abbreviation and the numeric data (hence the first row has one item 
less than the following ones).

'\code{ModeMain}' is entry point to both 
'\code{Mode}' and '\code{ModeC}' 
that enables the user to read the mineral data file, 
select the oxides and minerals to be used in the calculation.
 
The options '\code{select.oxides=FALSE}' and 
'\code{select.minerals=FALSE}' read the mineral file in its entirety, 
using all minerals and oxides present.

'\code{ModeAll}' is a front end that performs the constrained least squares 
calculation for samples specified by the function \code{\link{selectSamples}}. 
}

\value{
'\code{ModeMain}', '\code{Mode}' and '\code{ModeC}' return a list with two 
items. The first of them ('\code{table}') is a matrix with the 
real composition of the rock and its minerals, the calculated whole-rock 
composition and the residuals. The second ('\code{(un)constrained}') returns 
calculated mineral proportions and sum of squared residuals.

'\code{ModeAll}' returns a simple matrix listing, for each rock sample, 
calculated proportions of rock-forming minerals and the sum of squared residuals.

'\code{WRComp}' yields a vector with the calculated whole-rock composition.
}

\seealso{
For example of the mineral data, see file '\code{Test_data\\sazava mins.data}'.
}

\examples{
# Albarede (1995) - page 7
# Calculate WRComposition of olivine gabbro containing 40 \% olivine,
# 30 \% diopside and 30 \% plagioclase. 

mins<-matrix(c(40.01,0.00,14.35,45.64,0.00,0.00,54.69,0.00,3.27,16.51,
            25.52,0.00,48.07,33.37,0.00,0.00,16.31,2.25),3,6,byrow=TRUE)
rownames(mins)<-c("ol","di","plg")
colnames(mins)<-c("SiO2","Al2O3","FeO","MgO","CaO","Na2O")
print(mins)

f<-c(0.4,0.3,0.3)
names(f)<-c("ol","di","plg")
print(f)

rock<-WRComp(mins,f)
print(rock)

# Reverse 
mode1<-Mode(rock,mins)
mode2<-ModeC(rock,mins)

}
\references{
Albarede F (1995) Introduction to Geochemical Modeling. Cambridge University 
Press, Cambridge, p. 1-543
}

\keyword{manip}
\concept{Norm}
\concept{Calculation}
\concept{Menu: Calculations}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
