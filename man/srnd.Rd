\name{srnd}
\title{Recalculations of the Sr-Nd isotopic data}
\alias{srnd}
\alias{DMage}
\alias{DMGage}
\alias{DMLHage}
\alias{initial}
\alias{epsilon}

\section{Plugin}{SrNd.r}

\description{
    Age-corrects the Sr-Nd isotopic data to a given age; calculates initial 
    \eqn{\epsilon(Nd)} values and Nd model ages.
}

\usage{
    srnd(age="")

    initial(x,age,system="Nd")

    epsilon(WR,age)

    DMage(WR)

    DMGage(WR)

    DMLHage(WR,age,RCC=0.12)
}

\arguments{
  \item{age}{age in Ma: if empty, the user is prompted to enter a value}
  \item{x, WR}{isotopic data to be recalculated}
  \item{system}{character; which isotopic system Sr or Nd?}
  \item{RCC}{numeric; the \eqn{^{147}Sm/^{144}Nd}{147Sm/144Nd} ratio of the intermediate 
    crustal reservoir for calculation of the two-stage Nd model ages}
}

\details{
    Recalculates the Sr-Nd isotopic data and returns them in the numeric matrix 
    \code{init} with the following columns (DM = Depleted Mantle):

    \tabular{ll}{
    Age (Ma)\tab Age in Ma\cr
    87Sr/86Sri\tab Initial \eqn{^{87}Sr/^{86}Sr}{87Sr/86Sr} ratios\cr
    143Nd/144Ndi\tab Initial \eqn{^{143}Nd/^{144}Nd}{143Nd/144Nd}  ratios\cr
    EpsNdi\tab Initial \eqn{\epsilon(Nd)} values\cr
    TDM\tab Single-stage DM Nd model ages \emph{(Liew & Hofmann, 1988)}, function \code{DMage}\cr
    TDM.Gold\tab Single-stage DM Nd model ages \emph{(Goldstein et al., 1988)}, function \code{DMGage}\cr
    TDM.2stg\tab Two-stage DM Nd model ages \emph{(Liew & Hofmann, 1988)}, function \code{DMLHage}\cr
    }
}

\value{
    \item{init}{numeric matrix with the results}
}

\references{
    Goldstein S L, O'Nions R K & Hamilton P J (1984) A Sm-Nd isotopic study of 
    atmospheric dusts and particulates from major river systems. Earth Planet Sci 
    Lett 70: 221-236
    \href{http://dx.doi.org/10.1016/0012-821X(84)90007-4}{doi: 10.1016/0012-821X(84)90007-4}

    Liew T C & Hofmann A W (1988) Precambrian crustal components, plutonic 
    associations, plate environment of the Hercynian Fold Belt of Central Europe: 
    indications from a Nd and Sr isotopic study. Contrib Mineral Petrol 98: 129-138
    \href{http://dx.doi.org/10.1007/BF00402106}{doi: 10.1007/BF00402106}
}

\examples{
# recalculation to 500 Ma
srnd(500)

# print the isotopic parameters currently in the memory
init
}

\keyword{manip}
\concept{Calculation}
\concept{Isotopic}
\concept{Menu: Plugins}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
