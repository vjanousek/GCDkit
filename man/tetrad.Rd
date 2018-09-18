\name{tetrad}
\title{Lanthanide tetrad effect}
\alias{tetrad}

\section{Plugin}{tetrad.r}

\description{
Calculates lanthanide tetrad effect following the method of \emph{Irber (1999)}.
}

\usage{
tetrad(method=NULL)
}

\arguments{
    \item{method}{Normalization scheme.}
}

\details{
The \code{method} indicates which normalization scheme is to be used. It can be 
either 'Boynton' or 'Nakamura'. If not specified, the user is prompted to choose it interactively
by the function \code{\link{spider}}. 

The anomalies of individual elements are calculated as follows for the first
tetrad:

\deqn{Ce/Cet = \frac{Ce_{N}}{La_{N}^{\frac{2}{3}}*Nd_{N}^{\frac{1}{3}}}}{%
          Ce/Cet = CeN/(La^(2/3)*Nd^(1/3))}
        
\deqn{Pr/Prt = \frac{Pr_{N}}{La_{N}^{\frac{1}{3}}*Nd_{N}^{\frac{2}{3}}}}{%
          Pr/Prt = PrN/(La^(1/3)*Nd^(2/3))}
        
\deqn{t1 = \sqrt{Ce/Cet*Pr/Prt}}{%
          sqrt(Ce/Cet*Pr/Prt)}

By analogy, one can define for the third tetrad:        
\deqn{Tb/Tbt = \frac{Tb_{N}}{Gd_{N}^{\frac{2}{3}}*Ho_{N}^{\frac{1}{3}}}}{%
          Tb/Tbt = TbN/(Gd^(2/3)*Ho^(1/3))}
        
\deqn{Dy/Dyt = \frac{Dy_{N}}{Gd_{N}^{\frac{1}{3}}*Ho_{N}^{\frac{2}{3}}}}{%
          Dy/Dyt = DyN/(Gd^(1/3)*Ho^(2/3))}
        
\deqn{t3 = \sqrt{Tb/Tbt*Dy/Dyt}}{%
          sqrt(Tb/Tbt*Dy/Dyt)}
        
The magnitude of the tetrad effect is then calculated as a geometric mean:
\deqn{t3 = \sqrt{t1*t3}}{%
          sqrt(t1*t3)}

}

\value{
Returns a matrix '\code{results}' with the following columns:
  \item{Ce/Cet}{Ce anomaly}
  \item{Pr/Prt}{Pr anomaly}
  \item{t1}{first tetrad}
  \item{Tb/Tbt}{Tb anomaly}
  \item{Dy/Dyt}{Dy anomaly}
  \item{t3}{third tetrad}
  \item{TE1-3}{degree of lanthanide tetrad effect, geometric mean of t1 and t3}
}

\references{
Irber W (1999) The lanthanide tetrad effect and its correlation with K/Rb, 
Eu/Eu*, Sr/Eu, Y/Ho, and Zr/Hf of evolving peraluminous granite suites. 
Geochim Cosmochim Acta 63: 489-508
}

\examples{
    tetrad("Boynton")
}

\seealso{
    \code{\link{spider}}
}

\keyword{manip}
\concept{Calculation}
\concept{REE}
\concept{Tetrad effect}
\concept{Lanthanide tetrad effect}
\concept{Menu: Plugins}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
