\encoding{utf8}
\name{F-M-W diagram}
\title{Ohta + Arai (2007) FMW weathering index}
\alias{OhtaArai}

\description{
    Assigns data for the F-M-W diagram by \emph{Ohta & Arai (2007)}
    into Figaro template (list '\code{sheet}') and
    appropriate values into '\code{x.data}' and '\code{y.data}'. 
	
	This function is meant to be used with \code{\link{plotDiagram}}.
}

\usage{
    OhtaArai(fixTi=F)
}

\arguments{
    \item{fixTi}{logical, if \code{TRUE}, the \eqn{TiO_{2}}{TiO2} value of samples where this is not determined (or 0) will
			be replaced by FeOt/7.}
}

\details{
This (ternary) diagram has been proposed by \emph{Ohta & Arai (2007)} to identify chemically weathered igneous rocks. 
It turns out to be also very useful to separate para- from orthogneisses \emph{(Moyen et al. 2017)}.

The values of the apices are complex combination of oxides defining three end-members: M (mafic igneous rocks), 
F (felsic igneous rocks) and W (chemical weathering):

\deqn{
\begin{array}{lll}
	M = & \exp&(-0.395 \times ln(SiO_2)+0.206 \times ln(TiO_2)-0.316 \times ln(Al_2O_3)\\
		& 		&+0.160 \times ln(Fe_2O_3t)+0.246 \times ln(MgO)+0.368 \times ln(CaO)\\
		&		& +0.073 \times ln(Na_2O)-0.342 \times ln(K_2O)+2.266)\\
	\end{array}
}{%
	M = exp(-0.395×ln(SiO2)+0.206×ln(TiO2)-0.316×ln(Al2O3)+
	0.160×ln(Fe2O3t)+0.246×ln(MgO)+0.368×ln(CaO)+
	0.073×ln(Na2O)-0.342×ln(K2O)+2.266 )
}

\deqn{
\begin{array}{lll}
	F = & \exp&(0.191 \times ln(SiO_2)-0.397 \times ln(TiO_2)+0.020 \times ln(Al_2O_3)\\
		& 		&-0.375 \times ln(Fe_2O_3t)-0.243 \times ln(MgO)+0.079 \times ln(CaO)\\
		&		& +0.392 \times ln(Na_2O)+0.333 \times ln(K_2O)-0.892)\\
	\end{array}
}{%
	F = exp(0.191×ln(SiO2)-0.397×ln(TiO2)+0.020×ln(Al2O3)-0.375×
	ln(Fe2O3t)-0.243×ln(MgO)+0.079×ln(CaO)+0.392×
	ln(Na2O)+0.333×ln(K2O)-0.892)
}

\deqn{
\begin{array}{lll}
	W = & \exp&(0.203 \times ln(SiO_2)+0.191 \times ln(TiO_2)+0.296 \times ln(Al_2O_3)\\
		& 		&+0.215 \times ln(Fe_2O_3t)-0.002 \times ln(MgO)-0.448 \times ln(CaO)\\
		&		& -0.464 \times ln(Na_2O)+0.008 \times ln(K_2O)-1.374)\\
	\end{array}
}{%
	W = exp(0.203×ln(SiO2)+0.191×ln(TiO2)+0.296×ln(Al2O3)+0.215×
	ln(Fe2O3t)-0.002×ln(MgO)-0.448×ln(CaO)-0.464×
	ln(Na2O)+0.008×ln(K2O)-1.374)
}

    \if{html}{\figure{ohtaarai.png}}
    \if{latex}{\figure{ohtaarai.png}{options: width=4.5in}}
}

\value{
    A list containing the Figaro template. In addition the following global variables are modified:
    \item{sheet}{list with Figaro Style Sheet data}
    \item{x.data}{x coordinates in ternary projection}
    \item{y.data}{y coordinates in ternary projection}
}

	
\references{
Ohta T, Arai H (2007) Statistical empirical index of chemical weathering in igneous rocks: 
a new tool for evaluating the degree of weathering. Chem Geol 240: 280-297
\href{https://doi.org/10.1016/j.chemgeo.2007.02.017}{doi:10.1016/j.chemgeo.2007.02.017} 

Moyen JF, Laurent O, Chelle-Michou C, Couzinie S, Vanderhaeghe O, Zeh A, 
Villaros A, Gardien V (2017) Collision vs. subduction-related magmatism: 
two contrasting ways of granite formation and implications for crustal growth. 
Lithos 277: 154-177.
\href{http://dx.doi.org/10.1016/j.lithos.2016.09.018}{doi:10.1016/j.lithos.2016.09.018} 
}

\author{Jean-Francois Moyen, \email{jfmoyen@gmail.com}}

\examples{
# plot the diagram
# assuming a dataset is loaded, of course!
\dontrun{
plotDiagram("OhtaArai",FALSE)
}
}

\seealso{
    \link{figaro}
    \link{plotDiagram}
}

\keyword{hplot}
\concept{Classification}
\concept{Plot}
\concept{Alteration}
\concept{Menu: Classification}

