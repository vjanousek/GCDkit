\name{clr.transform} 
\title{Centered-log-ratio transformation} 
\alias{clr.trans} 
\alias{pr.comp.clr} 

\description{
    Implementation of centred-log-ratio (clr) transformation for compositional data. 
}

\usage{
    clr.trans(comp.data=NULL,GUI=FALSE)
    
    pr.comp.clr(comp.data="SiO2,TiO2,Al2O3,FeOt,MnO,MgO,CaO,Na2O,K2O",GUI=FALSE)
}

\arguments{
    \item{comp.data}{a numerical matrix; the data to be normalized. 
        Or just names of variables in the data matrix '\code{WR}'.}
    \item{GUI}{logical; is the function called from a menu (GUI)?}
}

\details{ 
    Compositional data - i.e., multivariate data in which the components sum up to some constant 
    (e.g. 1 or 100, for percentages) - are widespread in the geosciences. A typical 
    example represent major-element analyses from whole-rock samples. 
    
    Numerous workers have argued that much of correlation in such closed datasets is spurious, 
    due to the so-called constant sum or closure effect \emph{(e.g., Chayes 1960; Rock 1988; 
    Rollinson 1992, 1993).} 
    
    This effect arises from the fact that such components in the datasets cannot vary independently. 
    If one oxide, for instance \eqn{SiO_{2}}{SiO2} that typically dominates the whole-rock analyses 
    of many igneous rocks, increases in abundance, all other oxides must decrease. 
    Therefore, everything must be anti-correlated with silica. 
    
    For their correct statistical treatment, compositional data have to be transformed, 
    or 'opened'. A classic remedy to the closure effect are log-ratio transformations 
    \emph{(Aitchison 1986; Buccianti et al. eds 2006)}.
    
    The functions '\code{clr.trans}' and '\code{pr.comp.clr}' implement centred-log-ratio
    (clr) transformation. Data opening in this case is done by dividing each value of a variable 
    by the geometric mean of all the variables for that individual and then taking logarithms. 
    It is critical of course that all the variables should be expressed in the same measurement unit.
    
    For instance, for MgO, the centred-log-ratio transformed version is given as:
    
    \deqn{MgO\_clr=\ln \left( \frac{{{C}_{MgO}}}{\sqrt[n]{\prod\limits_{i=1}^{n}{{{C}_{i}}}}} \right)}{%
    MgO_clr = ln(MgO)/geom.mean}
    
    where '\code{ln}' is natural logarithm, '\code{C}' concentration in wt. \% of the selected variable 
    and the denominator a geometric mean of all variables being transformed 
    \emph{(e.g., Pawlowsky-Glahn V, Egozcue JJ (2006))}).
}

\seealso{
    See \emph{Reimann et al. (2008)} with \emph{van den Boogaart and Tolosana-Delgado (2013)} 
    for further details and \emph{van den Boogaart and Tolosana-Delgado (2008)}
    for implementation of a comprehensive R library dealing with compositional data.
}

\references{
    Aitchison J (1986) The Statistical Analysis of Compositional Data. Methuen, New York, pp 1-416
    
    Buccianti A, Mateu-Figueras G, Pawlowsky-Glahn V (eds) (2006) Compositional Data Analysis 
    in the Geosciences. Geological Society Special Publications 264, London, pp 1-212

    Chayes F (1960) On correlation between variables of constant sum. J Geophys Res 65:4185-4193
    \href{http://dx.doi.org/10.1029/JZ065i012p04185}{doi:10.1029/JZ065i012p04185}
    
    Pawlowsky-Glahn V, Egozcue JJ (2006) Compositional data and their analysis: an introduction. 
    In: Buccianti A, Mateu-Figueras G, Pawlowsky-Glahn V (eds) Compositional Data Analysis 
    in the Geosciences. Geological Society Special Publications 264, London, pp 1-10
    
    Reimann C, Filzmoser P, Garrett R, Dutter R (2008) Statistical Data Analysis Explained: 
    Applied Environmental Statistics with R. John Wiley & Sons, Chichester, pp 1-362
    
    Rock NMS (1988) Numerical geology. A source guide, glossary and selective bibliography to geological uses 
    of computers and statistics. Lecture Notes in Earth Sciences, vol 18. Springer, Berlin
    
    Rollinson HR (1992) Another look at the constant sum problem in geochemistry. Mineral Mag 56:469-475
    \href{http://dx.doi.org/10.1180/minmag.1992.056.385.03}{doi:10.1180/minmag.1992.056.385.03}
    
    Rollinson HR (1993) Using Geochemical Data: Evaluation, Presentation, Interpretation. 
    Longman, London, pp 1-352

    van den Boogaart KG, Tolosana-Delgado R (2008) "compositions": a unified R package to 
    analyze compositional data. Comput Geosci 34: 320-338 
    \href{http://dx.doi.org/10.1016/j.cageo.2006.11.017}{doi:10.1016/j.cageo.2006.11.017}
    
    van den Boogaart KG, Tolosana-Delgado R (2013) Analyzing Compositional Data with R. 
    Springer, Berlin, pp 1-258
}

\value{
    A numeric matrix '\code{results}'. The names of components are preserved, and supplemented 
    by a suffix '_clr'
}

\examples{
    data(sazava)
    accessVar("sazava")
    
    ox<-c("SiO2","Al2O3","FeOt","MgO","CaO")
    clr.trans(ox)
    addResults() # Needed to append the clr transformed data to the matrix 'WR'
    
    multiple(x = "SiO2_clr", y = "Al2O3_clr,FeOt_clr,MgO_clr,CaO_clr")
    plateCex(2)
    plateCexLab(1.3)
    
    pr.comp.clr(ox)
}

\keyword{manip}
\concept{Calculation}
\concept{Closure}
\concept{Menu: Plugins}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
    
    Vladimir Kusbach, \email{kusbach@gmail.com}
}
