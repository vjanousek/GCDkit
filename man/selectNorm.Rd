\name{selectNorm}
\title{Selecting the normalization data for spiderplots}
\alias{selectNorm}

\description{
Displays available normalization schemes and lets the user to choose one 
interactively.
}

\usage{
selectNorm(ref=NULL,elems = "Rb,Sr,Ba,Cr,Ni,La,Ce,Y,Zr",REE.only=FALSE,
multiple=FALSE) 
}

\arguments{
\item{ref}{character: a specification of the normalization scheme.}
\item{elems}{character: a default list of elements.}
\item{REE.only}{logical: should be only listed normalization schemes for REE?}
\item{multiple}{logical: is a result with several normalizing schemes allowed?}
}

\details{
A search pattern can be specified directly (in batch mode) in order to 
query the available normalizing model names. The corresponding parameter 
'\emph{ref}' can contain a substring appearing in the name of the normalizing scheme 
(or even a regular expression). 

Alternatively, the parameter '\emph{ref}' can refer to a name of a sample to be 
used for normalization, or even a regular expression if average of several
of them is desired.  

The function fails if no matches are found or the search in names of normalizing 
schemes is ambiguous (returns more than a single match), unless 
'\emph{multiple = TRUE}'.

The second possibility is to pick an option from the list of available normalizing schemes 
via GUI.  

The first option therein offers normalization \emph{by a single sample}. 
Its name can be typed in or, after pressing the Enter key, picked from a list. 

The second option is similar but it allows to normalize \emph{by average concentrations}
in a group of samples specified by one of the three searching mechanisms as 
above (see \code{\link{selectSubset}}).

Then the user is prompted to specify the list and order of elements/oxides that should appear on 
the plot. The easiest way is to type directly the names of the  columns, 
separated by commas. Alternatively can be used their sequence numbers or ranges. 
Also built-in lists can be employed, such as 'LILE', 'REE', 'major' and 'HFSE' 
or their combinations with the column names. These lists are simple character 
vectors, and additional ones can be built by the user (see Examples). Note that 
currently only a single, stand-alone, user-defined list can be employed as a 
search criterion. 

The samples to be plotted can be selected based on combination of three searching 
mechanisms (by sample name/label, range or a Boolean condition) - 
see \code{\link{selectSubset}} for details. 

The composition of \emph{various standards} 
available for normalization and subsequent plotting of 
\code{\link{spider}} diagrams is stored in the file 
'\code{spider.data}' in the main GCDkit directory. It is a comma delimited file 
such as:

\preformatted{
Normalization data used for spiderplots

MORB (Pearce 1983)
Sr,K,Rb,Ba,Th,Ta,Nb,Ce,P,Zr,Hf,Sm,Ti,Y,Yb
120,1245,2,20,.2,.18,3.5,10,534,90,2.4,3.3,8992,30,3.4

REE chondrite (Boynton 1984)
La,Ce,Pr,Nd,Pm,Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu
.31,.808,.122,.6,1,.195,.0735,.2590,.0474,
.322,.0718,.21,0.0324,.209,.0322

ORG (PearceEtAl.1984)
K2O,Rb,Ba,Th,Ta,Nb,Ce,Hf,Zr,Sm,Y,Yb
0.4,4,50,0.8,0.7,10,35,9,340,9,70,8.0
}

The first row is always skipped and can contain any comments. The following ones 
have a fixed structure. For each normalization scheme, the first row contains 
the title and reference. If title starts with '\code{REE}', the normalization is 
supposed to be for REE only and special parameters, such as '\code{Eu/Eu*}', are 
calculated. The second line gives a comma delimited list of elements in the order 
they should appear on the plot. The last line is a comma delimited list of 
normalization values. There are empty lines left between the normalization schemes.

As the file '\code{spider.data}' is read every time '\code{selectNorm} is called, 
the user can add or delete normalization schemes on his will using a text editor.
}

\value{
A numeric matrix with one row, containing the normalizing values. 
The row name contains the name of the model and reference.
}

\examples{
selectNorm()
selectNorm("Boynton")

# Regular expressions in action, we take the string from beginning
# and then replace space and left bracket by dots
selectNorm("^Primitive Mantle..McDonough 1995")

# Selecting several samples by regular expression
data(sazava)
accessVar("sazava")
selectNorm("Po-4",elems="Cs,Rb,Ba,Nb,La,Yb")
selectNorm("^Po",elems="Cs,Rb,Ba,Nb,La,Yb")

}

\references{
\bold{Implemented spiderplots:}

Anders E, Grevesse N (1989) Abundances of the elements: meteoritic and solar. 
Geochim Cosmochim Acta 53:197-214
\href{https://doi.org/10.1016/0016-7037(89)90286-X}{doi: 10.1016/0016-7037(89)90286-X4} 

Becker H, Horan M F, Walker R J, Gao S, Lorand J-P, Rudnick R L (2006) Highly 
siderophile element composition of the Earth's primitive upper mantle: constraints 
from new data on peridotite massifs and xenoliths. Geochim Cosmochim Acta 70: 4528-4550
\href{http://dx.doi.org/10.1016/j.gca.2006.06.004}{doi: 10.1016/j.gca.2006.06.004} 

Boynton W V (1984) Cosmochemistry of the rare earth elements: meteorite studies. 
In: Henderson P (eds) Rare Earth Element Geochemistry. Elsevier, Amsterdam, 
pp 63-114

Jochum K P (1996) Rhodium and other platinum-group elements in carbonaceous chondrites. 
Geochim Cosmochim Acta 60: 3353-3357
\href{http://dx.doi.org/10.1016/0016-7037(96)00186-X}{doi: 10.1016/0016-7037(96)00186-X} 

McDonough W, Sun S S (1995) The composition of the Earth. Chem Geol 120: 223-253
\href{http://dx.doi.org/10.1016/0009-2541(94)00140-4}{doi: 10.1016/0009-2541(94)00140-4} 

Nakamura N (1974) Determination of REE, Ba, Fe, Mg, Na and K in carbonaceous 
and ordinary chondrites. Geochim Cosmochim Acta 38: 757-775
\href{http://dx.doi.org/10.1016/0016-7037(74)90149-5}{doi: 10.1016/0016-7037(74)90149-5} 

Pearce J A (1983) Role of sub-continental lithosphere in magma genesis at active 
continental margins. Continental Basalts and Mantle Xenoliths. Shiva, Nantwich, 
pp 230-249

Pearce J A (1996) A user's guide to basalt discrimination diagrams. 
In: Wyman D A (eds) Trace Element Geochemistry of Volcanic Rocks: 
Applications for Massive Sulphide Exploration. Geological Association of Canada, 
Short Course Notes 12, pp 79-113

Pearce J A (2014) Immobile element fingerprinting of ophiolites. Elements 10: 101-108 
\href{http://dx.doi.org/10.2113/gselements.10.2.101}{doi: 10.2113/gselements.10.2.101}

Pearce J A, Harris N W, Tindle A G (1984) Trace element discrimination diagrams 
for the tectonic interpretation of granitic rocks. J Petrology 25: 956-983
\href{http://dx.doi.org/10.1093/petrology/25.4.956}{doi:10.1093/petrology/25.4.956 }

Sun S S, McDonough W F (1989) Chemical and isotopic systematics of oceanic 
basalts: implications for mantle composition and processes. In: Saunders A D, 
Norry M (eds) Magmatism in Ocean Basins. Geological Society of London Special 
Publications 42, pp 313-345

Sun S S, Bailey D K, Tarney J, Dunham K (1980) Lead isotopic study of young 
volcanic rocks from mid-ocean ridges, ocean islands and island arcs. Philos 
Trans R Soc London A297: 409-445
\href{http://dx.doi.org/10.1098/rsta.1980.0224}{doi: 10.1098/rsta.1980.022410.1029/95RG00262} 

Taylor S R, McLennan S M (1985) The Continental Crust: Its Composition and 
Evolution. Blackwell, Oxford, pp 1-312

Taylor S R, McLennan S M (1995) The geochemical evolution of the continental crust. 
Reviews in Geophysics 33: 241-265
\href{http://dx.doi.org/10.1029/95RG00262}{doi: 10.1029/95RG00262} 

Thompson R N (1982) British Tertiary province. Scott J Geol 18: 49-107

Weaver B L, Tarney J (1984) Empirical approach to estimating the composition 
of the continental crust. Nature 310: 575-577
\href{http://dx.doi.org/10.1038/310575a0}{doi: 10.1038/310575a0} 

Wood D A, Joron J L, Treuil M, Norry M, Tarney J (1979) Elemental and Sr 
isotope variations in basic lavas from Iceland and the surrounding ocean 
floor; the nature of mantle source inhomogeneities. Contrib Mineral Petrol 70: 
319-339
\href{http://dx.doi.org/10.1007/BF00375360}{doi: 10.1007/BF00375360} 
}

\keyword{dplot}
\concept{Menu: Plots}
\concept{Plot}
\concept{Spiderplot}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
