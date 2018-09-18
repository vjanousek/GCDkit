\name{loadData}
\title{Loading data into GCDkit}
\alias{loadData}
\alias{loadDataOdbc}

\description{
Loads data from a file (or, alternatively, a clipboard) into GCDkit. The files 
may contain plain text, or, if library RODBC (has been installed, can be in the 
dBase III/IV (*.dbf), Excel (*.xls), Access (*.mdb), PetroGraph (*.peg),
IgPet or NewPet (*.roc) formats.
}

\usage{
loadData(filename=NULL,separators = c("\t", ",", ";"," "), 
na.strings = c("NA","-","bd", "b.d.", "bdl", "b.d.l.", "N.A.","n.d."), 
clipboard = FALSE, merging = FALSE); 

loadDataOdbc(filename=NULL,na.strings=c("NA","-", "bd", 
"b.d.", "bdl", "b.d.l.", "N.A.","n.d."),merging=FALSE,
ODBC.choose=TRUE)
}

\arguments{
\item{filename}{fully qualified  name of the file to be loaded, including suffix.}
\item{separators}{strings that should be tested as prospective delimiters 
    separating individual items in the data file.}
\item{na.strings}{strings that will be interpreted, together with empty items, 
    zeros and negative numbers, as missing values (\code{NA}).}
\item{clipboard}{logical; is clipboard to be read instead of a file?}
\item{merging}{logical; is the function invoked during merging of two data files?}
\item{ODBC.choose}{logical; if TRUE, ODBC channel can be chosen interactively.}
}


\details{
If library RODBC is available, the functions attempt to establish an ODBC 
connection to the selected file, and open it as dBase III/IV (*.dbf), 
Excel (*.xls) or Access (*.mdb) format. The DBF files are used to store data by 
other popular geochemical packages, such as IgPet \emph{(Carr, 1995)} or MinPet 
\emph{(Richard, 1995)}. 

Another format that can be imported is *.csv. 
It is employed by geochemical database systems such as 
GEOROC 
(\url{http://georoc.mpch-mainz.gwdg.de/georoc/}) 
and PETDB 
(\url{http://www.petdb.org/}).

The import filter for the *.csv files has been tailored to keep the structure 
of these databases in mind. 

The package PetroGraph \emph{(Petrelli et al. 2005)} saves data into *.peg files 
that are also, in principle, *.csv files compatible with the GCDkit.  

Data files *.roc are yet another variant of *.csv files, used by NewPet 
\emph{(Clarke et al. 1994)}. This is not to be confused with the *.roc format 
designed for IgPet \emph{(Carr, 1995)}. This is a text file with a quite complex
structure, whose import is still largely experimental. 
DBF files are to be preferred for this purpose. 

If not successful, the function '\code{loadData}' assumes that it is dealing 
with a simple text file. 

On the other hand '\code{loadDataOdbc}' allows an ODBC channel to be 
specified interactively if \cr
'\code{ODBC.choose=TRUE}'.

Plain text files can be delimited by tabs, commas or semicolons (the delimiter 
is recognized automatically). Alternative separators list can be specified by 
the optional '\code{separators}' parameter. The Windows clipboard is just taken 
as a special kind of a tab-delimited text file.

In the text file, the first line contains names for the data columns (except for 
the first one that is automatically assumed to contain the sample names); hence 
the first line may (or may not) have one item less than the following ones. The 
data rows start with sample name and do not have to be all of the same length 
(the rest of the row is filled by '\code{NA}' automatically). 

Missing values ('\code{NA}') are allowed anywhere in the data file (naturally 
apart from sample and column names); any 
of \code{'NA', 'N.A.', '-', 'b.d.', 'bd', 'b.d.l.','bdl'} or \code{'n.d.'} are also 
treated as such, as specified by the parameter \code{na.strings}.

While loading, the values '\code{#WHATEVER!}' (Excel error messages) are also 
replaced  by '\code{NA}' automatically.

Please note that the function '\code{loadDataOdbc}', 
due to the current limitations of the RODBC
package, cannot handle correctly columns of mixed numeric
and textual data. In such a column all textual information is converted to
\code{'NA'} and this unfortunately concerns the sample names as well. If encountering
any problems, please use import from text file or via clipboard, which are much more 
robust.

The negative numbers and values '\code{< x}' (used by some authors to indicate 
items below detection limit) can be either replaced by their half (i.e. half of 
the detection limit) or '\code{NA}'. User is prompted which of these options he 
prefers.

Alternatively, the negative values can be viewed either as missing ('\code{NA}')
or can be imported, as may be desirable for instance for stable 
isotope data in the delta notation. 

Decimal commas, if present in text file, are converted to decimal points.

The data files can be practically freeform, i.e. no specified oxides/elements 
are required and no exact order of these is to be adhered to. Analyses can 
contain as many numeric columns as necessary, the names of oxides and trace 
elements are self-explanatory (e.g. \code{"SiO2", "Fe2O3", "Rb", "Nd"}. 

In the text files (or if pasting from clipboard), any line 
starting with the hash symbol (\code{'#'}) is ignored and can be used
to introduce comments or to prevent the given analysis from loading temporarily. 

Note that names of variables are case sensitive in R. However, any of the fully 
upper case names of the oxides/elements that appear in the following list are 
translated automatically to the appropriate capitalization:

\preformatted{
SiO2, TiO2, Al2O3, Fe2O3, FeO, MnO, MgO, CaO, Na2O, FeOt, Fe2O3t,
 
Li2O, mg#, Ac, Ag, Al, As, At, Au, Ba, Be, Bi, 

Br, Ca, Cd, Ce, Cl, Co, Cr, Cs, Cu, Dy, Er, Eu, 

Fe, Ga, Gd, Ge, Hf, Hg, Ho, In, Ir, La, Li, Lu, 

Mg, Mn, Mo, Na, Nb, Nd, Ne, Ni, Np, Os, Pa, Pb, 

Pd, Pm, Pr, Pt, Pu, Rb, Re, Rh, Ru, S, Sb, Sc,

Se, Si, Sm, Sn, Sr, Ta, Tb, Te, Th, Ti, Tl, Tm, 

Yb, Zn, Zr.
}

Total iron, if given, should be expressed either as ferrous oxide ('\code{FeOt}', 
'\code{FeOT}', '\code{FeOtot}', '\code{FeOTOT}' or '\code{FeO*}') or ferric oxide 
('\code{Fe2O3t}', '\code{Fe2O3T}', '\code{Fe2O3tot}', '\code{Fe2O3TOT}' or '\code{Fe2O3*}'). 

Structurally bound water can be named \code{'H2O.PLUS', 'H2O+', 'H2OPLUS', 
'H2OP'} or '\code{H2O_PLUS}'.

Upon loading, all the completely empty columns are removed first. Any non-numeric 
items found in a data column with one of the names listed in the above dictionary 
are assumed to be typos and replaced by '\code{NA}', after a warning appears. 
At the next stage all fully numeric data columns are stored in a numeric data 
matrix '\code{WR}'. 
 
For any missing major- and minor-element data (SiO2, TiO2, Al2O3, Fe2O3, FeO, 
MnO, MgO, CaO, Na2O, K2O, H2O.PLUS, CO2, P2O5, F, S), an empty (NA) column is 
created automatically.

The remaining, that is all at least partly textual data columns are transferred 
to the data frame '\code{labels}'. To this are also attached a column whose name 
starts with '\code{Symbol}' (if any) that is taken as containing plotting symbols 
and a column whose name is '\code{Colour}'  or '\code{Color}'(if any, capitalization 
does not matter) that may contain plotting colours specification. The relative 
size of the individual plotting symbols may be specified in a column named '\code{Size}' or 
'\code{cex}' that is also to be attached to the '\code{labels}'.

The plotting symbols can be given either by their code (see 
\code{\link{showSymbols}}) or directly as strings of single characters.
 
The colours can be specified as codes (1-49) or English names 
(see \code{\link{showColours}} 
or type 
\code{'colours()'} 
into the Console window).

If specifications of the plotting symbols and colours are missing completely, 
and at least one non-numeric variable is present, the user is prompted whether
he does not want to have the symbols and colours assigned automatically, from 1
to \emph{n}, according to the levels of the selected label. Otherwise default 
symbols (empty black circles) are used.

The default grouping is set on the basis of plotting symbols '(\code{labels$Symbol})'
or the data column used to autoassign the plotting symbols and colours.

Lastly, a backup copy of the data is stored in the list '\code{WRCube}' using the
function '\code{\link{pokeDataset}}'. 
It is stored either under the name of the file, or, if it already exists, 
under the file name with a time stamp attached.   
}

\note{
In order to ensure the database functionality, duplicated column (variable) names 
are not  allowed. This concerns, to a large extent, also the sample names. 
The only exception are CSV files - if duplicated samples are found, sequence 
numbers are assigned instead. 

All completely empty rows and columns in both labels and numeric data are ignored. 
}

\value{
  \item{WR}{numeric matrix: all numeric data}
  \item{labels}{data frame:  all at least partly character fields; 
  \code{labels$Symbol} contains plotting symbols and \code{labels$Colour} the plotting colours}
The function prints a short summary about the loaded file. It also loads and 
executes the Plugins, i.e. all the R code (*.r) that is currently stored in the 
subdirectory '\code{\\Plugin}'. Finally, the system  performs some recalculations
(calling '\code{Gcdkit.r}').
}

\references{
Carr M (1995) Program IgPet. Terra Softa, Somerset, New Jersey, U.S.A.
 
Clarke D, Mengel F, Coish RA, Kosinowski MHF(1994) NewPet for DOS, version 
94.01.07. Department of Earth Sciences, Memorial University of Newfoundland, 
Canada.

Petrelli M, Poli G, Perugini D, Peccerillo A (2005) PetroGraph: A new software 
to visualize, model, and present geochemical data in igneous petrology. 
Geochemistry Geophysics Geosystems 6: 1-15

Richard LR (1995) MinPet: Mineralogical and Petrological Data Processing System, 
Version 2.02. MinPet Geological Software, Quebec, Canada.
}

\seealso{
  '\code{\link{saveData}}' '\code{\link{mergeData}}'
  '\code{\link{pokeDataset}}'
  '\code{\link{showColours}}' '\code{\link{showSymbols}}' '\code{\link{read.table}}'
  '\code{\link{getwd}}' '\code{\link{setwd}}'
}

\examples{
# Sets the working path and loads the 'sazava' test data set
setwd(paste(gcdx.dir,"Test_data",sep="/")) 
loadData("sazava.data")
}
\keyword{file}
\concept{File}
\concept{Menu: GCDkit}
\author{
The RODBC package was written by Brian Ripley.

Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
