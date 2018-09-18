\name{EarthChem}
\title{Import from EarthChem.org}
\alias{EarthChem}

\description{
    This function serves for importing the whole-rock geochemical data from
    \href{ http://www.earthchem.org/portal}{EarthChem.org} online database. 
    Since 2010, EarthChem has been part of IEDA (Integrated Earth Data Applications), 
    the National Science Foundation (NSF)-funded data 
    facility for solid earth geoscience data.
    The data are gathered from several publicly available 
    databases such as PetDB, SedDB, NAVDAT, or are contributed, and can be accessed 
    through the EarthChem's REST Search Service utilized by our function.
}

\usage{
EarthChem(x)
}

\arguments{
  \item{x}{a list of parameters, given below, detailed account of which is to be found at the 
  web page with the 
  \href{http://ecp.iedadata.org/rest_search_documentation}{EarthChem REST Server Documentation}}.
  See also Examples. 
}

\details{
    The function \code{EarthChem} imports the specified data taking advantage of the EarthChem REST 
    Search Service, which accepts GET string variables that determine search criteria.
    The results are returned using html or xml protocols, and can be then imported to the \emph{GCDkit} 
    using the library \code{'XML'}. 

\bold{Possible parameters} are (in square brackets are \emph{GCDkit} default values)

\tabular{ll}{
\bold{Data source specification (reference(s))}\cr
    author \tab author \cr
    title \tab title of the article\cr
    journal \tab journal \cr
    doi \tab Digital Object Identifier (not always available) \cr
    minpubyear \tab minimum publication year of the citation article \cr
    maxpubyear \tab maximum publicationyear (reqd with the former option) \cr
    exactpubyear \tab exact year of publication \cr
    keyword \tab free-text generic descriptor field \cr
\bold{Sample ID, location or age}\cr    
    sampleid \tab sample number/identifier from the original database \cr
    polygon \tab geographic region, specified by geographic coordinates \cr
    north, east, south, west \tab coordinates of a geographic envelope, all to be provided together \cr
    minage \tab minimum age of the sample (Ma) \cr
    maxage \tab maximum age of the sample (Ma) \cr
    exactage \tab age of the sample (Ma) \cr
    geologicalage \tab geological age\cr
    material \tab either 'bulk', 'whole rock', 'glass' or 'inclusion' \cr
\bold{Output format}\cr   
    searchtype \tab type of search, only 'rowdata' (table of items matching the criteria) \cr 
                \tab is implemented so far [rowdata].\cr
    outputtype \tab either 'html' or 'xml' [html]\cr
    outputlevel \tab either 'sample' or 'method' [sample] \cr
    startrow \tab sequence number of the first output row minus 1 [0] \cr
    endrow \tab sequence number of the last output row minus 1 [number_of_hits-1] \cr
    standarditems \tab logical; output just the standard items? [yes]\cr
    outputitems \tab comma-separated list of output items \cr
    showcolumnnames \tab logical; import the names of variables? [yes]\cr
}
}

\value{
  (Invisibly) the query string. If no hits were found, the function returns 
  (again invisibly) the value -1.
  
  Imports the data into the \emph{GCDkit} system. Stores the imported dataset into memory 
  (i.e., the variable \code{WRCube}) together with the fields \code{source} that contains
  the string "EarthChem" and date with tome of the search, \code{EarthChem.query.var} with 
  the query variable (a list) and \code{EarthChem.query.url} with URL sent to the web service.  
   
  Then, the previously active dataset becomes the current one.   
}

\seealso{
For further details, see the 
\href{http://ecp.iedadata.org/soap_search_schema.xsd}{EarthChem REST Server Documentation}. 
}

\section{Warning}{
    XML library is required. 
}

\keyword{file}
\concept{File}
\concept{Menu: Data handling}

\author{
    Function by Vojtech Janousek, \email{vojtech.janousek@geology.cz}
    (with helpful assistance from Jason Ash, 
    \email{jasonash@ku.edu}).
    
    Tcl/Tk GUI by Oscar Laurent, \email{oscar.laurent@erdw.ethz.ch}
    
    The XML package was written by Duncan Temple Lang.
}

\examples{
# Some of these examples are based on original examples 
# from  http://ecp.iedadata.org/rest_search_documentation 
   
   EarthChem(list(author="smith",outputtype="html",showcolumnnames="yes",startrow=0,endrow=100,
     outputitems="sample_id,source,longitude,latitude"))
   
   EarthChem(list(author="janousek",outputtype="xml",showcolumnnames="yes",outputitems="sample_id,
     source,longitude,latitude",standarditems="yes"))

   EarthChem(list(author="moyen",outputtype="html",showcolumnnames="yes",outputitems=
     "sample_id,source,longitude,latitude",standarditems="yes"))

   EarthChem(list(geologicalage="cambrian",outputtype="html",startrow=0,endrow=100,
     outputitems="sample_id,source,longitude,latitude"))
   
   EarthChem(list(polygon="-101.953125 39.9375,-99.95625 39.9375,-100.603125 38.53125, -99.815625 
     36.95625,-101.98125 36.984375,-101.953125 39.9375",searchtype="rowdata",outputtype="html",
     outputitems="sampleid",showcolumnnames="yes",standarditems="yes"))

# Read a map directly into R
   query<-"http://ecp.iedadata.org/restsearchservice?
     north=49&east=-100&south=23&west=-24&outputtype=staticmap"
   #shell.exec(query) # easy solution, gets to browser
   filename<-paste(gcdx.dir,"map.jpg",sep="/")
   download.file(query,filename,method="internal",mode="wb")
   shell.exec(filename)
}

