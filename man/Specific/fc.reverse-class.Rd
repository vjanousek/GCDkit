\name{fc.reverse-class}
\Rdversion{1.1}
\docType{class}
\alias{fc.reverse-class}
\alias{initialize,fc.reverse-method}
\alias{petroEval,fc.reverse-method}

\title{Class \code{"fc.reverse"}}
\description{
    An object describing reverse models for fractional crystallization (either based on majors or traces) immediately evaluated
    by the least-squares method. 
}

\section{Objects from the Class}{
    Objects can be created by calls of the form 
    \code{new("fc.reverse", c0.lab, cL.lab, kd.name, min.name, min.used, normalize.min.comp, where, mjr.set, trc.set)}.
}

\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}; the first item [1]: user-defined name, the second item [2]: user comments}
    \item{\code{constraint.mjr}:}{Object of class \code{"character"}, specifying, how are the major-element data constrained: 
        "none", "fwd", "rev" or "fwd_i" (the last means inferred from forward modelling of trace elements)}
    \item{\code{constraint.trc}:}{Object of class \code{"character"}, specifying, how are the trace-element data constrained: 
        "none", "fwd", "rev" or "fwd_i" (the last means inferred from forward modelling of major elements)}
    \item{\code{constraint.iso}:}{Object of class \code{"character"} , specifying, how are the isotopic data constrained: 
        "none", "fwd", "rev" or "fwd_i" (the last means inferred from the forward modelling)}
    \item{\code{where}:}{Object of class \code{"character"}; name of the object (usually a data matrix or data frame) 
        with whole-rock data, from which e.g. C0 and CL are extracted (defaults to "WR") }
    \item{\code{c0.lab}:}{Object of class \code{"character"};  sample name of the parent (or end member A in mixing)}
    \item{\code{c0}:}{Object of class \code{"numeric"}; numeric vector with concentrations and/or isotopic ratios in the parent}
    \item{\code{cL.lab}:}{Object of class \code{"character"}; sample name of the daughter (or end member B in mixing), void for direct models}
    \item{\code{cL}:}{Object of class \code{"numeric"};  concentrations and/or isotopic ratios in the daughter, void for direct}
    \item{\code{cS}:}{Object of class \code{"matrix"}; 2D matrix with  solid composition, mostly vector if constant, or composition of the contaminant}
    \item{\code{formula.mjr}:}{Object of class \code{"expression"}; formula for calculating major elements, 
        So far, only mass balance is implemented, so it is always: \code{(c0 - (1 - ff) * cs)/ff}}
    \item{\code{formula.trc}:}{Object of class \code{"expression"}; formula for calculating trace elements}
    \item{\code{formula.iso}:}{Object of class \code{"expression"}; formula for calculating isotopes}
    \item{\code{kd.name}:}{Object of class \code{"character"}; name of the variable used in global environment 
        to store the table of distribution coefficients for individual minerals/trace elements}
    \item{\code{kd}:}{Object of class \code{"array"}; 3D matrix with values of distribution coefficients for individual minerals/trace elements, but is 2D if constant}
    \item{\code{dd}:}{Object of class \code{"array"};  3D matrix with bulk distribution coefficients for individual trace elements; a vector if the assemblage 
        does not change (the current implementation)}
    \item{\code{min.name}:}{Object of class \code{"character"}; name of the variable in global environment storing mineral compositions}
    \item{\code{min.comp}:}{Object of class \code{"array"} ; 3D mineral matrix of compositions, but 2D if constant (current implementation)}
    \item{\code{min.prop}:}{Object of class \code{"matrix"}; 0-1; 2D numeric matrix with proportions of minerals; a vector if the assemblage 
        does not change (the current implementation)}
    \item{\code{f.range}:}{Two-component vector of class \code{"numeric"};  proportion of the melt in the system (0-1)}
    \item{\code{f.direction}:}{Object of class \code{"character"}; is F increasing or decreasing - "inc" or "dec"}
    \item{\code{cL.calc}:}{Object of class \code{"matrix"}; numeric matrix with calculated evolved liquid composition(s). 
            Calculated, initially only for the values in f.range (i.e. for 0/1 and the requested value), but the \code{petroCalc}
            method allows to add intermediate steps}
    \item{\code{rsq}:}{Object of class \code{"numeric"}; R squared value for reverse model - a measure of the goodness of the fit}
    \item{\code{mjr.used}:}{Object of class \code{"character"}; vector with names of oxides used in building of the model
            (upon the user request or due to the data available)}
    \item{\code{trc.used}:}{Object of class \code{"character"}; vector with names of trace elements used in building of the model
            (upon the user request or due to the data available)}
    \item{\code{iso.used}:}{Object of class \code{"character"}; vector with names of isotopes used in building of the model}
    \item{\code{min.used}:}{Object of class \code{"character"}; minerals used in calculation.}
    \item{\code{intensive}:}{Object of class \code{"list"}; list of intensive parameters}
    \item{\code{graph.par}:}{Object of class \code{"list"}; list of graphical parameters, such as col, lwd, type, arrow, ticks....}
  }
}

\section{Extends}{
    Class \code{"\linkS4class{PetroModel}"}, directly.
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "fc.reverse")}: 
        assigns the slots, checks for completeness  and consistency.
        Input variables include  initial composition (\code{c0}), final liquid composition (\code{cL}), 
        table of mineral compositions (\code{min.comp}) minerals to be used for calculation 
        (\code{min.used}) or table of Kds min.(\code{kd}) (for major- or trace-element based models, respectively).
    }
    
    \item{petroEval}{\code{signature(.Object = "fc.reverse")}: calculates the least-squares reverse model.
        Output variables include F (\code{f.range}), mineral proportions (\code{min.prop}), sum of squared residuals (\code{rsq}) 
        and bulk cumulate composition (\code{cS} - for major-element model) or bulk distribution coefficients 
        (\code{min.dd} - for trace-element model). Output variable contains the amended modelling object.
	}

    \item{summary}{\code{signature(object = "PetroModel")}:
        Prints a formatted summary of the modelling object. 
    }
}}


\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
    
    Jean-Francois Moyen, \email{jfmoyen@gmail.com}
}


\seealso{
     \link{PetroModel-class}
      
     \link{simple.direct-class}
     
     \link{fcDirect} 

     \link{pmDirect} 
     
     \link{fcReverseFractional}
}

\examples{
    showClass("fc.reverse")
}

\keyword{classes} 
\keyword{GCDkit} 
\keyword{manip} 
\concept{Modelling} 
