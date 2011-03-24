\name{unk}
\alias{unk}
\title{ Starts GUI interface for unknownR }
\description{
  Starts GUI interface for unknownR.
}
\usage{
unk(fnam=path.expand("~/.knowns.Rdata"),size=20)
}
\arguments{
  \item{fnam}{ Full path to data file containing your knowns. By default this is .knowns.Rdata in your home folder. }
  \item{size}{ Font size. The main label displays the function name at \code{2*size}. }
}
\details{
  Fast and fun way to discover useful functions in \R that you didn't know you didn't know. Cheat sheets, reference cards, tip of the days, ticking off the index of reference manuals are other good ways to discover unknown unknowns, but how do you know you haven't missed something, and how do you keep up to date?
  
  Using \code{unk()} for the first time takes about 40 mins. Your \emph{known knowns} are saved leaving a list of \emph{known unknowns} to work through at your leisure. Once you 'know' all functions you have confidence you haven't missed anything. Chances are you will learn at least one new function that you wished you knew before.
  
  When \R is upgraded, simply run \code{unk()} again. Any new functions added by \R-core will automatically be presented to you (in case you missed them from NEWS).
  
  Tip: If you know you don't need to know a function, then just say you know it. That way it won't pop up in your known unknowns. So, pressing SPACE means 'I don't care about it' and/or 'I know it'. Really, you are panning for gold.
}
\value{
   A vector of known unknown function names is silently returned and written to the global environment as \code{tolearn}.
}
\references{
   Many thanks to Greg Snow for his answer on R-help, 28 May 2010. A search by the author of unknownR returned his answer several months later (\code{unk()} makes extensive use of the 'after' function in Tcl/Tk).
   
   \url{http://r.789695.n4.nabble.com/Wait-for-keystroke-or-timeout-tp2233895p2234858.html} 
}
\examples{
   \dontrun{
   unk()}
}


