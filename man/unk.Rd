\name{unk}
\alias{unk}
\title{ Starts GUI interface for unknownR }
\description{
  Starts GUI interface for unknownR.
}
\usage{
unk(pkgs=c("base","utils"),fnam=path.expand("~/.knowns.Rdata"),top=30,size=20,delay=3,redDelay=2)
}
\arguments{
  \item{pkgs}{ Names of packages from which to build function list. }
  \item{fnam}{ Full path to data file containing your knowns. By default this is .knowns.Rdata in your home folder. }
  \item{top}{ Number of packages from Crantastic to include. }
  \item{size}{ Font size. The main label displays the function name at \code{2*size}. }
  \item{delay}{ How long each function is displayed before being turned red, by default 3 seconds. }
  \item{redDelay}{ How long a function is red before 'Don't know' is assumed, by default 2 seconds. }
}
\details{
  Fast and fun way to discover useful functions in \R that you didn't know you didn't know. Cheat sheets, reference cards, tip of the days, ticking off the index of reference manuals and books (manually) are other good ways to discover unknown unknowns, but how do you know you haven't missed something, and how do you keep up to date?
  
  Using \code{unk()} for the first time takes about 40 mins. Your \emph{known knowns} are saved leaving a list of \emph{known unknowns} to work through at your leisure (using learn()). Once you 'know' all functions you have confidence you haven't missed anything. Chances are you will learn at least one new function that you wished you knew before.
  
  When \R is upgraded, simply run \code{unk()} again. Any new functions added by \R-core will automatically be presented to you (in case you missed them from NEWS).
  
  Tip: If you can tell from a function name alone that you aren't interested in it (and you are sure) then press SPACE to ignore it. Knowing that you don't need to know something is considered the same as knowing you know it by this package. That way it won't pop up in your known unknowns. Really, you are panning for gold.
}
\value{
   Nothing is returned. Responses are saved to \code{fnam}.
}
\seealso{ \code{\link{learn}} 
\if{html}{\out{<script type="text/javascript">var sc_project=6700858;var sc_invisible=1; var sc_security="3e5c47ee";</script><script type="text/javascript" src="http://www.statcounter.com/counter/counter.js"></script><noscript><div class="statcounter"><a title="website statistics" href="http://statcounter.com/free-web-stats/" target="_blank"><img class="statcounter" src="http://c.statcounter.com/6700858/0/3e5c47ee/1/" alt="website statistics"></a></div></noscript>}}}
\references{
   Homepage: \url{http://unknownr.r-forge.r-project.org/}

   Many thanks to Greg Snow for his answer on R-help, 28 May 2010. A search returned his answer several months later. \code{unk()} makes extensive use of the \code{after} function in Tcl/Tk.

   \url{http://r.789695.n4.nabble.com/Wait-for-keystroke-or-timeout-tp2233895p2234858.html} 
}
\examples{
   \dontrun{
   unk()
   learn()}
}


