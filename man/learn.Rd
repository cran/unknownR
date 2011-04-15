\name{learn}
\alias{learn}
\title{ Run learn() after unk() }
\description{
  Displays help for the known unknowns saved by \code{unk()}.
}
\usage{
learn(pkgs=NULL,relearn=FALSE,fnam = path.expand("~/.knowns.Rdata"))
}
\arguments{
  \item{pkgs}{ A character vector of package names. NULL means all known unknowns from any package \code{unk()} has been passed. }
  \item{relearn}{ Reread the manual pages again, even though you already \code{learn()}-ed them. }
  \item{fnam}{ Same as \code{fnam} in \code{unk()} }
}
\details{
  \code{help()} is called on each function name. Press ENTER for the next function. ~/.knowns.Rdata is updated to "learn()-ed" status as each function's help page is displayed. Run \code{unk()} afterwards when you know them.
  
  We recommend you set \code{options(help_type="html")} so that help is displayed in your internet browser. It may be convenient to resize the browser to fill most of your screen, with the R prompt shrunk and non-overlapping with the browser. It is then easier to put focus back the R prompt (to press ENTER) after you have clicked the browser to scroll or follow links.
  
  If you find mutiple windows, or multiple tabs being opened in your browser, you can try the following options.

\itemize{
\item \strong{Firefox 3.5+}.
   \enumerate{
   \item Edit->Preferences->Tabs->Open new windows in a new tab instead,  AND
   \item Enter "about:config" as the URL->'Here be dragons' OK->browser.link.open_newwindow->Set to 1
   }
   Now, for general browsing, ctrl-click to open links in a new tab.
   
\item \strong{Internet Explorer}

   Tools->Internet Options->Tab Settings->Open links from other programs in->The current tab
   
\item \strong{Safari}

   Preferences->General->Open links from Applications->In a new tab in the current window
   
   However, a new tab is created for each \code{help()}. Perhaps a Safari extension is available.
   
\item \strong{Chrome (including v10) and Chromium}

   We haven't found a way to stop a new tab for each \code{help()}.
}   
If you know how to make Safari or Chrome use the current tab for external applications, like Firefox and IE, please let \code{maintainer("unknownR")} know so this text may be updated. Thank you.

}
\value{
   Nothing. 
}
\seealso{ \code{\link{unk}} }
\references{
Homepage: \url{http://unknownr.r-forge.r-project.org/}
}
\examples{
   \dontrun{
   unk()
   learn()}
}


