funlist = function(pkgs,top) {
    # list all functions in base and utils which we would
    # like to 'know'. Exclude the many as.* methods, just
    # including "as" once for example. Include non-methods
    # that may look like methods such as 'write.table'.
    # Exclude functions in help file ns-internal and similar.
    require(tcltk)
    xx = unlist(lapply(pkgs,function(x){
        ans = objects(paste("package:",x,sep=""))
        names(ans) = rep(x,length(ans))
        ans
    }))
    xx = xx[grep("[<][-]",xx,invert=TRUE)]
    xx = xx[grep("[?]",xx,invert=TRUE)]
    defunct = sapply(xx, function(x) {
        thisfun = get(x)
        is.function(thisfun) && length(grep("[.]Defunct", deparse(thisfun)))
    })
    xx = xx[!defunct]
    nodots = xx[grep("^[^.]+$",xx)]
    nodots = nodots[!nodots %in% c("UseMethod","|","||")]
    i = 0
    tclServiceMode(FALSE)
    pb = tkProgressBar("unknownR", "Filtering function list ...", 0, 1)
    tclServiceMode(TRUE)
    realmethods = unlist(lapply(nodots, function(x) {
	    i <<- i + 1
        thisfun = get(x)
        if (is.function(thisfun)) {
            dd = deparse(thisfun)
            if (is.primitive(thisfun) || length(grep("UseMethod",dd))) {
                return(suppressWarnings(tryCatch(methods(x),error=function(e)NULL)))
            }
        } 
	    setTkProgressBar(pb,i/length(nodots))  
        NULL
    }))
    if (length(grep("^[^.]*$",realmethods))) stop("some methods don't have any .")
    xx = xx[!xx %in% realmethods]
    exclude = c("^as[.].+",
                "^Summary[.].+",
                "^Math[.].+",
                "^Ops[.].+",
                "^qr[.].+",
                "^all.equal[.].+",
                "^aspell[_].+",
                "^[^a-zA-Z0-9%]")
    for (e in exclude) xx = xx[grep(e,xx,invert=TRUE)]
    xx = sort(xx)
    # now find the .Rd page (one page often contains many functions)
    helppage = character(length(xx))
    fp = sapply(pkgs,.find.package)
    setTkProgressBar(pb,label="Filtering manual pages ...",value=0)
    for (i in seq(along=xx)) {
        thishelppage = utils:::index.search(xx[i], fp[names(xx)[i]])
        helppage[i] = if(length(thishelppage)) basename(thishelppage) else ""
        setTkProgressBar(pb,i/length(xx))
    }
    close(pb)

    # Remove functions with no help page. Only known reason for this is that
    # objects assigned in Rprofile.site are installed into base.
    # Thanks to Richard Cotton for reporting bug #1403 and the fix.
    remove = which(helppage=="")
    if (length(remove)) {
        if (!all(names(xx)[remove]=="base")) {
            tt = xx[remove][names(xx)[remove]!="base"]
            warning("Removed some objects without help pages that aren't in base (i.e. aren't assigned by your Rprofile.site):",paste(tt,collapse=","))
        } # else cat("Excluding objects in base that are assigned in Rprofile.site:",paste(xx[remove],collapse=","),"\n")
        xx = xx[-remove]
        helppage = helppage[-remove]
    }
    
    xx = paste(names(xx),xx,sep=":")
    names(xx) = helppage
    # Now remove some "internal" groups we don't think users need.
    # Obtained using sort(table(names(xx))) and looked at the largest groups manually
    # Removes 69 functions at the time of writing (R 2.12.1)
    before =  length(xx)
    remove = c("ns-internal",
               "sys.parent",
               "SweaveUtils",
               "numeric_version", # seems to be 7 different R version functions
               "ns-reflect",
               "bindenv",
               "converters",
               "base-internal")
    xx=xx[!names(xx) %in% remove]
    xx = xx[grep("-deprecated",names(xx),invert=TRUE)]
    # cat("Reduced",before,"to",length(xx),"using manual filter on .Rd names\n")
    
    pkgs = rownames(installed.packages(priority="high"))  # base and recommended
    pkgs = pkgs[!pkgs %in% c("base","utils")]
    if (top>0) {
        setTimeLimit(elapsed=10)
        cat("Reading toppkgs ...");flush.console()
        toppkgs = try(read.table("http://unknownr.r-forge.r-project.org/toppkgs.csv",skip=1,sep=",",header=TRUE,stringsAsFactors=FALSE))
        setTimeLimit(elapsed=Inf)
        if (inherits(toppkgs,"try-error")) { 
            cat("likely internet connection problem. Continuing without packages this time.\n")
        } else {
            cat("done.\n")
            assign(".unk.toppkgs",toppkgs,.GlobalEnv)
            pkgs = sort(unique(c(head(toppkgs$pkgs,top),pkgs)))
        }
    }
    pkgs = paste("PACKAGE",pkgs,sep=":")
    c(xx,pkgs)
}

