funlist = function() {
    # list all functions in base and utils which we would
    # like to 'know'. Exclude the many as.* methods, just
    # including "as" once for example. Include non-methods
    # that may look like methods such as 'write.table'.
    require(tcltk)
    xx = sort(c(objects(pos="package:base"), objects(pos="package:utils")))
    xx = xx[-grep("[<][-]",xx)]
    xx = xx[-grep("[?]",xx)]
    defunct = sapply(xx, function(x) {
        thisfun = get(x)
        is.function(thisfun) && length(grep("[.]Defunct", deparse(thisfun)))
    })
    xx = xx[!defunct]
    nodots = xx[grep("^[^.]+$",xx)]
    nodots = nodots[!nodots %in% c("UseMethod","|","||")]
    i = 0
    tclServiceMode(FALSE)
    pb = tkProgressBar("unknownR", "Filtering function list ...", 0, length(nodots))
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
	    setTkProgressBar(pb,i)  
        NULL
    }))
    close(pb)
    cat("\n");flush.console()
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
    for (e in exclude) xx = xx[-grep(e,xx)]
    xx = sort(xx)
    xx
}

