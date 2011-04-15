learn=function(pkgs=NULL,relearn=FALSE,fnam = path.expand("~/.knowns.Rdata"))
{
    if (!file.exists(fnam)) stop("Please run unk() first to create",fnam)
    tt = load(fnam)
    if (!identical(tt,"knowns")) stop(fnam," must contain a single object named 'knowns'")
    if (!is.numeric(knowns)) stop("knowns object in ",fnam," must be a numeric vector")
    if (is.null(names(knowns)) || any(duplicated(names(knowns)))) stop("Either knowns in ",fnam," has no names, or there is a duplicate in the names")
	if (!identical(grep(":",names(knowns)),seq(1,length(knowns)))) stop("':' does not exist in all names(knowns)")
	tolearn = knowns
	basepackages = rownames(installed.packages(priority="base"))
	
	if (!is.null(pkgs)) {
	    if (!is.character(pkgs)) stop("pkgs argument needs to be a character vector of package names")
        tolearn = tolearn[sapply(pkgs,function(x)grep(paste("^",x,":",sep=""),names(tolearn)))]
    }
    if (all(tolearn==1)) {
        cat("You already know all the functions in these packages. Nothing to learn()!\n")
        return(invisible())
    }
    if (relearn) tolearn[tolearn==0.5] = 0
	tolearn = tolearn[tolearn==0]
	if (!length(tolearn)) {
	    cat("You have already learn()-ed all functions in these packages that you didn't know.\nRun unk() to update your .knowns.Rdata file, or set relearn=TRUE to learn() them again.\n")
	    return(invisible())
	}
    cat("\nPlease arrange your browser and R window as suggested in ?learn\n")
    cat("To prevent multiple tabs/windows please read ?learn.\n\n")
    cat("Ctrl-C or ESC to quit learn() loop\n")
    cat("ENTER for next function\n\n")
    for (i in names(tolearn)) {   # i.e. known unknowns
        cat(i,"  ")
        tt = strsplit(i,split=":")[[1]]
        if (tt[1]=="PACKAGE") {
            pkg = tt[2]
            if (pkg %in% basepackages) {
                if (tools:::httpdPort == 0L) tools::startDynamicHelp()
                browseURL(paste("http://127.0.0.1:",tools:::httpdPort,"/library/",pkg,"/html/00Index.html",sep=""))
            }
            else browseURL(paste("http://crantastic.org/packages/",gsub("[.]","-",pkg),sep=""))
        } else {
            print(help(topic=tt[2],package=tt[1]))
        }
        scan(quiet=TRUE)
        knowns[i] = 0.5
        save(list="knowns",file=fnam)
    }
    cat("\nFinished learning your known unknowns. Run unk() again to update your knowns file.\n")
    invisible()
}


