unk = function(pkgs=c("base","utils"),fnam = path.expand("~/.knowns.Rdata"),top=30,size=20,delay=3,redDelay=2) {
    i=lock=esc=dlg=bool=starting=qlabel=qtext=pkgtext=unknowns=NULL
    knowns=numall=numkno=numunk=numleft=timeleft=num1=num2=num3=num4=num5=NULL
    numlabel1=numlabel2=numlabel3=numlabel4=numlabel5=NULL
    savedknowns=nowknowmode=NULL
    require(tcltk)
    if (delay<1) stop("delay must be at least 1 second")
    if (redDelay<1) stop("redDelay must be at least 1 second")
    if (is.name(substitute(pkgs))) pkgs = as.character(substitute(pkgs))
    if (top<0 || top>150) stop("top must be between 0 (i.e. crantastic feature off) and 150")
    recommendedpackages = rownames(installed.packages(priority="recommended"))
    basepackages = rownames(installed.packages(priority="base"))
    if (top>0 && exists(".unk.toppkgs")) {
        toppkgs = get(".unk.toppkgs")
        rownames(toppkgs) = toppkgs$pkgs
    } else toppkgs = data.frame(NULL)
    
saveanswers=function() {
    knowns = savedknowns
    knowns[head(unknowns,i)] = 1*head(bool,i)  # will reset 0.5 to 0 if don't know (=> re-learn)
    knowns = knowns[order(names(knowns))]
    savedknowns <<- knowns
    save(list="knowns",file=fnam)
    cat("Saved ",i," known/unknown response",if(i>1)"s"else""," to the ",length(savedknowns)," in ",fnam,"\n",sep="")
    knowns <<- knowns[names(knowns) %in% get(".unk.funlist")]
}

updatestatus = function() {
    tclvalue(numall) <<- length(get(".unk.funlist"))
    tclvalue(numkno) <<- sum(knowns==1)+sum(bool)
    if (nowknowmode) {
        tclvalue(numunk) <<- sum(knowns<1)-sum(bool)
    } else {
        tclvalue(numunk) <<- sum(knowns<1)+sum(!head(bool,i))
    }
    n = length(unknowns) - i
    tclvalue(numleft) <<- n
    s = n*3  # assume 3 sec per function (most users will go much faster and beat estimate)
    h = s%/%3600
    m = (s-h*3600)%/%60
    s = s%%60
    tclvalue(timeleft) <<- sprintf("%02d:%02d:%02d",h,m,s)
    invisible()
}

PressedBack = function() {
   if (i>0) {
       bool[i]<<-FALSE
       i<<-i-1
       tclvalue(qtext) = "Press SPACE to resume"
       tclvalue(pkgtext) = ""
       # in case user had finished (changing text) then pressed back
   }
   updatestatus()
   if (i==0) tkconfigure(backbutton,state="disabled")
}

PressedHelp = function() {
    print(help("unk",package="unknownR",help_type="html"))
    tkconfigure(helpbutton,foreground="purple")
}
PressedHomepage = function() {
    browseURL("http://unknownr.r-forge.r-project.org/")
    tkconfigure(homepagebutton,foreground="purple")
}
PressedTopPkgs = function() {
    browseURL("http://unknownr.r-forge.r-project.org/toppkgs.html")
    tkconfigure(toppkgsbutton,foreground="purple")
}

Know = function() {
    if (lock) return()
    if (!starting) {
        lock <<- TRUE
        bool[i] <<- TRUE
        tkconfigure(qlabel,fg="blue")
        updatestatus()
        tcl("after",500,Next)
        # provides visual feedback to user and prevents accidental double key presses.
    } else {
        esc <<- FALSE
        Next()  # display the first one
    }
}

assign(".DontKnow", function(thisi) {
# runs 2s after red regardless of space being pressed, but if space was pressed i would have incremented and thisi<i
    if (thisi==i && !lock && !esc) {
        updatestatus()
        Next()
    }
}, .GlobalEnv)

assign(".Red", function(thisi) {
    # needs to be in globalenv otherwise tcl "after" can't find it when passed an argument
    # "after" on the function names only (no arguments) such as Next seems ok, though.
    # It still needs to be defined within unk so lexical scope can find state variables.
    if (thisi==i && !lock && !esc) {
        tkconfigure(qlabel,fg="red")
        xx = parse(text=paste(".DontKnow(",thisi,")"))
        tcl("after",redDelay*1000,xx)
    }
}, .GlobalEnv)

Unlock = function() {
    lock <<- FALSE
}

Next = function() {
    if (esc) return()  # a very quick ESC following SPACE
    tkconfigure(qlabel,fg="black") 
    if (i == length(unknowns)) {
        # finished
        lock <<- FALSE
        esc <<- TRUE
        starting <<- TRUE
        if (nowknowmode && all(bool)) {
            tclvalue(qtext) = "Congrats! All stones turned."
        } else {
            tclvalue(qtext) = "Quit & save, then run learn()"
        }
        tclvalue(pkgtext) = ""
        if(i>0) tkconfigure(backbutton,state="normal")
        tkconfigure(helpbutton,state="normal")
        tkconfigure(homepagebutton,state="normal")
        tkconfigure(toppkgsbutton,state="normal")
    } else {
        i <<- i+1
        thisunk = strsplit(unknowns[i],split=":")
        prefix = thisunk[[1]][1]
        if (prefix=="PACKAGE") {
            pkg = thisunk[[1]][2]
            tt1 = "PACKAGE:"
            tt2 = paste("Package:",pkg)
            if (pkg %in% basepackages)
                tt1 = "Base package included in R:"
            if (pkg %in% toppkgs$pkgs)
                tt1 = paste("Top",top,"package on Crantastic:")
            if (pkg %in% recommendedpackages) 
                tt1 = "R-core recommended package included in R:"
            tclvalue(pkgtext) = tt1
            tclvalue(qtext) = tt2
        } else {
            tclvalue(pkgtext) = paste(prefix,":",sep="")  # base or utils by default
            tclvalue(qtext) = thisunk[[1]][2]
        }
        xx = parse(text=paste(".Red(",i,")"))
        tcl("after",delay*1000,xx)
        starting <<- FALSE
        lock <<- TRUE
        tcl("after",250,Unlock)
        # lock prevents presses intended for the very end of red which are a little too late counting as a know for the next one. Unexpected that user will see function, recognise, know it and press space all within 250ms. Also prevents holding down space.
        tkconfigure(backbutton,state="disabled")
        tkconfigure(helpbutton,state="disabled")
        tkconfigure(homepagebutton,state="disabled")
        tkconfigure(toppkgsbutton,state="disabled")
    }
}

Esc = function() {
    if (esc) {
        if (i>0) {
            ans = tclvalue(tkmessageBox(message=paste("Save your ",i," known/unknown response",if(i>1)"s"else""," to disk?",sep=""),type="yesnocancel"))
            if (ans=="cancel") return()
            if (ans=="yes") saveanswers()
            i <<- 0  # to not ask again on destroy (see end of unk())
        }
        tkdestroy(dlg)
    } else {
        tkconfigure(qlabel,fg="black")
        lock <<- FALSE
        esc <<- TRUE
        if (!bool[i]) i <<- i-1  # Quick ESC following SPACE should not forget the known
        starting <<- TRUE
        tclvalue(qtext) = "Press SPACE to resume"
        tclvalue(pkgtext) = ""
        if(i>0) tkconfigure(backbutton,state="normal")
        tkconfigure(helpbutton,state="normal")
        tkconfigure(homepagebutton,state="normal")
        tkconfigure(toppkgsbutton,state="normal")
    }
}

Skip = function() {
    if (!lock && !starting) {
        lock <<- TRUE
        tkconfigure(qlabel,fg="red")
        updatestatus()
        tcl("after",500,Next)
    }
}

    if (file.exists(fnam)) {
        tt = load(fnam)
        if (!identical(tt,"knowns")) stop(fnam,"must contain a single object named 'knowns'")
        if (is.logical(knowns)) {
            if (any(grep(":",names(knowns)))) stop("Detected 0.1 format but ':' exists in names")
            tt = names(knowns)%in%objects("package:base")
            ss = names(knowns)%in%objects("package:utils")
            if (!all(tt | ss)) stop("Detected 0.1 format but not all from base or utils")
            names(knowns)[tt] = paste("base:",names(knowns)[tt],sep="")
            names(knowns)[ss] = paste("utils:",names(knowns)[ss],sep="")
            mode(knowns) = "numeric"
            cat("Read old format of .knowns.Rdata ok\n")
        }
        if (!is.numeric(knowns)) stop("knowns object in",fnam,"must be a numeric vector")
        if (is.null(names(knowns)) || any(duplicated(names(knowns)))) stop("Either knowns in",fnam,"has no names, or there is a duplicate in the names")
	    if (!identical(grep(":",names(knowns)),seq(1,length(knowns)))) stop("':' does not exist in all names(knowns)")
	    # cat("Read ",length(knowns)," known/unknown response",if(length(knowns)>1)"s"," from ",fnam," ok\n",sep="")
	    savedknowns = knowns
    } else {
	    cat("First time running unk(). File",fnam,"does not exist\n")
	    savedknowns=knowns=numeric(0)
    }
    if (!exists(".unk.funlist",.GlobalEnv)
        || !all(pkgs %in% attr(get(".unk.funlist"),"pkgs"))
        || (top>0 && !exists(".unk.toppkgs",.GlobalEnv))) {
        funlist = funlist(pkgs,top)
        attr(funlist,"pkgs") = pkgs
        assign(".unk.funlist",funlist,.GlobalEnv)
    } else {
        # the funlist won't have changed in this R session. Restart R (or rm(.unk.funlist)) to build the list again.
        funlist = get(".unk.funlist",.GlobalEnv)
        if (top==0) funlist = grep("^PACKAGE:",funlist,invert=TRUE)
    }
    unknowns = funlist[!funlist %in% names(knowns)]
    knowns = knowns[names(knowns) %in% funlist]  # e.g. if swapping between packages, or a function in deprecated in base in future
    if (!length(unknowns)) {
        # All unknown unknowns have been moved to known knowns or known unknowns.
        unknowns = names(knowns)[knowns!=1]
        if (!length(unknowns)) {
            cat("Nothing to learn! You know all",length(funlist),"functions.\n")
            return()
        }
        title = "Do you (now) know?"
        nowknowmode = TRUE
    } else {
        title = "Do you know?"
        nowknowmode = FALSE
    }
    
    large = tkfont.create(family="courier",size=size*2,weight="bold")
    other = tkfont.create(family="ariel",size=size,weight="bold")
    hlink = tkfont.create(family="ariel",size=size,weight="bold",underline="true")
    small = tkfont.create(family="ariel",size=size/2,weight="bold")
    tclServiceMode(FALSE)
    dlg = tktoplevel()
    tkwm.title(dlg,"unknownR")

    bg = tail(strsplit(tclvalue(tkconfigure(dlg,"-background"))," ")[[1]],1)
    helpbutton = tkbutton(dlg,text="Help",command=PressedHelp,font=hlink,activeforeground="blue",relief="flat",bd=0,activebackground=bg)
    homepagebutton = tkbutton(dlg,text="Homepage",command=PressedHomepage,font=hlink,activeforeground="blue",relief="flat",bd=0,activebackground=bg)
    toppkgsbutton = tkbutton(dlg,text="Top pkgs",command=PressedTopPkgs,font=hlink,activeforeground="blue",relief="flat",bd=0,activebackground=bg)
    tkgrid(tklabel(dlg,text=""))
    tkgrid(tklabel(dlg,text=title,font=other),columnspan=4)
    #tkgrid(tklabel(dlg,text=""))
    tkgrid(helpbutton,column=3,row=1,sticky="w",padx="50")
    tkgrid(homepagebutton,column=3,row=1,sticky="e")
    tkgrid(toppkgsbutton,column=0,row=1,sticky="w")
    pkgtext = tclVar("")
    pkgname = tklabel(dlg,text=tclvalue(pkgtext),font=small)
    tkconfigure(pkgname,textvariable=pkgtext)
    tkgrid(pkgname,columnspan=4)  #row=2,sticky="w",padx="20")
    qtext = tclVar("Press SPACE to start")
    qlabel = tklabel(dlg,text=tclvalue(qtext),width=max(30,max(nchar(funlist))),font=large,relief="ridge",bd=10,bg="light yellow")
    tkconfigure(qlabel,textvariable=qtext)
    tkgrid(qlabel,columnspan=4)
    tkgrid(tklabel(dlg,text=""))
    
    numall = tclVar(0)
    numkno = tclVar(0)
    numunk = tclVar(0)
    numleft = tclVar(0)    
    timeleft = tclVar("")
    
    num1 = tklabel(dlg,textvariable=numall,width=10,anchor="e",font=other)
    num2 = tklabel(dlg,textvariable=numkno,width=10,anchor="e",fg="blue",font=other)
    num3 = tklabel(dlg,textvariable=numunk,width=10,anchor="e",fg="red",font=other)
    num4 = tklabel(dlg,textvariable=numleft,width=10,anchor="e",font=other)
    num5 = tklabel(dlg,textvariable=timeleft,width=10,anchor="e",font=other)
    numlabel1 = tklabel(dlg,text="All:",font=other)
    numlabel2 = tklabel(dlg,text="Known:",fg="blue",font=other)
    numlabel3 = tklabel(dlg,text="Unknown:",fg="red",font=other)
    numlabel4 = tklabel(dlg,text="Remaining:",font=other)
    numlabel5 = tklabel(dlg,text="Estimated time:",font=other)
    tkgrid(numlabel1,num1)
    tkgrid(numlabel2,num2,label6<-tklabel(dlg,text="SPACE : ",fg="blue",font=other),label7<-tklabel(dlg,text="I know it/Ignore it",fg="blue",font=other))
    tkgrid(numlabel3,num3,label8<-tklabel(dlg,text="ENTER : ",fg="red",font=other),label9<-tklabel(dlg,text="I don't know it",fg="red",font=other))
    tkgrid(numlabel4,num4,label10<-tklabel(dlg,text="ESC : ",font=other),label11<-tklabel(dlg,text="Pause/Save/Quit",font=other))
    backbutton = tkbutton(dlg,text="Back",command=PressedBack,font=other,bd=2,state="disabled")
    tkgrid(numlabel5,num5,backbutton,label12<-tklabel(dlg,text="Undo last answer",font=other))
    tkgrid(tklabel(dlg,text=""),tklabel(dlg,text=""))
    tkgrid.configure(numlabel1,numlabel2,numlabel3,numlabel4,numlabel5,label6,label8,label10,backbutton,sticky="e")
    tkgrid.configure(num1,num2,num3,num4,num5,label7,label9,label11,label12,sticky="w")
    tkgrid.columnconfigure(dlg,0,weight=1)
    tkgrid.columnconfigure(dlg,1,weight=5)
    tkgrid.columnconfigure(dlg,2,weight=1)
    tkgrid.columnconfigure(dlg,3,weight=5)
    tkgrid(tklabel(dlg,text=""))
    
    i = 0
    lock = FALSE
    esc = TRUE
    unknowns = sample(unknowns)
    bool = rep(FALSE,length(unknowns))
    starting = TRUE
    
    tkbind(dlg,"<space>", Know)
    tkbind(dlg,"<Return>", Skip)
    tkbind(dlg,"<Escape>", Esc)
    updatestatus()
    tclServiceMode(TRUE)
    tkfocus(dlg)
    tkwait.window(dlg)
    if (i>0 && tclvalue(tkmessageBox(message=paste("Save your ",i," known/unknown response",if(i>1)"s"else""," to disk?",sep=""),type="yesno"))=="yes") {
        saveanswers()
    }
    if (length(knowns) == length(funlist)) {
        if (any(knowns==0)) {
            cat("Now type learn() to view help for your",sum(knowns==0),"known unknowns. Run unk() again when you know them.\n")
        }
        if (all(knowns==1)) {
            cat("Congratulations! You have no known unknown functions left to learn.\nWe hope this package helped you discover something you didn't know you didn't know.\n")
        }
    } else {
        n = length(funlist)-length(knowns)
        cat("You have ",n," (",trunc(100*n/length(funlist)),"% of ",length(funlist),") unknown unknowns remaining.\nRun unk() any time to continue where you left off.\n",sep="")
    }
    tkdestroy(dlg)
    invisible()
}


