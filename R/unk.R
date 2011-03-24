unk = function(fnam = path.expand("~/.knowns.Rdata"),size=20) {
    i=lock=esc=dlg=bool=starting=qlabel=qtext=unknowns=NULL
    knowns=numall=numkno=numunk=numleft=timeleft=num1=num2=num3=num4=num5=NULL
    numlabel1=numlabel2=numlabel3=numlabel4=numlabel5=NULL
    savedknowns=nowknowmode=backbutton=NULL
    require(tcltk)
    
saveanswers=function() {
    knowns = savedknowns
    knowns[head(unknowns,i)] = head(bool,i)
    knowns = knowns[order(names(knowns))]
    savedknowns <<- knowns
    save(list="knowns",file=fnam)
    cat("Saved ",i," known/unknown response",if(i>1)"s"else""," to the ",length(savedknowns)," in ",fnam,"\n",sep="")
    knowns <<- knowns[names(knowns) %in% get(".unk.funlist")]
}

updatestatus = function() {
    tclvalue(numall) <<- length(get(".unk.funlist"))
    tclvalue(numkno) <<- sum(knowns)+sum(bool)
    if (nowknowmode) {
        tclvalue(numunk) <<- sum(!knowns)-sum(bool)
    } else {
        tclvalue(numunk) <<- sum(!knowns)+sum(!head(bool,i))
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
       # in case user had finished (changing text) then pressed back
   }
   updatestatus()
   if (i==0) tkconfigure(backbutton,state="disabled")
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
        tcl("after",2000,xx)
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
        if(i>0) tkconfigure(backbutton,state="active")
    } else {
        i <<- i+1
        tclvalue(qtext) = unknowns[i]
        xx = parse(text=paste(".Red(",i,")"))
        tcl("after",3000,xx)
        starting <<- FALSE
        lock <<- TRUE
        tcl("after",250,Unlock)
        # lock prevents presses intended for the very end of red which are a little too late counting as a know for the next one. Unexpected that user will see function, recognise, know it and press space all within 250ms. Also prevents holding down space.
        tkconfigure(backbutton,state="disabled")
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
        if(i>0) tkconfigure(backbutton,state="active")
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
        if (!is.logical(knowns)) stop("knowns object in",fnam,"must be a logical vector")
        if (is.null(names(knowns)) || any(duplicated(names(knowns)))) stop("Either knowns in",fnam,"has no names, or there is a duplicate in the names")
	    # cat("Read ",length(knowns)," known/unknown response",if(length(knowns)>1)"s"," from ",fnam," ok\n",sep="")
	    savedknowns = knowns
    } else {
	    cat("First time running unk(). File",fnam,"does not exist\n")
	    knowns=logical(0)
	    savedknowns=logical(0)
    }
    if (!exists(".unk.funlist",.GlobalEnv)) {
        funlist = funlist()
        assign(".unk.funlist",funlist,.GlobalEnv)
    } else {
        # the funlist won't have changed in this R session. Restart R (or rm(.unk.funlist)) to build the list again.
        funlist = get(".unk.funlist",.GlobalEnv)
    }
    unknowns = funlist[!funlist %in% names(knowns)]
    knowns = knowns[names(knowns) %in% funlist]  # e.g. if swapping between packages, or a function in deprecated in base in future
    if (!length(unknowns)) {
        # All unknown unknowns have been moved to known knowns or known unknowns.
        unknowns = names(knowns)[!knowns]
        if (!length(unknowns)) {
            cat("Nothing to learn! You know all",length(funlist),"functions.\n")
            return()
        }
        title = "Do you (now) know? :"
        nowknowmode = TRUE
    } else {
        title = "Do you know? :"
        nowknowmode = FALSE
    }
    knowns = knowns

    large = tkfont.create(family="courier",size=size*2,weight="bold")
    other = tkfont.create(family="ariel",size=size,weight="bold")
    tclServiceMode(FALSE)
    dlg = tktoplevel()
    tkwm.title(dlg,"unknownR")
    tkgrid(tklabel(dlg,text=""))
    tkgrid(tklabel(dlg,text=title,font=other),columnspan=4)
    tkgrid(tklabel(dlg,text=""))
    
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
    numlabel1 = tklabel(dlg,text="All functions:",font=other)  #relief="groove"
    numlabel2 = tklabel(dlg,text="Known:",fg="blue",font=other)
    numlabel3 = tklabel(dlg,text="Unknown:",fg="red",font=other)
    numlabel4 = tklabel(dlg,text="Remaining:",font=other)
    numlabel5 = tklabel(dlg,text="Estimated time:",font=other)
    tkgrid(numlabel1,num1)
    tkgrid(numlabel2,num2,label6<-tklabel(dlg,text="SPACE : ",fg="blue",font=other),label7<-tklabel(dlg,text="I know it",fg="blue",font=other))
    tkgrid(numlabel3,num3,label8<-tklabel(dlg,text="ENTER : ",fg="red",font=other),label9<-tklabel(dlg,text="I don't know it",fg="red",font=other))
    tkgrid(numlabel4,num4,label10<-tklabel(dlg,text="ESC : ",font=other),label11<-tklabel(dlg,text="Pause/Save/Quit",font=other))
    backbutton = tkbutton(dlg,text="Back",command=PressedBack,font=other,bd=2,state="disabled")
    tkgrid(numlabel5,num5,backbutton,label12<-tklabel(dlg,text="Undo last answer",font=other))
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
    tkbind(dlg,"<Return>", Skip)  # anticipate most people will go space or q to get through it quickly
    tkbind(dlg,"<Escape>", Esc)
    updatestatus()
    tclServiceMode(TRUE)
    tkfocus(dlg)
    tkwait.window(dlg)
    if (i>0 && tclvalue(tkmessageBox(message=paste("Save your ",i," known/unknown response",if(i>1)"s"else""," to disk?",sep=""),type="yesno"))=="yes") {
        saveanswers()
    }
    if (length(knowns) == length(funlist)) {
        tolearn = names(knowns)[!knowns]
        if (length(tolearn)) {
            assign("tolearn",tolearn,envir=.GlobalEnv)
            cat("Now type learn() to view help for your",length(tolearn),"known unknowns. Run unk() again when you know them.\n")
        } else {
            cat("Congratulations! You have no known unknown functions left to learn.\nWe hope this package helped you discover something you didn't know you didn't know.\n")
        }
    } else {
        n = length(funlist)-length(knowns)
        cat("You have ",n," (",trunc(100*n/length(funlist)),"% of ",length(funlist),") unknown unknowns remaining.\nRun unk() any time to continue where you left off.\n",sep="")
    }
    tkdestroy(dlg)
    invisible()

}


