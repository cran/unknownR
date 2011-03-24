learn=function(x=get("tolearn"))
{
    cat("\nPlease arrange your browser and R window as suggested in ?learn\n")
    cat("To prevent multiple tabs/windows please read ?learn.\n\n")
    cat("Ctrl-C or ESC to quit learn() loop\n")
    cat("ENTER for next function\n\n")
    for (i in x) {
        print(help(i))
        cat(i," ")
        scan(quiet=TRUE)
    }
    cat("\nFinished learning your known unknowns. Run unk() again to update your knowns file.\n")
    invisible()
}


