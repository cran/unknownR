popcon = function(scrape=TRUE) {
    popcon = "http://crantastic.org/popcon"
    crantasticpath = "http://crantastic.org/packages/"
    insiderpath = "http://www.inside-r.org/packages/cran/"
    
    if (scrape) {
        popcon = do.call("rbind",lapply(1:3,function(i) {
            cat("Fetching page",i,"of",popcon,"...")
            html = scan(paste(popcon,"?page=",i,sep=""),character(),quiet=TRUE)
            cat("done\n")
            n = grep("[/]packages[/]",html)[-1]   # -1 removes the header "all" line
            pkgs = html[n]
            pkgs = gsub("<.+","",pkgs)
            pkgs = gsub(".+>","",pkgs)
            d = sapply(mapply(seq,n+1,c(tail(n-1,-1),tail(n,1)+15)),function(i)paste(html[i],collapse=""))
            d = strsplit(d,split="<td>")
            avgvote = suppressWarnings(as.numeric(gsub("/.*","",sapply(d,"[",4))))
            users = suppressWarnings(as.integer(gsub("<.*","",sapply(d,"[",5))))
            data.frame(pkgs,avgvote,users,stringsAsFactors=FALSE)
        }))
        nas = is.na(popcon$avgvote) | is.na(popcon$users)
        if (any(nas)) {
            cat("Likely blanks in popcon (i.e. no vote) :\n")
            print(popcon[nas,,drop=FALSE])
        }
        # now fetch the number of votes (which is only on the detail page for each package)
        numvotes = sapply(popcon$pkgs, function(i){
            cat("Fetching",i,"from Crantastic ... ")
            html = scan(paste(crantasticpath,gsub("[.]","-",i),sep=""),character(),quiet=TRUE)
            ans = as.integer(gsub("[(]","",html[grep("vote.*)",html)-1])[1])
            if (is.na(ans) || ans<0) stop("likely format error (Crantastic)")
            cat(ans,"\n")
            ans
        })
        # fetch the votes from Inside-R 
        insidervotes = sapply(popcon$pkgs, function(i) {
            cat("Fetching",i,"from Inside-R ... ")
            html = scan(paste(insiderpath,i,sep=""),character(),quiet=TRUE)
            ans = html[grep("vote.*>[0-9]+<",html)]
            if (!length(ans)) return(0)  # some pkgs had no votes showing on Inside-R
            ans = as.integer(sapply(strsplit(ans,split="[<>]"),"[",4))[1]
            if (is.na(ans) || ans<0) stop("likely format error (Inside-R)")
            cat(ans,"\n")
            ans
        })
        ans = cbind(popcon,numvotes,insidervotes)
        
        ans$score = round(with(ans, ((avgvote*numvotes+3)/(numvotes+1) + (users*5+3)/(users+1))/2),3)
        # all packages start with one 3* vote
        # an "i use this!" counts as a 5*, again starting with one default 3*
        # then equally weight vote and users
        
        ans$crantasticrank = 1:nrow(ans)
        ans = ans[order(-ans$score),]
        ans$rank = 1:nrow(ans)
        rownames(ans) = NULL
        ans = ans[,c("pkgs","avgvote","numvotes","users","score","rank","crantasticrank","insidervotes")]
        colnames(ans)[7:8] = c("Crantastic.rank","Inside-R.votes")
        save(list="ans",file="ans.Rdata")
    } else load("ans.Rdata")
    
    fnam = path.expand("~/R/unknownr/www/toppkgs.csv")
    write(format(Sys.time(),"%d %b %Y"),fnam)
    suppressWarnings(write.table(ans,fnam,append=TRUE,sep=",",row.names=FALSE,quote=FALSE))
    # warning is about column names being added
    cat("Written",fnam,"\n")
    
    fnam = path.expand("~/R/unknownr/www/toppkgs.html")
    require(hwriter)  # yes, I have voted for hwriter
    p = openPage(fnam, link.css="hwriter.css")
    hwrite('<br>This package list is compiled and used by <a href="http://unknownr.r-forge.r-project.org/">unknownR</a> to help users<br>easily and quickly discover useful packages rated by other users. By<br>',p)
    hwrite('default the top 30 are included in unknownR\'s list (plus R-core<br>recommended packages included in R).<br><br>',p)    
    hwrite(c('Data is scraped from Crantastic and Inside-R. See',hwrite('footnote', link='#footnote'),'.<br>'),p,table=FALSE)
    hwrite('Note that Inside-R appears to multiply the number of votes by a scaling factor.<br><br>',p)
    colnames(ans)[1]="CRAN package"
    hwrite(ans, p, row.names=FALSE,
                   table.style="margin-left:0px",
                   row.bgcolor=list('#aaffaa'),
                   row.style=list('font-weight:bold'),
                   col.style=list(avgvote='text-align:right',users='text-align:right',numvotes='text-align:right',score='text-align:right',rank='text-align:right',Crantastic.rank='text-align:right',"Inside-R.votes"='text-align:right'),
                   col.links=list("CRAN package"=paste("http://www.inside-r.org/packages/cran/",ans[,1],sep="")))
    
    hwrite('<br><a href="http://crantastic.org/popcon">Crantastic\'s ranking</a> seems inappropriate for unknownR\'s needs:<br>',p,name="footnote")
    hwrite('<ul><li>Packages with just one single vote (5*) are ranked highly because they have the maximum vote of 5.000.',p)
    hwrite('<li>ggplot2 is ranked 41st (when writing this), which doesn\'t seem correct given it has the most votes and the most users, by far.',p)
    hwrite('<li>Some packages on page <a href="http://crantastic.org/popcon?page=2">2</a> and <a href="http://crantastic.org/popcon?page=3">3</a> have many votes and users, but are harder to find.</ul>',p)
    hwrite('The data is scraped and a default 3* vote is added to each package to address these issues.<br>',p)
    hwrite('No claim is made that this is the <em>best</em> method, just that it is better than Crantastic\'s rank.<br>',p)
    hwrite('This adjustment has been suggested to Hadley. If popcon is changed, this page can be removed.<br>',p)
    hwrite('The R function that generates this page, including the adjustment formula, is <a href="https://r-forge.r-project.org/scm/viewvc.php/pkg/R/popcon.R?view=markup&root=unknownr">here</a>.<br><br>',p)
    hwrite('The content and data from <a href="http://crantastic.org/">Crantastic</a> and <a href="http://www.inside-r.org/">Inside-R</a> is available under the <a href="http://creativecommons.org/licenses/by-sa/3.0/">CC Attribution-Share Alike 3.0 Unported</a> license.<br>',p)
    hwrite('The derived data on this page is also available under the <a href="http://creativecommons.org/licenses/by-sa/3.0/">CC Attribution-Share Alike 3.0 Unported</a> license.',p)
    hwrite('<script type="text/javascript">var sc_project=6700858;var sc_invisible=1; var sc_security="3e5c47ee";</script><script type="text/javascript" src="http://www.statcounter.com/counter/counter.js"></script><noscript><div class="statcounter"><a title="website statistics" href="http://statcounter.com/free-web-stats/" target="_blank"><img class="statcounter" src="http://c.statcounter.com/6700858/0/3e5c47ee/1/" alt="website statistics"></a></div></noscript>',p)
    closePage(p)
    cat("Written",fnam,"\n")
    browseURL(paste("file://", fnam, sep = ""))
    invisible()
}


