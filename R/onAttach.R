".onAttach"=function(libname, pkgname) {
    cat("unknownR",as.character(packageVersion("unknownR")),"\n")
    cat("   Run unk() first, then learn()\n")
    cat("   Others have found it helpful to first read the short presentation\n")
    cat("   on the homepage: http://unknownr.r-forge.r-project.org/\n")
    cat("   Please read ?unk and ?learn, too.\n")
}

