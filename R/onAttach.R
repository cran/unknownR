".onAttach"=function(libname, pkgname) {
    packageStartupMessage("unknownR",as.character(packageVersion("unknownR")),"\n")
    packageStartupMessage("   Run unk() first, then learn()\n")
    packageStartupMessage("   Others have found it helpful to first read the short presentation\n")
    packageStartupMessage("   on the homepage: http://unknownr.r-forge.r-project.org/\n")
    packageStartupMessage("   Please read ?unk and ?learn, too.\n")
}

