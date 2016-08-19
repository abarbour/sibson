.onUnload <- function(libpath) {
  #library.dynam.unload("sibson", libpath)
}
##
# executed after .onLoad is executed, once the namespace is visible to user
.onAttach <- function(...) {
  ##
  pack <- "sibson"
  packageStartupMessage(
    sprintf("Loaded %s (%s) -- Fault reactiviation according to Rick Sibson",
            pack, utils::packageVersion(pack)))
}
