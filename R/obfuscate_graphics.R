##
## converts filename of graphic to an obfuscated form and then
## includes the graphic.  Assumes png for the time being
##
obfuscate_graphics = function(fn,...)
{
    ofn = paste0(round(runif(1,0,10000000)),"_",round(runif(1,0,10000000)),".png")
    x=file.copy(fn,ofn)
    knitr::include_graphics(ofn,...)
}
