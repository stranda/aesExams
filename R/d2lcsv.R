###
### help R/exams create content for d2l
###  assumes Rmd
prepD2L_upload = function(ex,pts,n=1,name=c("examI"))
{
    if (!dir.exists('images')) dir.create('images')
    meta = exams2brightspace(ex,n=n,name=name,
                 converter="pandoc",
                 base64=F,
                 verbose=T)
    supfiles=c(sapply(meta,sapply,function(e){e$supplements}))
    supfiles=unlist(supfiles[sapply(supfiles,length)>0])
    print(supfiles)
    rfn=paste0(round(runif(1,0,1000000)),".csv")
    sed1='s|src=""|src=""images/|'
    sed2='s|alt="".*""|alt=""image""|'
    exstr = paste("cat ",paste0(name,".csv"),"| sed '",sed1,"'| sed '",sed2,"' >",rfn)
    system(exstr)
    file.copy(rfn,paste0(name,".csv"),overwrite=T)
    unlink(rfn)
    dummy=file.copy(supfiles,'images',overwrite=T)
    meta
}
