#Copy source-code inside the export folder, to be uploaded to DRE along other results
file.copy(paste0(pathCDM,'METADATA.csv'), exportDir, overwrite = T)
file.copy(paste0(pathCDM,'CDM_SOURCE.csv'), exportDir, overwrite = T)
scr2zip <- dir(sourceDir, full.names = TRUE, recursive=TRUE) 
functions2zip <- dir(functionsDir, full.names = TRUE, recursive=TRUE) 
meta2zip <- dir(metaDir, full.names = TRUE, recursive=TRUE) 
zip(zipfile = "originSourceCode", files = c(scr2zip,functions2zip,meta2zip))
file.copy(paste0(projectDir,"/originSourceCode.zip"), originDir, recursive=TRUE)
unlink(paste0(projectDir,"/originSourceCode.zip"))
