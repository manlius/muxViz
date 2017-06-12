dfLanguage <- data.frame()

loadLanguageFile <- function(){
    cat("\n")
    cat("Loading language file...\n")

    dfLanguage <<- read.csv("lang/default.csv",header=T)
    rownames(dfLanguage) <<- dfLanguage$object
    dfLanguage$object <<- as.character(dfLanguage$object)
    dfLanguage$message <<- as.character(dfLanguage$message)
    dfLanguage$type <<- as.character(dfLanguage$type)

    cat("   + Done!\n")
    cat("\n")
}

getText <- function(ObjectName){
    return(dfLanguage[ObjectName,]$message)
}

loadLanguageFile()