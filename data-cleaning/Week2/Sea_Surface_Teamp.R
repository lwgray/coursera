### Note to self,  I use the original document that was provided

ab <- read.fwf('t4.tab', widths=62, skip=4)
cd <- gsub('-', ' ', ab$V1)
b <- data.frame("Week"=character(), "SST"=numeric(), 
                "SSTA" = numeric(), "SST.1" = numeric(), "SSTA.1" = numeric(), "SST.2" = numeric(), 
                "SSTA.2" = numeric(), "SST.3" = numeric(), "SSTA.3" = numeric())
for (i in seq(cd)){
        x <- cd[i]
        y <- strsplit(trimws(x), '\\s+')
        y <- as.data.frame(y)
        y <- t(y)
        colnames(y) <- c("Week", "SST", "SSTA", "SST.1", "SSTA.1", "SST.2", "SSTA.2", "SST.3", "SSTA.3")
        rownames(y) <- c()
        b <- rbind(b,y)
}

b

# A future note:  I should have preprocessed the data by using notepad++ to replace the '-' with a 'space'
# this would have eliminated several steps.
