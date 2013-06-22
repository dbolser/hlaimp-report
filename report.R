
if(!interactive()){
  postscript(file="test.ps", paper='a4', horizontal=TRUE)
}

## List of directories containing unzipped HLAIMP results
dirs <- c("Dan", "Nao")
dirs <- c("Ash", "Dan", "Mum", "Dad")
dirs <- c("Dan", "Sam", "Sbm")



## Data taken from the files in those directories
data <- list()



## Grab data
for(dir in dirs){
  #print(dir)
  
  files <-
    dir(path=dir, pattern="[^s].txt")
  
  for(file in files){
    #print(file)
    
    data[[file]][[dir]] <-
      read.table(paste(dir, file, sep='/'),
                 header=TRUE, row.names=1)
    
    ## The internal individual name is arbitrary, we use the dirname
    ## as an identifier
    rownames(data[[file]][[dir]]) <- c(dir)
  }
}

length(data)



## Plot it
for(name in names(data)){
  print(name)
  
  ## Give each individual values for each possible genotype
  all.names <-
    unique(unlist(lapply(data[[name]], colnames)))
  
  data[[name]] <-
    lapply(data[[name]], function(x){
      m <- setdiff(all.names, names(x))
      if(length(m))
        x[,m] <- 0
      x
    })

  ## Merge data
  fuck.me <- 
    do.call("rbind", data[[name]])
  
  ## Filter small values
  fuck.me.filt <-
    fuck.me[, apply(fuck.me, 2, max) > 0.1]
  
  ## Make pretty lables (ARRRRRRRRRRR!)
  x <-
    colnames(fuck.me.filt)
  
  x <-
    substr(x, 2, 10)
  
  ## HaRRRRrr
  n <- 8
  x <- paste(substr(x, 1, n-1), ":",
             substr(x, n, nchar(x)), sep = "")
  
  ## HaRRRRrr
  n <- 3
  x <- paste(substr(x, 1, n-1), ":",
             substr(x, n, nchar(x)), sep = "")
  
  ## HaRRRRrr
  x <-
    sub('_', '\n', x)

  y <- 
    paste('HLA', substr(name, 4, nchar(name)-4), sep="-")
  
  barplot(as.matrix(fuck.me.filt),
          main=y,
          beside=TRUE,
          ylab="Probability of the given HLA genotype",
          legend=TRUE, col=c(2:9)[1:length(dirs)],
          las=3,
          names=x,
          cex.names=2
          )
  
}
