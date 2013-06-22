
if(!interactive()){
  postscript(file="test.ps", paper='a4', horizontal=TRUE)
}

## List of directories containing unzipped HLAIMP results
dirs <- c("Dan", "Nao")
dirs <- c("Ash", "Dan", "Mum", "Dad")

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
for(n in names(data)){
  print(n)

  ## Give each individual values for each possible genotype
  all.names <-
    unique(unlist(lapply(data[[n]], colnames)))
  
  data[[n]] <-
    lapply(data[[n]], function(x){
      m <- setdiff(all.names, names(x))
      if(length(m))
        x[,m] <- 0
      x
    })

  ## Merge data
  fuck.me <- 
    do.call("rbind", data[[n]])

  
  ## Make pretty lables
  all.names <-
    substr(all.names, 2, 10)
  all.names <-
    sub('_', '\n', all.names)

  main <-
    substring(n, 4,
              char(n)
              length(n)
              )
  
  barplot(as.matrix(fuck.me),
          main=n,
          beside=TRUE,
          legend=TRUE, col=c(2:9)[1:length(dirs)],
          las=3,
          names=all.names)
  
}
