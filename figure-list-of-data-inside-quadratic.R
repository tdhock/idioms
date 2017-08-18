works_with_R("3.4.1", data.table="1.10.5", ggplot2="2.1.0", microbenchmark="1.4.2.1")

## system("ln -s ~/projects/chip-seq-paper/chunks list.of.data")
library(data.table)
pre <- "http://members.cbio.mines-paristech.fr/~thocking/chip-seq-chunk-db/"
files.df <- read.table(paste0(pre, "bedGraph.count.signal.txt"), header=TRUE)
list.of.data <- list()
list.of.times <- list()
N <- 100
for(i in 1:N){
  f <- files.df[i, "file"]
  path <- file.path("list.of.data", f)
  if(!file.exists(path)){
    u <- paste0(pre, f)
    dir.create(dirname(path), showWarnings=FALSE, recursive=TRUE)
    download.file(u, path)
  }
  cat(sprintf("%4d / %4d %s\n", i, nrow(files.df), f))
  counts.dt <- fread(paste("zcat", path))
  chunk.id <- dirname(dirname(paste(f)))
  sample.id <- sub(".bedGraph.gz", "", basename(paste(f)), fixed=TRUE)
  list.of.data[[i]] <- data.table(sample.id, chunk.id, counts.dt)
}

system.time({
  inside.dt <- NULL
  for(i in 1:N){
    inside.dt <- rbind(inside.dt, list.of.data[[i]])
  }
})

system.time({
  outside.dt.list <- list()
  for(i in 1:N){
    outside.dt.list[[i]] <- list.of.data[[i]]
  }
  outside.dt <- do.call(rbind, outside.dt.list)
})

identical(inside.dt, outside.dt)

library(microbenchmark)
timing.list <- list()
last.element.vec <- seq(10, N, by=10)
for(last.element in last.element.vec){
  print(last.element)
  time.df <- microbenchmark(inside={
    inside.dt <- NULL
    for(i in 1:last.element){
      inside.dt <- rbind(inside.dt, list.of.data[[i]])
    }
  }, outside={
    outside.dt.list <- list()
    for(i in 1:N){
      outside.dt.list[[i]] <- list.of.data[[i]]
    }
    outside.dt <- do.call(rbind, outside.dt.list)
  }, times=3)
  timing.list[[paste(last.element)]] <- data.table(last.element, time.df)
}

timing <- do.call(rbind, timing.list)
timing[, seconds := time/1e9]
library(ggplot2)
with.legend <- ggplot()+
  geom_point(aes(
    last.element, seconds, color=expr),
    shape=1,
    data=timing)+
  scale_x_continuous(breaks=last.element.vec, limits=c(10, 110))
library(directlabels)
(with.labels <- direct.label(with.legend, list("last.points", dl.trans(x=x+0.2))))
png("figure-list-of-data-inside-quadratic.png")
print(with.labels)
dev.off()
