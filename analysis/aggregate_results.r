fs <- list.files("results")
results <- list()
o <- list()

pb <- progress::progress_bar$new(format = "[:bar] :current :spin :total :eta", total = length(fs))
for (i in seq_along(fs)) {
  results[[i]] <- read.csv(file.path("results", fs[i]))
  mtime <- file.info(file.path("results", fs[i]))$mtime
  o[[i]] <- mtime
  pb$tick(1)
}

results <- do.call(rbind, results)

idx <- order(results[["idx_trial"]], results[["idx_condition"]])
results <- results[idx, ]

write.csv(results, "analysis/results.csv", row.names = FALSE)
dim(results)

o <- unlist(o)
o <- sort(o)

bench <- data.frame(
  time = o,
  completed = 1:length(o)
)

m <- lm(time ~ completed, bench)
estimated_endtime <- predict(m, data.frame(completed = 1260 * 100))

plot(bench$time, bench$completed)
est <- as.POSIXct(estimated_endtime, origin = "1970-01-01")
est
est - Sys.time()
