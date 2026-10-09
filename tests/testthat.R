library(testthat)
library(sas2r)
results <- test_check("sas2r")

# Keep remote check logs useful when server timings differ from local runs.
timings <- aggregate(real ~ file, as.data.frame(results), sum)
timings <- timings[order(timings$real, decreasing = TRUE), ]
cat("\nSlowest test files (elapsed seconds):\n")
print(utils::head(timings, 10L), row.names = FALSE)
