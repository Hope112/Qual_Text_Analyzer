# One-click check. In RStudio, open this file and click "Source" (or run it as a
# background job with the project folder as working directory).
# Installs any missing packages, runs tests/check_app.R, and writes the results
# to tests/check_results.txt.

root <- getwd()
if (!file.exists(file.path(root, "FinaL_Qual_App", "app.R"))) {
  stop("Set the working directory to the project folder (the one containing FinaL_Qual_App/) first.")
}
log_path <- file.path(root, "tests", "check_results.txt")

need <- c("shiny", "shinydashboard", "shinycssloaders", "shinyjs", "waiter", "DT",
          "readr", "readxl", "haven", "dplyr", "tidyr", "stringr",
          "tidytext", "tm", "slam", "topicmodels",
          "ggplot2", "plotly", "wordcloud", "RColorBrewer")

options(repos = c(CRAN = "https://cloud.r-project.org"))
con <- file(log_path, open = "wt")
sink(con)
sink(con, type = "message")

status <- tryCatch({
  cat(R.version.string, "\n")
  missing <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  cat("Missing packages:", if (length(missing)) paste(missing, collapse = ", ") else "none", "\n\n")
  if (length(missing)) install.packages(missing)
  t0 <- Sys.time()
  source(file.path(root, "tests", "check_app.R"), local = new.env())
  cat(sprintf("\nCheck run time: %.1f seconds\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  "ALL CHECKS PASSED"
}, error = function(e) paste("STOPPED WITH ERROR:", conditionMessage(e)))

cat("\n", status, "\n", sep = "")
sink(type = "message")
sink()
close(con)
cat(readLines(log_path), sep = "\n")
