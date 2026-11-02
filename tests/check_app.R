# Automated checks for the Qualitative Text Analyzer.
# Run from the project folder:   Rscript tests/check_app.R
# (or in RStudio: source("tests/check_app.R"))
#
# Uses sample_data/sample_course_feedback.csv. Prints PASS/FAIL per check and
# stops with an error if anything fails.

suppressPackageStartupMessages(library(shiny))

find_root <- function() {
  for (d in c(".", "..")) if (file.exists(file.path(d, "FinaL_Qual_App", "app.R"))) return(normalizePath(d))
  stop("Run this script from the project folder (the one containing FinaL_Qual_App/).")
}
root <- find_root()
app <- new.env()
sys.source(file.path(root, "FinaL_Qual_App", "app.R"), envir = app)
if (exists("before_checks", mode = "function")) before_checks(app)   # test hook (not needed normally)

results <- character(0)
check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  results[[label]] <<- if (ok) "PASS" else "FAIL"
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", label))
}

sample_path <- file.path(root, "sample_data", "sample_course_feedback.csv")
df <- app$read_upload(sample_path, basename(sample_path))
res <- app$run_analysis(df, "comments", k = 4, min_docs = 2, seed = 1234)
cat(sprintf("Sample: %d rows, %d with text, %d in topic model, %d model terms\n\n",
            res$counts$rows, res$counts$with_text, res$counts$in_model, res$counts$vocab_model))

# --- Loading --------------------------------------------------------------------
check("Reads CSV; text columns detected", identical(app$text_columns(df), c("program", "comments")))
check("Default column guess is the long-text column", app$guess_text_column(df, app$text_columns(df)) == "comments")
check("Upper-case extension accepted", {
  tmp <- file.path(tempdir(), "UPPER.CSV"); file.copy(sample_path, tmp, overwrite = TRUE)
  nrow(app$read_upload(tmp, "UPPER.CSV")) == nrow(df)
})

# --- Fix 1: topic results line up with the original rows -----------------------
dt <- res$doc_topics
check("Topic model fitted", is.null(res$topic_error) && !is.null(res$beta))
check("Every modeled row_id is a row that has text",
      all(dt$row_id %in% res$docs$row_id[res$docs$has_text]))
check("DTM document names are the original row numbers",
      setequal(as.integer(rownames(res$dtm)), dt$row_id))
check("Rows without text get no topic (blank, NA, '123', spaces, '5/5')", {
  no_text <- which(!res$docs$has_text)
  length(no_text) == 5 && !any(no_text %in% dt$row_id)
})
check("Each modeled document's words appear in its own original text", {
  m <- as.matrix(res$dtm)
  all(vapply(seq_len(nrow(m)), function(i) {
    rid <- as.integer(rownames(m)[i])
    words <- colnames(m)[m[i, ] > 0]
    txt <- tolower(stringr::str_replace_all(df$comments[rid], "\u2019", "'"))
    all(vapply(words, function(w) grepl(w, txt, fixed = TRUE), logical(1)))
  }, logical(1)))
})
check("Topic shares sum to 1 for each document", {
  s <- tapply(res$gamma$gamma, res$gamma$row_id, sum)
  all(abs(s - 1) < 1e-6)
})
check("One dominant topic per document (no duplicate rows from ties)", !any(duplicated(dt$row_id)))
check("Default alpha 0.1 is recorded and used", identical(res$settings$alpha, 0.1) &&
        (is.na(res$alpha_used) || abs(res$alpha_used - 0.1) < 1e-8))
check("Short responses get clear topic assignments (median dominant share > 0.5)",
      stats::median(dt$dominant_share) > 0.5)
check("beta sums to 1 within each topic", {
  s <- tapply(res$beta$beta, res$beta$topic, sum)
  all(abs(s - 1) < 1e-6)
})

# --- Fix 5: one cleaning pipeline -------------------------------------------------
vocab <- unique(res$content$word)
check("Contractions removed as stopwords (no 'dont', 'didnt', 'wasnt', 'don't')",
      !any(c("dont", "didnt", "wasnt", "don't", "didn't", "wasn't") %in% vocab))
check("Possessives normalized ('instructor's' -> 'instructor')",
      !"instructor's" %in% vocab && "instructor" %in% vocab)
check("Topic-model vocabulary is a subset of the frequency vocabulary",
      all(colnames(res$dtm) %in% res$freq$word))

# --- Sentiment ------------------------------------------------------------------
bw <- res$sentiment$by_word
check("Sentiment counts words that are also stopwords ('great', 'helpful', 'useful')",
      all(c("great", "useful") %in% bw$word))
check("Words listed as both positive and negative in Bing are dropped",
      !any(c("envious", "enviously", "enviousness") %in% app$bing_lexicon()$word))
check("Document scores lie in [-1, 1]",
      all(res$sentiment$by_doc$score >= -1 & res$sentiment$by_doc$score <= 1, na.rm = TRUE))
neg <- app$run_analysis(df, "comments", k = 4, handle_negation = TRUE)
check("Negation option flips 'Not a good experience ...'", {
  rid <- which(df$comments == "Not a good experience with the lab equipment, but the TAs were helpful.")
  before <- res$sentiment$by_doc$score[res$sentiment$by_doc$row_id == rid]
  after  <- neg$sentiment$by_doc$score[neg$sentiment$by_doc$row_id == rid]
  before > after
})

# --- Topic interpretation helpers -------------------------------------------------
sc <- app$score_terms(res$beta, res$marginals, 0.6)
check("Relevance with lambda = 1 ranks terms by beta", {
  t1 <- app$top_terms(app$score_terms(res$beta, res$marginals, 1), 10)
  b1 <- res$beta %>% dplyr::group_by(topic) %>% dplyr::slice_max(beta, n = 10, with_ties = FALSE)
  all(sort(t1$term[t1$topic == 1]) == sort(b1$term[b1$topic == 1]))
})
check("Lift is finite and positive", all(is.finite(sc$lift) & sc$lift > 0))
check("Representative documents come from the requested topic", {
  rd <- app$representative_docs(res$gamma, res$docs, 2, 3)
  nrow(rd) == 3 && all(!is.na(rd$text))
})
check("UMass coherence is finite and <= 0", {
  top <- sc %>% dplyr::filter(topic == 1) %>% dplyr::arrange(dplyr::desc(beta)) %>% head(10)
  u <- app$umass_coherence(res$dtm, top$term)
  is.finite(u) && u <= 0
})

kd <- app$k_diagnostics(res$dtm, 2:4, seed = 1234)
check("k diagnostics return one row per k with finite values",
      nrow(kd) == 3 && all(is.finite(kd$heldout_perplexity)) && all(is.finite(kd$mean_umass_coherence)))

# --- Failure handling -------------------------------------------------------------
check("k >= number of documents keeps frequencies/sentiment and reports a topic error", {
  r <- app$run_analysis(df, "comments", k = 500)
  !is.null(r$topic_error) && nrow(r$freq) > 0 && is.null(r$beta)
})
check("Numeric column gives a clear error", {
  e <- tryCatch(app$run_analysis(df, "rating", k = 2), error = function(e) conditionMessage(e))
  is.character(e) && grepl("fewer than 3 rows", e)
})

# --- Plots and files ---------------------------------------------------------------
check("Topic plot builds", inherits(ggplot2::ggplot_build(app$make_topic_plot(app$top_terms(sc, 10))), "ggplot_built"))
check("Sentiment plot builds", inherits(ggplot2::ggplot_build(app$make_sentiment_plot(res$sentiment$by_doc)), "ggplot_built"))
check("Plots convert to interactive plotly", {
  p <- plotly::ggplotly(app$make_topic_plot(app$top_terms(sc, 10)), tooltip = "text")
  inherits(p, "plotly")
})

# --- User interface ----------------------------------------------------------------
ui_html <- as.character(app$ui)
check("UI has a How to Use tab", grepl("How to Use", ui_html, fixed = TRUE) && grepl('data-value="howto"', ui_html, fixed = TRUE))
check("Settings have hover help", grepl("help-tip", ui_html, fixed = TRUE))
check("How to cite section and version shown", grepl("How to cite", ui_html, fixed = TRUE) &&
        grepl(paste("Version", app$APP_VERSION), ui_html, fixed = TRUE))
check("Summary download records version and citation", {
  sl <- app$summary_lines(res, "x.csv")
  any(grepl(paste("App version:", app$APP_VERSION), sl, fixed = TRUE)) && any(grepl("Adegoke, H., & Anyaibe, O. (2026)", sl, fixed = TRUE))
})
check("Citation uses the Zenodo DOI", grepl(paste0("https://doi.org/", app$APP_DOI), ui_html, fixed = TRUE) &&
        any(grepl(app$APP_DOI, app$summary_lines(res, "x.csv"), fixed = TRUE)))
check("The app does not call tidytext::tidy() (it needs reshape2, which deployment misses)",
      !any(grepl("tidy\\(", sub("#.*$", "", readLines(file.path(root, "FinaL_Qual_App", "app.R"))))))
check("Topic tables match tidytext::tidy() on the fitted model", {
  if (!methods::is(res$model, "LDA") || !requireNamespace("reshape2", quietly = TRUE)) TRUE else {
    b <- dplyr::inner_join(tidytext::tidy(res$model, matrix = "beta"), res$beta, by = c("topic", "term"))
    g <- tidytext::tidy(res$model, matrix = "gamma") %>% dplyr::mutate(row_id = as.integer(document)) %>%
      dplyr::inner_join(res$gamma, by = c("row_id", "topic"))
    nrow(b) == nrow(res$beta) && max(abs(b$beta.x - b$beta.y)) < 1e-10 &&
      nrow(g) == nrow(res$gamma) && max(abs(g$gamma.x - g$gamma.y)) < 1e-10
  }
})
check("CITATION.cff matches the app version", {
  cff <- readLines(file.path(root, "CITATION.cff"))
  any(grepl(paste0("^version: ", app$APP_VERSION, "$"), cff))
})
check("Example data is bundled with the app", !is.na(app$EXAMPLE_PATH) && file.exists(app$EXAMPLE_PATH))

# --- Shiny server: example data ---------------------------------------------------------
testServer(app$server, {
  msg <- tryCatch({ output$dataSummary; "" }, error = function(e) conditionMessage(e))
  check("Server: tabs show guidance before any analysis", grepl("no results yet", msg))
  session$setInputs(loadExample = 1)
  check("Server: 'Load example data' loads 500 rows", !is.null(rv$data) && nrow(rv$data) == 500 && isTRUE(rv$is_example))
  session$setInputs(textColumn = "comments", customStopwords = "", negation = FALSE, minDocs = 3,
                    numTopics = 6, ldaMethod = "VEM", seed = 1234, alpha = 0.1, estimateAlpha = FALSE,
                    topN = 20, minFreq = 2, maxWords = 100, lambda = 0.6, nTerms = 10, repN = 5)
  session$setInputs(analyze = 1)
  check("Server: example data analyzes with 6 topics", !is.null(rv$results$beta) && rv$results$settings$k == 6)
  boxes <- as.character(output$summaryBoxes$html)
  check("Server: summary boxes report counts", grepl("responses analyzed", boxes) && grepl("Mixed", boxes))
})

# --- Shiny server: Excel sheet picker -----------------------------------------------------
xlsx_path <- file.path(root, "sample_data", "course_eval_survey.xlsx")
if (file.exists(xlsx_path)) testServer(app$server, {
  session$setInputs(file = data.frame(name = "course_eval_survey.xlsx", size = file.size(xlsx_path),
                                      type = "", datapath = xlsx_path))
  check("Server: Excel file loads its first sheet", !is.null(rv$data) && nrow(rv$data) == 500)
  session$setInputs(sheet = "Codebook")
  check("Server: choosing another sheet reloads the data", !is.null(rv$data) && nrow(rv$data) == 6)
})

# --- Shiny server: upload -> analyze -> outputs -> downloads -------------------------
testServer(app$server, {
  session$setInputs(file = data.frame(name = "sample_course_feedback.csv", size = file.size(sample_path),
                                      type = "text/csv", datapath = sample_path))
  check("Server: file loads", !is.null(rv$data) && nrow(rv$data) == 60)
  session$setInputs(textColumn = "comments", customStopwords = "course, class", negation = FALSE,
                    minDocs = 2, numTopics = 4, ldaMethod = "VEM", seed = 1234,
                    alpha = 0.1, estimateAlpha = FALSE,
                    topN = 20, minFreq = 2, maxWords = 100, lambda = 0.6, nTerms = 10, repN = 5)
  session$setInputs(analyze = 1)
  check("Server: analysis runs and the spinner is released", !is.null(rv$results))
  check("Server: custom stopwords applied", !any(c("course", "class") %in% rv$results$freq$word))
  check("Server: summary renders", grepl("Rows excluded", output$dataSummary))
  check("Server: summary records alpha", grepl("alpha: 0.1", output$dataSummary))

  dl <- function(id) {
    path <- output[[id]]
    is.character(path) && file.exists(path) && file.size(path) > 0
  }
  check("Server: summary TXT download", dl("downloadSummary"))
  check("Server: word frequency CSV download", dl("downloadWordFreq"))
  check("Server: word cloud PNG download", dl("downloadWordCloud"))
  check("Server: sentiment plot PNG download", dl("downloadSentimentPlot"))
  check("Server: topic plot PNG download", dl("downloadTopicPlot"))
  check("Server: topic terms CSV download", dl("downloadTopicData"))
  check("Server: document-topic CSV keeps every original row, aligned", {
    out <- readr::read_csv(output$downloadDocTopicData, show_col_types = FALSE)
    nrow(out) == 60 && sum(out$in_topic_model) == nrow(rv$results$dtm) &&
      identical(out$comments, df$comments)
  })
  check("Server: sentiment CSV keeps every original row", {
    out <- readr::read_csv(output$downloadSentimentDocs, show_col_types = FALSE)
    nrow(out) == 60 && "sentiment_score" %in% names(out)
  })

  session$setInputs(kMin = 2, kMax = 4, kStep = 1)
  session$setInputs(runK = 1)
  check("Server: k diagnostics run", !is.null(rv$kdiag) && nrow(rv$kdiag) == 3)
  check("Server: k diagnostics CSV download", dl("downloadK"))

  # Uploading a new file clears old results
  session$setInputs(file = data.frame(name = "again.csv", size = file.size(sample_path),
                                      type = "text/csv", datapath = sample_path))
  check("Server: new upload clears previous results", is.null(rv$results) && is.null(rv$kdiag))
})

cat(sprintf("\n%d checks: %d passed, %d failed\n", length(results),
            sum(results == "PASS"), sum(results == "FAIL")))
if (any(results == "FAIL")) stop("Some checks failed: ", paste(names(results)[results == "FAIL"], collapse = "; "))
