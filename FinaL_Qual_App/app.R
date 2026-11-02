# =============================================================================
# Qualitative Text Analyzer
# Word frequencies, lexicon sentiment, and LDA topic modeling for open-ended
# text (survey responses, interview excerpts, comments).
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinycssloaders)
  library(shinyjs)
  library(waiter)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tidytext)
  library(ggplot2)
  library(plotly)
  library(wordcloud)   # also attaches RColorBrewer
})
# Used with :: only (must be installed): readr, readxl, haven, tm, slam, topicmodels

options(shiny.maxRequestSize = 50 * 1024^2)   # allow uploads up to 50 MB

COLORS <- list(
  primary  = "#0dc5c1",
  positive = "#4CAF50",
  negative = "#F44336",
  balanced = "#FFB300",
  none     = "#9E9E9E"
)

SENTIMENT_LEVELS <- c("Positive", "Negative", "Balanced", "No lexicon words")

NEGATORS <- c("not", "no", "never", "without", "nor", "cannot", "hardly",
              "barely", "neither", "nobody", "nothing", "none")

`%||%` <- function(a, b) if (is.null(a)) b else a

APP_VERSION <- "2.0.1"
APP_DOI <- "10.5281/zenodo.23891341"   # Zenodo DOI for all versions (resolves to the latest release)
APP_CITATION <- paste0("Adegoke, H., & Anyaibe, O. (2026). Qualitative Text Analyzer (Version ", APP_VERSION,
                       ") [Computer software]. Zenodo. https://doi.org/", APP_DOI)

# -----------------------------------------------------------------------------
# Data loading
# -----------------------------------------------------------------------------

read_upload <- function(path, name, sheet = NULL) {
  ext <- tolower(tools::file_ext(name))
  df <- switch(
    ext,
    csv = {
      enc <- tryCatch(readr::guess_encoding(path)$encoding[1], error = function(e) NA)
      if (is.na(enc) || !nzchar(enc) || enc == "ASCII") enc <- "UTF-8"
      readr::read_csv(path, locale = readr::locale(encoding = enc),
                      show_col_types = FALSE, progress = FALSE)
    },
    xlsx = ,
    xls = readxl::read_excel(path, sheet = sheet %||% 1),
    sav = haven::as_factor(haven::read_sav(path), only_labelled = TRUE),
    stop("Unsupported file type '.", ext, "'. Use .csv, .xlsx, .xls or .sav.", call. = FALSE)
  )
  df <- as_tibble(df, .name_repair = "unique")
  # "row_id" is used internally to link results back to the original rows
  if ("row_id" %in% names(df)) names(df)[names(df) == "row_id"] <- "row_id_original"
  df
}

text_columns <- function(df) {
  names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
}

# Pick the text-like column with the longest average entry as the default
guess_text_column <- function(df, cols) {
  if (length(cols) <= 1) return(cols[1])
  avg_len <- vapply(cols, function(cl) mean(nchar(as.character(df[[cl]])), na.rm = TRUE), numeric(1))
  avg_len[is.nan(avg_len)] <- 0
  cols[which.max(avg_len)]
}

# -----------------------------------------------------------------------------
# Text processing (one pipeline shared by every tab)
# -----------------------------------------------------------------------------

prepare_docs <- function(df, text_col) {
  txt <- as.character(df[[text_col]])
  tibble(
    row_id = seq_len(nrow(df)),
    text   = str_replace_all(txt, "[\u2018\u2019\u02BC`]", "'")   # curly -> straight apostrophes
  ) %>%
    mutate(has_text = !is.na(text) & str_detect(text, "[[:alpha:]]"))
}

# All word tokens, in order, before stopword removal.
# Lower-cases and strips punctuation (unnest_tokens), keeps contractions intact
# so they match the stopword list, drops possessive 's, numbers, single letters.
tokenize_docs <- function(docs) {
  docs %>%
    filter(has_text) %>%
    select(row_id, text) %>%
    unnest_tokens(word, text) %>%
    mutate(word = str_remove(word, "'s$")) %>%
    filter(str_detect(word, "[[:alpha:]]"),
           !str_detect(word, "[[:digit:]]"),
           nchar(word) > 1) %>%
    group_by(row_id) %>%
    mutate(position = row_number()) %>%
    ungroup()
}

build_stopwords <- function(custom = "") {
  extra <- str_split(str_to_lower(custom %||% ""), "[,;\n]+")[[1]]
  extra <- str_trim(str_replace_all(extra, "[\u2018\u2019]", "'"))
  unique(c(tidytext::stop_words$word, extra[nzchar(extra)]))
}

word_frequencies <- function(content) {
  content %>%
    group_by(word) %>%
    summarise(n = n(), documents = n_distinct(row_id), .groups = "drop") %>%
    arrange(desc(n), word) %>%
    mutate(percent = 100 * n / sum(n))
}

# -----------------------------------------------------------------------------
# Sentiment (Bing lexicon)
# -----------------------------------------------------------------------------

# Bing lists a few words as both positive and negative (e.g., "envious");
# those are dropped so no word is counted twice.
bing_lexicon <- function() {
  tidytext::get_sentiments("bing") %>%
    distinct() %>%
    group_by(word) %>%
    filter(n_distinct(sentiment) == 1) %>%
    ungroup()
}

is_negator <- function(w) !is.na(w) & (w %in% NEGATORS | str_detect(w, "n't$"))

# Matches lexicon words on the full token stream (before stopword removal),
# because many common sentiment words ("good", "great", "problem") are stopwords.
score_sentiment <- function(tokens, docs, handle_negation = FALSE, lexicon = bing_lexicon()) {
  matched <- tokens %>%
    arrange(row_id, position) %>%
    group_by(row_id) %>%
    mutate(prev1 = lag(word, 1), prev2 = lag(word, 2)) %>%
    ungroup() %>%
    inner_join(lexicon, by = "word") %>%
    mutate(
      negated  = handle_negation & (is_negator(prev1) | is_negator(prev2)),
      polarity = if_else(sentiment == "positive", 1L, -1L) * if_else(negated, -1L, 1L)
    )

  per_doc <- matched %>%
    group_by(row_id) %>%
    summarise(positive = sum(polarity > 0), negative = sum(polarity < 0), .groups = "drop")

  by_doc <- docs %>%
    filter(has_text) %>%
    select(row_id, text) %>%
    left_join(per_doc, by = "row_id") %>%
    mutate(
      positive = coalesce(positive, 0L),
      negative = coalesce(negative, 0L),
      score    = if_else(positive + negative > 0,
                         (positive - negative) / (positive + negative), NA_real_),
      category = factor(case_when(
        is.na(score) ~ "No lexicon words",
        score > 0    ~ "Positive",
        score < 0    ~ "Negative",
        TRUE         ~ "Balanced"
      ), levels = SENTIMENT_LEVELS)
    )

  by_word <- matched %>%
    group_by(word, lexicon_sentiment = sentiment) %>%
    summarise(occurrences = n(),
              times_negated = sum(negated),
              net_contribution = sum(polarity),
              .groups = "drop") %>%
    arrange(desc(occurrences), word)

  list(
    by_doc  = by_doc,
    by_word = by_word,
    coverage = list(
      matched_tokens = nrow(matched),
      total_tokens   = nrow(tokens),
      docs_with      = sum(!is.na(by_doc$score)),
      docs           = nrow(by_doc)
    )
  )
}

# -----------------------------------------------------------------------------
# Topic modeling (LDA)
# -----------------------------------------------------------------------------

# Document names in the DTM are the original row_id values, so model output
# always links back to the right row.
build_dtm <- function(content, min_docs = 2) {
  counts <- content %>% count(row_id, word, name = "n")
  keep <- counts %>%
    count(word, name = "doc_freq") %>%
    filter(doc_freq >= min_docs) %>%
    pull(word)
  counts <- counts %>% filter(word %in% keep)
  n_docs <- n_distinct(counts$row_id)
  if (length(keep) < 2 || n_docs < 3) {
    stop(sprintf(paste("Too little text left for topic modeling (%d documents, %d terms",
                       "after dropping terms used in fewer than %d documents).",
                       "Lower the minimum document count or check the selected column."),
                 n_docs, length(keep), min_docs), call. = FALSE)
  }
  tidytext::cast_dtm(counts, row_id, word, n)
}

# alpha = document-topic prior. NA means "estimate from the data" (VEM) or the
# package default 50/k (Gibbs); both tend to spread short responses evenly
# across all topics, so a small fixed value (0.1) is the default.
fit_lda <- function(dtm, k, method = "VEM", seed = 1234, alpha = 0.1) {
  control <- if (method == "Gibbs") {
    list(seed = seed, burnin = 500, iter = 2000)
  } else {
    list(seed = seed)
  }
  if (!is.na(alpha)) {
    control$alpha <- alpha
    if (method == "VEM") control$estimate.alpha <- FALSE
  }
  topicmodels::LDA(dtm, k = k, method = method, control = control)
}

# Reads the topic-term (beta) and document-topic (gamma) tables straight from the
# fitted model. topicmodels stores beta on the log scale for both VEM and Gibbs.
# (tidytext::tidy() would do the same but needs reshape2, which deployment tools
# don't detect because the app never names it.)
topic_tables <- function(model) {
  beta  <- exp(model@beta)          # k x V
  terms <- model@terms
  k     <- nrow(beta)
  gamma <- model@gamma              # D x k
  docs  <- model@documents
  list(
    beta  = tibble(topic = rep(seq_len(k), times = length(terms)),
                   term  = rep(terms, each = k),
                   beta  = as.vector(beta)),
    gamma = tibble(row_id = as.integer(rep(docs, times = k)),
                   topic  = rep(seq_len(k), each = length(docs)),
                   gamma  = as.vector(gamma))
  )
}

term_marginals <- function(dtm) {
  tf <- slam::col_sums(dtm)
  tibble(term = names(tf), p_w = as.numeric(tf) / sum(tf))
}

# Relevance (Sievert & Shirley, 2014): lambda*log(beta) + (1 - lambda)*log(lift),
# where lift = beta / p(term). lambda = 1 ranks by beta alone.
score_terms <- function(beta, marginals, lambda = 0.6) {
  beta %>%
    left_join(marginals, by = "term") %>%
    mutate(lift = beta / p_w,
           relevance = lambda * log(beta) + (1 - lambda) * log(lift))
}

top_terms <- function(scored, n = 10) {
  scored %>%
    group_by(topic) %>%
    arrange(desc(relevance), desc(beta), .by_group = TRUE) %>%
    slice_head(n = n) %>%
    mutate(rank = row_number()) %>%
    ungroup()
}

# gamma = estimated share of each document attributed to each topic.
# "Mixed" flags documents whose top two shares are within `mixed_gap`.
assign_topics <- function(gamma, mixed_gap = 0.10) {
  dominant <- gamma %>%
    group_by(row_id) %>%
    arrange(desc(gamma), topic, .by_group = TRUE) %>%
    summarise(dominant_topic  = first(topic),
              dominant_share  = first(gamma),
              runner_up_share = nth(gamma, 2, default = 0),
              .groups = "drop") %>%
    mutate(assignment = if_else(dominant_share - runner_up_share < mixed_gap, "Mixed", "Clear"))
  wide <- gamma %>%
    arrange(topic) %>%
    mutate(topic = paste0("topic_", topic)) %>%
    pivot_wider(names_from = topic, values_from = gamma)
  dominant %>% left_join(wide, by = "row_id")
}

representative_docs <- function(gamma, docs, topic_id, n = 5) {
  gamma %>%
    filter(topic == topic_id) %>%
    slice_max(gamma, n = n, with_ties = FALSE) %>%
    left_join(docs %>% select(row_id, text), by = "row_id") %>%
    select(row_id, gamma, text)
}

# UMass coherence (Mimno et al., 2011) for one topic's top terms, ordered by rank.
umass_coherence <- function(dtm, terms) {
  terms <- intersect(terms, colnames(dtm))
  if (length(terms) < 2) return(NA_real_)
  B  <- as.matrix(dtm[, terms]) > 0
  d  <- colSums(B)
  co <- crossprod(B)
  s <- 0
  for (m in 2:length(terms)) {
    for (l in seq_len(m - 1)) s <- s + log((co[m, l] + 1) / d[l])
  }
  s
}

# Fits VEM models over a grid of k. Held-out perplexity uses a random 20% of
# documents (when there are at least 20); coherence uses the full corpus.
k_diagnostics <- function(dtm, ks, seed = 1234, n_top = 10, holdout = 0.2, on_progress = NULL,
                          alpha = 0.1) {
  set.seed(seed)
  n <- nrow(dtm)
  use_holdout <- n >= 20
  train <- dtm
  test <- NULL
  if (use_holdout) {
    test_idx <- sample.int(n, max(2, round(n * holdout)))
    train <- dtm[-test_idx, ]
    keep  <- slam::col_sums(train) > 0
    train <- train[, keep]
    test  <- dtm[test_idx, keep]
    test  <- test[slam::row_sums(test) > 0, ]
  }
  out <- vector("list", length(ks))
  for (i in seq_along(ks)) {
    k <- ks[i]
    if (!is.null(on_progress)) on_progress(i, k)
    m <- fit_lda(train, k, "VEM", seed, alpha)
    perp <- if (!is.null(test) && nrow(test) > 0) {
      topicmodels::perplexity(m, newdata = test)
    } else {
      NA_real_
    }
    coh <- topic_tables(m)$beta %>%
      group_by(topic) %>%
      arrange(desc(beta), .by_group = TRUE) %>%
      slice_head(n = n_top) %>%
      summarise(c = umass_coherence(dtm, term), .groups = "drop")
    out[[i]] <- tibble(k = k,
                       heldout_perplexity = perp,
                       mean_umass_coherence = mean(coh$c, na.rm = TRUE))
  }
  bind_rows(out)
}

# -----------------------------------------------------------------------------
# Full analysis (pure function: no Shiny state)
# -----------------------------------------------------------------------------

run_analysis <- function(df, text_col, custom_stopwords = "", handle_negation = FALSE,
                         min_docs = 2, k = 5, method = "VEM", seed = 1234, alpha = 0.1) {
  if (!text_col %in% names(df)) stop("Column not found: ", text_col, call. = FALSE)
  k        <- suppressWarnings(as.integer(k))
  min_docs <- suppressWarnings(as.integer(min_docs))
  seed     <- suppressWarnings(as.integer(seed))
  if (is.na(k) || k < 2) stop("Number of topics must be a whole number of at least 2.", call. = FALSE)
  if (is.na(min_docs) || min_docs < 1) min_docs <- 1L
  if (is.na(seed)) seed <- 1234L
  method <- if (identical(method, "Gibbs")) "Gibbs" else "VEM"
  alpha  <- suppressWarnings(as.numeric(alpha))
  if (length(alpha) != 1 || (!is.na(alpha) && alpha <= 0)) alpha <- 0.1

  docs <- prepare_docs(df, text_col)
  if (sum(docs$has_text) < 3) {
    stop("The selected column has fewer than 3 rows containing text.", call. = FALSE)
  }
  stops   <- build_stopwords(custom_stopwords)
  tokens  <- tokenize_docs(docs)
  content <- tokens %>% filter(!word %in% stops)
  if (nrow(content) == 0) stop("No words are left after removing stopwords.", call. = FALSE)

  res <- list(
    docs      = docs,
    tokens    = tokens,
    content   = content,
    freq      = word_frequencies(content),
    sentiment = score_sentiment(tokens, docs, handle_negation),
    settings  = list(text_col = text_col, k = k, method = method, seed = seed, alpha = alpha,
                     min_docs = min_docs, custom_stopwords = custom_stopwords %||% "",
                     handle_negation = handle_negation, n_stopwords = length(stops)),
    counts    = list(rows = nrow(df),
                     with_text = sum(docs$has_text),
                     with_tokens = n_distinct(content$row_id),
                     tokens_all = nrow(tokens),
                     tokens_content = nrow(content),
                     vocab_content = n_distinct(content$word),
                     in_model = NA_integer_,
                     vocab_model = NA_integer_)
  )

  # Topic model: a failure here keeps the frequency and sentiment results
  topic_part <- tryCatch({
    dtm <- build_dtm(content, min_docs)
    if (k >= nrow(dtm)) {
      stop(sprintf("Number of topics (%d) must be smaller than the number of documents in the model (%d).",
                   k, nrow(dtm)), call. = FALSE)
    }
    model <- fit_lda(dtm, k, method, seed, alpha)
    tt <- topic_tables(model)
    alpha_used <- tryCatch(as.numeric(model@alpha), error = function(e) NA_real_)
    list(dtm = dtm, model = model, beta = tt$beta, gamma = tt$gamma, alpha_used = alpha_used,
         marginals = term_marginals(dtm), doc_topics = assign_topics(tt$gamma))
  }, error = function(e) list(topic_error = conditionMessage(e)))

  res <- c(res, topic_part)
  if (!is.null(res$dtm)) {
    res$counts$in_model    <- nrow(res$dtm)
    res$counts$vocab_model <- ncol(res$dtm)
  }
  res
}

summary_lines <- function(res, file_name = "") {
  s <- res$settings
  cn <- res$counts
  cov <- res$sentiment$coverage
  c(
    "Qualitative Text Analyzer - analysis summary",
    "============================================",
    sprintf("App version: %s", APP_VERSION),
    sprintf("File: %s", file_name %||% ""),
    sprintf("Text column: %s", s$text_col),
    sprintf("Run at: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "",
    "Documents",
    sprintf("  Rows in file:                          %d", cn$rows),
    sprintf("  Rows with text:                        %d", cn$with_text),
    sprintf("  Rows excluded (missing/no letters):    %d", cn$rows - cn$with_text),
    sprintf("  Rows with words after stopwords:       %d", cn$with_tokens),
    sprintf("  Rows in topic model:                   %s", ifelse(is.na(cn$in_model), "not fitted", cn$in_model)),
    "",
    "Words",
    sprintf("  Tokens (all words):                    %d", cn$tokens_all),
    sprintf("  Tokens after stopword removal:         %d", cn$tokens_content),
    sprintf("  Distinct words after stopword removal: %d", cn$vocab_content),
    sprintf("  Terms in topic model vocabulary:       %s", ifelse(is.na(cn$vocab_model), "not fitted", cn$vocab_model)),
    "",
    "Sentiment (Bing lexicon)",
    sprintf("  Tokens matched to the lexicon:         %d of %d (%.1f%%)",
            cov$matched_tokens, cov$total_tokens,
            100 * cov$matched_tokens / max(1, cov$total_tokens)),
    sprintf("  Rows with at least one lexicon word:   %d of %d", cov$docs_with, cov$docs),
    sprintf("  Negation handling:                     %s", ifelse(isTRUE(s$handle_negation), "on", "off")),
    "",
    "Settings",
    sprintf("  Stopwords: tidytext stop_words + %d custom (%d total)",
            s$n_stopwords - length(unique(tidytext::stop_words$word)), s$n_stopwords),
    sprintf("  Custom stopwords: %s", ifelse(nzchar(s$custom_stopwords), s$custom_stopwords, "(none)")),
    sprintf("  Topic model: LDA, k = %d, estimation = %s, seed = %d", s$k, s$method, s$seed),
    sprintf("  Topic prior alpha: %s%s",
            if (is.na(s$alpha)) "estimated / package default" else format(s$alpha),
            if (!is.null(res$alpha_used) && !is.na(res$alpha_used)) sprintf(" (value used: %.3g)", res$alpha_used) else ""),
    sprintf("  Minimum documents per term: %d", s$min_docs),
    if (!is.null(res$topic_error)) sprintf("  Topic model not fitted: %s", res$topic_error),
    "",
    "How to cite",
    paste0("  ", APP_CITATION),
    "  See the How to Use tab for the method references."
  )
}

# -----------------------------------------------------------------------------
# Plot and table helpers
# -----------------------------------------------------------------------------

make_topic_plot <- function(top) {
  top %>%
    mutate(topic = factor(paste("Topic", topic), levels = paste("Topic", sort(unique(topic)))),
           term  = reorder_within(term, relevance, topic)) %>%
    ggplot(aes(x = term, y = beta, fill = topic,
               text = sprintf("%s<br>beta = %.4f<br>lift = %.2f", str_remove(term, "___.*$"), beta, lift))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~topic, scales = "free_y", ncol = 3) +
    coord_flip() +
    scale_x_reordered() +
    theme_minimal(base_size = 12) +
    labs(x = NULL, y = "beta = P(term | topic)",
         subtitle = "Terms ordered by relevance; bar length shows beta") +
    theme(strip.text = element_text(face = "bold"))
}

make_sentiment_plot <- function(by_doc) {
  counts <- by_doc %>%
    count(category, .drop = FALSE) %>%
    mutate(percent = 100 * n / sum(n))
  ggplot(counts, aes(x = category, y = n, fill = category,
                     text = sprintf("%s<br>%d documents (%.1f%%)", category, n, percent))) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = c("Positive" = COLORS$positive, "Negative" = COLORS$negative,
                                 "Balanced" = COLORS$balanced, "No lexicon words" = COLORS$none)) +
    theme_minimal(base_size = 12) +
    labs(x = NULL, y = "Number of documents",
         title = "Documents by net sentiment")
}

make_k_plot <- function(kd) {
  kd %>%
    pivot_longer(c(heldout_perplexity, mean_umass_coherence), names_to = "metric", values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(metric = recode(metric,
                           heldout_perplexity = "Held-out perplexity (lower is better)",
                           mean_umass_coherence = "Mean UMass coherence (higher is better)")) %>%
    ggplot(aes(k, value)) +
    geom_line(color = COLORS$primary, linewidth = 1) +
    geom_point(size = 2.5) +
    facet_wrap(~metric, scales = "free_y") +
    scale_x_continuous(breaks = kd$k) +
    theme_minimal(base_size = 13) +
    labs(x = "Number of topics (k)", y = NULL)
}

draw_cloud <- function(freq, min_freq, max_words, seed) {
  words <- freq %>% filter(n >= min_freq)
  if (nrow(words) == 0) {
    plot.new()
    text(0.5, 0.5, "No words meet the minimum frequency.")
    return(invisible())
  }
  set.seed(seed)
  wordcloud(words = words$word, freq = words$n, min.freq = min_freq,
            max.words = max_words, random.order = FALSE, rot.per = 0.15,
            colors = brewer.pal(8, "Dark2"), scale = c(4, 0.5))
}

dt_table <- function(df, page_length = 10, buttons = TRUE, ...) {
  opts <- list(pageLength = page_length, scrollX = TRUE, ...)
  text_idx <- which(names(df) == "text") - 1   # DataTables columns are 0-indexed
  if (length(text_idx)) opts$columnDefs <- list(list(targets = text_idx, className = "text-col"))
  if (buttons) {
    opts$dom <- "Bfrtip"
    opts$buttons <- list("copy", "csv", "excel")
    datatable(df, rownames = FALSE, extensions = "Buttons", options = opts)
  } else {
    opts$dom <- opts$dom %||% "frtip"
    datatable(df, rownames = FALSE, options = opts)
  }
}

info_box <- function(...) div(class = "alert alert-info", ...)

# Label with a hover tooltip (question-mark icon)
with_help <- function(label, help) {
  tagList(label, " ", tags$span(class = "help-tip", title = help, icon("circle-question")))
}

MSG_NO_RESULTS <- paste("There are no results yet. Go to Upload & Settings, load the example data or your own",
                        "file, and click Analyze.")

# Bundled example data (500 synthetic course-evaluation responses)
EXAMPLE_FILE <- "course_eval_survey.csv"
example_data_path <- function() {
  candidates <- c(file.path("example_data", EXAMPLE_FILE),
                  file.path("FinaL_Qual_App", "example_data", EXAMPLE_FILE))
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) normalizePath(hit[1]) else NA_character_
}
EXAMPLE_PATH <- example_data_path()

# ---- How to Use content -------------------------------------------------------
howto_tab <- function() {
  step <- function(n, ...) {
    div(class = "howto-step",
        div(class = "howto-num", n),
        div(class = "howto-body", p(...)))
  }
  term <- function(name, ...) tagList(tags$dt(name), tags$dd(...))

  tagList(
    fluidRow(
      box(
        title = "Quick start", status = "primary", solidHeader = TRUE, width = 12,
        p("This app helps you explore open-ended text such as survey comments. It shows which words come up",
          "most, whether responses sound positive or negative, and which themes (topics) recur."),
        step(1, "Go to", tags$em("Upload & Settings"), "and upload a .csv, .xlsx, .xls or .sav file that has",
             "one response in each row. You can also click", tags$em("Load example data"), "to try the app first."),
        step(2, "Choose the column that holds the responses. The app suggests the column with the longest",
             "entries and shows a few examples so you can check it."),
        step(3, "Click Analyze. The default settings suit most survey data, and a few hundred responses take",
             "a few seconds."),
        step(4, HTML("Look through the tabs on the left, starting with <em>Data Summary</em>. Every table and",
                     "plot can be downloaded.")),
        div(style = "margin-top: 10px;",
            actionButton("goExample", "Try it with example data", icon = icon("flask"), class = "btn-primary"),
            actionButton("goUpload", "Upload my own file", icon = icon("upload")))
      )
    ),
    fluidRow(
      box(
        title = "Preparing your file", status = "info", solidHeader = TRUE, width = 6,
        tags$ul(
          tags$li("Put each response in its own row. Other columns, such as an ID, group or rating, are kept",
                  "and included in the downloads, so you can merge the results back into your data."),
          tags$li("Store the responses as text. Only text columns appear in the column list, so in SPSS the",
                  "variable needs to be a string."),
          tags$li("You can upload .csv files in any encoding, Excel files (.xlsx or .xls, where you choose the",
                  "sheet) and SPSS .sav files, up to 50 MB."),
          tags$li("Rows that are empty or contain only something like N/A or a number are left out of the",
                  "analysis, and the Data Summary tab shows how many there were."),
          tags$li("If the app is hosted on a shared server, remove names and other identifiers before uploading.")
        )
      ),
      box(
        title = "What each tab shows", status = "info", solidHeader = TRUE, width = 6,
        tags$table(class = "table table-condensed howto-table",
          tags$tbody(
            tags$tr(tags$td("Data Summary"), tags$td("This tab shows how many rows were analyzed or left out and lists every setting used. You can download it for your methods section.")),
            tags$tr(tags$td("Word Frequencies"), tags$td("This tab lists the most common words after stopwords are removed, with the number of responses that use each one.")),
            tags$tr(tags$td("Word Cloud"), tags$td("This tab shows the same counts as a picture, and you can change the minimum frequency and the number of words.")),
            tags$tr(tags$td("Sentiment Analysis"), tags$td("This tab gives each response a positive or negative score and lists the sentiment words it found.")),
            tags$tr(tags$td("Topic Analysis"), tags$td("This tab shows the words that define each topic and includes a tool for choosing the number of topics.")),
            tags$tr(tags$td("Document Topics"), tags$td("This tab shows each response's share of every topic, its main topic, and the most typical responses for each topic."))
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "Choosing settings", status = "info", solidHeader = TRUE, width = 6,
        p("Hover over the", icon("circle-question"), "icons on the Upload & Settings tab for help with each setting."),
        tags$ul(
          tags$li("For the number of topics, start with 4 to 8 when you have a few hundred short responses.",
                  "After a first run, use", tags$em("Choosing the number of topics"), "on the Topic Analysis tab",
                  "and rerun with the value you pick."),
          tags$li("Keeping only words used in at least 2 to 5 responses gives cleaner topics. Lower this number",
                  "if the app reports that the topic model has too little text."),
          tags$li("Add extra stopwords for words that appear in almost every response without saying anything",
                  "about themes, such as the course or program name."),
          tags$li("The advanced settings (VEM, an alpha of 0.1 and seed 1234) suit short responses. Changing the",
                  "seed and rerunning is a good way to check that the topics are stable.")
        )
      ),
      box(
        title = "Reading the results", status = "info", solidHeader = TRUE, width = 6,
        tags$dl(class = "howto-terms",
          term("Sentiment score", "Each response's score is its number of positive words minus negative words,",
               "divided by their total, using the Bing lexicon. It runs from -1 to +1, and responses with no",
               "lexicon words get no score."),
          term("beta", "Beta is how likely a word is within a topic. Compare it with 1/V, which the Topic Analysis",
               "tab shows, rather than with a fixed cutoff."),
          term("Lift and relevance (lambda)", "A lift above 1 means a word is more typical of the topic than of",
               "the data overall. Lowering lambda ranks topic-specific words higher, and 0.6 is a good place to start."),
          term("gamma", "Gamma is a response's estimated share of each topic. Because responses can mix topics,",
               "it isn't a confidence score."),
          term("Dominant topic and Mixed", "The dominant topic is the one with the largest share. A response is",
               "marked Mixed when its top two shares are within 0.10 of each other, which means it doesn't",
               "belong clearly to one topic."),
          term("Perplexity and coherence", "These help you compare values of k. Lower held-out perplexity and",
               "higher coherence are better, but they often disagree, so read the topics as well.")
        )
      )
    ),
    fluidRow(
      box(
        title = "Good practice", status = "info", solidHeader = TRUE, width = 6,
        tags$ul(
          tags$li("Treat topics as a starting point for reading and coding, not as finished themes.",
                  "Name a topic only after reading its representative responses."),
          tags$li("Topic models work best with many responses that are at least a sentence or two long.",
                  "With very short answers, expect rougher topics."),
          tags$li("The sentiment score comes from counting words, so it misses sarcasm and context. Read a",
                  "sample of scored responses before you report it."),
          tags$li("Download the summary on the Data Summary tab to record the exact settings. The same file,",
                  "settings and seed always reproduce the same results.")
        )
      ),
      box(
        title = "Troubleshooting", status = "warning", solidHeader = TRUE, width = 6,
        tags$dl(class = "howto-terms",
          term("\"No text columns found\"", "The responses are stored as numbers or codes. Save the column as",
               "text (a string variable) and upload the file again."),
          term("\"Topic model not fitted: too little text\"", "Lower \"Keep terms used in at least N documents\",",
               "remove some extra stopwords, or add more responses. The word and sentiment results are still available."),
          term("Almost every response is Mixed", "Lower alpha to something like 0.05, make sure the",
               "estimate-alpha box is unticked, or reduce the number of topics."),
          term("Topics are hard to interpret", "Add common filler words as stopwords, lower lambda, try another",
               "number of topics, and read the representative responses."),
          term("The file won't load", "Check that it's under 50 MB. For Excel files, pick the right sheet, and",
               "for CSV files, try saving the file again as \"CSV UTF-8\"."),
          term("The app is slow", "Gibbs sampling, a large number of topics and the k diagnostics all take longer.",
               "VEM with fewer than 10 topics is the quickest.")
        )
      )
    ),
    fluidRow(
      box(
        title = "How to cite", status = "primary", solidHeader = TRUE, width = 12,
        p("Please cite the app in APA 7 style as follows:"),
        div(class = "cite-block",
            HTML(paste0("Adegoke, H., &amp; Anyaibe, O. (2026). <em>Qualitative Text Analyzer</em> (Version ", APP_VERSION,
                        ") [Computer software]. Zenodo. https://doi.org/", APP_DOI))),
        p("This DOI always points to the latest release. Each release also has its own DOI, which is listed",
          "on the Zenodo page if you need to point to one specific release."),
        p("Please also cite the methods behind your results. The last reference applies only if you used the",
          "k diagnostics."),
        tags$ul(class = "cite-refs",
          tags$li(HTML("Blei, D. M., Ng, A. Y., &amp; Jordan, M. I. (2003). Latent Dirichlet allocation. <em>Journal of Machine Learning Research, 3</em>, 993&ndash;1022.")),
          tags$li(HTML("Gr&uuml;n, B., &amp; Hornik, K. (2011). topicmodels: An R package for fitting topic models. <em>Journal of Statistical Software, 40</em>(13), 1&ndash;30. https://doi.org/10.18637/jss.v040.i13")),
          tags$li(HTML("Silge, J., &amp; Robinson, D. (2016). tidytext: Text mining and analysis using tidy data principles in R. <em>Journal of Open Source Software, 1</em>(3), 37. https://doi.org/10.21105/joss.00037")),
          tags$li(HTML("Hu, M., &amp; Liu, B. (2004). Mining and summarizing customer reviews. <em>Proceedings of the 10th ACM SIGKDD International Conference on Knowledge Discovery and Data Mining</em>, 168&ndash;177.")),
          tags$li(HTML("Sievert, C., &amp; Shirley, K. (2014). LDAvis: A method for visualizing and interpreting topics. <em>Proceedings of the Workshop on Interactive Language Learning, Visualization, and Interfaces</em>, 63&ndash;70.")),
          tags$li(HTML("Mimno, D., Wallach, H., Talley, E., Leenders, M., &amp; McCallum, A. (2011). Optimizing semantic coherence in topic models. <em>Proceedings of EMNLP</em>, 262&ndash;272."))
        ),
        p("The summary download on the Data Summary tab lists the app version and every setting you used,",
          "so you can report it alongside your results."),
        p(class = "help-block", HTML(paste(
          "For example: Responses were analyzed with the Qualitative Text Analyzer",
          sprintf("(Version %s; Adegoke &amp; Anyaibe, 2026). We fit a latent Dirichlet allocation model (Blei et al., 2003)", APP_VERSION),
          "with k = 6 topics using variational EM in topicmodels (Gr&uuml;n &amp; Hornik, 2011), with alpha fixed at 0.1",
          "and seed 1234. Sentiment was scored with the Bing lexicon (Hu &amp; Liu, 2004).")))
      )
    )
  )
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Qualitative Text Analyzer", titleWidth = 280),
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar",
      menuItem("How to Use",         tabName = "howto",         icon = icon("circle-info")),
      menuItem("Upload & Settings",  tabName = "upload",        icon = icon("upload")),
      menuItem("Data Summary",       tabName = "summary",       icon = icon("table")),
      menuItem("Word Frequencies",   tabName = "frequencies",   icon = icon("chart-bar")),
      menuItem("Word Cloud",         tabName = "wordcloud",     icon = icon("cloud")),
      menuItem("Sentiment Analysis", tabName = "sentiment",     icon = icon("heart")),
      menuItem("Topic Analysis",     tabName = "topics",        icon = icon("diagram-project")),
      menuItem("Document Topics",    tabName = "doc_topic_map", icon = icon("map"))
    ),
    div(class = "sidebar-version", paste("Version", APP_VERSION))
  ),
  dashboardBody(
    useShinyjs(),
    use_waiter(),
    tags$head(tags$style(HTML("
      .download-btn { margin: 5px 5px 5px 0; }
      td.text-col, th.text-col { min-width: 380px; white-space: normal; }
      .help-block { color: #666; }
      .summary-text pre { white-space: pre-wrap; font-size: 13px; }
      .alert-info { background-color: #eef7fb !important; color: #31708f !important; border-color: #bce8f1 !important; }
      .alert-warning { background-color: #fcf8e3 !important; color: #8a6d3b !important; border-color: #faebcc !important; }
      .alert-danger { background-color: #f2dede !important; color: #a94442 !important; border-color: #ebccd1 !important; }
      .alert-success { background-color: #dff0d8 !important; color: #3c763d !important; border-color: #d6e9c6 !important; }
      .help-tip { color: #3c8dbc; cursor: help; }
      .howto-step { display: flex; align-items: flex-start; margin: 10px 0; }
      .howto-num { flex: 0 0 30px; height: 30px; border-radius: 50%; background: #3c8dbc; color: #fff;
                   font-weight: bold; text-align: center; line-height: 30px; margin-right: 12px; }
      .howto-body p { margin: 2px 0 0 0; }
      .howto-terms dt { margin-top: 8px; }
      .howto-terms dd { margin-left: 0; color: #444; }
      .howto-table td:first-child { white-space: nowrap; }
      details.advanced { margin: 6px 0 12px 0; }
      details.advanced summary { cursor: pointer; font-weight: bold; color: #3c8dbc; margin-bottom: 8px; }
      .shiny-output-error-validation { font-family: inherit; color: #31708f; background: #eef7fb; border: 1px solid #bce8f1;
                                       border-radius: 4px; padding: 12px; font-size: 14px; }
      .or-divider { text-align: center; color: #999; margin: 8px 0; }
      .sidebar-version { color: #8aa4af; font-size: 12px; padding: 12px 15px; }
      .cite-block { background: #f7f7f7; border-left: 4px solid #3c8dbc; padding: 10px 14px; margin: 8px 0 12px 0; }
      .cite-refs li { margin-bottom: 4px; }
    "))),

    tabItems(
      # ---- How to Use ---------------------------------------------------------
      tabItem(tabName = "howto", howto_tab()),

      # ---- Upload & Settings --------------------------------------------------
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "1. Data", status = "primary", solidHeader = TRUE, width = 6,
            uiOutput("exampleButton"),
            fileInput("file", with_help("Upload your file (.csv, .xlsx, .xls, .sav; up to 50 MB)",
                                        "Put each response in its own row. Other columns, such as IDs, groups or ratings, are kept in the downloads."),
                      accept = c(".csv", ".xlsx", ".xls", ".sav")),
            uiOutput("uploadError"),
            uiOutput("dataStatus"),
            uiOutput("sheetSelect"),
            uiOutput("columnSelect"),
            uiOutput("columnPreview")
          ),
          box(
            title = "2. Text cleaning", status = "primary", solidHeader = TRUE, width = 6,
            p(class = "help-block",
              "Text is lower-cased and split into words. Punctuation, numbers, single letters",
              "and possessive 's are removed, then tidytext's stopword list is applied",
              "(sentiment matching happens before stopword removal). Rows with no text",
              "are excluded and counted on the Data Summary tab."),
            textAreaInput("customStopwords",
                          with_help("Extra stopwords (comma-separated)",
                                    "These words are ignored along with common English words. Use this for words such as the course or program name that appear in almost every response."),
                          placeholder = "e.g., course, class, professor", rows = 2),
            checkboxInput("negation",
                          with_help("Handle negation in sentiment",
                                    "When this is on, a sentiment word counts the other way if not, no, never or a -n't word appears in the two words before it, so 'not helpful' counts as negative."),
                          value = FALSE)
          )
        ),
        fluidRow(
          box(
            title = "3. Topic model (LDA)", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(4, numericInput("numTopics",
                                     with_help("Number of topics (k)",
                                               "This is how many themes the model looks for. Start with 4 to 8 for a few hundred short responses."),
                                     value = 5, min = 2, max = 50, step = 1)),
              column(4, numericInput("minDocs",
                                     with_help("Keep terms used in at least N documents",
                                               "Words used in fewer responses than this are left out of the topic model. Higher values give cleaner topics, and lower values help when there is too little text."),
                                     value = 2, min = 1, step = 1))
            ),
            p(class = "help-block",
              "Not sure what k to use? Run the analysis once, then use",
              tags$em("Choosing the number of topics"), "on the Topic Analysis tab."),
            tags$details(
              class = "advanced",
              tags$summary("Advanced settings"),
              fluidRow(
                column(4, selectInput("ldaMethod",
                                      with_help("Estimation method",
                                                "VEM is fast and is the default. Gibbs sampling is slower and sometimes gives more distinct topics."),
                                      c("Variational EM (VEM)" = "VEM", "Gibbs sampling" = "Gibbs"))),
                column(4, numericInput("alpha",
                                       with_help("Topic prior (alpha)",
                                                 "Alpha controls how many topics a response tends to mix. Small values such as 0.1 give each short response one or two main topics."),
                                       value = 0.1, min = 0.001, step = 0.05)),
                column(4, numericInput("seed",
                                       with_help("Random seed",
                                                 "Running the same file with the same settings and seed gives the same results. Try another seed to check that the topics are stable."),
                                       value = 1234, step = 1))
              ),
              checkboxInput("estimateAlpha",
                            "Estimate alpha from the data (VEM) or use the package default of 50/k (Gibbs) instead. This isn't recommended for short responses.",
                            value = FALSE)
            ),
            actionButton("analyze", "Analyze", icon = icon("play"), class = "btn-primary btn-lg"),
            uiOutput("analyzeHint", inline = TRUE)
          )
        )
      ),

      # ---- Data Summary -------------------------------------------------------
      tabItem(
        tabName = "summary",
        uiOutput("summaryBoxes"),
        uiOutput("nextSteps"),
        fluidRow(
          box(
            title = "Data Overview", status = "primary", solidHeader = TRUE, width = 12,
            div(class = "summary-text",
                verbatimTextOutput("dataSummary") %>% withSpinner(color = COLORS$primary)),
            downloadButton("downloadSummary", "Download summary (TXT)", class = "download-btn")
          )
        )
      ),

      # ---- Word Frequencies ---------------------------------------------------
      tabItem(
        tabName = "frequencies",
        fluidRow(
          box(
            title = "Most frequent words", status = "primary", solidHeader = TRUE, width = 12,
            sliderInput("topN", "Number of words to show", min = 10, max = 100, value = 20, step = 5),
            p(class = "help-block",
              "Percent is each word's share of all the words left after stopword removal, and Documents is",
              "the number of responses that contain the word."),
            DTOutput("wordFreqTable") %>% withSpinner(color = COLORS$primary),
            downloadButton("downloadWordFreq", "Download all word frequencies (CSV)", class = "download-btn")
          )
        )
      ),

      # ---- Word Cloud ---------------------------------------------------------
      tabItem(
        tabName = "wordcloud",
        fluidRow(
          box(
            title = "Word Cloud", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(6, sliderInput("minFreq", "Minimum word frequency", min = 1, max = 20, value = 2)),
              column(6, sliderInput("maxWords", "Maximum words", min = 25, max = 300, value = 100, step = 25))
            ),
            plotOutput("wordCloud", height = "600px") %>% withSpinner(color = COLORS$primary),
            downloadButton("downloadWordCloud", "Download word cloud (PNG)", class = "download-btn")
          )
        )
      ),

      # ---- Sentiment ----------------------------------------------------------
      tabItem(
        tabName = "sentiment",
        fluidRow(
          box(
            title = "Sentiment by document", status = "primary", solidHeader = TRUE, width = 12,
            info_box(
              h4("How sentiment is scored", style = "margin-top: 0;"),
              p("Each word is looked up in the Bing lexicon, which lists words as positive or negative. A response's",
                "score is its number of positive words minus negative words, divided by their total, so it runs from",
                "-1 (all negative) to +1 (all positive). Responses with no lexicon words get no score."),
              p("This approach has limits. It ignores context, so it misses sarcasm and domain meanings such as",
                "'positive test'. Negation is handled only when that option is turned on in the settings, and then",
                "only for a negator within the two words before. Words that aren't in the lexicon count as neither",
                "positive nor negative.")
            ),
            uiOutput("sentimentCoverage"),
            plotlyOutput("sentimentPlot", height = "380px") %>% withSpinner(color = COLORS$primary),
            downloadButton("downloadSentimentPlot", "Download plot (PNG)", class = "download-btn")
          )
        ),
        fluidRow(
          box(
            title = "Document scores", status = "primary", solidHeader = TRUE, width = 12,
            DTOutput("sentimentDocTable") %>% withSpinner(color = COLORS$primary),
            downloadButton("downloadSentimentDocs", "Download document scores with original data (CSV)",
                           class = "download-btn")
          )
        ),
        fluidRow(
          box(
            title = "Sentiment words found", status = "primary", solidHeader = TRUE, width = 12,
            DTOutput("sentimentWordTable") %>% withSpinner(color = COLORS$primary),
            downloadButton("downloadSentimentWords", "Download sentiment words (CSV)", class = "download-btn")
          )
        )
      ),

      # ---- Topic Analysis -----------------------------------------------------
      tabItem(
        tabName = "topics",
        fluidRow(
          box(
            title = "Top terms by topic", status = "primary", solidHeader = TRUE, width = 12,
            uiOutput("topicStatus"),
            fluidRow(
              column(6, sliderInput("lambda", "Relevance weight (lambda)", min = 0, max = 1, value = 0.6, step = 0.1)),
              column(6, sliderInput("nTerms", "Terms per topic", min = 5, max = 20, value = 10, step = 1))
            ),
            uiOutput("topicPlotUI"),
            downloadButton("downloadTopicPlot", "Download plot (PNG)", class = "download-btn"),
            br(), br(),
            uiOutput("betaNote"),
            h4("Top terms per topic"),
            DTOutput("topicTable") %>% withSpinner(color = COLORS$primary),
            downloadButton("downloadTopicData", "Download term table with beta, lift, relevance (CSV)",
                           class = "download-btn")
          )
        ),
        fluidRow(
          box(
            title = "Choosing the number of topics", status = "primary", solidHeader = TRUE, width = 12,
            p("This fits a VEM model for each value of k, using the current cleaning and alpha settings.",
              "Held-out perplexity is computed on a random 20% of the responses, which needs at least 20",
              "responses, and UMass coherence uses each topic's top 10 terms. The two often disagree, so use",
              "them alongside your reading of the topics rather than as a rule. Each run can compare up to",
              "15 values of k."),
            fluidRow(
              column(3, numericInput("kMin", "Smallest k", value = 2, min = 2, step = 1)),
              column(3, numericInput("kMax", "Largest k", value = 10, min = 3, step = 1)),
              column(3, numericInput("kStep", "Step", value = 1, min = 1, step = 1)),
              column(3, br(), actionButton("runK", "Run diagnostics", icon = icon("chart-line")))
            ),
            plotOutput("kPlot", height = "350px"),
            DTOutput("kTable"),
            downloadButton("downloadK", "Download diagnostics (CSV)", class = "download-btn")
          )
        )
      ),

      # ---- Document Topics ----------------------------------------------------
      tabItem(
        tabName = "doc_topic_map",
        fluidRow(
          box(
            title = "Document topic shares", status = "primary", solidHeader = TRUE, width = 12,
            info_box(
              p("Gamma is the estimated share of a response that belongs to each topic. LDA lets every response",
                "mix topics, so gamma is not a confidence score. The dominant topic is the one with the largest",
                "share, and a response is marked Mixed when its top two shares are within 0.10 of each other.")
            ),
            DTOutput("docTopicTable") %>% withSpinner(color = COLORS$primary),
            downloadButton("downloadDocTopicData", "Download topic shares with original data (CSV)",
                           class = "download-btn")
          )
        ),
        fluidRow(
          box(
            title = "Representative documents", status = "primary", solidHeader = TRUE, width = 12,
            p(class = "help-block", "These are the responses with the highest share of the selected topic."),
            fluidRow(
              column(4, uiOutput("repTopicSelect")),
              column(4, numericInput("repN", "Number of documents", value = 5, min = 1, max = 50))
            ),
            DTOutput("repDocsTable")
          )
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL, file_name = NULL, source = NULL, is_example = FALSE,
                       error = NULL, results = NULL, kdiag = NULL)

  results_ready <- reactive(!is.null(rv$results))
  topics_ready  <- reactive(!is.null(rv$results$beta))

  # Download buttons are disabled until there is something to download
  observe({
    for (id in c("downloadSummary", "downloadWordFreq", "downloadWordCloud",
                 "downloadSentimentPlot", "downloadSentimentDocs", "downloadSentimentWords")) {
      toggleState(id, condition = results_ready())
    }
    for (id in c("downloadTopicPlot", "downloadTopicData", "downloadDocTopicData", "runK")) {
      toggleState(id, condition = topics_ready())
    }
    toggleState("downloadK", condition = !is.null(rv$kdiag))
    toggleState("analyze", condition = !is.null(rv$data) && !is.null(input$textColumn) &&
                  input$textColumn %in% names(rv$data))
  })

  # Friendly messages instead of empty panels
  need_results <- function() validate(need(!is.null(rv$results), MSG_NO_RESULTS))
  need_topics <- function() {
    need_results()
    validate(need(!is.null(rv$results$beta),
                  paste("The topic model was not fitted:", rv$results$topic_error %||% "",
                        "Word and sentiment results are still available.")))
  }

  go_to <- function(tab) updateTabItems(session, "sidebar", tab)

  # ---- Loading ---------------------------------------------------------------
  load_data <- function(path, name, sheet = NULL, example = FALSE) {
    rv$results <- NULL
    rv$kdiag   <- NULL
    rv$error   <- NULL
    rv$data    <- NULL
    rv$source  <- list(path = path, name = name)
    rv$is_example <- example
    waiter_show(html = tagList(spin_1(), h4("Loading data...")))
    on.exit(waiter_hide(), add = TRUE)
    tryCatch({
      rv$data      <- read_upload(path, name, sheet)
      rv$file_name <- name
    }, error = function(e) {
      rv$error <- paste("Could not read the file:", conditionMessage(e))
    })
  }

  observeEvent(input$file, load_data(input$file$datapath, input$file$name))

  observeEvent(input$sheet, {
    req(rv$source)
    load_data(rv$source$path, rv$source$name, input$sheet, rv$is_example)
  }, ignoreInit = TRUE)

  load_example <- function() {
    if (is.na(EXAMPLE_PATH)) {
      showNotification("The example data file is missing from the app's example_data folder.", type = "error")
      return(invisible())
    }
    load_data(EXAMPLE_PATH, EXAMPLE_FILE, example = TRUE)
    updateNumericInput(session, "numTopics", value = 6)
    updateNumericInput(session, "minDocs", value = 3)
    showNotification("The example data is loaded (500 made-up course evaluations), and the suggested settings of 6 topics and terms in at least 3 documents are applied. Click Analyze to continue.",
                     type = "message", duration = 10)
  }
  observeEvent(input$loadExample, load_example())
  observeEvent(input$goExample, { load_example(); go_to("upload") })
  observeEvent(input$goUpload, go_to("upload"))

  output$exampleButton <- renderUI({
    if (is.na(EXAMPLE_PATH)) return(NULL)
    tagList(
      actionButton("loadExample", "Load example data", icon = icon("flask")),
      span(class = "help-block", style = "display: inline; margin-left: 8px;",
           "This loads 500 made-up course-evaluation responses so you can try the app."),
      div(class = "or-divider", "or")
    )
  })

  output$dataStatus <- renderUI({
    req(rv$data)
    div(class = "alert alert-success", icon("check"),
        sprintf(" %s%s is loaded, with %d rows and %d columns.", rv$file_name,
                if (isTRUE(rv$is_example)) " (example data)" else "", nrow(rv$data), ncol(rv$data)))
  })

  output$analyzeHint <- renderUI({
    msg <- if (is.null(rv$data)) {
      "Load data above first."
    } else if (is.null(input$textColumn)) {
      "Choose the text column first."
    } else {
      "The data is ready to analyze. A few hundred responses take a few seconds."
    }
    span(class = "help-block", style = "display: inline; margin-left: 10px;", msg)
  })

  output$uploadError <- renderUI({
    req(rv$error)
    div(class = "alert alert-danger", rv$error)
  })

  output$sheetSelect <- renderUI({
    req(rv$source)
    ext <- tolower(tools::file_ext(rv$source$name))
    if (!ext %in% c("xlsx", "xls")) return(NULL)
    sheets <- tryCatch(readxl::excel_sheets(rv$source$path), error = function(e) NULL)
    if (length(sheets) <= 1) return(NULL)
    selectInput("sheet", with_help("Sheet", "This workbook has several sheets; choose the one with the responses."),
                choices = sheets, selected = sheets[1])
  })

  output$columnSelect <- renderUI({
    req(rv$data)
    cols <- text_columns(rv$data)
    if (length(cols) == 0) {
      return(div(class = "alert alert-warning",
                 "No text columns found in this file. The text to analyze must be stored as text, not numbers."))
    }
    selectInput("textColumn",
                with_help("Text column to analyze",
                          "The app suggests the column with the longest entries. Check the examples below."),
                choices = cols, selected = guess_text_column(rv$data, cols))
  })

  output$columnPreview <- renderUI({
    req(rv$data, input$textColumn, input$textColumn %in% names(rv$data))
    txt <- as.character(rv$data[[input$textColumn]])
    has <- !is.na(txt) & str_detect(txt, "[[:alpha:]]")
    words <- str_count(txt[has], "\\S+")
    examples <- if (any(has)) str_trunc(utils::head(txt[has], 3), 140) else "(no text)"
    div(class = "help-block",
        sprintf("This column has %d rows, %d of them with text, and a median of %s words per response. Here are a few examples:",
                length(txt), sum(has), if (any(has)) stats::median(words) else 0),
        tags$ul(lapply(examples, function(x) tags$li(tags$em(x)))))
  })

  # ---- Analysis --------------------------------------------------------------
  observeEvent(input$analyze, {
    req(rv$data, input$textColumn)
    rv$results <- NULL
    rv$kdiag   <- NULL
    waiter_show(html = tagList(spin_1(), h4("Analyzing text and fitting the topic model...")))
    on.exit(waiter_hide(), add = TRUE)

    res <- tryCatch(
      run_analysis(rv$data, input$textColumn,
                   custom_stopwords = input$customStopwords,
                   handle_negation  = isTRUE(input$negation),
                   min_docs         = input$minDocs,
                   k                = input$numTopics,
                   method           = input$ldaMethod,
                   seed             = input$seed,
                   alpha            = if (isTRUE(input$estimateAlpha)) NA else input$alpha),
      error = function(e) {
        showNotification(paste("Analysis stopped:", conditionMessage(e)), type = "error", duration = NULL)
        NULL
      }
    )
    if (is.null(res)) return(invisible())

    excluded <- res$counts$rows - res$counts$with_text
    if (excluded > 0) {
      showNotification(sprintf("%d of %d rows had no text and were excluded.", excluded, res$counts$rows),
                       type = "message", duration = 8)
    }
    if (!is.null(res$topic_error)) {
      showNotification(paste("Topic model not fitted:", res$topic_error), type = "warning", duration = NULL)
    }
    rv$results <- res
    showNotification("The analysis is complete, and the results are on the tabs in the left menu.", type = "message", duration = 6)
    go_to("summary")
  })

  # ---- Summary boxes and next steps ----------------------------------------------
  output$summaryBoxes <- renderUI({
    res <- rv$results
    req(res)
    cov <- res$sentiment$coverage
    topics_box <- if (is.null(res$beta)) {
      valueBox("0", "topic model not fitted (see Topic Analysis)", icon = icon("diagram-project"),
               color = "yellow", width = 3)
    } else {
      valueBox(res$settings$k, sprintf("topics; %d of %d responses Mixed",
                                       sum(res$doc_topics$assignment == "Mixed"), nrow(res$doc_topics)),
               icon = icon("diagram-project"), color = "green", width = 3)
    }
    fluidRow(
      valueBox(res$counts$with_text, sprintf("responses analyzed (of %d rows)", res$counts$rows),
               icon = icon("comments"), color = "blue", width = 3),
      valueBox(res$counts$rows - res$counts$with_text, "rows excluded (no text)",
               icon = icon("ban"), color = "light-blue", width = 3),
      topics_box,
      valueBox(sprintf("%.0f%%", 100 * cov$docs_with / max(1, cov$docs)), "contain sentiment words",
               icon = icon("heart"), color = "orange", width = 3)
    )
  })

  output$nextSteps <- renderUI({
    req(rv$results)
    fluidRow(box(
      title = "Next steps", status = "success", solidHeader = TRUE, width = 12,
      actionButton("nextFreq", "Top words", icon = icon("chart-bar")),
      actionButton("nextSent", "Sentiment", icon = icon("heart")),
      actionButton("nextTopics", "Topics", icon = icon("diagram-project")),
      actionButton("nextDocs", "Responses by topic", icon = icon("map")),
      span(class = "help-block", style = "display: inline; margin-left: 10px;",
           "It's best to name a topic only after reading its representative responses.")
    ))
  })
  observeEvent(input$nextFreq, go_to("frequencies"))
  observeEvent(input$nextSent, go_to("sentiment"))
  observeEvent(input$nextTopics, go_to("topics"))
  observeEvent(input$nextDocs, go_to("doc_topic_map"))

  # ---- Summary -----------------------------------------------------------------
  output$dataSummary <- renderPrint({
    need_results()
    cat(summary_lines(rv$results, rv$file_name), sep = "\n")
  })

  # ---- Word frequencies --------------------------------------------------------
  output$wordFreqTable <- renderDT({
    need_results()
    rv$results$freq %>%
      head(input$topN) %>%
      transmute(Word = word, Count = n, Documents = documents, `Percent (%)` = round(percent, 2)) %>%
      dt_table(page_length = 20) %>%
      formatStyle("Count", background = styleColorBar(c(0, rv$results$freq$n[1]), "#b8daff"),
                  backgroundSize = "98% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  }, server = FALSE)

  # ---- Word cloud --------------------------------------------------------------
  output$wordCloud <- renderPlot({
    need_results()
    draw_cloud(rv$results$freq, input$minFreq, input$maxWords, rv$results$settings$seed)
  })

  # ---- Sentiment ---------------------------------------------------------------
  output$sentimentCoverage <- renderUI({
    if (is.null(rv$results)) return(info_box(MSG_NO_RESULTS))
    cov <- rv$results$sentiment$coverage
    p(sprintf("%d of %d words (%.1f%%) matched the lexicon, and %d of %d responses contain at least one sentiment word.",
              cov$matched_tokens, cov$total_tokens, 100 * cov$matched_tokens / max(1, cov$total_tokens),
              cov$docs_with, cov$docs))
  })

  sentiment_plot <- reactive({
    req(rv$results)
    make_sentiment_plot(rv$results$sentiment$by_doc)
  })

  output$sentimentPlot <- renderPlotly({
    ggplotly(sentiment_plot(), tooltip = "text") %>%
      layout(showlegend = FALSE) %>%
      config(displaylogo = FALSE)
  })

  output$sentimentDocTable <- renderDT({
    req(rv$results)
    rv$results$sentiment$by_doc %>%
      transmute(row_id, category, score = round(score, 3), positive, negative, text) %>%
      dt_table() %>%
      formatStyle("category", backgroundColor = styleEqual(
        SENTIMENT_LEVELS, c("#c8e6c9", "#ffcdd2", "#ffecb3", "#f5f5f5")))
  }, server = FALSE)

  output$sentimentWordTable <- renderDT({
    req(rv$results)
    dt_table(rv$results$sentiment$by_word)
  }, server = FALSE)

  # ---- Topics ------------------------------------------------------------------
  scored_terms <- reactive({
    req(topics_ready())
    score_terms(rv$results$beta, rv$results$marginals, input$lambda)
  })

  top_terms_r <- reactive(top_terms(scored_terms(), input$nTerms))

  topic_plot <- reactive(make_topic_plot(top_terms_r()))

  topic_plot_height <- reactive({
    req(topics_ready())
    max(400, ceiling(rv$results$settings$k / 3) * (90 + 22 * input$nTerms))
  })

  output$topicStatus <- renderUI({
    if (is.null(rv$results)) return(info_box(MSG_NO_RESULTS))
    if (!is.null(rv$results$topic_error)) {
      div(class = "alert alert-warning", "The topic model could not be fitted.", rv$results$topic_error,
          "The word and sentiment results are still available.")
    }
  })

  output$topicPlotUI <- renderUI({
    req(topics_ready())
    plotlyOutput("topicPlot", height = paste0(topic_plot_height(), "px")) %>%
      withSpinner(color = COLORS$primary)
  })

  output$topicPlot <- renderPlotly({
    ggplotly(topic_plot(), tooltip = "text") %>%
      layout(showlegend = FALSE) %>%
      config(displaylogo = FALSE)
  })

  output$betaNote <- renderUI({
    req(topics_ready())
    V <- rv$results$counts$vocab_model
    info_box(
      h4("Reading the terms", style = "margin-top: 0;"),
      p("Beta is the probability of a word within a topic, P(term | topic). Its size depends on how many words",
        "the model uses, so there is no universal cutoff.",
        sprintf("With %d terms, a word spread evenly across all of them would have a beta of 1/%d = %.4f.", V, V, 1 / V)),
      p("Lift is beta divided by the word's overall frequency. A lift above 1 means the word is more typical",
        "of this topic than of the data as a whole."),
      p("Terms are ranked by relevance, which is lambda x log(beta) + (1 - lambda) x log(lift) (Sievert & Shirley,",
        "2014). A lambda of 1 ranks by beta alone, so frequent words dominate, while lower values favor words that",
        "are specific to the topic. The authors suggest starting at 0.6.")
    )
  })

  output$topicTable <- renderDT({
    req(topics_ready())
    top_terms_r() %>%
      mutate(label = sprintf("%s (%.3f)", term, beta), topic = paste("Topic", topic)) %>%
      select(rank, topic, label) %>%
      pivot_wider(names_from = topic, values_from = label) %>%
      rename(Rank = rank) %>%
      dt_table(page_length = input$nTerms, buttons = FALSE, dom = "t", ordering = FALSE)
  })

  # ---- k diagnostics -----------------------------------------------------------
  observeEvent(input$runK, {
    req(topics_ready())
    ks <- tryCatch(seq(as.integer(input$kMin), as.integer(input$kMax), by = as.integer(input$kStep)),
                   error = function(e) integer(0))
    n_docs <- nrow(rv$results$dtm)
    ks <- ks[!is.na(ks) & ks >= 2 & ks < n_docs]
    if (length(ks) == 0) {
      showNotification(sprintf("Choose k values of at least 2 and below the number of documents (%d).", n_docs),
                       type = "error")
      return(invisible())
    }
    if (length(ks) > 15) {
      showNotification("That is more than 15 models. Narrow the range or increase the step.", type = "error")
      return(invisible())
    }
    rv$kdiag <- NULL
    withProgress(message = "Fitting topic models", value = 0, {
      rv$kdiag <- tryCatch(
        k_diagnostics(rv$results$dtm, ks, rv$results$settings$seed,
                      on_progress = function(i, k) incProgress(1 / length(ks), detail = paste("k =", k)),
                      alpha = rv$results$settings$alpha),
        error = function(e) {
          showNotification(paste("Diagnostics stopped:", conditionMessage(e)), type = "error", duration = NULL)
          NULL
        }
      )
    })
  })

  output$kPlot <- renderPlot({
    req(topics_ready())
    validate(need(!is.null(rv$kdiag),
                  "Set a range of k above and click Run diagnostics to compare models."))
    make_k_plot(rv$kdiag)
  })

  output$kTable <- renderDT({
    req(rv$kdiag)
    rv$kdiag %>%
      mutate(across(-k, ~ round(.x, 2))) %>%
      dt_table(buttons = FALSE, dom = "t", pageLength = 15)
  })

  # ---- Document topics ---------------------------------------------------------
  output$docTopicTable <- renderDT({
    need_topics()
    rv$results$doc_topics %>%
      left_join(rv$results$docs %>% select(row_id, text), by = "row_id") %>%
      mutate(dominant_topic = paste("Topic", dominant_topic)) %>%
      select(row_id, dominant_topic, dominant_share, runner_up_share, assignment, text, starts_with("topic_")) %>%
      mutate(across(c(dominant_share, runner_up_share, starts_with("topic_")), ~ round(.x, 3))) %>%
      dt_table() %>%
      formatStyle("dominant_share", background = styleColorBar(c(0, 1), "#b8daff"),
                  backgroundSize = "98% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  }, server = FALSE)

  output$repTopicSelect <- renderUI({
    req(topics_ready())
    k <- rv$results$settings$k
    selectInput("repTopic", "Topic", choices = setNames(seq_len(k), paste("Topic", seq_len(k))))
  })

  output$repDocsTable <- renderDT({
    req(topics_ready(), input$repTopic)
    representative_docs(rv$results$gamma, rv$results$docs, as.integer(input$repTopic),
                        max(1, as.integer(input$repN %||% 5), na.rm = TRUE)) %>%
      mutate(gamma = round(gamma, 3)) %>%
      dt_table(buttons = FALSE, dom = "t", pageLength = 50)
  })

  # ---- Downloads ---------------------------------------------------------------
  with_row_id <- function(df) df %>% mutate(row_id = row_number(), .before = 1)

  output$downloadSummary <- downloadHandler(
    filename = function() paste0("analysis-summary-", Sys.Date(), ".txt"),
    content  = function(file) writeLines(summary_lines(rv$results, rv$file_name), file)
  )

  output$downloadWordFreq <- downloadHandler(
    filename = function() paste0("word-frequencies-", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(rv$results$freq, file)
  )

  output$downloadWordCloud <- downloadHandler(
    filename = function() paste0("wordcloud-", Sys.Date(), ".png"),
    content  = function(file) {
      png(file, width = 2400, height = 1800, res = 300)
      on.exit(dev.off(), add = TRUE)
      draw_cloud(rv$results$freq, input$minFreq, input$maxWords, rv$results$settings$seed)
    }
  )

  output$downloadSentimentPlot <- downloadHandler(
    filename = function() paste0("sentiment-distribution-", Sys.Date(), ".png"),
    content  = function(file) ggsave(file, sentiment_plot(), width = 7, height = 4.5, dpi = 300, bg = "white")
  )

  output$downloadSentimentDocs <- downloadHandler(
    filename = function() paste0("sentiment-by-document-", Sys.Date(), ".csv"),
    content  = function(file) {
      out <- with_row_id(rv$data) %>%
        left_join(rv$results$sentiment$by_doc %>% select(-text) %>%
                    rename(sentiment_positive = positive, sentiment_negative = negative,
                           sentiment_score = score, sentiment_category = category),
                  by = "row_id")
      readr::write_csv(out, file, na = "")
    }
  )

  output$downloadSentimentWords <- downloadHandler(
    filename = function() paste0("sentiment-words-", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(rv$results$sentiment$by_word, file)
  )

  output$downloadTopicPlot <- downloadHandler(
    filename = function() paste0("topic-terms-", Sys.Date(), ".png"),
    content  = function(file) {
      ggsave(file, topic_plot(), width = 11, height = topic_plot_height() / 90,
             dpi = 300, bg = "white", limitsize = FALSE)
    }
  )

  output$downloadTopicData <- downloadHandler(
    filename = function() paste0("topic-terms-", Sys.Date(), ".csv"),
    content  = function(file) {
      top_terms_r() %>%
        transmute(topic, rank, term, beta, lift, relevance, lambda = input$lambda) %>%
        readr::write_csv(file)
    }
  )

  output$downloadDocTopicData <- downloadHandler(
    filename = function() paste0("document-topics-", Sys.Date(), ".csv"),
    content  = function(file) {
      out <- with_row_id(rv$data) %>%
        left_join(rv$results$doc_topics, by = "row_id") %>%
        mutate(in_topic_model = !is.na(dominant_topic))
      readr::write_csv(out, file, na = "")
    }
  )

  output$downloadK <- downloadHandler(
    filename = function() paste0("k-diagnostics-", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(rv$kdiag, file)
  )
}

shinyApp(ui = ui, server = server)
