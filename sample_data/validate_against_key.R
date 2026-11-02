# Compares the app's topic and sentiment output on course_eval_survey.csv with
# the themes and polarities the responses were built from (answer key).
# Writes sample_data/validation_output.txt.
root <- getwd()   # run from the project folder
suppressPackageStartupMessages(library(dplyr))
app <- new.env()
sys.source(file.path(root, "FinaL_Qual_App", "app.R"), envir = app)
d   <- app$read_upload(file.path(root, "sample_data", "course_eval_survey.csv"), "x.csv")
key <- readr::read_csv(file.path(root, "sample_data", "course_eval_answer_key.csv"), show_col_types = FALSE) %>%
  mutate(row_id = row_number(),
         n_themes = ifelse(is.na(themes) | themes == "", 0, lengths(strsplit(themes, ";"))),
         theme1 = sub(":.*", "", sub(";.*", "", themes)),
         n_pos = vapply(strsplit(ifelse(is.na(themes), "", themes), ";"), function(x) sum(grepl(":pos$", x)), numeric(1)),
         true_net = ifelse(n_themes > 0, (2 * n_pos - n_themes) / n_themes, NA))

out <- file.path(root, "sample_data", "validation_output.txt")
sink(out)
cat("Validation on course_eval_survey.csv (500 rows)\n\n")

topic_check <- function(alpha, label) {
  res <- app$run_analysis(d, "comments", k = 6, min_docs = 3, method = "VEM", seed = 1234, alpha = alpha)
  dt <- res$doc_topics %>% left_join(key, by = "row_id")
  single <- dt %>% filter(n_themes == 1, theme1 != "generic")
  tab <- table(topic = single$dominant_topic, true_theme = single$theme1)
  topic_label <- apply(tab, 1, function(r) colnames(tab)[which.max(r)])
  purity <- sum(apply(tab, 1, max)) / sum(tab)
  cat(sprintf("===== %s (alpha used = %.3g) =====\n", label, res$alpha_used))
  cat(sprintf("Documents in model: %d; median dominant share: %.2f; Mixed: %d\n",
              res$counts$in_model, median(dt$dominant_share), sum(dt$assignment == "Mixed")))
  cat(sprintf("Single-theme responses: %d; purity (dominant topic matches its majority theme): %.0f%%\n",
              sum(tab), 100 * purity))
  cat(sprintf("Distinct themes recovered as a topic's majority: %d of 6\n", length(unique(topic_label))))
  cat("\nDominant topic x true theme (single-theme responses):\n")
  print(tab)
  top <- app$top_terms(app$score_terms(res$beta, res$marginals, 0.6), 8)
  cat("\nTop terms (relevance, lambda = 0.6):\n")
  for (t in sort(unique(top$topic))) {
    cat(sprintf("  Topic %d [%s]: %s\n", t, topic_label[as.character(t)],
                paste(top$term[top$topic == t], collapse = ", ")))
  }
  cat("\n")
  res
}
res <- topic_check(0.1, "alpha fixed at 0.1 (new default)")
invisible(topic_check(NA, "alpha estimated (old behaviour)"))

cat("===== Sentiment =====\n")
s <- res$sentiment$by_doc %>% left_join(key, by = "row_id") %>%
  left_join(d %>% mutate(row_id = row_number()) %>% select(row_id, overall_rating, program), by = "row_id")
cat(sprintf("Coverage: %d of %d documents have a score\n", sum(!is.na(s$score)), nrow(s)))
cat(sprintf("Spearman correlation, sentiment score vs overall rating: %.2f\n",
            cor(s$score, s$overall_rating, method = "spearman", use = "complete.obs")))
cat(sprintf("Spearman correlation, sentiment score vs true net polarity: %.2f\n",
            cor(s$score, s$true_net, method = "spearman", use = "complete.obs")))
one <- s %>% filter(n_themes == 1, !is.na(score), score != 0)
cat(sprintf("Sign agreement with true polarity (single-theme, scored, non-zero): %.0f%% of %d\n",
            100 * mean(sign(one$score) == sign(one$true_net)), nrow(one)))
cat("\nMean sentiment score by program:\n")
print(s %>% group_by(program) %>% summarise(n = n(), mean_score = round(mean(score, na.rm = TRUE), 2)) %>% as.data.frame(), row.names = FALSE)
sink()
cat(readLines(out), sep = "\n")
