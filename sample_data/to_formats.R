d <- readr::read_csv("course_eval_survey.csv", show_col_types = FALSE, na = character())
codebook <- data.frame(
  variable = names(d),
  description = c("Respondent ID", "Delivery format (Online, In-person, Hybrid)", "Cohort year",
                  "Overall course rating, 1 = very poor to 5 = excellent",
                  "Open-ended: What did you think of this course?",
                  "Open-ended: What would you change?"))
writexl::write_xlsx(list(Responses = d, Codebook = codebook), "course_eval_survey.xlsx")
s <- d
s$program <- haven::labelled(match(d$program, c("Online", "In-person", "Hybrid")),
                             c(Online = 1, `In-person` = 2, Hybrid = 3), label = "Delivery format")
s$overall_rating <- haven::labelled(d$overall_rating,
                     c(`Very poor` = 1, Poor = 2, Fair = 3, Good = 4, Excellent = 5), label = "Overall rating")
attr(s$comments, "label") <- "What did you think of this course?"
attr(s$suggestions, "label") <- "What would you change?"
haven::write_sav(s, "course_eval_survey.sav")
back <- haven::read_sav("course_eval_survey.sav")
cat(nrow(back), "rows in .sav; program labels:", names(attr(back$program, "labels")), "\n")
