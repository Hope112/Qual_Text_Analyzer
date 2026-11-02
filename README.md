# 📊 Qualitative Text Analyzer

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23891341.svg)](https://doi.org/10.5281/zenodo.23891341)

The Qualitative Text Analyzer is an R Shiny app for exploring open-ended text such as survey responses, interview excerpts and comments. It shows which words come up most, how positive or negative each response sounds, and which themes recur, using latent Dirichlet allocation (LDA) topic modeling.

You can try it online at https://diodnl-hope-adegoke.shinyapps.io/Qualitative_Analysis/.

---

## Features

The app opens on a How to Use tab that explains how to prepare a file, choose settings, read each result and fix common problems. Every setting has hover help, and a Load example data button lets you try the app with 500 made-up course-evaluation responses.

You can upload .csv, .xlsx, .xls and SPSS .sav files of up to 50 MB. For Excel files you choose the sheet, and SPSS value labels are kept. The app lists only text columns and suggests the one with the longest responses.

Every tab uses the same cleaned text. The app lower-cases the text, removes punctuation, numbers and possessive 's, and removes stopwords from the tidytext list along with any extra words you add.

The Word Frequencies tab counts each word and the number of responses that use it, and the Word Cloud tab shows the same counts as a picture that matches the downloaded image.

The Sentiment Analysis tab scores each response with the Bing lexicon as (positive − negative) / (positive + negative). It also lists the sentiment words found and reports how much of the text the lexicon covers. Negation handling is available as an option.

The Topic Analysis tab fits an LDA model with VEM or Gibbs estimation, a fixed seed and an adjustable topic prior (α, 0.1 by default, which suits short responses). Terms are ranked by relevance (Sievert & Shirley, 2014), with β and lift shown. A diagnostics panel compares held-out perplexity and UMass coherence across a range of topic numbers.

The Document Topics tab shows each response's share of every topic (γ), its dominant topic and whether it is mixed between topics, along with the most representative responses for each topic.

Every table downloads as a CSV file, and the response-level files keep all of your original rows and columns. Plots download as PNG files, and a text summary records the counts and settings for your methods section.

---

## Required R packages

```r
install.packages(c(
  "shiny", "shinydashboard", "shinycssloaders", "shinyjs", "waiter", "DT",
  "readr", "readxl", "haven", "dplyr", "tidyr", "stringr",
  "tidytext", "tm", "slam", "topicmodels",
  "ggplot2", "plotly", "wordcloud", "RColorBrewer"
))
```

On macOS, topicmodels needs GSL if R builds it from source (`brew install gsl`). The CRAN binary for your R version usually avoids this.

---

## How to run the app

Clone the repository:

```bash
git clone https://github.com/Hope112/Qual_Text_Analyzer.git
cd Qual_Text_Analyzer
```

Then, in R or RStudio, run this from the project folder:

```r
shiny::runApp("FinaL_Qual_App")
```

The app opens on the How to Use tab. To try it straight away, click Try it with example data and then Analyze. To use your own data, go to Upload & Settings, upload your file, choose the text column and click Analyze. Results appear on the tabs on the left, starting with Data Summary. To choose the number of topics, run the diagnostics on the Topic Analysis tab and then rerun the analysis with the value you pick.

The test data in the repository is all synthetic. The example that the app's button loads is `FinaL_Qual_App/example_data/course_eval_survey.csv`. The `sample_data` folder holds the same 500 responses as .csv, .xlsx (with a second Codebook sheet) and .sav files, plus `course_eval_answer_key.csv`, which records the themes and tone each response was built from. The script `sample_data/validate_against_key.R` compares the app's output with that key. The file `sample_data/sample_course_feedback.csv` holds 60 short responses that the automated checks use.

---

## Checking the installation

Run this from the project folder:

```bash
Rscript tests/check_app.R
```

It runs 61 checks on the sample data, covering how topic results line up with the original rows, the text cleaning, sentiment, the topic tools, every download and the user interface. The last line should report 0 failed.

---

## Methods notes

Rows that are missing, blank or contain only numbers are excluded, and the Data Summary tab reports how many. Every result is linked to its original row through `row_id`, which is the row number in the uploaded file.

Sentiment words are matched before stopwords are removed, because many sentiment words, such as *good*, *great* and *problem*, are on the stopword list. Bing words listed as both positive and negative are dropped. When negation handling is on, a word's polarity is reversed if *not*, *no*, *never* or a *-n't* word appears in the two words before it.

β is the probability of a term within a topic. Its size depends on the vocabulary size, so there is no universal threshold, and the app shows the uniform baseline of 1/V for comparison. Relevance is λ·log β + (1 − λ)·log(lift), where lift is β divided by the term's overall frequency.

The topic prior α defaults to a fixed 0.1. Estimating α (VEM) or using the package default of 50/k (Gibbs) spreads short responses almost evenly across all topics. On the sample data, the estimated α reached 25.8 and every response's top topic share was about 1/k. Both options remain available.

γ is the estimated share of a response that belongs to each topic, and it is not a confidence score. A response is marked Mixed when its top two shares are within 0.10 of each other.

The k diagnostics fit VEM models on a random 80% of the responses and compute perplexity on the other 20%, which needs at least 20 responses. UMass coherence (Mimno et al., 2011) uses each topic's top 10 terms across all responses.

## Limitations

Lexicon sentiment ignores sarcasm and domain-specific meanings, and the negation handling is a simple rule. LDA works poorly with very short or very few responses, so with short survey answers you should read a topic's representative responses before interpreting it. Models are fitted on the same R process that serves the app, so a long fit makes every user of that process wait.

---

## How to cite

If you use the app in published work, please cite it along with the methods behind your results, and report your settings. The summary download on the Data Summary tab lists them. GitHub's Cite this repository button, which comes from `CITATION.cff`, gives the citation in APA and BibTeX formats.

> Adegoke, H., & Anyaibe, O. (2026). *Qualitative Text Analyzer* (Version 2.0.1) [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.23891341

This DOI always points to the latest release. Each release also has its own DOI, listed on the Zenodo page, if you need to point to one specific release.

The method references are Blei, Ng and Jordan (2003) for LDA, Grün and Hornik (2011) for topicmodels, Silge and Robinson (2016) for tidytext, Hu and Liu (2004) for the Bing lexicon and Sievert and Shirley (2014) for term relevance. If you used the k diagnostics, also cite Mimno et al. (2011). The full references are on the app's How to Use tab.

---

## License

The app is released under the MIT License. See the `LICENSE` file for details.

---

## Contact

Hope Adegoke and Oluchi Anyaibe created the app as an R programming course project in 2025, and Hope Adegoke maintains version 2.0. The live app is at https://diodnl-hope-adegoke.shinyapps.io/Qualitative_Analysis/. For questions, suggestions or feature requests, please open an issue at https://github.com/Hope112/Qual_Text_Analyzer/issues.
