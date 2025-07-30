# 📊 Qualitative Text Analyzer

A comprehensive and interactive R Shiny dashboard for performing qualitative text analysis using natural language processing techniques such as word frequency analysis, sentiment classification, topic modeling (LDA), and document-topic mapping.

---

## Features

- 📁 **Flexible Uploads**: Supports `.csv`, `.xlsx`, and `.sav` file formats.
- 🧹 **Text Preprocessing**: Includes customizable cleaning options and stopword removal.
- 📈 **Word Frequency Visualization**: View top terms, percentages, and downloadable summaries.
- ☁️ **Word Cloud Generation**: Dynamically create and export word clouds.
- ❤️ **Sentiment Analysis**: Classify sentiment using the `bing` lexicon and visualize results.
- 🧠 **Topic Modeling (LDA)**: Extract and interpret latent topics from unstructured text.
- 🗺️ **Document-Topic Mapping**: Assign documents to dominant topics with confidence scores.
- 📥 **Downloadable Results**: Export all analysis outputs including tables, plots, and reports.

---

## Required R Packages

Make sure the following R packages are installed before launching the app:

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyjs", "shinythemes", "shinycssloaders", "waiter",
  "readr", "readxl", "haven", "dplyr", "tidyr", "stringr", "tidytext", "textdata",
  "ggplot2", "plotly", "wordcloud", "tm", "topicmodels", "igraph", "reshape2", "DT"
))
```

---

## How to Run the App

1. Clone the repository:

```bash
git clone https://github.com/Hope112/qualitative-text-analyzer.git
cd qualitative-text-analyzer
```

2. Open R or RStudio and run the app:

```r
shiny::runApp("app.R")
```

3. Use the dashboard interface to upload your data and begin analysis.

---

## Sample Use Cases

- Analyze open-ended survey responses from educational or customer feedback.
- Explore latent themes in interview transcripts or policy documents.
- Conduct lightweight sentiment classification on textual datasets.
- Demonstrate qualitative text mining in an interactive classroom setting.

---

## File Upload Guidelines

- Supported formats: `.csv`, `.xlsx`, `.sav`.
- The dataset must contain at least one column with textual data.
- After uploading, use the dropdown to select the column to analyze.

---

## Screenshot Previews

> You may add screenshots or animated GIFs here using:
> 
> ```markdown
> ![Upload Tab](screenshots/upload.png)
> ![Word Cloud](screenshots/wordcloud.png)
> ```

---

## Behind the Scenes

- **Text Cleaning**: Lowercasing, punctuation/number removal, whitespace cleanup.
- **Tokenization**: Unnesting tokens via `tidytext`.
- **Sentiment Analysis**: Bing lexicon (`textdata::get_sentiments("bing")`).
- **Topic Modeling**: Latent Dirichlet Allocation (LDA) using `topicmodels::LDA`.
- **Visualization**: `ggplot2`, `plotly`, and `wordcloud` for interactive plots.

---

## Limitations

- Sentiment analysis is **context-agnostic**: it does not detect negations, sarcasm, or domain-specific terms.
- LDA may generate incoherent topics if data is too short, sparse, or noisy.
- Model interpretability and performance depend on proper preprocessing.

---

## Downloads

Each module allows downloading of:
- Word frequencies (CSV)
- Word cloud (PNG)
- Sentiment distribution and sentiment tables (HTML/CSV)
- Topic term data (CSV)
- Document-topic assignments (CSV)

---

## Contact

Created and maintained by Hope Adegoke.  
For questions, suggestions, or feature requests, please open an [issue] https://github.com/Hope112/Qual_Text_Analyzer/issues.

---
