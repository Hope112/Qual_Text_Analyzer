# Version 2.0.1 (2026-11-02)

The topic model failed on shinyapps.io with the message "there is no package called 'reshape2'". The app read the fitted model through `tidytext::tidy()`, which depends on reshape2, but deployment tools only install packages that the app names, and reshape2 had been removed from the package list as unused. The app now reads the topic tables directly from the fitted model, so it no longer needs reshape2, and two new checks guard against this. The app's citation now uses the Zenodo DOI for all versions (10.5281/zenodo.23891341), which always points to the latest release.

# Version 2.0.0 (2026-11-02)

Hope Adegoke and Oluchi Anyaibe released version 1.0 in May 2025 as an R programming course project and published it on shinyapps.io. Version 2.0.0, released in November 2026, contains the fixes and additions below. Its results differ from version 1.0 on the same data, as the methods changes explain.

All changes are in `FinaL_Qual_App/app.R` unless noted, and line numbers refer to the original file.

## Bugs fixed

1. Topic results were attached to the wrong rows (original lines 397 and 443–458). The document-term matrix numbered documents by their position after rows without text were dropped, and the app then joined those numbers to the original row numbers. Any blank response therefore shifted topics onto other rows, and some rows vanished. The matrix is now built from the tokens with `cast_dtm()` and keyed by the original `row_id`.
2. The loading screen could freeze (lines 426 and 439). A `return()` inside `tryCatch` skipped `waiter_hide()`, and the spinner is now closed with `on.exit()`.
3. The plot downloads failed (lines 790 and 815). `saveWidget` was never loaded, and `last_plot()` returned whichever plot had been built last on the server. Plots now come from reactives and download as PNG files.
4. The table copy, CSV and Excel buttons never appeared because the DT Buttons extension was not loaded. It is loaded now, and those tables are sent to the browser in full so the buttons export every row rather than only the visible page.
5. Sentiment scoring missed common words. Stopwords were removed before the lexicon lookup, so 35 Bing words that are also stopwords were never counted. Twenty-nine of them are positive, such as *good*, *great*, *best*, *useful* and *like*, and six are negative, such as *problem* and *sorry*. Matching now happens before stopword removal.
6. The Bing words listed as both positive and negative (*envious*, *enviously* and *enviousness*) were counted twice, and they are now dropped.
7. Unchecking "Remove rows with missing values" turned missing responses into the word `NA`. The checkbox is gone, and rows without text are always excluded and reported.
8. Results from an earlier analysis stayed on screen after a new upload or a failed rerun, so tabs could mix two analyses. Earlier results are now cleared.
9. Some code did nothing or caused problems, and it has been removed. `downloadSummary` read an output object, which Shiny does not allow, and had no button. The per-response sentiment scores were computed but never shown. A local `stripWhitespace()` overrode the tm function of the same name.

## Methods changes

The frequency and topic tabs used different stopword lists (tidytext's 728 words and tm's 174), and stripping apostrophes first left words like *dont* and *cant* in the topics. Every tab now uses the same tokens, contractions match the stopword list, possessive 's is removed, and you can add your own stopwords.

The fixed β cutoffs, such as "> 0.10 very strong", were removed. Terms are now ranked by relevance with an adjustable λ, and the app shows β, lift and the 1/V baseline.

All γ values are shown and exported, and the word "confidence" no longer describes them. Responses with a near-tie between their top two topics are marked Mixed, and ties no longer create duplicate rows.

The Document Topics tab now lists the most representative responses for each topic, and the Topic Analysis tab has diagnostics that compare held-out perplexity and UMass coherence over a range of k.

Sentiment is reported for each response as a score from −1 to 1, which matches the explanation on screen, along with the lexicon coverage and the list of matched words. Negation handling is available as an option.

You can now choose VEM or Gibbs estimation and set the seed, and both are recorded in the summary.

The topic prior α now defaults to a fixed 0.1, which you can change, and estimating it remains an option. Testing with the real topicmodels package showed the need for this. With α estimated (VEM reached 25.8) or at the Gibbs default of 50/k, every short response was spread almost evenly across the topics, with a median top share of 0.25 at k = 4 and all 50 responses marked Mixed. With α at 0.1, the median top share was 0.91 for VEM and 0.84 for Gibbs, and the topics were easier to read. The k diagnostics use the same α.

The minimum number of documents per term now controls the topic model's vocabulary, and the word cloud's frequency slider moved to the Word Cloud tab, where it applies.

## Usability

The app now opens on a How to Use tab with a quick start, guidance on preparing a file, a description of each tab, advice on choosing settings, explanations for reading the results, good practice and troubleshooting.

A Load example data button loads 500 synthetic course evaluations and applies suggested settings.

Every setting has hover help, and the estimation method, alpha and seed sit under Advanced settings.

After loading, the app confirms the file, rows and columns and shows three example responses from the chosen column.

The Analyze button stays disabled until data and a text column are chosen, and a hint says what is missing.

Before an analysis, each tab shows a short instruction instead of an empty panel, and the topic tabs explain when the model could not be fitted.

The Data Summary tab shows headline numbers for the responses analyzed and excluded, the topics with their Mixed count, and the share of responses with sentiment words, along with buttons to the next tabs.

## Citation

The version number, 2.0.0, appears in the sidebar and in the summary download, together with the recommended citation. The How to Use tab and the README explain how to cite the app and list the method references. A `CITATION.cff` file makes GitHub show a Cite this repository button. Version 2.0.0 is archived on Zenodo with the DOI 10.5281/zenodo.23891342, and 10.5281/zenodo.23891341 refers to all versions.

## Robustness

File extensions are no longer case-sensitive, .xls files are supported, you can choose the sheet in an Excel file, CSV encodings are detected, the upload limit is now 50 MB and SPSS value labels are kept.

Only text columns are listed, with a preview showing the number of rows with text, the median length and examples.

The number of topics is checked to be a whole number of at least 2 and smaller than the number of responses.

If the topic model cannot be fitted, the app still shows the frequencies and sentiment and explains why.

Download buttons stay disabled until there is something to download, and the response-level CSV files keep every original row and column along with `row_id`.

## Repository

The README now has the correct clone URL and `runApp("FinaL_Qual_App")` path, an accurate package list without the unused igraph, reshape2, shinythemes and textdata packages, methods notes and citation guidance.

The repository now has an MIT license.

The `sample_data` folder holds the synthetic test data, and `tests/check_app.R` holds the automated checks, which you can run with `Rscript tests/check_app.R`.

## Not changed

Model fitting still runs on the R process that serves the app. Running fits in the background would need the promises and future packages, which is only worth doing if the app will be shared on a server.

The folder name `FinaL_Qual_App` is unchanged so that existing links keep working.
