# Required packages
library(shiny)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(dplyr)
library(readr)
library(readxl)
library(haven)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(shinyjs)
library(waiter)
library(shinythemes)
library(stringr)
library(topicmodels)
library(tm)
library(plotly)
library(reshape2)
library(igraph)
library(tidyr)
library(textdata)

# Initialize DT buttons extension
options(DT.options = list(
  'pageLength' = 10,
  'dom' = 'Bfrtip',
  'buttons' = c('copy', 'csv', 'excel', 'pdf')
))

# Color palette
COLOR_PALETTE <- list(
  primary = "#0dc5c1",
  positive = "#4CAF50",
  negative = "#F44336",
  neutral = "#3C3D37",
  topic_colors = RColorBrewer::brewer.pal(8, "Set2")
)

# Helper functions
clean_text <- function(text) {
  text %>%
    str_to_lower() %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("[[:digit:]]", "") %>%
    str_replace_all("\r\n|\r|\n", " ") %>%
    str_trim() %>%
    str_squish()
}

stripWhitespace <- function(str) {
  str_trim(str_squish(str))
}

# UI definition
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(title = "Qualitative Text Analyzer"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Upload & Settings", tabName = "upload", icon = icon("file-upload")),
                menuItem("Data Summary", tabName = "summary", icon = icon("table")),
                menuItem("Word Frequencies", tabName = "frequencies", icon = icon("chart-bar")),
                menuItem("Word Cloud", tabName = "wordcloud", icon = icon("cloud")),
                menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("heart")),
                menuItem("Topic Analysis", tabName = "topics", icon = icon("project-diagram")),
                menuItem("Document Topics", tabName = "doc_topic_map", icon = icon("map"))
    )
  ),
  
  # Body
  dashboardBody(
    useShinyjs(),
    use_waiter(),
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .small-box {margin-bottom: 15px;}
        .download-btn {margin: 5px;}
        .plot-container {margin-top: 15px;}
        pre {
          white-space: pre-wrap;
          word-wrap: break-word;
          max-height: none !important;
          line-height: 1.2;
          font-size: 14px;
          margin: 0;
          padding: 10px;
        }
        .dataSummary {
          font-family: monospace;
          white-space: pre;
          overflow-x: auto;
          padding: 10px;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Upload Data & Configure Analysis", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    fileInput("file", 
                              "Upload File (Accepted formats: .csv, .xlsx, .sav)",
                              accept = c(".csv", ".xlsx", ".sav")),
                    uiOutput("uploadError"),
                    uiOutput("columnSelect"),
                    numericInput("numTopics", 
                                 "Number of Topics to Extract:", 
                                 value = 5, 
                                 min = 2, 
                                 max = 20),
                    sliderInput("minFreq", 
                                "Minimum Word Frequency:", 
                                min = 1, 
                                max = 10, 
                                value = 2),
                    checkboxInput("removeNA", "Remove rows with missing values", value = TRUE),
                    actionButton("analyze", 
                                 "Analyze", 
                                 icon = icon("play-circle"),
                                 class = "btn-primary")
                )
              )
      ),
      
      # Data Summary tab
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Data Overview",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    verbatimTextOutput("dataSummary") %>% 
                      withSpinner(color = COLOR_PALETTE$primary)
                )
              )
      ),
      
      # Word Frequencies tab
      tabItem(tabName = "frequencies",
              fluidRow(
                box(title = "Top 10 Most Frequent Words", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    DTOutput("wordFreqTable") %>% 
                      withSpinner(color = COLOR_PALETTE$primary),
                    downloadButton("downloadWordFreq", "Download Complete Word Frequency Data", 
                                   class = "download-btn")
                )
              )
      ),
      
      # Word Cloud tab
      tabItem(tabName = "wordcloud",
              fluidRow(
                box(title = "Word Cloud Visualization", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    plotOutput("wordCloud", height = "600px") %>% 
                      withSpinner(color = COLOR_PALETTE$primary),
                    downloadButton("downloadWordCloud", "Download Plot", 
                                   class = "download-btn")
                )
              )
      ),
      
      #Sentiment analysis
      tabItem(tabName = "sentiment",
              fluidRow(
                box(title = "Sentiment Distribution", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    div(
                      class = "alert alert-info",
                      style = "margin-bottom: 20px;",
                      h4("About Sentiment Analysis:", style = "margin-top: 0;"),
                      p("This analysis uses the 'bing' lexicon to classify words as positive or negative. 
                  The sentiment score ranges from -1 (most negative) to 1 (most positive)."),
                      p("Limitations:"),
                      p("Context is not considered (e.g., 'not good' would be counted as positive because 'good' is positive)"),
                      p("Domain-specific meanings might be missed"),
                      p("New or slang words might not be included"),
                      p("Sarcasm cannot be detected"),
                    ),
                    plotlyOutput("sentimentPlot") %>% 
                      withSpinner(color = COLOR_PALETTE$primary),
                    downloadButton("downloadSentimentPlot", "Download Plot", 
                                   class = "download-btn")
                )
              ),
              fluidRow(
                box(title = "Sentiment Details",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    DTOutput("sentimentTable") %>% 
                      withSpinner(color = COLOR_PALETTE$primary),
                    downloadButton("downloadSentimentData", "Download Sentiment Data", 
                                   class = "download-btn")
                )
              )
      ),
      
      # Topic Analysis tab
      tabItem(tabName = "topics",
              fluidRow(
                box(title = "Topic Term Distribution", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    plotlyOutput("topicPlot") %>% 
                      withSpinner(color = COLOR_PALETTE$primary),
                    downloadButton("downloadTopicPlot", "Download Plot", 
                                   class = "download-btn"),
                    br(), br(),
                    div(
                      class = "alert alert-info",
                      style = "margin-top: 15px;",
                      h4("How to Interpret Term Probabilities (β):", style = "margin-top: 0;"),
                      tags$ul(
                        tags$li(HTML("<strong>β > 0.10:</strong> Very strong association with the topic")),
                        tags$li(HTML("<strong>0.05 < β < 0.10:</strong> Strong association")),
                        tags$li(HTML("<strong>0.01 < β < 0.05:</strong> Moderate association")),
                        tags$li(HTML("<strong>β < 0.01:</strong> Weak association"))
                      )
                    ),
                    h4("Top Terms by Topic"),
                    DTOutput("topicTable") %>% 
                      withSpinner(color = COLOR_PALETTE$primary),
                    downloadButton("downloadTopicData", "Download Topic Data", 
                                   class = "download-btn")
                )
              )
      ),
      
      # Document Topics tab
      tabItem(tabName = "doc_topic_map",
              fluidRow(
                box(title = "Document Topic Assignments", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    DTOutput("docTopicTable") %>% 
                      withSpinner(color = COLOR_PALETTE$primary),
                    downloadButton("downloadDocTopicData", "Download Document-Topic Data", 
                                   class = "download-btn")
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    data = NULL,
    processed_text = NULL,
    sentiment_data = NULL,
    error = NULL,
    topic_model = NULL,
    doc_topic_matrix = NULL,
    topic_term_matrix = NULL,
    word_frequencies = NULL,
    word_frequencies_complete = NULL,
    doc_topic_assignments = NULL,
    doc_counts = NULL
  )
  
  # Define corpus processing function
  processCorpus <- function(corpus) {
    # Convert to lowercase
    corpus <- tm_map(corpus, content_transformer(tolower))
    
    # Remove punctuation
    removeCustomPunct <- function(x) {
      gsub("[[:punct:]]", " ", x)
    }
    corpus <- tm_map(corpus, content_transformer(removeCustomPunct))
    
    # Remove numbers
    removeCustomNum <- function(x) {
      gsub("[[:digit:]]", " ", x)
    }
    corpus <- tm_map(corpus, content_transformer(removeCustomNum))
    
    # Remove stopwords
    removeCustomStopwords <- function(x) {
      words <- unlist(strsplit(x, "\\s+"))
      words <- words[!words %in% stopwords("english")]
      paste(words, collapse = " ")
    }
    corpus <- tm_map(corpus, content_transformer(removeCustomStopwords))
    
    # Remove extra whitespace
    corpus <- tm_map(corpus, content_transformer(stripWhitespace))
    return(corpus)
  }
  
  # Data loading
  observeEvent(input$file, {
    waiter_show(html = tagList(spin_1(), "Loading data..."))
    
    tryCatch({
      ext <- tools::file_ext(input$file$name)
      
      rv$data <- switch(ext,
                        csv = read_csv(input$file$datapath),
                        xlsx = read_excel(input$file$datapath),
                        sav = read_sav(input$file$datapath),
                        stop("Invalid file format"))
      
      rv$error <- NULL
      
    }, error = function(e) {
      rv$error <- paste("Error loading file:", e$message)
      rv$data <- NULL
    })
    
    waiter_hide()
  })
  
  # Column selection UI
  output$columnSelect <- renderUI({
    req(rv$data)
    selectInput("textColumn", 
                "Select Text Column:", 
                choices = names(rv$data))
  })
  
  # Error handling UI
  output$uploadError <- renderUI({
    if (!is.null(rv$error)) {
      div(class = "alert alert-danger", rv$error)
    }
  })
  # Main analysis
  observeEvent(input$analyze, {
    req(rv$data, input$textColumn)
    waiter_show(html = tagList(spin_1(), "Analyzing text and generating topics..."))
    
    tryCatch({
      # Text processing with selected column
      text_data <- rv$data %>%
        mutate(
          doc_id = row_number(),
          text_clean = clean_text(!!sym(input$textColumn))
        )
      
      if (input$removeNA) {
        text_data <- text_data %>% filter(!is.na(text_clean))
      }
      
      # Process complete word frequencies first
      rv$processed_text <- text_data %>%
        unnest_tokens(word, text_clean) %>%
        anti_join(stop_words)
      
      rv$word_frequencies_complete <- rv$processed_text %>%
        count(word, sort = TRUE) %>%
        mutate(percentage = n / sum(n) * 100)
      
      # Store top 10 for display
      rv$word_frequencies <- rv$word_frequencies_complete %>%
        head(10)
      
      # Create corpus with better error handling
      tryCatch({
        # Check for empty documents
        empty_docs <- text_data %>%
          filter(text_clean == "" | is.na(text_clean)) %>%
          nrow()
        
        if(empty_docs > 0) {
          showNotification(
            paste("Found", empty_docs, "empty documents that will be removed."),
            type = "warning"
          )
          text_data <- text_data %>% filter(text_clean != "" & !is.na(text_clean))
        }
        
        # Create corpus with non-empty documents
        corpus <- VCorpus(VectorSource(text_data$text_clean))
        
        # Process corpus
        corpus <- processCorpus(corpus)
        
        # Check for empty documents after processing
        checkEmpty <- function(x) {
          stripWhitespace(x$content) == ""
        }
        empty_after <- sum(sapply(corpus, checkEmpty))
        
        if(empty_after > 0) {
          showNotification(
            paste("After processing,", empty_after, "documents became empty and will be removed."),
            type = "warning"
          )
          # Remove empty documents
          keepDoc <- function(x) {
            stripWhitespace(x$content) != ""
          }
          corpus <- corpus[sapply(corpus, keepDoc)]
        }
        
        # Create DTM
        dtm <- DocumentTermMatrix(corpus)
        
        # Check DTM validity
        if (dim(dtm)[1] == 0 || dim(dtm)[2] == 0) {
          showNotification("No valid terms found after processing. Please check your data.", type = "error")
          return()
        }
        
        # Store document count information
        rv$doc_counts <- list(
          original = nrow(rv$data),
          after_cleaning = nrow(text_data),
          final = dim(dtm)[1]
        )
        
        # Proceed with topic modeling only if we have enough documents
        if(dim(dtm)[1] < 2) {
          showNotification("Not enough valid documents for analysis after processing.", type = "error")
          return()
        }
        
        # Topic Modeling
        rv$topic_model <- LDA(dtm, k = input$numTopics, control = list(seed = 1234))
        rv$topic_term_matrix <- tidy(rv$topic_model, matrix = "beta")
        rv$doc_topic_matrix <- tidy(rv$topic_model, matrix = "gamma")
        
        # Document-topic assignments
        rv$doc_topic_assignments <- rv$doc_topic_matrix %>%
          group_by(document) %>%
          slice_max(gamma, n = 1) %>%
          ungroup() %>%
          mutate(
            document = as.numeric(document),
            main_topic = paste("Topic", topic),
            topic_probability = gamma
          ) %>%
          # Join with original data
          left_join(text_data, by = c("document" = "doc_id"))
        
        # Process sentiment by document
        word_sentiments <- rv$processed_text %>%
          inner_join(get_sentiments("bing")) %>%
          group_by(doc_id, sentiment) %>%
          summarise(count = n(), .groups = "drop") %>%
          pivot_wider(
            names_from = sentiment,
            values_from = count,
            values_fill = list(count = 0)
          ) %>%
          mutate(
            total_words = positive + negative,
            sentiment_score = case_when(
              total_words == 0 ~ 0,
              TRUE ~ (positive - negative) / total_words
            )
          )
        
        rv$sentiment_data <- word_sentiments
        
      }, error = function(e) {
        showNotification(paste("Error in text processing:", e$message), type = "error")
        return()
      })
      
    }, error = function(e) {
      showNotification(paste("Error in analysis:", e$message), type = "error")
    })
    
    waiter_hide()
  })
  # Data Summary
  output$dataSummary <- renderPrint({
    req(rv$processed_text, rv$data)  # Require both processed text and original data
    
    isolate({
      cat("Dataset Summary:\n")
      cat("===============\n")
      cat("Total number of documents:", nrow(rv$data), "\n")
      cat("Total number of unique words:", n_distinct(rv$processed_text$word), "\n")
      cat("Total word count:", nrow(rv$processed_text), "\n")
      
      # Add document statistics
      if (!is.null(rv$doc_counts)) {
        cat("\nDocument Processing:\n")
        cat("Original documents:", rv$doc_counts$original, "\n")
        cat("After cleaning:", rv$doc_counts$after_cleaning, "\n")
        cat("Final processed:", rv$doc_counts$final, "\n")
      }
    })
  })
  
  # Word Frequencies Table
  output$wordFreqTable <- renderDT({
    req(rv$word_frequencies)
    datatable(
      rv$word_frequencies %>%
        mutate(percentage = round(percentage, 2)) %>%
        rename(Word = word, Frequency = n, "Percentage (%)" = percentage),
      options = list(dom = 't', pageLength = 10),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Frequency',
        background = styleColorBar(rv$word_frequencies$n, '#b8daff')
      )
  })
  
  # Word Cloud
  output$wordCloud <- renderPlot({
    req(rv$word_frequencies_complete)
    
    plot_words <- rv$word_frequencies_complete %>%
      filter(n >= input$minFreq)
    
    wordcloud(
      words = plot_words$word,
      freq = plot_words$n,
      min.freq = input$minFreq,
      max.words = 100,
      random.order = FALSE,
      colors = brewer.pal(8, "Dark2"),
      scale = c(4, 0.5)
    )
  })
  
  #Sentiment plot
  output$sentimentPlot <- renderPlotly({
    req(rv$processed_text)
    
    # Get all words and their counts
    all_words <- rv$processed_text %>%
      count(word, sort = TRUE) %>%
      rename(total = n)
    
    # Get sentiment words
    sentiment_words <- rv$processed_text %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE)
    
    # Calculate total words in each category
    total_words <- sum(all_words$total)
    sentiment_summary <- sentiment_words %>%
      group_by(sentiment) %>%
      summarise(count = sum(n)) %>%
      mutate(percentage = count/total_words * 100)
    
    # Add neutral category
    neutral_count <- total_words - sum(sentiment_summary$count)
    sentiment_summary <- bind_rows(
      sentiment_summary,
      tibble(
        sentiment = "neutral",
        count = neutral_count,
        percentage = neutral_count/total_words * 100
      )
    )
    
    # Create plot
    p <- plot_ly(sentiment_summary, type = "bar") %>%
      add_trace(
        x = ~sentiment,
        y = ~percentage,
        text = ~paste(
          sentiment,
          "\nCount:", count,
          "\nPercentage:", round(percentage, 1), "%"
        ),
        hoverinfo = "text",
        marker = list(
          color = ~case_when(
            sentiment == "positive" ~ COLOR_PALETTE$positive,
            sentiment == "negative" ~ COLOR_PALETTE$negative,
            TRUE ~ COLOR_PALETTE$neutral
          )
        )
      ) %>%
      layout(
        title = "Distribution of Word Sentiments",
        xaxis = list(title = "Sentiment Category"),
        yaxis = list(title = "Percentage of Words (%)"),
        showlegend = FALSE,
        annotations = list(
          x = 0.5,
          y = 1.1,
          text = paste("Total Words Analyzed:", total_words),
          xref = "paper",
          yref = "paper",
          showarrow = FALSE
        )
      )
    
    p
  })
  
  # Sentiment Table
  output$sentimentTable <- renderDT({
    req(rv$processed_text)
    
    # Get all words and their counts
    all_words <- rv$processed_text %>%
      count(word, sort = TRUE) %>%
      rename(total = n)
    
    # Get sentiment classifications
    sentiment_words <- rv$processed_text %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(
        sentiment_ratio = (positive - negative)/(positive + negative),
        overall_sentiment = ifelse(sentiment_ratio > 0, "Positive", "Negative")
      )
    
    # Combine all words with sentiment words
    all_word_sentiments <- all_words %>%
      left_join(sentiment_words) %>%
      mutate(
        negative = ifelse(is.na(negative), 0, negative),
        positive = ifelse(is.na(positive), 0, positive),
        sentiment_ratio = ifelse(is.na(sentiment_ratio), 0, sentiment_ratio),
        overall_sentiment = ifelse(is.na(overall_sentiment), "Neutral", overall_sentiment)
      ) %>%
      arrange(desc(total))
    
    datatable(
      all_word_sentiments,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'overall_sentiment',
        backgroundColor = styleEqual(
          c("Positive", "Negative", "Neutral"),
          c("#c8e6c9", "#ffcdd2", "#f5f5f5")
        )
      ) %>%
      formatRound('sentiment_ratio', 3)
  })
  
  # Topic Visualization
  output$topicPlot <- renderPlotly({
    req(rv$topic_term_matrix)
    
    # Get top 10 terms for each topic
    top_terms <- rv$topic_term_matrix %>%
      group_by(topic) %>%
      slice_max(beta, n = 10) %>%
      ungroup() %>%
      mutate(
        topic = paste("Topic", topic),
        term = reorder_within(term, beta, topic)
      )
    
    p <- ggplot(top_terms, aes(x = term, y = beta, fill = topic)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~topic, scales = "free_y") +
      coord_flip() +
      scale_x_reordered() +
      theme_minimal() +
      labs(
        title = "Top 10 Terms by Topic",
        x = "Terms",
        y = "Beta (Term Probability)"
      ) +
      theme(
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 12, face = "bold")
      )
    
    ggplotly(p)
  })
  
  # Topic Table
  output$topicTable <- renderDT({
    req(rv$topic_term_matrix)
    
    # Get top 10 terms for each topic with their probabilities
    top_terms <- rv$topic_term_matrix %>%
      group_by(topic) %>%
      slice_max(beta, n = 10) %>%
      mutate(
        term = paste0(term, " (", round(beta, 4), ")"),
        rank = row_number()
      ) %>%
      select(topic, rank, term) %>%
      pivot_wider(
        names_from = topic,
        values_from = term,
        names_prefix = "Topic "
      ) %>%
      select(-rank)
    
    datatable(
      top_terms,
      options = list(
        dom = 't',
        ordering = FALSE,
        pageLength = 10
      ),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; color: black;',
        'Top 10 Terms per Topic (with probabilities)'
      )
    )
  })
  
  # Document-Topic Table
  output$docTopicTable <- renderDT({
    req(rv$doc_topic_assignments)
    datatable(
      rv$doc_topic_assignments %>%
        select(-document, -topic) %>% 
        select(main_topic, topic_probability, everything(), -text_clean) %>%
        mutate(topic_probability = round(topic_probability, 3)),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'topic_probability',
        background = styleColorBar(c(0,1), '#b8daff')
      )
  })

  
  # Download handlers
  output$downloadWordFreq <- downloadHandler(
    filename = function() {
      paste("complete-word-frequencies-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv(rv$word_frequencies_complete, file)
    }
  )
  
  output$downloadWordCloud <- downloadHandler(
    filename = function() {
      paste("wordcloud-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      wordcloud(
        words = rv$word_frequencies_complete$word,
        freq = rv$word_frequencies_complete$n,
        min.freq = input$minFreq,
        max.words = 200,
        random.order = FALSE,
        colors = brewer.pal(8, "Dark2"),
        scale = c(4, 0.5)
      )
      dev.off()
    }
  )
  
  output$downloadSentimentPlot <- downloadHandler(
    filename = function() {
      paste("sentiment-distribution-", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      saveWidget(last_plot(), file)
    }
  )
  
  output$downloadSentimentData <- downloadHandler(
    filename = function() {
      paste("sentiment-analysis-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      sentiment_data <- rv$processed_text %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(total = positive + negative,
               sentiment_ratio = (positive - negative)/(positive + negative))
      
      write_csv(sentiment_data, file)
    }
  )
  
  output$downloadTopicPlot <- downloadHandler(
    filename = function() {
      paste("topic-visualization-", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      saveWidget(last_plot(), file)
    }
  )
  
  output$downloadTopicData <- downloadHandler(
    filename = function() {
      paste("topic-top10-terms-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      top_terms_export <- rv$topic_term_matrix %>%
        group_by(topic) %>%
        slice_max(beta, n = 10) %>%
        mutate(
          topic = paste("Topic", topic),
          probability = round(beta, 4)
        ) %>%
        select(topic, term, probability) %>%
        arrange(topic, desc(probability))
      
      write_csv(top_terms_export, file)
    }
  )
  
  output$downloadDocTopicData <- downloadHandler(
    filename = function() {
      paste("document-topics-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv(rv$doc_topic_assignments, file)
    }
  )
  
  output$downloadSummary <- downloadHandler(
    filename = function() {
      paste("analysis-summary-", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      writeLines(isolate(output$dataSummary()), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)