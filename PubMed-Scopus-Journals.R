#------------------------------------------------------------------------------#
#         Systematic Search for Potential Journals for Your Paper               #
#                                                                              #
#                       One Health UPCH - Jorge Canari                         #
#                                                                              #
#                              February 2023                                   #
#   Follow us on Social Media  https://x.com/OneHealthUPCH  (One Health)       #
#   Follow us on Social Media  https://x.com/Jorge_Canari                      #
#------------------------------------------------------------------------------#

rm(list = ls())

# Install and load necessary libraries
# install.packages("rentrez")    # To access PubMed
# install.packages("dplyr")      # For data manipulation
# install.packages("ggplot2")    # For visualization
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("xml2")
# install.packages("rvest")
library(xml2)
library(rvest)
library(rentrez)
library(dplyr)
library(ggplot2)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# PUBMED ------------------------------------------------------------------

# 🟢 STEP 1: Configure your email (Required for PubMed API)
entrez_email <- "your.email@gmail.pe"  # 🔹 Replace with your email

# 🟢 STEP 2: Define the search query in PubMed
query <- "(food insecurity[Title]) AND (qualitative[Title/Abstract]) AND (coping[Title/Abstract]) "

# 🟢 STEP 3: Retrieve up to 1000 article IDs
search_results <- entrez_search(db = "pubmed", term = query, retmax=1000)

# 🟢 STEP 4: Extract details of retrieved articles
articles <- entrez_summary(db = "pubmed", id = search_results$ids)

# 🟢 STEP 5: Retrieve journal names
journal_names_pubmed <- sapply(articles, function(x) x$source)

# 🟢 STEP 6: Count the number of articles per journal
journal_counts_pubmed <- as.data.frame(table(journal_names_pubmed))
colnames(journal_counts_pubmed) <- c("Journal", "Count")

# 🟢 STEP 7: Sort by publication count
journal_counts_pubmed <- journal_counts_pubmed %>% arrange(desc(Count))
# Verification
print(journal_counts_pubmed[1:10, ])


# SCOPUS ------------------------------------------------------------------

# 🟢 STEP 1: Configure your Scopus API key
# Request an API Key at https://dev.elsevier.com/ (requires institutional access) (tutorial https://www.youtube.com/watch?v=dT-fKh49k3M)
scopus_api_key <- "YOUR_API_KEY"  # 🔹 Replace with your API Key


# 🟢 STEP 2: Define the search query in Scopus
scopus_query <- "TITLE(\"food insecurity\") AND ABS(\"qualitative\") AND ABS(\"coping\")"

# 🟢 STEP 3: Adjust retrieval limits
max_results <- 100  # Maximum articles to retrieve
batch_size <- 25     # Number of articles per request
start <- 0           # Starting point for retrieval
all_journal_names_scopus <- c()  # Empty list to store journal names

# 🟢 STEP 4: Retrieve articles in batches (PAGINATION)
while (start < max_results) {
  scopus_url <- paste0(
    "https://api.elsevier.com/content/search/scopus?query=", 
    URLencode(scopus_query),
    "&count=", batch_size,
    "&start=", start,
    "&apiKey=", scopus_api_key
  )
  
  response <- GET(scopus_url, add_headers("Accept" = "application/xml"))
  xml_data <- read_xml(content(response, "text", encoding = "UTF-8"))
  journals_scopus <- xml_data %>% xml_find_all("//prism:publicationName") %>% xml_text()
  journals_scopus <- na.omit(journals_scopus)
  
  if (length(journals_scopus) > 0) {
    all_journal_names_scopus <- c(all_journal_names_scopus, journals_scopus)
  }
  
  print(paste("✅ Retrieved articles from Scopus, batch", start, "to", start + batch_size))
  start <- start + batch_size
}

# 🟢 STEP 5: Count the number of articles per journal
if (length(all_journal_names_scopus) > 0) {
  journal_counts_scopus <- as.data.frame(table(all_journal_names_scopus), stringsAsFactors = FALSE)
  colnames(journal_counts_scopus) <- c("Journal", "Count")
  journal_counts_scopus$Count <- as.numeric(as.character(journal_counts_scopus$Count))
  journal_counts_scopus <- journal_counts_scopus %>% arrange(desc(Count))
} else {
  print("⚠️ No journals found in Scopus results.")
}

journal_counts_scopus <- journal_counts_scopus %>% arrange(desc(Count))
# Verification
print(journal_counts_scopus[1:10, ])


# COMBINE PUBMED AND SCOPUS RESULTS ----------------------------------
journal_counts_pubmed$Journal <- paste0(journal_counts_pubmed$Journal, " - PubMed")
journal_counts_scopus$Journal <- paste0(journal_counts_scopus$Journal, " - Scopus")

# Merge results into a consolidated table
journal_counts_combined <- bind_rows(journal_counts_pubmed, journal_counts_scopus) %>%
  arrange(desc(Count))

# Display combined ranking
print("📊 Combined journal ranking from PubMed and Scopus:")
print(journal_counts_combined[1:20, ])  # Display top 20

# Generate combined bar chart
ggplot(journal_counts_combined[1:20, ], aes(x = reorder(Journal, Count), y = Count, fill = substr(Journal, nchar(Journal)-5, nchar(Journal)))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 20 Journals Publishing Qualitative Studies on Food Insecurity and Coping Strategies",
       x = "Journal", y = "Number of Publications", fill = "Source") +
  theme_minimal()
