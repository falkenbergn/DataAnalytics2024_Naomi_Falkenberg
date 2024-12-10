#Warren Alpert Estrogen, Testosterone, and Androgen
# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# ---- FILTERING ----

# Updated function to query UniProt API for protein details
query_uniprot <- function(protein_id) {
  url <- paste0("https://rest.uniprot.org/uniprotkb/", protein_id, ".json")
  response <- GET(url)
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, as = "text"))
    return(data)
  } else {
    return(NULL)
  }
}

# ---- FEMALES ----
# --- Female DS LM ---
# Load CSV file of Female DS LM proteins
protein_data <- read.csv("C:/Users/chaos/Downloads/FDSLM_Intersect_10_18_24.csv", fileEncoding = "UTF-8")

# Initialize a new column for hormone relation
protein_data$Hormone_Related <- NA

# Define keywords to check for estrogen, testosterone, or androgen relations
hormone_keywords <- c("estrogen", "androgen", "testosterone")

# Loop through each protein and query UniProt (or similar database)
for (i in 1:nrow(protein_data)) {
  protein_id <- protein_data$GeneName[i]  # assuming Protein column contains UniProt IDs
  result <- query_uniprot(protein_id)
  
  if (!is.null(result)) {
    # Check fields for hormone-related keywords
    description_text <- tolower(paste(result$keywords, result$comments, result$functions, collapse = " "))
    if (any(grepl(paste(hormone_keywords, collapse = "|"), description_text))) {
      protein_data$Hormone_Related[i] <- TRUE
    } else {
      protein_data$Hormone_Related[i] <- FALSE
    }
  } else {
    protein_data$Hormone_Related[i] <- NA
  }
}

#Summary
summary_counts <- table(protein_data$Hormone_Related, useNA = "ifany")

# Display counts
print(summary_counts)

# Filter for Female DS LM hormone-related proteins
hormone_related_proteins <- protein_data %>% filter(Hormone_Related == TRUE)

# Save the filtered female DS LM proteins to a CSV file
write.csv(hormone_related_proteins, "hormone_related_proteins_female_ds_lm_12_2_24.csv", row.names = FALSE)

# --- Female DS AD ---
# Load CSV file of Female DS AD proteins
protein_data2 <- read.csv("C:/Users/chaos/Downloads/FDSAD_Intersect_10_18_24.csv", fileEncoding = "UTF-8")

# Initialize a new column for hormone relation
protein_data2$Hormone_Related <- NA

# Define keywords to check for estrogen, testosterone, or androgen relations
hormone_keywords <- c("estrogen", "androgen", "testosterone")

# Loop through each protein and query UniProt (or similar database)
for (i in 1:nrow(protein_data2)) {
  protein_id2 <- protein_data2$GeneName[i]  # assuming Protein column contains UniProt IDs
  result2 <- query_uniprot(protein_id2)
  
  if (!is.null(result2)) {
    # Check fields for hormone-related keywords
    description_text <- tolower(paste(result2$keywords, result2$comments, result2$functions, collapse = " "))
    if (any(grepl(paste(hormone_keywords, collapse = "|"), description_text))) {
      protein_data2$Hormone_Related[i] <- TRUE
    } else {
      protein_data2$Hormone_Related[i] <- FALSE
    }
  } else {
    protein_data2$Hormone_Related[i] <- NA
  }
}
  #Summary
  summary_counts2 <- table(protein_data2$Hormone_Related, useNA = "ifany")

  # Display counts
  print(summary_counts2)
  
  # Filter for Female DS AD hormone-related proteins
  hormone_related_proteins2 <- protein_data2 %>% filter(Hormone_Related == TRUE)
  # Save the filtered female DS AD proteins to a CSV file
  write.csv(hormone_related_proteins2, "hormone_related_proteins_female_ds_ad_12_2_24.csv", row.names = FALSE)
  
  #--- Female SW LM ---
  # Load CSV file of Female DS AD proteins
  protein_data3 <- read.csv("C:/Users/chaos/Downloads/FSWLM_Intersect_10_18_24.csv", fileEncoding = "UTF-8")
  
  # Initialize a new column for hormone relation
  protein_data3$Hormone_Related <- NA
  
  # Define keywords to check for estrogen, testosterone, or androgen relations
  hormone_keywords <- c("estrogen", "androgen", "testosterone")
  
  # Loop through each protein and query UniProt (or similar database)
  for (i in 1:nrow(protein_data3)) {
    protein_id3 <- protein_data3$GeneName[i]  # assuming Protein column contains UniProt IDs
    result3 <- query_uniprot(protein_id3)
    
    if (!is.null(result3)) {
      # Check fields for hormone-related keywords
      description_text <- tolower(paste(result3$keywords, result3$comments, result3$functions, collapse = " "))
      if (any(grepl(paste(hormone_keywords, collapse = "|"), description_text))) {
        protein_data3$Hormone_Related[i] <- TRUE
      } else {
        protein_data3$Hormone_Related[i] <- FALSE
      }
    } else {
      protein_data3$Hormone_Related[i] <- NA
    }
  }
  #Summary
  summary_counts3 <- table(protein_data3$Hormone_Related, useNA = "ifany")
  
  # Display counts
  print(summary_counts3)
  
  # Filter for Female SW LM hormone-related proteins
  hormone_related_proteins3 <- protein_data3 %>% filter(Hormone_Related == TRUE)
  
  # Save the filtered female SW LM proteins to a CSV file
  write.csv(hormone_related_proteins3, "hormone_related_proteins_female_sw_lm_12_2_24.csv", row.names = FALSE)
  
  # --- Female SW AD ---
  # Load CSV file of Female SW AD proteins
  protein_data4 <- read.csv("C:/Users/chaos/Downloads/FSWAD_Intersect_10_18_24.csv", fileEncoding = "UTF-8")
  
  # Initialize a new column for hormone relation
  protein_data4$Hormone_Related <- NA
  
  # Define keywords to check for estrogen, testosterone, or androgen relations
  hormone_keywords <- c("estrogen", "androgen", "testosterone")
  
  # Loop through each protein and query UniProt (or similar database)
  for (i in 1:nrow(protein_data4)) {
    protein_id4 <- protein_data4$GeneName[i]  # assuming Protein column contains UniProt IDs
    result4 <- query_uniprot(protein_id4)
    
    if (!is.null(result4)) {
      # Check fields for hormone-related keywords
      description_text <- tolower(paste(result4$keywords, result4$comments, result4$functions, collapse = " "))
      if (any(grepl(paste(hormone_keywords, collapse = "|"), description_text))) {
        protein_data4$Hormone_Related[i] <- TRUE
      } else {
        protein_data4$Hormone_Related[i] <- FALSE
      }
    } else {
      protein_data4$Hormone_Related[i] <- NA
    }
  }
  #Summary
  summary_counts4 <- table(protein_data4$Hormone_Related, useNA = "ifany")
  
  # Display counts
  print(summary_counts4)
   
  # Filter for Female SW AD hormone-related proteins
  hormone_related_proteins4 <- protein_data4 %>% filter(Hormone_Related == TRUE)
  
  # Save the filtered female SW AD proteins to a CSV file
  write.csv(hormone_related_proteins4, "hormone_related_proteins_female_sw_ad_12_2_24.csv", row.names = FALSE)
  
  # ---- MALES ----
  # --- Male DS LM ---
  # Load CSV file of Male DS LM proteins
  protein_data5 <- read.csv("C:/Users/chaos/Downloads/MDSLM_Intersect_10_18_24.csv", fileEncoding = "UTF-8")
  
  # Initialize a new column for hormone relation
  protein_data5$Hormone_Related <- NA
  
  # Define keywords to check for estrogen, testosterone, or androgen relations
  hormone_keywords <- c("estrogen", "androgen", "testosterone")
  
  # Loop through each protein and query UniProt (or similar database)
  for (i in 1:nrow(protein_data5)) {
    protein_id5 <- protein_data5$GeneName[i]  # assuming Protein column contains UniProt IDs
    result5 <- query_uniprot(protein_id5)
    
    if (!is.null(result5)) {
      # Check fields for hormone-related keywords
      description_text <- tolower(paste(result5$keywords, result5$comments, result5$functions, collapse = " "))
      if (any(grepl(paste(hormone_keywords, collapse = "|"), description_text))) {
        protein_data5$Hormone_Related[i] <- TRUE
      } else {
        protein_data5$Hormone_Related[i] <- FALSE
      }
    } else {
      protein_data5$Hormone_Related[i] <- NA
    }
  }
  #Summary
  summary_counts5 <- table(protein_data5$Hormone_Related, useNA = "ifany")
  
  # Display counts
  print(summary_counts5)
  
  # Filter for Male DS LM hormone-related proteins
  hormone_related_proteins5 <- protein_data5 %>% filter(Hormone_Related == TRUE)
  
  # Save the filtered male ds lm proteins to a CSV file
  write.csv(hormone_related_proteins5, "hormone_related_proteins_male_ds_lm_12_2_24.csv", row.names = FALSE)
  
  # --- Male DS AD ---
  # Load CSV file of Male DS AD proteins
  protein_data6 <- read.csv("C:/Users/chaos/Downloads/MDSAD_Intersect_10_18_24.csv", fileEncoding = "UTF-8")
  
  # Initialize a new column for hormone relation
  protein_data6$Hormone_Related <- NA
  
  # Define keywords to check for estrogen, testosterone, or androgen relations
  hormone_keywords <- c("estrogen", "androgen", "testosterone")
  
  # Loop through each protein and query UniProt (or similar database)
  for (i in 1:nrow(protein_data6)) {
    protein_id6 <- protein_data6$GeneName[i]  # assuming Protein column contains UniProt IDs
    result6 <- query_uniprot(protein_id6)
    
    if (!is.null(result6)) {
      # Check fields for hormone-related keywords
      description_text <- tolower(paste(result6$keywords, result6$comments, result6$functions, collapse = " "))
      if (any(grepl(paste(hormone_keywords, collapse = "|"), description_text))) {
        protein_data6$Hormone_Related[i] <- TRUE
      } else {
        protein_data6$Hormone_Related[i] <- FALSE
      }
    } else {
      protein_data6$Hormone_Related[i] <- NA
    }
  }
  #Summary
  summary_counts6 <- table(protein_data6$Hormone_Related, useNA = "ifany")
  
  # Display counts
  print(summary_counts6)
  
  # Filter for Male DS AD hormone-related proteins
  hormone_related_proteins6 <- protein_data6 %>% filter(Hormone_Related == TRUE)
  
  # Save the filtered male ds ad proteins to a CSV file
  write.csv(hormone_related_proteins6, "hormone_related_proteins_male_ds_ad_12_2_24.csv", row.names = FALSE)
  
  # --- Male SW LM ---
  # Load CSV file of Male SW LM proteins
  protein_data7 <- read.csv("C:/Users/chaos/Downloads/MSWLM_Intersect_10_18_24.csv", fileEncoding = "UTF-8")
  
  # Initialize a new column for hormone relation
  protein_data7$Hormone_Related <- NA
  
  # Define keywords to check for estrogen, testosterone, or androgen relations
  hormone_keywords <- c("estrogen", "androgen", "testosterone")
  
  # Loop through each protein and query UniProt (or similar database)
  for (i in 1:nrow(protein_data7)) {
    protein_id7 <- protein_data7$GeneName[i]  # assuming Protein column contains UniProt IDs
    result7 <- query_uniprot(protein_id7)
    
    if (!is.null(result7)) {
      # Check fields for hormone-related keywords
      description_text <- tolower(paste(result7$keywords, result7$comments, result7$functions, collapse = " "))
      if (any(grepl(paste(hormone_keywords, collapse = "|"), description_text))) {
        protein_data7$Hormone_Related[i] <- TRUE
      } else {
        protein_data7$Hormone_Related[i] <- FALSE
      }
    } else {
      protein_data7$Hormone_Related[i] <- NA
    }
  }
  
  #Summary
  summary_counts7 <- table(protein_data7$Hormone_Related, useNA = "ifany")
  
  # Display counts
  print(summary_counts7)
  
  # Filter for Male SW LM hormone-related proteins
  hormone_related_proteins7 <- protein_data7 %>% filter(Hormone_Related == TRUE)
  
  # Save the filtered male sw lm proteins to a CSV file
  write.csv(hormone_related_proteins7, "hormone_related_proteins_male_sw_lm_12_2_24.csv", row.names = FALSE)
  
  # --- Male SW AD ---
  # Load CSV file of Male SW AD proteins
  protein_data8 <- read.csv("C:/Users/chaos/Downloads/MSWAD_Intersect_10_18_24.csv", fileEncoding = "UTF-8")
  
  # Initialize a new column for hormone relation
  protein_data8$Hormone_Related <- NA
  
  # Define keywords to check for estrogen, testosterone, or androgen relations
  hormone_keywords <- c("estrogen", "androgen", "testosterone")
  
  # Loop through each protein and query UniProt (or similar database)
  for (i in 1:nrow(protein_data8)) {
    protein_id8 <- protein_data8$GeneName[i]  # assuming Protein column contains UniProt IDs
    result8 <- query_uniprot(protein_id8)
    
    if (!is.null(result8)) {
      # Check fields for hormone-related keywords
      description_text <- tolower(paste(result8$keywords, result8$comments, result8$functions, collapse = " "))
      if (any(grepl(paste(hormone_keywords, collapse = "|"), description_text))) {
        protein_data8$Hormone_Related[i] <- TRUE
      } else {
        protein_data8$Hormone_Related[i] <- FALSE
      }
    } else {
      protein_data8$Hormone_Related[i] <- NA
    }
  }
  
  #Summary
  summary_counts8 <- table(protein_data8$Hormone_Related, useNA = "ifany")
  
  # Display counts
  print(summary_counts8)
  
  # Filter for Male SW AD hormone-related proteins
  hormone_related_proteins8 <- protein_data8 %>% filter(Hormone_Related == TRUE)
  # Save the filtered male sw ad proteins to a CSV file
  write.csv(hormone_related_proteins8, "hormone_related_proteins_male_sw_ad_12_2_24.csv", row.names = FALSE)

  # --- Analysis 1: Gene Expression for All Proteins in All Groups ---
  # Load libraries
  library(tidyverse)
  
  # -- Females --
  # Female DS LM
  f_ds_lm <-read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_lm_12_2_24.csv")
  
  # Reshape to use the Fitted.TP columns
  f_ds_lm_long <- f_ds_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")))
  
  #Plot and visualize
  ggplot(f_ds_lm_long, aes(x = Time_Point, y = Gene_Expression, group = GeneName)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") + # Facet by gene name
    labs(title = " Female DS LM Gene Expression Over Time (Fitted Time Points)",
         x = "Time Point",
         y = "Gene Expression") +
    theme_minimal()
  
  #Female DS AD
  f_ds_ad <-read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_ad_12_2_24.csv")
  
  # Reshape to use the Fitted.TP columns
  f_ds_ad_long <- f_ds_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")))
  
  #Plot and visualize
  ggplot(f_ds_ad_long, aes(x = Time_Point, y = Gene_Expression, group = GeneName)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") + # Facet by gene name
    labs(title = " Female DS AD Gene Expression Over Time (Fitted Time Points)",
         x = "Time Point",
         y = "Gene Expression") +
    theme_minimal()
  
  # Female SW LM
  f_sw_lm <-read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_lm_12_2_24.csv")
  
  # Reshape to use the Fitted.TP columns
  f_sw_lm_long <- f_sw_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")))
  
  # Plot and visualize
  ggplot(f_sw_lm_long, aes(x = Time_Point, y = Gene_Expression, group = GeneName)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") + # Facet by gene name
    labs(title = " Female SW LM Gene Expression Over Time (Fitted Time Points)",
         x = "Time Point",
         y = "Gene Expression") +
    theme_minimal()
  
  # Female SW AD
  f_sw_ad <-read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_ad_12_2_24.csv")
  
  # Reshape to use the Fitted.TP columns
  f_sw_ad_long <- f_sw_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")))
  
  # Plot and visualize
  ggplot(f_sw_ad_long, aes(x = Time_Point, y = Gene_Expression, group = GeneName)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") + # Facet by gene name
    labs(title = " Female SW AD Gene Expression Over Time (Fitted Time Points)",
         x = "Time Point",
         y = "Gene Expression") +
    theme_minimal()
  
  
  # -- Males --
  # Male DS LM
  m_ds_lm <-read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_lm_12_2_24.csv")
  
  # Reshape to use the Fitted.TP columns
  m_ds_lm_long <- m_ds_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")))
  
  # Plot and visualize
  ggplot(m_ds_lm_long, aes(x = Time_Point, y = Gene_Expression, group = GeneName)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") + # Facet by gene name
    labs(title = " Male DS LM Gene Expression Over Time (Fitted Time Points)",
         x = "Time Point",
         y = "Gene Expression") +
    theme_minimal()
  
  # Male DS AD
  m_ds_ad <-read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_ad_12_2_24.csv")
  
  # Reshape to use the Fitted.TP columns
  m_ds_ad_long <- m_ds_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")))
  
  # Plot and visualize
  ggplot(m_ds_ad_long, aes(x = Time_Point, y = Gene_Expression, group = GeneName)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") + # Facet by gene name
    labs(title = " Male DS AD Gene Expression Over Time (Fitted Time Points)",
         x = "Time Point",
         y = "Gene Expression") +
    theme_minimal()
 
  # Male SW LM 
  m_sw_lm <-read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_lm_12_2_24.csv")
  
  # Reshape to use the Fitted.TP columns
  m_sw_lm_long <- m_sw_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")))
  
  # Plot and visualize
  ggplot(m_sw_lm_long, aes(x = Time_Point, y = Gene_Expression, group = GeneName)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") + # Facet by gene name
    labs(title = " Male SW LM Gene Expression Over Time (Fitted Time Points)",
         x = "Time Point",
         y = "Gene Expression") +
    theme_minimal()
  
  # Male SW AD
  m_sw_ad <-read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_ad_12_2_24.csv")
  
  # Reshape to use the Fitted.TP columns
  m_sw_ad_long <- m_sw_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")))
  
  # Plot and visualize
  ggplot(m_sw_ad_long, aes(x = Time_Point, y = Gene_Expression, group = GeneName)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") + # Facet by gene name
    labs(title = " Male SW AD Gene Expression Over Time (Fitted Time Points)",
         x = "Time Point",
         y = "Gene Expression") +
    theme_minimal()
  
  # -- Statistical significance --
  # - Assigning Genes as Estrogen, Testosterone, Androgen, or More -
  # Load necessary libraries
  library(dplyr)
  
  # Read all datasets
  f_ds_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_lm_12_2_24.csv")
  f_ds_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_ad_12_2_24.csv")
  f_sw_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_lm_12_2_24.csv")
  f_sw_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_ad_12_2_24.csv")
  m_ds_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_lm_12_2_24.csv")
  m_ds_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_ad_12_2_24.csv")
  m_sw_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_lm_12_2_24.csv")
  m_sw_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_ad_12_2_24.csv")
  
  # Add a 'Source' column to each dataset to identify which dataset they belong to
  f_ds_lm$Source <- "f_ds_lm"
  f_ds_ad$Source <- "f_ds_ad"
  f_sw_lm$Source <- "f_sw_lm"
  f_sw_ad$Source <- "f_sw_ad"
  m_ds_lm$Source <- "m_ds_lm"
  m_ds_ad$Source <- "m_ds_ad"
  m_sw_lm$Source <- "m_sw_lm"
  m_sw_ad$Source <- "m_sw_ad"
  
  # Combine all datasets into one
  combined_data <- bind_rows(f_ds_lm, f_ds_ad, f_sw_lm, f_sw_ad, 
                             m_ds_lm, m_ds_ad, m_sw_lm, m_sw_ad)
  
  # Define the categories and their corresponding gene names
  estrogen_genes <- c("D3YXK2", "E9Q394", "O35129", "O54941", "O70503", "P18572", "P84091", "P97765", "Q01279", "Q14AX6", "Q3TVI8", "Q3U0B3", "Q3UGY8", "Q3UL36", "Q61387", "Q61510", "Q64127", "Q6PAQ4", "Q6PDK2", "Q6ZQM8", "Q7TQI3", "Q8BX02", "Q8BYR2", "Q8CCN5", "Q8K078", "Q8K3J5", "Q8K4B0", "Q91YS8", "Q9CR16", "Q9CY57", "Q9D071", "Q9DBD5", "Q9JIF0", "Q9JJ28", "Q9JLJ0", "Q8BVP5")
  testosterone_genes <- c("P30115", "P50637", "Q62433", "Q9WUP4", "Q9WTL8")
  androgen_genes <- c("O88939", "O89079", "P50580", "P62962", "P98078", "Q3TLD5", "Q5U4H9", "Q69ZK6", "Q6PCM1", "Q6ZQ88", "Q8R326", "Q8VD75", "Q99LX0", "Q99N69", "Q99NG0", "Q9CY21", "Q9D8T7", "Q9JI90", "Q9WU42")
  morethanone_genes <- c("A2CG63", "B2RWS6", "B9EKI3", "F8VPQ2", "O88736", "P11930", "P49117", "P69566", "Q501J6", "Q5SVQ0", "Q61656", "Q64429", "Q64435", "Q6VN19", "Q80YR5", "Q8R3E3", "Q91YR7", "Q922B1", "Q99L04", "Q99LB2", "Q9CQK7", "Q9CXR1", "Q9EQ06", "Q9QXN3", "Q9WTZ0", "Q8BP71")
  
  # Add a category column based on GeneName
  combined_data <- combined_data %>%
    mutate(category = case_when(
      GeneName %in% estrogen_genes ~ "Estrogen",
      GeneName %in% testosterone_genes ~ "Testosterone",
      GeneName %in% androgen_genes ~ "Androgen",
      GeneName %in% morethanone_genes ~ "More Than One",
      TRUE ~ "Other"
    ))
  
  # Filter and arrange by category and P.Value, keeping the Source
  top_genes <- combined_data %>%
    filter(category %in% c("Estrogen", "Testosterone", "Androgen", "More Than One")) %>%
    group_by(category, Source) %>%
    arrange(category, Source, P.Value) %>%
    slice_head(n = 30) %>%
    select(GeneName, category, Source, P.Value)
  
  # View the result
  print(top_genes)
 
  # Load necessary libraries
  library(dplyr)
  library(grid)
  library(ggplot2)
  
  # Step 1: Identify and count overlaps by gene across sources
  overlap_genes <- top_genes %>%
    group_by(GeneName) %>%
    summarise(sources = list(unique(Source))) %>%
    filter(length(sources) > 1)  # Keep genes that appear in more than one source
  
  # View the overlaps (genes that appear in more than one source)
  print(overlap_genes)
  
  
  # --- Estrogen-Only Genes ---
  # -- Females --
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  f_ds_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_lm_12_2_24.csv")
  f_ds_lm_long <- f_ds_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS LM")
  
  # -- DS AD --
  f_ds_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_ad_12_2_24.csv")
  f_ds_ad_long <- f_ds_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS AD")
  
  # -- SW LM --
  f_sw_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_lm_12_2_24.csv")
  f_sw_lm_long <- f_sw_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW LM")
  
  # -- SW AD --
  f_sw_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_ad_12_2_24.csv")
  f_sw_ad_long <- f_sw_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW AD")
  
  # Filter for the five genes of interest
  estrogen_genes_of_interest <- c("P97765", "Q3TVI8", "Q3U0B3", "Q64127", "Q8BYR2")
  f_ds_lm_long <- f_ds_lm_long %>% filter(GeneName %in% estrogen_genes_of_interest)
  f_ds_ad_long <- f_ds_ad_long %>% filter(GeneName %in% estrogen_genes_of_interest)
  f_sw_lm_long <- f_sw_lm_long %>% filter(GeneName %in% estrogen_genes_of_interest)
  f_sw_ad_long <- f_sw_ad_long %>% filter(GeneName %in% estrogen_genes_of_interest)
  
  # Combine all groups into one dataset
  estrogen_combined_data <- bind_rows(f_ds_lm_long, f_ds_ad_long, f_sw_lm_long, f_sw_ad_long)
  
  # Plot gene expression over time for the four groups
  ggplot(estrogen_combined_data, aes(x = Time_Point, y = Gene_Expression, group = interaction(GeneName, Group), color = Group)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") +  # Facet by gene name
    labs(title = "Female Estrogen-Related Gene Expression Over Time",
         x = "Time Point",
         y = "Gene Expression") +
    scale_color_manual(values = c("DS LM" = "darkblue", "DS AD" = "lightblue", 
                                  "SW LM" = "red", "SW AD" = "orange")) +  # Set the colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  # Summarize the combined dataset by GeneName and Group
  estrogen_summary <- estrogen_combined_data %>%
    group_by(GeneName, Group) %>%
    summarize(
      Mean_Expression = mean(Gene_Expression, na.rm = TRUE),
      SD_Expression = sd(Gene_Expression, na.rm = TRUE),
      Max_Expression = max(Gene_Expression, na.rm = TRUE),
      Min_Expression = min(Gene_Expression, na.rm = TRUE),
      .groups = "drop"
    )
  # View the summary table
  print(estrogen_summary)
  
  
  # -- Males --
  # -- DS LM --
  m_ds_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_lm_12_2_24.csv")
  m_ds_lm_long <- m_ds_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS LM")
  
  # -- DS AD --
  m_ds_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_ad_12_2_24.csv")
  m_ds_ad_long <- m_ds_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS AD")
  
  # -- SW LM --
  m_sw_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_lm_12_2_24.csv")
  m_sw_lm_long <- m_sw_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW LM")
  
  # -- SW AD --
  m_sw_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_ad_12_2_24.csv")
  m_sw_ad_long <- m_sw_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW AD")
  
  # Filter for the five genes of interest
  estrogen_genes_of_interest <- c("P97765", "Q3TVI8", "Q3U0B3", "Q64127", "Q8BYR2")
  m_ds_lm_long <- m_ds_lm_long %>% filter(GeneName %in% estrogen_genes_of_interest)
  m_ds_ad_long <- m_ds_ad_long %>% filter(GeneName %in% estrogen_genes_of_interest)
  m_sw_lm_long <- m_sw_lm_long %>% filter(GeneName %in% estrogen_genes_of_interest)
  m_sw_ad_long <- m_sw_ad_long %>% filter(GeneName %in% estrogen_genes_of_interest)
  
  # Combine all groups into one dataset
  male_estrogen_combined_data <- bind_rows(m_ds_lm_long, m_ds_ad_long, m_sw_lm_long, m_sw_ad_long)
  
  # Plot gene expression over time for the four groups
  ggplot(male_estrogen_combined_data, aes(x = Time_Point, y = Gene_Expression, group = interaction(GeneName, Group), color = Group)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") +  # Facet by gene name
    labs(title = "Male Estrogen-Related Gene Expression Over Time",
         x = "Time Point",
         y = "Gene Expression") +
    scale_color_manual(values = c("DS LM" = "darkblue", "DS AD" = "lightblue", 
                                  "SW LM" = "red", "SW AD" = "orange")) +  # Set the colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  # -- Estrogen All --
  # -- Function to process datasets --
  process_estrogen_data <- function(file_path, sex, group_label) {
    read.csv(file_path) %>%
      pivot_longer(
        cols = starts_with("Fitted.TP"), 
        names_to = "Time_Point", 
        values_to = "Gene_Expression"
      ) %>%
      mutate(
        Time_Point = factor(
          Time_Point, 
          levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                     "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")
        ),
        Group = group_label,
        Sex = sex
      )
  }
  
  # -- Process male datasets --
  m_ds_lm <- process_estrogen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_lm_12_2_24.csv", 
    "Male", "DS LM"
  )
  m_ds_ad <- process_estrogen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_ad_12_2_24.csv", 
    "Male", "DS AD"
  )
  m_sw_lm <- process_estrogen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_lm_12_2_24.csv", 
    "Male", "SW LM"
  )
  m_sw_ad <- process_estrogen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_ad_12_2_24.csv", 
    "Male", "SW AD"
  )
  
  # -- Process female datasets --
  f_ds_lm <- process_estrogen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_lm_12_2_24.csv", 
    "Female", "DS LM"
  )
  f_ds_ad <- process_estrogen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_ad_12_2_24.csv", 
    "Female", "DS AD"
  )
  f_sw_lm <- process_estrogen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_lm_12_2_24.csv", 
    "Female", "SW LM"
  )
  f_sw_ad <- process_estrogen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_ad_12_2_24.csv", 
    "Female", "SW AD"
  )
  
  # -- Define the color scheme --
  color_scheme <- c(
    "Male DS LM" = "purple",
    "Female DS LM" = "darkblue",
    "Male DS AD" = "green",
    "Female DS AD" = "lightblue",
    "Male SW LM" = "pink",
    "Female SW LM" = "red",
    "Male SW AD" = "gold",  
    "Female SW AD" = "orange"
  )
  
  # -- Combine data and include Group-Sex column --
  estrogen_all <- bind_rows(m_ds_lm, m_ds_ad, m_sw_lm, m_sw_ad, f_ds_lm, f_ds_ad, f_sw_lm, f_sw_ad) %>%
    mutate(Group_Sex = paste(Sex, Group))
  
  # -- Filter for testosterone-related genes --
  estrogen_genes <- c("P97765", "Q3TVI8", "Q3U0B3", "Q64127", "Q8BYR2")
  estrogen_all <- estrogen_all %>% filter(GeneName %in% estrogen_genes)
  
  # -- Plot gene expression  --
  ggplot(estrogen_all, aes(
    x = Time_Point, 
    y = Gene_Expression, 
    group = interaction(GeneName, Group_Sex), 
    color = Group_Sex
  )) +
    geom_line() +
    geom_point() +
    facet_grid(Group ~ GeneName, scales = "free_y") +  # Facet by group and gene name
    labs(
      title = "Comparison of Estrogen-related Gene Expression by Sex and Group",
      x = "Time Point",
      y = "Gene Expression"
    ) +
    scale_color_manual(values = color_scheme) +  # Apply the custom color scheme
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
      legend.title = element_blank()  # Remove legend title for cleaner appearance
    )
  
  # --- Testosterone-Only Genes ---
  # -- Females --
  # -- DS LM --
  f_ds_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_lm_12_2_24.csv")
  f_ds_lm_long <- f_ds_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS LM")
  
  # -- DS AD --
  f_ds_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_ad_12_2_24.csv")
  f_ds_ad_long <- f_ds_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS AD")
  
  # -- SW LM --
  f_sw_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_lm_12_2_24.csv")
  f_sw_lm_long <- f_sw_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW LM")
  
  # -- SW AD --
  f_sw_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_ad_12_2_24.csv")
  f_sw_ad_long <- f_sw_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW AD")
  
  # Filter for the five genes of interest
  genes_of_interest <- c("P30115", "P50637", "Q62433", "Q9WUP4", "Q9WTL8")
  f_ds_lm_long <- f_ds_lm_long %>% filter(GeneName %in% genes_of_interest)
  f_ds_ad_long <- f_ds_ad_long %>% filter(GeneName %in% genes_of_interest)
  f_sw_lm_long <- f_sw_lm_long %>% filter(GeneName %in% genes_of_interest)
  f_sw_ad_long <- f_sw_ad_long %>% filter(GeneName %in% genes_of_interest)
  
  # Combine all groups into one dataset
  testosterone_combined_data_1 <- bind_rows(f_ds_lm_long, f_ds_ad_long, f_sw_lm_long, f_sw_ad_long)
  
  # Plot gene expression over time for the four groups
  ggplot(testosterone_combined_data_1, aes(x = Time_Point, y = Gene_Expression, group = interaction(GeneName, Group), color = Group)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") +  # Facet by gene name
    labs(title = "Female Testosterone-Related Gene Expression Over Time",
         x = "Time Point",
         y = "Gene Expression") +
    scale_color_manual(values = c("DS LM" = "darkblue", "DS AD" = "lightblue", 
                                  "SW LM" = "red", "SW AD" = "orange")) +  # Set the colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  # -- Males --
   # -- DS LM --
  m_ds_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_lm_12_2_24.csv")
  m_ds_lm_long <- m_ds_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS LM")
  
  # -- DS AD --
  m_ds_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_ad_12_2_24.csv")
  m_ds_ad_long <- m_ds_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS AD")
  
  # -- SW LM --
  m_sw_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_lm_12_2_24.csv")
  m_sw_lm_long <- m_sw_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW LM")
  
  # -- SW AD --
  m_sw_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_ad_12_2_24.csv")
  m_sw_ad_long <- m_sw_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW AD")
  
  # Filter for the five genes of interest
  genes_of_interest <- c("P30115", "P50637", "Q62433", "Q9WUP4", "Q9WTL8")
  m_ds_lm_long <- m_ds_lm_long %>% filter(GeneName %in% genes_of_interest)
  m_ds_ad_long <- m_ds_ad_long %>% filter(GeneName %in% genes_of_interest)
  m_sw_lm_long <- m_sw_lm_long %>% filter(GeneName %in% genes_of_interest)
  m_sw_ad_long <- m_sw_ad_long %>% filter(GeneName %in% genes_of_interest)
  
  # Combine all groups into one dataset
  testosterone_combined_data <- bind_rows(m_ds_lm_long, m_ds_ad_long, m_sw_lm_long, m_sw_ad_long)
  
  # Plot gene expression over time for the four groups
  ggplot(testosterone_combined_data, aes(x = Time_Point, y = Gene_Expression, group = interaction(GeneName, Group), color = Group)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") +  # Facet by gene name
    labs(title = "Male Testosterone-Related Gene Expression Over Time",
         x = "Time Point",
         y = "Gene Expression") +
    scale_color_manual(values = c("DS LM" = "darkblue", "DS AD" = "lightblue", 
                                  "SW LM" = "red", "SW AD" = "orange")) +  # Set the colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  # -- Testosterone All --
  # -- Function to process datasets --
  process_testosterone_data <- function(file_path, sex, group_label) {
    read.csv(file_path) %>%
      pivot_longer(
        cols = starts_with("Fitted.TP"), 
        names_to = "Time_Point", 
        values_to = "Gene_Expression"
      ) %>%
      mutate(
        Time_Point = factor(
          Time_Point, 
          levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                     "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")
        ),
        Group = group_label,
        Sex = sex
      )
  }
  
  # -- Process male datasets --
  m_ds_lm <- process_testosterone_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_lm_12_2_24.csv", 
    "Male", "DS LM"
  )
  m_ds_ad <- process_testosterone_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_ad_12_2_24.csv", 
    "Male", "DS AD"
  )
  m_sw_lm <- process_testosterone_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_lm_12_2_24.csv", 
    "Male", "SW LM"
  )
  m_sw_ad <- process_testosterone_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_ad_12_2_24.csv", 
    "Male", "SW AD"
  )
  
  # -- Process female datasets --
  f_ds_lm <- process_testosterone_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_lm_12_2_24.csv", 
    "Female", "DS LM"
  )
  f_ds_ad <- process_testosterone_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_ad_12_2_24.csv", 
    "Female", "DS AD"
  )
  f_sw_lm <- process_testosterone_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_lm_12_2_24.csv", 
    "Female", "SW LM"
  )
  f_sw_ad <- process_testosterone_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_ad_12_2_24.csv", 
    "Female", "SW AD"
  )
  
  # -- Define the color scheme --
  color_scheme <- c(
    "Male DS LM" = "purple",
    "Female DS LM" = "darkblue",
    "Male DS AD" = "green",
    "Female DS AD" = "lightblue",
    "Male SW LM" = "pink",
    "Female SW LM" = "red",
    "Male SW AD" = "gold",  
    "Female SW AD" = "orange"
  )
  
  # -- Combine data and include Group-Sex column --
  testosterone_all <- bind_rows(m_ds_lm, m_ds_ad, m_sw_lm, m_sw_ad, f_ds_lm, f_ds_ad, f_sw_lm, f_sw_ad) %>%
    mutate(Group_Sex = paste(Sex, Group))
  
  # -- Filter for testosterone-related genes --
  testosterone_genes <- c("P30115", "P50637", "Q62433", "Q9WUP4", "Q9WTL8")
  testosterone_all <- testosterone_all %>% filter(GeneName %in% testosterone_genes)
  
  # -- Plot gene expression --
  ggplot(testosterone_all, aes(
    x = Time_Point, 
    y = Gene_Expression, 
    group = interaction(GeneName, Group_Sex), 
    color = Group_Sex
  )) +
    geom_line() +
    geom_point() +
    facet_grid(Group ~ GeneName, scales = "free_y") +  # Facet by group and gene name
    labs(
      title = "Comparison of Testosterone-related Gene Expression by Sex and Group",
      x = "Time Point",
      y = "Gene Expression"
    ) +
    scale_color_manual(values = color_scheme) +  # Apply the custom color scheme
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
      legend.title = element_blank()  # Remove legend title for cleaner appearance
    )
  
  # --- Androgen-Only Genes ---
  # -- Females --
  # -- DS LM --
  library(dplyr)
  library(tidyr)
  f_ds_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_lm_12_2_24.csv")
  f_ds_lm_long <- f_ds_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS LM")
  
  # -- DS AD --
  f_ds_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_ad_12_2_24.csv")
  f_ds_ad_long <- f_ds_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS AD")
  
  # -- SW LM --
  f_sw_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_lm_12_2_24.csv")
  f_sw_lm_long <- f_sw_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW LM")
  
  # -- SW AD --
  f_sw_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_ad_12_2_24.csv")
  f_sw_ad_long <- f_sw_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW AD")
  
  # Filter for the five genes of interest
  androgen_genes_of_interest <- c("O88939", "O89079", "P50580", "P62962", "P98078")
  f_ds_lm_long <- f_ds_lm_long %>% filter(GeneName %in% androgen_genes_of_interest)
  f_ds_ad_long <- f_ds_ad_long %>% filter(GeneName %in% androgen_genes_of_interest)
  f_sw_lm_long <- f_sw_lm_long %>% filter(GeneName %in% androgen_genes_of_interest)
  f_sw_ad_long <- f_sw_ad_long %>% filter(GeneName %in% androgen_genes_of_interest)
  
  # Combine all groups into one dataset
  androgen_combined_data <- bind_rows(f_ds_lm_long, f_ds_ad_long, f_sw_lm_long, f_sw_ad_long)
  
  # Plot gene expression over time for the four groups
  library(ggplot2)
  ggplot(androgen_combined_data, aes(x = Time_Point, y = Gene_Expression, group = interaction(GeneName, Group), color = Group)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") +  # Facet by gene name
    labs(title = "Female Androgen-Related Gene Expression Over Time",
         x = "Time Point",
         y = "Gene Expression") +
    scale_color_manual(values = c("DS LM" = "darkblue", "DS AD" = "lightblue", 
                                  "SW LM" = "red", "SW AD" = "orange")) +  # Set the colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  # -- Males --
  # -- DS LM --
  m_ds_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_lm_12_2_24.csv")
  m_ds_lm_long <- m_ds_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS LM")
  
  # -- DS AD --
  m_ds_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_ad_12_2_24.csv")
  m_ds_ad_long <- m_ds_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS AD")
  
  # -- SW LM --
  m_sw_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_lm_12_2_24.csv")
  m_sw_lm_long <- m_sw_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW LM")
  
  # -- SW AD --
  m_sw_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_ad_12_2_24.csv")
  m_sw_ad_long <- m_sw_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW AD")
  
  # Filter for the five genes of interest
  androgen_genes_of_interest <- c("O88939", "O89079", "P50580", "P62962", "P98078")
  m_ds_lm_long <- m_ds_lm_long %>% filter(GeneName %in% androgen_genes_of_interest)
  m_ds_ad_long <- m_ds_ad_long %>% filter(GeneName %in% androgen_genes_of_interest)
  m_sw_lm_long <- m_sw_lm_long %>% filter(GeneName %in% androgen_genes_of_interest)
  m_sw_ad_long <- m_sw_ad_long %>% filter(GeneName %in% androgen_genes_of_interest)
  
  # Combine all groups into one dataset
  male_androgen_combined_data <- bind_rows(m_ds_lm_long, m_ds_ad_long, m_sw_lm_long, m_sw_ad_long)
  
  # Plot gene expression over time for the four groups
  ggplot(male_androgen_combined_data, aes(x = Time_Point, y = Gene_Expression, group = interaction(GeneName, Group), color = Group)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") +  # Facet by gene name
    labs(title = "Male Androgen-Related Gene Expression Over Time",
         x = "Time Point",
         y = "Gene Expression") +
    scale_color_manual(values = c("DS LM" = "darkblue", "DS AD" = "lightblue", 
                                  "SW LM" = "red", "SW AD" = "orange")) +  # Set the colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  # -- Androgen All --
  # -- Function to process datasets --
  process_androgen_data <- function(file_path, sex, group_label) {
    read.csv(file_path) %>%
      pivot_longer(
        cols = starts_with("Fitted.TP"), 
        names_to = "Time_Point", 
        values_to = "Gene_Expression"
      ) %>%
      mutate(
        Time_Point = factor(
          Time_Point, 
          levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                     "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")
        ),
        Group = group_label,
        Sex = sex
      )
  }
  
  # -- Process male datasets --
  m_ds_lm <- process_androgen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_lm_12_2_24.csv", 
    "Male", "DS LM"
  )
  m_ds_ad <- process_androgen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_ad_12_2_24.csv", 
    "Male", "DS AD"
  )
  m_sw_lm <- process_androgen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_lm_12_2_24.csv", 
    "Male", "SW LM"
  )
  m_sw_ad <- process_androgen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_ad_12_2_24.csv", 
    "Male", "SW AD"
  )
  
  # -- Process female datasets --
  f_ds_lm <- process_androgen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_lm_12_2_24.csv", 
    "Female", "DS LM"
  )
  f_ds_ad <- process_androgen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_ad_12_2_24.csv", 
    "Female", "DS AD"
  )
  f_sw_lm <- process_androgen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_lm_12_2_24.csv", 
    "Female", "SW LM"
  )
  f_sw_ad <- process_androgen_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_ad_12_2_24.csv", 
    "Female", "SW AD"
  )
  
  # -- Define the color scheme --
  color_scheme <- c(
    "Male DS LM" = "purple",
    "Female DS LM" = "darkblue",
    "Male DS AD" = "green",
    "Female DS AD" = "lightblue",
    "Male SW LM" = "pink",
    "Female SW LM" = "red",
    "Male SW AD" = "gold",  
    "Female SW AD" = "orange"
  )
  
  # -- Combine data and include Group-Sex column --
  androgen_all <- bind_rows(m_ds_lm, m_ds_ad, m_sw_lm, m_sw_ad, f_ds_lm, f_ds_ad, f_sw_lm, f_sw_ad) %>%
    mutate(Group_Sex = paste(Sex, Group))
  
  # -- Filter for testosterone-related genes --
  androgen_genes <- c("O88939", "O89079", "P50580", "P62962", "P98078")
  androgen_all <- androgen_all %>% filter(GeneName %in% androgen_genes)
  
  # -- Plot gene expression --
  ggplot(androgen_all, aes(
    x = Time_Point, 
    y = Gene_Expression, 
    group = interaction(GeneName, Group_Sex), 
    color = Group_Sex
  )) +
    geom_line() +
    geom_point() +
    facet_grid(Group ~ GeneName, scales = "free_y") +  # Facet by group and gene name
    labs(
      title = "Comparison of Androgen-related Gene Expression by Sex and Group",
      x = "Time Point",
      y = "Gene Expression"
    ) +
    scale_color_manual(values = color_scheme) +  # Apply the custom color scheme
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
      legend.title = element_blank()  # Remove legend title for cleaner appearance
    )
  
  # --- More Than One Genes ---
  # -- Females --
  # -- DS LM --
  library(dplyr)
  library(tidyr)
  f_ds_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_lm_12_2_24.csv")
  f_ds_lm_long <- f_ds_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS LM")
  
  # -- DS AD --
  f_ds_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_ad_12_2_24.csv")
  f_ds_ad_long <- f_ds_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS AD")
  
  # -- SW LM --
  f_sw_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_lm_12_2_24.csv")
  f_sw_lm_long <- f_sw_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW LM")
  
  # -- SW AD --
  f_sw_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_ad_12_2_24.csv")
  f_sw_ad_long <- f_sw_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW AD")
  
  # Filter for the five genes of interest
  combination_genes_of_interest <- c("P11930", "Q61656", "Q64429", "Q8BP71", "Q8R3E3")
  f_ds_lm_long <- f_ds_lm_long %>% filter(GeneName %in% combination_genes_of_interest)
  f_ds_ad_long <- f_ds_ad_long %>% filter(GeneName %in% combination_genes_of_interest)
  f_sw_lm_long <- f_sw_lm_long %>% filter(GeneName %in% combination_genes_of_interest)
  f_sw_ad_long <- f_sw_ad_long %>% filter(GeneName %in% combination_genes_of_interest)
  
  # Combine all groups into one dataset
  morethanone_combined_data <- bind_rows(f_ds_lm_long, f_ds_ad_long, f_sw_lm_long, f_sw_ad_long)
  
  # Plot gene expression over time for the four groups
  library(ggplot2)
  ggplot(morethanone_combined_data, aes(x = Time_Point, y = Gene_Expression, group = interaction(GeneName, Group), color = Group)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") +  # Facet by gene name
    labs(title = "Female More Than One-Related Gene Expression Over Time",
         x = "Time Point",
         y = "Gene Expression") +
    scale_color_manual(values = c("DS LM" = "darkblue", "DS AD" = "lightblue", 
                                  "SW LM" = "red", "SW AD" = "orange")) +  # Set the colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  # -- Males --
  # -- DS LM --
  m_ds_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_lm_12_2_24.csv")
  m_ds_lm_long <- m_ds_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS LM")
  
  # -- DS AD --
  m_ds_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_ad_12_2_24.csv")
  m_ds_ad_long <- m_ds_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "DS AD")
  
  # -- SW LM --
  m_sw_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_lm_12_2_24.csv")
  m_sw_lm_long <- m_sw_lm %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW LM")
  
  # -- SW AD --
  m_sw_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_ad_12_2_24.csv")
  m_sw_ad_long <- m_sw_ad %>%
    pivot_longer(cols = starts_with("Fitted.TP"), 
                 names_to = "Time_Point", 
                 values_to = "Gene_Expression") %>%
    mutate(Time_Point = factor(Time_Point, 
                               levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                                          "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")),
           Group = "SW AD")
  
  # Filter for the five genes of interest
  combination_genes_of_interest <- c("P11930", "Q61656", "Q64429", "Q8BP71", "Q8R3E3")
  m_ds_lm_long <- m_ds_lm_long %>% filter(GeneName %in% combination_genes_of_interest)
  m_ds_ad_long <- m_ds_ad_long %>% filter(GeneName %in% combination_genes_of_interest)
  m_sw_lm_long <- m_sw_lm_long %>% filter(GeneName %in% combination_genes_of_interest)
  m_sw_ad_long <- m_sw_ad_long %>% filter(GeneName %in% combination_genes_of_interest)
  
  # Combine all groups into one dataset
  male_morethanone_combined_data <- bind_rows(m_ds_lm_long, m_ds_ad_long, m_sw_lm_long, m_sw_ad_long)
  
  # Plot gene expression over time for the four groups
  ggplot(male_morethanone_combined_data, aes(x = Time_Point, y = Gene_Expression, group = interaction(GeneName, Group), color = Group)) +
    geom_line() +
    geom_point() +
    facet_wrap(~GeneName, scales = "free_y") +  # Facet by gene name
    labs(title = "Male More Than One-Related Gene Expression Over Time",
         x = "Time Point",
         y = "Gene Expression") +
    scale_color_manual(values = c("DS LM" = "darkblue", "DS AD" = "lightblue", 
                                  "SW LM" = "red", "SW AD" = "orange")) +  # Set the colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  # -- More Than One All --
  # -- Function to process datasets --
  process_combination_data <- function(file_path, sex, group_label) {
    read.csv(file_path) %>%
      pivot_longer(
        cols = starts_with("Fitted.TP"), 
        names_to = "Time_Point", 
        values_to = "Gene_Expression"
      ) %>%
      mutate(
        Time_Point = factor(
          Time_Point, 
          levels = c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                     "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")
        ),
        Group = group_label,
        Sex = sex
      )
  }
  
  # -- Process male datasets --
  m_ds_lm <- process_combination_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_lm_12_2_24.csv", 
    "Male", "DS LM"
  )
  m_ds_ad <- process_combination_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_ad_12_2_24.csv", 
    "Male", "DS AD"
  )
  m_sw_lm <- process_combination_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_lm_12_2_24.csv", 
    "Male", "SW LM"
  )
  m_sw_ad <- process_combination_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_ad_12_2_24.csv", 
    "Male", "SW AD"
  )
  
  # -- Process female datasets --
  f_ds_lm <- process_combination_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_lm_12_2_24.csv", 
    "Female", "DS LM"
  )
  f_ds_ad <- process_combination_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_ad_12_2_24.csv", 
    "Female", "DS AD"
  )
  f_sw_lm <- process_combination_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_lm_12_2_24.csv", 
    "Female", "SW LM"
  )
  f_sw_ad <- process_combination_data(
    "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_ad_12_2_24.csv", 
    "Female", "SW AD"
  )
  
  # -- Define the color scheme --
  color_scheme <- c(
    "Male DS LM" = "purple",
    "Female DS LM" = "darkblue",
    "Male DS AD" = "green",
    "Female DS AD" = "lightblue",
    "Male SW LM" = "pink",
    "Female SW LM" = "red",
    "Male SW AD" = "gold",  
    "Female SW AD" = "orange"
  )
  
  # -- Combine data and include Group-Sex column --
  morethanone_all <- bind_rows(m_ds_lm, m_ds_ad, m_sw_lm, m_sw_ad, f_ds_lm, f_ds_ad, f_sw_lm, f_sw_ad) %>%
    mutate(Group_Sex = paste(Sex, Group))
  
  # -- Filter for testosterone-related genes --
  combination_genes <- c("P11930", "Q61656", "Q64429", "Q8BP71", "Q8R3E3")
  morethanone_all <- morethanone_all %>% filter(GeneName %in% combination_genes)
  
  # -- Plot gene expression --
  ggplot(morethanone_all, aes(
    x = Time_Point, 
    y = Gene_Expression, 
    group = interaction(GeneName, Group_Sex), 
    color = Group_Sex
  )) +
    geom_line() +
    geom_point() +
    facet_grid(Group ~ GeneName, scales = "free_y") +  # Facet by group and gene name
    labs(
      title = "Comparison of More Than One-related Gene Expression by Sex and Group",
      x = "Time Point",
      y = "Gene Expression"
    ) +
    scale_color_manual(values = color_scheme) +  # Apply the custom color scheme
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
      legend.title = element_blank()  # Remove legend title for cleaner appearance
    )
  
  # --- Analysis 2: Heat Maps ---
  # --- Heat Maps 1: Combined Test  ---
  # Reshape data for the heatmap 
  install.packages("pheatmap")
  install.packages("tidyverse")
  library(pheatmap)
  library(tidyverse)

  # Define file paths
  file_paths <- list(
    f_ds_lm = "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_lm_12_2_24.csv",
    f_ds_ad = "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_ad_12_2_24.csv",
    f_sw_lm = "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_lm_12_2_24.csv",
    f_sw_ad = "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_ad_12_2_24.csv",
    m_ds_lm = "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_lm_12_2_24.csv",
    m_ds_ad = "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_ad_12_2_24.csv",
    m_sw_lm = "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_lm_12_2_24.csv",
    m_sw_ad = "C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_ad_12_2_24.csv"
  )
  
  # Read all datasets into a named list
  datasets <- lapply(file_paths, read.csv)
  
  # Define gene lists
  gene_lists <- list(
    estrogen = c("P97765", "Q3TVI8", "Q3U0B3", "Q64127", "Q8BYR2"),
    testosterone = c("P30115", "P50637", "Q62433", "Q9WUP4", "Q9WTL8"),
    androgen = c("O88939", "O89079", "P50580", "P62962", "P98078"),
    combination = c("P11930", "Q61656", "Q64429", "Q8BP71", "Q8R3E3")
  )
  
  # Define time points
  time_points <- c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25",
                   "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")
  
  # Validate column names
  validate_columns <- function(dataset, time_points) {
    missing_columns <- setdiff(time_points, colnames(dataset))
    if (length(missing_columns) > 0) {
      stop(paste("Missing columns in dataset:", paste(missing_columns, collapse = ", ")))
    }
  }
  
  # Prepare and generate heatmaps
  generate_individual_heatmap <- function(datasets, gene_list, gene_list_name, time_points) {
    combined_data <- bind_rows(
      lapply(names(datasets), function(group) {
        dataset <- datasets[[group]]
        validate_columns(dataset, time_points) # Ensure required columns exist
        dataset %>%
          filter(GeneName %in% gene_list) %>%
          select(GeneName, all_of(time_points)) %>%
          mutate(Group = group) # Add group label
      })
    )
    
    # Pivot data for heatmap
    heatmap_data <- combined_data %>%
      pivot_longer(cols = -c(GeneName, Group), names_to = "TimePoint", values_to = "Expression") %>%
      unite("RowLabel", Group, GeneName, sep = "_") %>% # Create unique row labels
      pivot_wider(names_from = TimePoint, values_from = Expression) %>%
      column_to_rownames("RowLabel") %>%
      as.data.frame()
    
    # Create heatmap
    pheatmap(
      heatmap_data,
      scale = "row",
      cluster_rows = TRUE,
      cluster_cols = FALSE,
      show_rownames = TRUE,
      show_colnames = TRUE,
      main = paste(gene_list_name, "Gene Expression Heatmap"),
      color = colorRampPalette(c("blue", "yellow"))(100)
    )
  }
  
  # Generate heatmaps for each gene list separately
  generate_individual_heatmap(datasets, gene_lists$estrogen, "Estrogen", time_points)
  generate_individual_heatmap(datasets, gene_lists$testosterone, "Testosterone", time_points)
  generate_individual_heatmap(datasets, gene_lists$androgen, "Androgen", time_points)
  generate_individual_heatmap(datasets, gene_lists$combination, "Combination", time_points)
  
  # --- Heat Maps 2: Individual  ---
  # Load required libraries
  # Load required libraries
  library(tidyverse)
  library(pheatmap)
  
  # Read datasets
  f_ds_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_lm_12_2_24.csv")
  f_ds_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_ds_ad_12_2_24.csv")
  f_sw_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_lm_12_2_24.csv")
  f_sw_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_female_sw_ad_12_2_24.csv")
  m_ds_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_lm_12_2_24.csv")
  m_ds_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_ds_ad_12_2_24.csv")
  m_sw_lm <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_lm_12_2_24.csv")
  m_sw_ad <- read.csv("C:/Users/chaos/OneDrive/Documents/hormone_related_proteins_male_sw_ad_12_2_24.csv")
  
  # Define time points
  time_points <- c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25", 
                   "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")
  
  # Define gene lists for hormone categories
  gene_lists <- list(
    estrogen = c("P97765", "Q3TVI8", "Q3U0B3", "Q64127", "Q8BYR2"),
    testosterone = c("P30115", "P50637", "Q62433", "Q9WUP4", "Q9WTL8"),
    androgen = c("O88939", "O89079", "P50580", "P62962", "P98078"),
    combination = c("P11930", "Q61656", "Q64429", "Q8BP71", "Q8R3E3")
  )
  
  # Custom color palette
  blue_to_yellow <- colorRampPalette(c("blue", "yellow"))(100)
  
  # Function to create heatmap
  generate_heatmap <- function(data, genes, title) {
    filtered_data <- data %>%
      filter(GeneName %in% genes) %>%
      select(GeneName, all_of(time_points)) %>%
      column_to_rownames("GeneName") %>%
      as.data.frame()
    
    if (nrow(filtered_data) == 0) {
      warning(paste("No data available for", title))
      return(NULL)
    }
    
    pheatmap(
      filtered_data,
      scale = "row",
      cluster_rows = TRUE,
      cluster_cols = FALSE,
      show_rownames = TRUE,
      show_colnames = TRUE,
      main = title,
      color = blue_to_yellow
    )
  }
  
  # Iterate over datasets, sex, and hormone categories
  datasets <- list(
    "Female DS LM" = f_ds_lm, "Female DS AD" = f_ds_ad, 
    "Female SW LM" = f_sw_lm, "Female SW AD" = f_sw_ad,
    "Male DS LM" = m_ds_lm, "Male DS AD" = m_ds_ad,
    "Male SW LM" = m_sw_lm, "Male SW AD" = m_sw_ad
  )
  
  for (dataset_name in names(datasets)) {
    for (category in names(gene_lists)) {
      title <- paste(dataset_name, category, "Gene Expression Heatmap")
      generate_heatmap(datasets[[dataset_name]], gene_lists[[category]], title)
    }
  }

  # --- PCA Plots 1: Individual ---
  # Load required libraries
  library(tidyverse)
  library(FactoMineR)
  library(factoextra)

  # Function to generate PCA plot with explained variance
  generate_pca_plot <- function(data, genes, title) {
    # Filter data for the selected genes and time points
    filtered_data <- data %>%
      filter(GeneName %in% genes) %>%
      select(GeneName, all_of(time_points)) %>%
      column_to_rownames("GeneName") %>%
      as.data.frame()
    
    # Check if data is empty
    if (nrow(filtered_data) == 0) {
      warning(paste("No data available for", title))
      return(NULL)
    }
    
    # Perform PCA (scale = TRUE standardizes the data)
    pca_result <- prcomp(filtered_data, scale. = TRUE)
    
    # Extract percentage of variance explained by PC1 and PC2
    pca_var_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2) * 100
    pc1_var <- round(pca_var_explained[1], 2)
    pc2_var <- round(pca_var_explained[2], 2)
    
    # Convert PCA results to a data frame for plotting
    pca_data <- as.data.frame(pca_result$x)
    
    # Create a plot title with variance explained
    plot_title <- paste(title, "- PC1: ", pc1_var, "%, PC2: ", pc2_var, "%", sep = "")
    
    # Plot the first two principal components (PC1 vs. PC2)
    pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2)) +
      geom_point(aes(color = rownames(pca_data))) +  # Use rownames as color (gene names)
      labs(title = plot_title, x = paste("PC1 (", pc1_var, "%)", sep = ""), y = paste("PC2 (", pc2_var, "%)", sep = "")) +
      theme_minimal() +
      theme(legend.position = "none")
    
    return(pca_plot)
  }
  
  # Define time points
  time_points <- c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25",
                   "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")
  
  # Define datasets
  datasets <- list(
    "Female DS LM" = f_ds_lm, "Female DS AD" = f_ds_ad, 
    "Female SW LM" = f_sw_lm, "Female SW AD" = f_sw_ad,
    "Male DS LM" = m_ds_lm, "Male DS AD" = m_ds_ad,
    "Male SW LM" = m_sw_lm, "Male SW AD" = m_sw_ad
  )
  
  # Define gene lists for hormone categories
  gene_lists <- list(
    estrogen = c("P97765", "Q3TVI8", "Q3U0B3", "Q64127", "Q8BYR2"),
    testosterone = c("P30115", "P50637", "Q62433", "Q9WUP4", "Q9WTL8"),
    androgen = c("O88939", "O89079", "P50580", "P62962", "P98078"),
    combination = c("P11930", "Q61656", "Q64429", "Q8BP71", "Q8R3E3")
  )
  
  # PCA plots for each combination
  for (dataset_name in names(datasets)) {
    for (category in names(gene_lists)) {
      title <- paste(dataset_name, category, "PCA Plot")
      pca_plot <- generate_pca_plot(datasets[[dataset_name]], gene_lists[[category]], title)
      
      if (!is.null(pca_plot)) {
        print(pca_plot)  # Print the plot
      }
    }
  }
  
  # --- PCA Plots 2: Combined ---
  # Function to generate PCA plot with explained variance
  generate_pca_plot <- function(data, genes, title) {
    # Filter data for the selected genes and time points
    filtered_data <- data %>%
      filter(GeneName %in% genes) %>%
      select(GeneName, all_of(time_points), Dataset) %>%
      mutate(UniqueID = paste(GeneName, Dataset, sep = "_")) %>%  # Create a unique identifier
      column_to_rownames("UniqueID") %>%  # Use the unique identifier as row names
      as.data.frame()
    
    # Check if data is empty
    if (nrow(filtered_data) == 0) {
      warning(paste("No data available for", title))
      return(NULL)
    }
    
    # Select only numeric columns for PCA
    numeric_data <- filtered_data %>%
      select(-GeneName, -Dataset) %>%  # Remove non-numeric columns
      na.omit()  # Remove rows with NA values
    
    # Ensure that the data is numeric
    numeric_data <- as.data.frame(lapply(numeric_data, as.numeric))
    
    # Perform PCA (scale = TRUE standardizes the data)
    pca_result <- prcomp(numeric_data, scale. = TRUE)
    
    # Extract percentage of variance explained by PC1 and PC2
    pca_var_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2) * 100
    pc1_var <- round(pca_var_explained[1], 2)
    pc2_var <- round(pca_var_explained[2], 2)
    
    # Convert PCA results to a data frame for plotting
    pca_data <- as.data.frame(pca_result$x)
    pca_data$Dataset <- filtered_data$Dataset
    
    # Create a plot title with variance explained
    plot_title <- paste(title, "- PC1: ", pc1_var, "%, PC2: ", pc2_var, "%", sep = "")
    
    # Plot the first two principal components (PC1 vs. PC2)
    pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Dataset)) +
      geom_point() +  # Use points for each gene
      labs(title = plot_title, x = paste("PC1 (", pc1_var, "%)", sep = ""), y = paste("PC2 (", pc2_var, "%)", sep = "")) +
      theme_minimal() +
      theme(legend.position = "top")
    
    return(pca_plot)
  }
  
  # Define time points
  time_points <- c("Fitted.TP16", "Fitted.TP19", "Fitted.TP22", "Fitted.TP25",
                   "Fitted.TP28", "Fitted.TP31", "Fitted.TP34", "Fitted.TP37", "Fitted.TP40")
  
  # Define datasets and add Dataset column to identify the source
  datasets <- list(
    "Female DS LM" = f_ds_lm, "Female DS AD" = f_ds_ad, 
    "Female SW LM" = f_sw_lm, "Female SW AD" = f_sw_ad,
    "Male DS LM" = m_ds_lm, "Male DS AD" = m_ds_ad,
    "Male SW LM" = m_sw_lm, "Male SW AD" = m_sw_ad
  )
  
  # Add Dataset column to each dataset
  f_ds_lm$Dataset <- "Female DS LM"
  f_ds_ad$Dataset <- "Female DS AD"
  f_sw_lm$Dataset <- "Female SW LM"
  f_sw_ad$Dataset <- "Female SW AD"
  m_ds_lm$Dataset <- "Male DS LM"
  m_ds_ad$Dataset <- "Male DS AD"
  m_sw_lm$Dataset <- "Male SW LM"
  m_sw_ad$Dataset <- "Male SW AD"
  
  # Define gene lists for hormone categories
  gene_lists <- list(
    estrogen = c("P97765", "Q3TVI8", "Q3U0B3", "Q64127", "Q8BYR2"),
    testosterone = c("P30115", "P50637", "Q62433", "Q9WUP4", "Q9WTL8"),
    androgen = c("O88939", "O89079", "P50580", "P62962", "P98078"),
    combination = c("P11930", "Q61656", "Q64429", "Q8BP71", "Q8R3E3")
  )
  
  # Function to combine datasets for female and male separately for each hormone category
  combine_female_male_data <- function(female_datasets, male_datasets, hormone_genes) {
    # Filter female datasets
    female_data <- bind_rows(
      lapply(female_datasets, function(ds) {
        ds %>%
          filter(GeneName %in% hormone_genes) %>%
          select(GeneName, all_of(time_points), Dataset) %>%
          mutate(Sex = "Female")
      })
    )
    
    # Filter male datasets
    male_data <- bind_rows(
      lapply(male_datasets, function(ds) {
        ds %>%
          filter(GeneName %in% hormone_genes) %>%
          select(GeneName, all_of(time_points), Dataset) %>%
          mutate(Sex = "Male")
      })
    )
    
    # Combine female and male data
    combined_data <- bind_rows(female_data, male_data)
    
    return(combined_data)
  }
  
  # PCA plots for each hormone category (Estrogen, Testosterone, Androgen, Combination)
  for (category in names(gene_lists)) {
    # Combine data for females and males for the current hormone category
    combined_data <- combine_female_male_data(
      female_datasets = list(f_ds_lm, f_ds_ad, f_sw_lm, f_sw_ad),
      male_datasets = list(m_ds_lm, m_ds_ad, m_sw_lm, m_sw_ad),
      hormone_genes = gene_lists[[category]]
    )
    
    # Create title for the plot
    title <- paste("PCA Plot for", category, "Hormone Category")
    
    # Generate the PCA plot
    pca_plot <- generate_pca_plot(combined_data, gene_lists[[category]], title)
    
    if (!is.null(pca_plot)) {
      print(pca_plot)  # Print the plot
    }
  }
