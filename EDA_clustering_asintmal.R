library(ggplot2)
library(dplyr)
library(ggpubr)
library(factoextra)
library(tidyr)

df <- read.csv("densities_visits.csv")

###########################
# FORMATTING THE DATA
###########################

# Extracting columns with substrings 'pcr', 'hrp', and 'pfldh'
pcr_cols <- grep("pcr", names(df), value = TRUE)
hrp_cols <- grep("hrp", names(df), value = TRUE)
pfldh_cols <- grep("pfldh", names(df), value = TRUE)

# Concatenating columns with the same pattern into one column for each pattern
pcr_combined <- do.call(rbind, lapply(pcr_cols, function(x) df[[x]]))
hrp_combined <- do.call(rbind, lapply(hrp_cols, function(x) df[[x]]))
pfldh_combined <- do.call(rbind, lapply(pfldh_cols, function(x) df[[x]]))

# Transpose the concatenated matrices to convert rows to columns
pcr_combined <- t(pcr_combined)
hrp_combined <- t(hrp_combined)
pfldh_combined <- t(pfldh_combined)

# Create dataframes from the concatenated matrices
df_pcr <- as.data.frame(pcr_combined, stringsAsFactors = FALSE)
rownames(df_pcr) <- df$asint2
colnames(df_pcr) <- c("day7", "day14", "day21", "day28")
df_hrp <- as.data.frame(hrp_combined, stringsAsFactors = FALSE)
rownames(df_hrp) <- df$asint2
colnames(df_hrp) <- c("day7", "day14", "day21", "day28")
df_pfldh <- as.data.frame(pfldh_combined, stringsAsFactors = FALSE)
rownames(df_pfldh) <- df$asint2
colnames(df_pfldh) <- c("day7", "day14", "day21", "day28")


####################################
# CORRELATION BETWEN MEASUREMENTS
####################################

all_pcr <- as.data.frame(c(df_pcr$day7, df_pcr$day14, df_pcr$day21, df_pcr$day28))
colnames(all_pcr)[1] <- "all_pcr" 
all_hrp <- as.data.frame(c(df_hrp$day7, df_hrp$day14, df_hrp$day21, df_hrp$day28))
colnames(all_hrp)[1] <- "all_hrp" 
all_pfldh <- as.data.frame(c(df_pfldh$day7, df_pfldh$day14, df_pfldh$day21, df_pfldh$day28))
colnames(all_pfldh)[1] <- "all_pfldh" 

df_corr <- as.data.frame(cbind(all_pcr = all_pcr$all_pcr, all_hrp= all_hrp$all_hrp, all_pfldh =all_pfldh$all_pfldh))


# Define a function to calculate correlation and create scatterplot with correlation value
draw_scatterplot <- function(x, y, df, scale_data = FALSE) {
  
  # Filter out rows with NA or 0
  filtered_df <- df[complete.cases(df[x], df[y]) & df[x] != 0 & df[y] != 0, ]
  
  # transform the data
  if (scale_data) {
    filtered_df[[x]] <- log10(filtered_df[[x]])
    filtered_df[[y]] <- log10(filtered_df[[y]])
  }
  
  # Calculate correlation
  corr <- cor(filtered_df[[x]], filtered_df[[y]])
  
  # Create scatterplot
  p <- ggplot(filtered_df, aes_string(x = x, y = y)) +
    geom_point() +
    geom_text(aes(label = paste("Correlation =", round(corr, 2)), x = Inf, y = -Inf),
              hjust = 1, vjust = 0, size = 4) +
    labs(title = paste("Scatterplot of", x, "vs", y))
  
  return(p)
}

# Create scatterplots for each pair of columns
plot_pcr_hrp <- draw_scatterplot("all_pcr", "all_hrp", df_corr, scale_data = TRUE)
plot_pcr_pfldh <- draw_scatterplot("all_pcr", "all_pfldh", df_corr, scale_data = TRUE)
plot_hrp_pfldh <- draw_scatterplot("all_hrp", "all_pfldh", df_corr, scale_data = TRUE)

# Display the plots
print(plot_pcr_hrp)
print(plot_pcr_pfldh)
print(plot_hrp_pfldh)


####################################
# CLSUTERING ANALYSIS
####################################

# CALCULATE PERCENTAGE CHANGE BETWEEN SAMPLINGS
compute_percentage_changes <- function(df) {
  # Initialize an empty dataframe to store the percentage_changes
  df_percentage_changes <- data.frame(matrix(ncol = 0, nrow = nrow(df)))
  
  # Iterate over column indices to compute percentage change between time points
  for (i in 1:(ncol(df) - 1)) {
    for (j in (i + 1):ncol(df)) {
      # Compute the percentage_change between the columns
      percentage_change <- ((df[[j]] - df[[i]]) / df[[i]]) * 100
      
      # Create column name for the percentage_change
      col_name <- paste("percentage_change", names(df)[i], names(df)[j], sep = "_")
      
      # Add percentage_change to the dataframe
      df_percentage_changes[[col_name]] <- percentage_change
    }
  }
  
  return(df_percentage_changes)
}

# compute changes
df_pcr_percentage_changes <- compute_percentage_changes(df_pcr)
df_hrp_percentage_changes <- compute_percentage_changes(df_hrp)
df_pfldh_percentage_changes <- compute_percentage_changes(df_pfldh)


## FULL KMEANS ANALYSIS
perform_kmeans_analysis <- function(DF) {
  # Remove NA rows
  df_percentage_changes_complete_cases <- DF[complete.cases(DF),]
  
  # Identify columns containing infinite values
  cols_with_inf <- sapply(df_percentage_changes_complete_cases, function(x) any(is.infinite(x)))
  df_percentage_changes_corrected <- df_percentage_changes_complete_cases
  df_percentage_changes_corrected[cols_with_inf] <- lapply(df_percentage_changes_corrected[cols_with_inf], function(x) replace(x, is.infinite(x), 1))
  
  # Define a range of k values to explore
  k_values <- 1:10  # You can adjust the range as needed
  
  # Initialize a vector to store the total within-cluster sum of squares (WSS) for each k
  wss <- numeric(length(k_values))
  
  # Perform k-means clustering for each k and calculate WSS
  for (i in 1:length(k_values)) {
    k <- k_values[i]
    kmeans_result <- kmeans(df_percentage_changes_corrected[,c(1,4,6)], centers = k, nstart = 100)
    wss[i] <- kmeans_result$tot.withinss
  }
  
  # Plot the elbow plot
  elbow_plot <- ggplot() +
    geom_line(aes(x = k_values, y = wss)) +
    geom_point(aes(x = k_values, y = wss)) +
    labs(title = "Elbow Plot for k-means Clustering",
         x = "Number of Clusters (k)",
         y = "Total Within-Cluster Sum of Squares (WSS)")+
    theme_minimal()
  
  # # Save the grid plot as a PNG file with the name of the function input
  filename <- paste(gsub(" ", "_", deparse(substitute(DF))), "_", "elbowplot", ".png", sep="")
  ggsave(filename, elbow_plot, width = 6, height = 6, dpi = 300, bg ="white")
  
  
  
  # Select the optimal number of clusters based on the elbow plot
  optimal_k <- 2:5
  
  # Create an empty list to store plots
  plot_list <- list()
  
  # Perform k-means clustering and plot for each optimal k
  for (k in optimal_k) {
    # Perform k-means clustering with the optimal k
    kmeans_result <- kmeans(df_percentage_changes_corrected[,c(1,4,6)], centers = k, nstart = 10000, iter.max = 10000)
    
    # Plot kmeans clustering
    kmeans_plot <- fviz_cluster(kmeans_result, data = df_percentage_changes_corrected[,c(1,4,6)],
                                palette = c("pink2", "#00AFBB", "#E7B800", "orange4", "limegreen", "darkviolet"), 
                                geom = "point",
                                ellipse.type = "convex", 
                                ggtheme = theme_bw())
    
    # Convert kmeans_result$centers to a dataframe
    centers_df <- as.data.frame(kmeans_result$centers)
    
    # Add cluster numbers as a column
    centers_df$Cluster <- rownames(centers_df)
    
    # Define the desired order of variables
    desired_order <- c("percentage_change_day7_day14", "percentage_change_day14_day21", "percentage_change_day21_day28")
    
    # Reshape the dataframe to long format
    centers_df_long <- pivot_longer(centers_df, -Cluster, names_to = "Variable", values_to = "Value")
    
    # Reorder the levels of the Variable column
    centers_df_long$Variable <- factor(centers_df_long$Variable, levels = desired_order)
    
    # Plot the line plot
    line_plot <- ggplot(centers_df_long, aes(x = Variable, y = Value, group = Cluster, color = Cluster)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = c("pink2", "#00AFBB", "#E7B800", "orange4", "limegreen", "darkviolet")) +
      labs(title = paste("Cluster Centers (k =", k, ")"),
           x = "",
           y = "Relative Change in Parasite Density")+
      theme_minimal()
    
    # Add plots to the list
    plot_list[[length(plot_list) + 1]] <- kmeans_plot
    plot_list[[length(plot_list) + 1]] <- line_plot
  }
  
  # Arrange plots in a grid
  grid_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 2)
  
  # # Save the grid plot as a PNG file with the name of the function input
  filename <- paste(gsub(" ", "_", deparse(substitute(DF))), "_", "kmeans_plot", ".png", sep="")
  ggsave(filename, grid_plot, width = 12, height = 16, dpi = 300, bg ="white")
  
  # Return the elbow plot and the grid plot
  return(list(elbow_plot, grid_plot))
}

# perform analysis
perform_kmeans_analysis(df_pcr_percentage_changes)
perform_kmeans_analysis(df_hrp_percentage_changes)
perform_kmeans_analysis(df_pfldh_percentage_changes)
