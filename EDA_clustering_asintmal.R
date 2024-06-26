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
    labs(title = paste(x, "vs", y))+
    theme_minimal()
  
  return(p)
}

# Create scatterplots for each pair of columns
plot_pcr_hrp <- draw_scatterplot("all_pcr", "all_hrp", df_corr, scale_data = TRUE)
plot_pcr_pfldh <- draw_scatterplot("all_pcr", "all_pfldh", df_corr, scale_data = TRUE)
plot_hrp_pfldh <- draw_scatterplot("all_hrp", "all_pfldh", df_corr, scale_data = TRUE)

# Create the plot list by concatenating the plots
plot_list <- list(plot_pcr_hrp, plot_pcr_pfldh, plot_hrp_pfldh)

# Arrange plots in a grid
grid_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 1)

# # Save the grid plot as a PNG file with the name of the function input
ggsave("metrics_correlations.png", grid_plot, width = 10, height = 16, dpi = 300, bg ="white")



####################################
# CLSUTERING ANALYSIS
####################################

# CALCULATE PERCENTAGE CHANGE BETWEEN SAMPLINGS
compute_pchs <- function(df) {
  # Initialize an empty dataframe to store the pchs
  df_pchs <- data.frame(matrix(ncol = 0, nrow = nrow(df)))
  
  # Iterate over column indices to compute percentage change between time points
  for (i in 1:(ncol(df) - 1)) {
    for (j in (i + 1):ncol(df)) {
      # Compute the pch between the columns
      pch <- ((df[[j]] - df[[i]]) / df[[i]]) * 100 #alternative: log(df[[j]] / df[[i]])
      
      # Create column name for the pch
      col_name <- paste("pch", names(df)[i], names(df)[j], sep = "_")
      
      # Add pch to the dataframe
      df_pchs[[col_name]] <- pch
    }
  }
  
  return(df_pchs)
}

# compute changes
df_pcr_pchs <- compute_pchs(df_pcr)
df_hrp_pchs <- compute_pchs(df_hrp)
df_pfldh_pchs <- compute_pchs(df_pfldh)


## FULL KMEANS ANALYSIS
perform_kmeans_analysis <- function(DF, all = 3) {
  # Remove NA rows
  df_pchs_complete_cases <- DF[complete.cases(DF),]
  
  # Identify columns containing infinite values
  cols_with_inf <- sapply(df_pchs_complete_cases, function(x) any(is.infinite(x)))
  df_pchs_corrected <- df_pchs_complete_cases
  df_pchs_corrected[cols_with_inf] <- lapply(df_pchs_corrected[cols_with_inf], function(x) replace(x, is.infinite(x), 1))
  
  if (all == 3){
    df_pchs_corrected <- df_pchs_corrected[,c(1,4,6)]
  }
  
  # Define a range of k values to explore
  k_values <- 1:10  # You can adjust the range as needed
  
  # Initialize a vector to store the total within-cluster sum of squares (WSS) for each k
  wss <- numeric(length(k_values))
  
  # Perform k-means clustering for each k and calculate WSS
  for (i in 1:length(k_values)) {
    k <- k_values[i]
    kmeans_result <- kmeans(df_pchs_corrected, centers = k, nstart = 100)
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
  optimal_k <- 2:6
  
  # Create an empty list to store plots
  plot_list <- list()

  # Perform k-means clustering and plot for each optimal k
  for (k in optimal_k) {
    # Perform k-means clustering with the optimal k
    set.seed(69)
    kmeans_result <- kmeans(df_pchs_corrected, centers = k, nstart = 10000, iter.max = 10000)
    
    # Add cluster assignment as a new column to df_pchs_corrected
    df_pchs_corrected[[paste0("cluster_", k)]] <- kmeans_result$cluster
    
    wss <- kmeans_result$betweenss /kmeans_result$totss *100
    
    # Plot kmeans clustering
    kmeans_plot <- fviz_cluster(kmeans_result, data = df_pchs_corrected[, 1:all],
                                palette = c("pink2", "#00AFBB", "#E7B800", "orange4", "limegreen", "darkviolet", "red2", "orange"), 
                                geom = "point",
                                ellipse.type = "convex", 
                                ggtheme = theme_bw()) +
      labs(title = paste("Within cluster sum of squares by cluster: =", round(wss, 1), "%"))
    
    # Convert kmeans_result$centers to a dataframe
    centers_df <- as.data.frame(kmeans_result$centers[,1:all])
    
    # Add cluster numbers as a column
    centers_df$Cluster <- rownames(centers_df)
    
    # Define the desired order of variables
    if (all == 3){
      desired_order <- c("pch_day7_day14", "pch_day14_day21", "pch_day21_day28")
    }
    
    if (all == 4){
      desired_order <- c("day7", "day14", "day21", "day28")
    }
    
    # Reshape the dataframe to long format
    centers_df_long <- pivot_longer(centers_df, -Cluster, names_to = "Variable", values_to = "Value")
    
    # Reorder the levels of the Variable column
    centers_df_long$Variable <- factor(centers_df_long$Variable, levels = desired_order)
    
    # Plot the line plot
    line_plot <- ggplot(centers_df_long, aes(x = Variable, y = Value, group = Cluster, color = Cluster)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = c("pink2", "#00AFBB", "#E7B800", "orange4", "limegreen", "darkviolet", "red2", "orange")) +
      labs(title = paste("Cluster Centers (k =", k, ")"),
           x = "",
           y = "Relative Change in Parasite Density") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
    
    # Add plots to the list
    plot_list[[length(plot_list) + 1]] <- kmeans_plot
    plot_list[[length(plot_list) + 1]] <- line_plot
  }
  
  # Arrange plots in a grid
  grid_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 2)
  
  # # Save the grid plot as a PNG file with the name of the function input
  filename <- paste(gsub(" ", "_", deparse(substitute(DF))), "_", "kmeans_plot", ".png", sep="")
  ggsave(filename, grid_plot, width = 16, height = 24, dpi = 300, bg ="white")
  
  # Return the elbow plot and the grid plot
  return(df_pchs_corrected)
}

# perform analysis for change in days 7 to 14, 14 to 21 and 21 to 28.
clusters_pcr <- perform_kmeans_analysis(df_pcr_pchs, 3) #3 cuando quiera los deltas 
clusters_hrp <- perform_kmeans_analysis(df_hrp_pchs, 3)
clusters_pfldh <- perform_kmeans_analysis(df_pfldh_pchs, 3)

#output cluster labels
merge_clusters_with_df <- function(df, clusters_pcr) {
  # Extract columns with the substring "cluster_" from clusters_pcr
  cluster_cols <- grep("cluster_", names(clusters_pcr), value = TRUE)
  
  # shit code but works
  if (any(grepl("ASIN", rownames(clusters_pcr)))){
    rownames(df) <- df[,1]
  }
  
  # Merge the selected columns from clusters_pcr with df based on row names
  merged_df <- merge(df, clusters_pcr[, cluster_cols], by.x = 0, by.y = "row.names", all.x = TRUE)
  
  # Rename the row names column
  rownames(merged_df) <- merged_df$Row.names
  
  # Remove the redundant column
  merged_df <- merged_df[, -1]
  
  # If there are no matches, replace missing values with NA
  merged_df[is.na(merged_df)] <- NA
  
  # Convert row names to numeric and order them
  rownames_numeric <- as.numeric(rownames(merged_df))
  merged_df <- merged_df[order(rownames_numeric), ]
  
  return(merged_df)
}

# output cluster labels for each sample
clusters_pcr_merged_df <- merge_clusters_with_df(df, clusters_pcr)
write.csv(clusters_pcr_merged_df, "clusters_pcr_merged_df.csv")

clusters_hrp_merged_df <- merge_clusters_with_df(df, clusters_hrp)
write.csv(clusters_hrp_merged_df, "clusters_hrp_merged_df.csv")

clusters_pfldh_merged_df <- merge_clusters_with_df(df, clusters_pfldh)
write.csv(clusters_pfldh_merged_df, "clusters_pfldh_merged_df.csv")


#TODO:
  # 1) CAMBIAR PERCENTAGE CHANGE POR RATE OF CHANGE? es que sólo puede disminuir 99%, pero aumentar infinito%, lo que dificulta interpretar. 
        #ahora mismo sólo considero el cambio, más no la densidad... checar!: weighted percentage change (like, multiplying the percentage change times the parasite density?? maybe this takes into account also densities huh...)
  # 2) TRATAMIENTO DE NA: medias de otros datos? a qué trayectoria se parecen más según los datos doisponibles?


# perform analysis WITHOUT ANY FORMULA OR TRANSFORMATION, ONLY PARASITE COUNTS
clusters_pcr <- perform_kmeans_analysis(df_pcr, 4) #4 cuando quiera los daos crudos
clusters_hrp <- perform_kmeans_analysis(df_hrp, 4)
clusters_pfldh <- perform_kmeans_analysis(df_pfldh, 4)

# output cluster labels for each sample
clusters_pcr_merged_df <- merge_clusters_with_df(df, clusters_pcr)
write.csv(clusters_pcr_merged_df, "clusters_pcr_merged_df_raw_data.csv")

clusters_hrp_merged_df <- merge_clusters_with_df(df, clusters_hrp)
write.csv(clusters_hrp_merged_df, "clusters_hrp_merged_df_raw_data.csv")

clusters_pfldh_merged_df <- merge_clusters_with_df(df, clusters_pfldh)
write.csv(clusters_pfldh_merged_df, "clusters_pfldh_merged_df_raw_data.csv")



########################################
### CALCULATE STATS FOR EACH CLUSTER ###
########################################

library(tidyr)

# Define the function to calculate stats for non-cluster columns
calculate_stats <- function(data, cluster_col) {
  stats <- data %>%
    drop_na() %>%
    group_by(!!sym(cluster_col)) %>%
    summarise(
      across(
        .cols = !contains(c("cluster", "asint2")),
        .fns = list(mean = mean, std_dev = sd), #excluded median = median
        .names = "{.col}_{.fn}",
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    mutate(across(contains("std_dev"), ~ifelse(is.na(.), 0, .)))
  return(stats)
}


# Extract cluster columns
cluster_cols_hrp <- grep("cluster", names(clusters_hrp), value = TRUE)
cluster_cols_pcr <- grep("cluster", names(clusters_pcr), value = TRUE)
cluster_cols_pfldh <- grep("cluster", names(clusters_pfldh), value = TRUE)

# Calculate stats for each cluster column
stats_hrp <- lapply(cluster_cols_hrp, function(col) calculate_stats(clusters_hrp, col))
names(stats_hrp) <- cluster_cols_hrp

stats_pcr <- lapply(cluster_cols_pcr, function(col) calculate_stats(clusters_pcr, col))
names(stats_pcr) <- cluster_cols_pcr

stats_pfldh <- lapply(cluster_cols_pfldh, function(col) calculate_stats(clusters_pfldh, col))
names(stats_pfldh) <- cluster_cols_pfldh





##plotting### not done yet

# Function to plot line plot with error bars for each pair
plot_error_bars <- function(stats, measure) {
  # Create a data frame for plotting
  plot_data <- as.data.frame(stats)
  plot_data$cluster <- factor(plot_data$cluster_2)
  
  # Reshape the data to long format
  plot_data_long <- tidyr::pivot_longer(plot_data, cols = -cluster_2, names_to = "variable", values_to = "value")
  
  # Extract time points and error bars
  plot_data_long$time <- gsub(".*_", "", plot_data_long$variable)
  plot_data_long$error <- ifelse(grepl("std_dev", plot_data_long$variable), plot_data_long$value, NA)
  plot_data_long <- subset(plot_data_long, !is.na(error))
  
  # Plot line plot with error bars
  p <- ggplot(plot_data_long, aes(x = time, y = value, group = cluster, color = cluster)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.2) +
    labs(title = paste("Mean and Std Dev for", measure),
         x = "Day",
         y = "Value") +
    theme_minimal()
  
  return(p)
}


# Plot for each pair of columns
measure_names <- c("day7", "day14", "day21", "day28")
plot_list <- list()
for (measure in measure_names) {
  p <- plot_error_bars(stats_hrp$cluster_2, measure)
  plot_list[[length(plot_list) + 1]] <- p
}

# Arrange plots in a grid
grid_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 2)
print(grid_plot)


