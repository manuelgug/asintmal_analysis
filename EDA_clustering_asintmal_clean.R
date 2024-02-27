library(ggplot2)
library(dplyr)
library(ggpubr)
library(factoextra)
library(tidyr)
library(cowplot)
library(cluster)

df <- readxl::read_excel("DB_Manuel_25Feb24.xls")
df <- as.data.frame(df)
rownames(df) <- df$asint2

alfredo_labels <- df[,1:7]

#remove alfredo's labels
df <- df[,c(-1:-7)]

###--------------------------------------APPROACH 1: CLUSTERING---------------------------------------------

###########################
# FORMATING THE DATA
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
rownames(df_pcr) <- rownames(df)
colnames(df_pcr) <- c("day7", "day14", "day21", "day28")
df_hrp <- as.data.frame(hrp_combined, stringsAsFactors = FALSE)
rownames(df_hrp) <- rownames(df)
colnames(df_hrp) <- c("day7", "day14", "day21", "day28")
df_pfldh <- as.data.frame(pfldh_combined, stringsAsFactors = FALSE)
rownames(df_pfldh) <- rownames(df)
colnames(df_pfldh) <- c("day7", "day14", "day21", "day28")

# means of 3 measurements
means_day7 <- rowMeans(cbind(df_pcr$day7, df_hrp$day7, df_pfldh$day7))
means_day14 <- rowMeans(cbind(df_pcr$day14, df_hrp$day14, df_pfldh$day14))
means_day21 <- rowMeans(cbind(df_pcr$day21, df_hrp$day21, df_pfldh$day21))
means_day28 <- rowMeans(cbind(df_pcr$day28, df_hrp$day28, df_pfldh$day28))

df_means <- as.data.frame(cbind(means_day7, means_day14, means_day21, means_day28))

rownames(df_means) <- rownames(df)
colnames(df_means) <- c("day7", "day14", "day21", "day28")


# medians of 3 measurements
medians_day7 <- apply(cbind(df_pcr$day7, df_hrp$day7, df_pfldh$day7), 1, median, na.rm = TRUE)
medians_day14 <- apply(cbind(df_pcr$day14, df_hrp$day14, df_pfldh$day14), 1, median, na.rm = TRUE)
medians_day21 <- apply(cbind(df_pcr$day21, df_hrp$day21, df_pfldh$day21), 1, median, na.rm = TRUE)
medians_day28 <- apply(cbind(df_pcr$day28, df_hrp$day28, df_pfldh$day28), 1, median, na.rm = TRUE)

df_medians <- as.data.frame(cbind(medians_day7, medians_day14, medians_day21, medians_day28))

rownames(df_medians) <- rownames(df)
colnames(df_medians) <- c("day7", "day14", "day21", "day28")


####################################
# REMOVE OUTLIERS
####################################

remove_outliers_multivariate <- function(df) {
  # Calculate Mahalanobis distance
  mahalanobis_dist <- mahalanobis(na.omit(df), colMeans(na.omit(df)), cov(na.omit(df)))
  
  # Set a threshold for outlier detection
  threshold <- qchisq(0.95, df = ncol(df))
  
  # Identify outliers
  outliers <- which(mahalanobis_dist > threshold)
  
  # Remove outliers
  df_no_outliers <- df[-outliers, ]
  
  return(df_no_outliers)
}

# remove outliers
df_pcr_no_outliers <- remove_outliers_multivariate(df_pcr)
df_hrp_no_outliers <- remove_outliers_multivariate(df_hrp)
df_pfldh_no_outliers <- remove_outliers_multivariate(df_pfldh)
df_means_no_outliers <- remove_outliers_multivariate(df_means)
df_medians_no_outliers <- remove_outliers_multivariate(df_medians)


####################################
# SUMMARY STATS
####################################

counts_pfldh <- na.omit(c(df_pfldh$day7,df_pfldh$day14, df_pfldh$day21, df_pfldh$day28))
summary_counts_pfldh <- summary(counts_pfldh)

counts_pcr <- na.omit(c(df_pcr$day7, df_pcr$day14, df_pcr$day21, df_pcr$day28))
summary_counts_pcr <- summary(counts_pcr)
mode(counts_pcr)

counts_hrp <- na.omit(c(df_hrp$day7, df_hrp$day14, df_hrp$day21, df_hrp$day28))
summary_counts_hrp <- summary(counts_hrp)

counts_means <- na.omit(c(df_means$day7, df_means$day14, df_means$day21, df_means$day28))
summary_counts_means <- summary(counts_means)

counts_medians <- na.omit(c(df_medians$day7, df_medians$day14, df_medians$day21, df_medians$day28))
summary_counts_medians <- summary(counts_medians)

counts_pfldh_no_outliers <- na.omit(c(df_pfldh_no_outliers$day7, df_pfldh_no_outliers$day14, df_pfldh_no_outliers$day21, df_pfldh_no_outliers$day28))
summary_counts_pfldh_no_outliers <- summary(counts_pfldh_no_outliers)

counts_pcr_no_outliers <- na.omit(c(df_pcr_no_outliers$day7, df_pcr_no_outliers$day14, df_pcr_no_outliers$day21, df_pcr_no_outliers$day28))
summary_counts_pcr_no_outliers <- summary(counts_pcr_no_outliers)

counts_hrp_no_outliers <- na.omit(c(df_hrp_no_outliers$day7, df_hrp_no_outliers$day14, df_hrp_no_outliers$day21, df_hrp_no_outliers$day28))
summary_counts_hrp_no_outliers <- summary(counts_hrp_no_outliers)

counts_means_no_outliers <- na.omit(c(df_means_no_outliers$day7, df_means_no_outliers$day14, df_means_no_outliers$day21, df_means_no_outliers$day28))
summary_counts_means_no_outliers <- summary(counts_means_no_outliers)

counts_medians_no_outliers <- na.omit(c(df_medians_no_outliers$day7, df_medians_no_outliers$day14, df_medians_no_outliers$day21, df_medians_no_outliers$day28))
summary_counts_medians_no_outliers <- summary(counts_medians_no_outliers)


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

## FULL KMEANS ANALYSIS
perform_kmeans_analysis <- function(DF, loga = F) {
  # Remove NA rows
  df_pchs_complete_cases <- DF[complete.cases(DF),]
  
  # Identify columns containing infinite values
  cols_with_inf <- sapply(df_pchs_complete_cases, function(x) any(is.infinite(x)))
  df_pchs_corrected <- df_pchs_complete_cases
  df_pchs_corrected[cols_with_inf] <- lapply(df_pchs_corrected[cols_with_inf], function(x) replace(x, is.infinite(x), 1))
  
  # Define a range of k values to explore
  k_values <- 2:20  # You can adjust the range as needed
  
  # Initialize a vector to store the total within-cluster sum of squares (WSS) for each k
  wss <- numeric(length(k_values))
  # Create an empty dataframe to store silhouette scores
  sil_df <- data.frame(k = numeric(), silhouette = numeric())
  
  # Perform k-means clustering for each k and calculate WSS
  for (i in 1:length(k_values)) {
    k <- k_values[i]
    set.seed(69)
    kmeans_result <- kmeans(df_pchs_corrected, centers = k, nstart = 100)
    wss[i] <- kmeans_result$tot.withinss
    # Calculate silhouette scores
    sil <- silhouette(kmeans_result$cluster, dist(df_pchs_corrected))
    avg_sil <- mean(sil[, "sil_width"])
    
    # Store silhouette scores in the dataframe
    sil_df <- rbind(sil_df, data.frame(k = k, silhouette = avg_sil))
  }
  
  # Create the elbow plot
  elbow_plot <- ggplot() +
    geom_line(aes(x = k_values, y = wss)) +
    geom_point(aes(x = k_values, y = wss)) +
    labs(title = "Elbow Plot for k-means Clustering",
         x = "Number of Clusters (k)",
         y = "Total Within-Cluster Sum of Squares (WSS)")+
    theme_minimal()
  
  # Create the silhouette plot
  silhouette_plot <- ggplot(sil_df, aes(x = k, y = silhouette)) +
    geom_line() +
    geom_point() +
    labs(title = "Silhouette Plot", x = "Number of Clusters (k)", y = "Average Silhouette Width") +
    theme_minimal()
  
  # Combine the plots into a single 2-panel figure with 2 columns
  combined_plot <- plot_grid(elbow_plot, silhouette_plot, ncol = 2)
  
  # Save the combined plot as a PNG file with the name of the function input
  filename <- paste(gsub(" ", "_", deparse(substitute(DF))), "_elbow_silhouette_plot", ".png", sep="")
  ggsave(filename, combined_plot, width = 12, height = 6, dpi = 300, bg = "white")
  
  
  # Select the optimal number of clusters based on the elbow plot
  optimal_k <- 2:11
  
  # Create an empty list to store plots
  plot_list <- list()
  
  # Create an empty dataframe to store silhouette scores
  sil_df <- data.frame(k = numeric(), silhouette = numeric())
  
  # Perform k-means clustering and plot for each optimal k
  for (k in optimal_k) {
    # Perform k-means clustering with the optimal k
    set.seed(69)
    kmeans_result <- kmeans(df_pchs_corrected, centers = k, nstart = 10000, iter.max = 10000)
    
    # Add cluster assignment as a new column to df_pchs_corrected
    df_pchs_corrected[[paste0("cluster_", k)]] <- kmeans_result$cluster
    
    wss <- kmeans_result$betweenss / kmeans_result$totss * 100
    
    # Calculate silhouette scores
    sil <- silhouette(kmeans_result$cluster, dist(df_pchs_corrected))
    avg_sil <- mean(sil[, "sil_width"])
    
    # Store silhouette scores in the dataframe
    sil_df <- rbind(sil_df, data.frame(k = k, silhouette = avg_sil))
    
    # Plot kmeans clustering
    kmeans_plot <- fviz_cluster(kmeans_result, data = df_pchs_corrected[, 1:4],
                                palette = c("pink2", "#00AFBB", "magenta", "orange4", "limegreen", "darkviolet", "red2", "orange", "yellow2", "grey54", "black"), 
                                geom = "point",
                                ellipse.type = "convex", 
                                ggtheme = theme_bw()) +
      labs(title = paste("Within cluster sum of squares by cluster: =", round(wss, 1), "%"))
    
    # Convert kmeans_result$centers to a dataframe
    centers_df <- as.data.frame(kmeans_result$centers[,1:4])
    
    # Add cluster numbers as a column
    centers_df$Cluster <- rownames(centers_df)
    
    # Define the desired order of variables
    desired_order <- c("day7", "day14", "day21", "day28")
    
    # Reshape the dataframe to long format
    centers_df_long <- pivot_longer(centers_df, -Cluster, names_to = "Variable", values_to = "Value")
    
    # Reorder the levels of the Variable column
    centers_df_long$Variable <- factor(centers_df_long$Variable, levels = desired_order)
    
    ylabel = c("Parasite Density")
    #ss = summary_stats[c(2, 3, 5)]
    
    mean_centers <- mean(centers_df_long$Value)
    sd_centers <- sd(centers_df_long$Value)
    
    ss <- c(mean_centers - sd_centers, mean_centers, mean_centers + sd_centers) 
    names(ss) <- c("-minus1σ", "mean", "1σ")
    
    if (loga == TRUE){
      mean_centers <- mean(log(centers_df_long$Value))
      sd_centers <- sd(log(centers_df_long$Value))
      
      ss <- c(mean_centers - sd_centers, mean_centers, mean_centers + sd_centers) 
      names(ss) <- c("-1σ", "mean", "1σ")
      
      centers_df_long$Value <- log(centers_df_long$Value)
      ylabel <- c("log(Parasite Density)")
    }
    
    # Plot the line plot
    line_plot <- ggplot(centers_df_long, aes(x = Variable, y = Value, group = Cluster, color = Cluster)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = c("pink2", "#00AFBB", "magenta", "orange4", "limegreen", "darkviolet", "red2", "orange", "yellow2", "grey54", "black")) +
      labs(title = paste("Cluster Centers (k =", k, ")"),
           x = "",
           y = ylabel) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
      geom_hline(yintercept = ss, color = c("gray76", "gray76", "gray76"), linetype = "dashed")+
      annotate("text", y = ss, x = 0.75, label = names(ss))
    
    # Add silhouette scores to the line plot title
    line_plot <- line_plot + labs(title = paste("Cluster Centers (k =", k, ") \nAverage Silhouette Width =", round(avg_sil, 2)))
    
    # Add plots to the list
    plot_list[[length(plot_list) + 1]] <- kmeans_plot
    plot_list[[length(plot_list) + 1]] <- line_plot
  }
  
  
  # Arrange plots in a grid
  grid_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 2)
  
  # Save the grid plot as a PNG file with the name of the function input
  filename <- paste(gsub(" ", "_", deparse(substitute(DF))), "_", "kmeans_plot", ".png", sep="")
  ggsave(filename, grid_plot, width = 16, height = 36, dpi = 300, bg ="white")
  
  
  # Return the elbow plot and the grid plot
  return(df_pchs_corrected)
}

# perform analysis WITHOUT ANY FORMULA OR TRANSFORMATION (log is for viz of lineplot), ONLY PARASITE COUNTS
clusters_pcr <- perform_kmeans_analysis(df_pcr, loga =T)
clusters_hrp <- perform_kmeans_analysis(df_hrp, loga =T)
clusters_pfldh <- perform_kmeans_analysis(df_pfldh, loga =T)
clusters_means <- perform_kmeans_analysis(df_means, loga =T)
clusters_medians <- perform_kmeans_analysis(df_medians, loga =T)

clusters_pcr_no_outliers <- perform_kmeans_analysis(df_pcr_no_outliers, loga =T)
clusters_hrp_no_outliers <- perform_kmeans_analysis(df_hrp_no_outliers, loga =T)
clusters_pfldh_no_outliers <- perform_kmeans_analysis(df_pfldh_no_outliers, loga =T)
clusters_means_no_outliers <- perform_kmeans_analysis(df_means_no_outliers, loga =T)
clusters_medians_no_outliers <- perform_kmeans_analysis(df_medians_no_outliers, loga =T)



#output cluster labels
merge_clusters_with_df <- function(df, clusters_pcr) {
  # Extract columns with the substring "cluster_" from clusters_pcr
  cluster_cols <- grep("cluster_", names(clusters_pcr), value = TRUE)
  
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
write.csv(clusters_pcr_merged_df, "clusters_pcr_merged_df_raw_data.csv")

clusters_hrp_merged_df <- merge_clusters_with_df(df, clusters_hrp)
write.csv(clusters_hrp_merged_df, "clusters_hrp_merged_df_raw_data.csv")

clusters_pfldh_merged_df <- merge_clusters_with_df(df, clusters_pfldh)
write.csv(clusters_pfldh_merged_df, "clusters_pfldh_merged_df_raw_data.csv")

clusters_means_merged_df <- merge_clusters_with_df(df, clusters_means)
write.csv(clusters_means_merged_df, "clusters_means_merged_df_raw_data.csv")

clusters_medians_merged_df <- merge_clusters_with_df(df, clusters_medians)
write.csv(clusters_medians_merged_df, "clusters_medians_merged_df_raw_data.csv")


clusters_pcr_merged_df_no_outliers <- merge_clusters_with_df(df, clusters_pcr_no_outliers)
write.csv(clusters_pcr_merged_df_no_outliers, "clusters_pcr_merged_df_no_outliers_raw_data.csv")

clusters_hrp_merged_df_no_outliers <- merge_clusters_with_df(df, clusters_hrp_no_outliers)
write.csv(clusters_hrp_merged_df_no_outliers, "clusters_hrp_merged_df_no_outliers_raw_data.csv")

clusters_pfldh_merged_df_no_outliers <- merge_clusters_with_df(df, clusters_pfldh_no_outliers)
write.csv(clusters_pfldh_merged_df_no_outliers, "clusters_pfldh_merged_df_no_outliers_raw_data.csv")

clusters_means_merged_df_no_outliers <- merge_clusters_with_df(df, clusters_means_no_outliers)
write.csv(clusters_means_merged_df_no_outliers, "clusters_means_merged_df_no_outliers_raw_data.csv")

clusters_medians_merged_df_no_outliers <- merge_clusters_with_df(df, clusters_medians_no_outliers)
write.csv(clusters_medians_merged_df_no_outliers, "clusters_medians_merged_df_no_outliers_raw_data.csv")





##ouput best result for alfredo
hrp_best_clustering_alfredo <- merge(clusters_hrp_merged_df, alfredo_labels, by = "row.names")
hrp_best_clustering_alfredo <- hrp_best_clustering_alfredo[,-24]
colnames(hrp_best_clustering_alfredo)[1] <- "asint2" 

write.csv(hrp_best_clustering_alfredo, "hrp_best_clustering_alfredo.csv")
## igual y hasta aquí es suficiente.

#### DONE! ####






########################################
### CALCULATE STATS FOR EACH CLUSTER ###
########################################

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
# 
# library(ggplot2)
# 
# # Function to plot mean and std as error bars for each pair
# plot_error_bars <- function(stats, measure) {
#   # Create a data frame for plotting
#   plot_data <- as.data.frame(stats)
#   plot_data$cluster <- factor(plot_data$cluster_2)
#   
#   # Plot mean and std as error bars
#   p <- ggplot(plot_data, aes(x = cluster, y = !!sym(paste0(measure, "_mean")), ymin = !!sym(paste0(measure, "_mean")) - !!sym(paste0(measure, "_std_dev")), 
#                              ymax = !!sym(paste0(measure, "_mean")) + !!sym(paste0(measure, "_std_dev")))) +
#     geom_errorbar(width = 0.2) +
#     geom_point(size = 3) +
#     labs(title = paste("Mean and Std Dev for", measure),
#          x = "Cluster",
#          y = "Value") +
#     theme_minimal()
#   
#   return(p)
# }
# 
# # Plot for each pair of columns
# measure_names <- c("day7", "day14", "day21", "day28")
# plot_list <- list()
# for (measure in measure_names) {
#   p <- plot_error_bars(stats_hrp$cluster_2, measure)
#   plot_list[[length(plot_list) + 1]] <- p
# }
# 
# # Arrange plots in a grid
# grid_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 2)
# print(grid_plot)



# Assuming your data is stored in a list called stats_hrp
stats_hrp_long <- stats_hrp$cluster_2 %>%
  pivot_longer(cols = -cluster_2,
               names_to = c(".value", "day"),
               names_sep = "_")

# Reorder the columns
stats_hrp_long <- stats_hrp_long[, c(1, 4, 2, 3)]

# Print the resulting data frame
print(stats_hrp_long)



###--------------------------------------APPROACH 2: THRESHOLDS (distributions of counts for each measurement-----------------------------------

#histograms of counts for each metric


#df_pfldh
counts_pfldh <- na.omit(c(df_pfldh$day7,df_pfldh$day14, df_pfldh$day21, df_pfldh$day28))

# Summary of counts_pcr
summary_counts_pfldh <- summary(counts_pfldh)

# Create histogram
histogram <- hist(counts_pfldh, plot = FALSE, breaks = 30)

# Convert histogram to data frame
hist_data <- data.frame(
  count = histogram$counts,
  breaks = histogram$mids
)

# Create ggplot
ggplot(hist_data, aes(x = breaks, y = count)) +
  geom_bar(stat = "identity", fill = "gray", color = "gray") +
  labs(x = "Value", y = "Frequency", title = "Histogram of counts_pfldh") +
  theme_minimal() +
  # Adding ablines for summary components
  geom_vline(xintercept = summary_counts_pfldh[2:5], color = c("blue", "green", "red", "purple"))+
  annotate("text", x = summary_counts_pfldh[2:5], y = 0, label = names(summary_counts_pfldh)[2:5], vjust = -0.5)



#df_hrp
counts_hrp <- na.omit(c(df_hrp$day7,df_hrp$day14, df_hrp$day21, df_hrp$day28))

# Summary of counts_pcr
summary_counts_hrp <- summary(counts_hrp)

# Create histogram
histogram <- hist(counts_hrp, plot = FALSE, breaks = 30)

# Convert histogram to data frame
hist_data <- data.frame(
  count = histogram$counts,
  breaks = histogram$mids
)

# Create ggplot
ggplot(hist_data, aes(x = breaks, y = count)) +
  geom_bar(stat = "identity", fill = "gray", color = "gray") +
  labs(x = "Value", y = "Frequency", title = "Histogram of counts_hrp") +
  theme_minimal() +
  # Adding ablines for summary components
  geom_vline(xintercept = summary_counts_hrp[2:5], color = c("blue", "green", "red", "purple"))+
  annotate("text", x = summary_counts_hrp[2:5], y = 0, label = names(summary_counts_hrp)[2:5], vjust = -0.5)



#df_pcr
counts_pcr <- na.omit(c(df_pcr$day7,df_pcr$day14, df_pcr$day21, df_pcr$day28))

# Summary of counts_pcr
summary_counts_pcr <- summary(counts_pcr)

# Create histogram
histogram <- hist(counts_pcr, plot = FALSE, breaks = 30)

# Convert histogram to data frame
hist_data <- data.frame(
  count = histogram$counts,
  breaks = histogram$mids
)

# Create ggplot
ggplot(hist_data, aes(x = breaks, y = count)) +
  geom_bar(stat = "identity", fill = "gray", color = "gray") +
  labs(x = "Value", y = "Frequency", title = "Histogram of counts_pcr") +
  theme_minimal() +
  # Adding ablines for summary components
  geom_vline(xintercept = summary_counts_pcr[2:5], color = c("blue", "green", "red", "purple"))+
  annotate("text", x = summary_counts_pcr[2:5], y = 0, label = names(summary_counts_pcr)[2:5], vjust = -0.5)


#use 1st and 3rd quantile as thresholds
pfldh_thresh <- summary_counts_pfldh[c("1st Qu.", "3rd Qu.")]
hrp_thresh <- summary_counts_hrp[c("1st Qu.", "3rd Qu.")]
pcr_thresh <- summary_counts_pcr[c("1st Qu.", "3rd Qu.")]

# Apply the condition to the columns
df_levels <- df %>%
  mutate_at(vars(matches("pcr")), ~case_when(
    is.na(.) ~ NA_character_,
    . < pcr_thresh[1] ~ "L",
    . > pcr_thresh[2] ~ "H",
    TRUE ~ "M"
  ))

df_levels <- df_levels %>%
  mutate_at(vars(matches("hrp")), ~case_when(
    is.na(.) ~ NA_character_,
    . < hrp_thresh[1] ~ "L",
    . > hrp_thresh[2] ~ "H",
    TRUE ~ "M"
  ))

df_levels <- df_levels %>%
  mutate_at(vars(matches("pfldh")), ~case_when(
    is.na(.) ~ NA_character_,
    . < pfldh_thresh[1] ~ "L",
    . > pfldh_thresh[2] ~ "H",
    TRUE ~ "M"
  ))

#merge with original df
df_final_thresolds <- merge(df, df_levels, by ="row.names")
rownames(df_final_thresolds) <- df_final_thresolds$Row.names
df_final_thresolds <- df_final_thresolds[,-1]

# output
write.csv(df_final_thresolds, "thresholds_all.csv")


# Find the rows and columns where the column name contains "pcr" and "y"
pcr_y_cols <- grep("pcr.*y", colnames(df_final_thresolds), value = TRUE)
hrp_y_cols <- grep("hrp.*y", colnames(df_final_thresolds), value = TRUE)
pfldh_y_cols <- grep("pfldh.*y", colnames(df_final_thresolds), value = TRUE)

# Apply paste0 to the selected cells
pcr_trajectories <- apply(df_final_thresolds[, pcr_y_cols], 1, function(row) paste(row, collapse = ""))
hrp_trajectories <- apply(df_final_thresolds[, hrp_y_cols], 1, function(row) paste(row, collapse = ""))
pfldh_trajectories <- apply(df_final_thresolds[, pfldh_y_cols], 1, function(row) paste(row, collapse = ""))

all_trajectories <- as.data.frame(cbind(pcr_trajectories, hrp_trajectories, pfldh_trajectories))

# Identify rows where any column contains "NA" as a substring
rows_without_na_pattern <- apply(all_trajectories, 1, function(row) !any(grepl("NA", row)))

# Subset the dataframe to keep only rows without the "NA" pattern
all_trajectories <- all_trajectories[rows_without_na_pattern, ]

# output
write.csv(all_trajectories, "thresholds_all_trajectories.csv")



#line plots
df_pcr_trajectories_for_lineplots <- merge(df_pcr, all_trajectories["pcr_trajectories"], by ="row.names", all.y = T) # NA rows are auto removed
rownames(df_pcr_trajectories_for_lineplots) <- df_pcr_trajectories_for_lineplots$Row.names
df_pcr_trajectories_for_lineplots <- df_pcr_trajectories_for_lineplots[,-1]

df_pcr_trajectories_for_lineplots_melteed <- pivot_longer(df_pcr_trajectories_for_lineplots, 
                                                          cols = starts_with("day"),
                                                          names_to = "Day",
                                                          values_to = "Value")

df_pcr_trajectories_for_lineplots_melteed$Day <- factor(df_pcr_trajectories_for_lineplots_melteed$Day, levels = c("day7", "day14", "day21", "day28"))

color_palette <- colorRampPalette(c("cyan", "orange"))(n = length(unique(df_pcr_trajectories_for_lineplots_melteed$pcr_trajectories)))

ggplot(df_pcr_trajectories_for_lineplots_melteed, aes(x = Day, y = Value, color = pcr_trajectories, group = pcr_trajectories)) +
  geom_line(alpha =0.5, linewidth = 1) +
  labs(x = "Day", y = "Value", color = "PFldH Trajectories") +
  scale_color_manual(values = color_palette) +
  theme_minimal() +
  theme(legend.position = "bottom")


df_hrp_trajectories_for_lineplots <- merge(df_hrp, all_trajectories["hrp_trajectories"], by ="row.names", all.y = T) # NA rows are auto removed
rownames(df_hrp_trajectories_for_lineplots) <- df_hrp_trajectories_for_lineplots$Row.names
df_hrp_trajectories_for_lineplots <- df_hrp_trajectories_for_lineplots[,-1]

df_hrp_trajectories_for_lineplots_melteed <- pivot_longer(df_hrp_trajectories_for_lineplots, 
                                                          cols = starts_with("day"),
                                                          names_to = "Day",
                                                          values_to = "Value")

df_hrp_trajectories_for_lineplots_melteed$Day <- factor(df_hrp_trajectories_for_lineplots_melteed$Day, levels = c("day7", "day14", "day21", "day28"))

color_palette <- colorRampPalette(c("cyan", "orange"))(n = length(unique(df_hrp_trajectories_for_lineplots_melteed$hrp_trajectories)))

# Create the plot with the manual color scale
ggplot(df_hrp_trajectories_for_lineplots_melteed, aes(x = Day, y = Value, color = hrp_trajectories, group = hrp_trajectories)) +
  geom_line(alpha = 0.5, linewidth = 1) +
  labs(x = "Day", y = "Value", color = "HRP Trajectories") +
  scale_color_manual(values = color_palette) +
  theme_minimal() +
  theme(legend.position = "bottom")


df_pfldh_trajectories_for_lineplots <- merge(df_pfldh, all_trajectories["pfldh_trajectories"], by ="row.names", all.y = T) # NA rows are auto removed
rownames(df_pfldh_trajectories_for_lineplots) <- df_pfldh_trajectories_for_lineplots$Row.names
df_pfldh_trajectories_for_lineplots <- df_pfldh_trajectories_for_lineplots[,-1]

df_pfldh_trajectories_for_lineplots_melteed <- pivot_longer(df_pfldh_trajectories_for_lineplots, 
                                                            cols = starts_with("day"),
                                                            names_to = "Day",
                                                            values_to = "Value")

df_pfldh_trajectories_for_lineplots_melteed$Day <- factor(df_pfldh_trajectories_for_lineplots_melteed$Day, levels = c("day7", "day14", "day21", "day28"))

color_palette <- colorRampPalette(c("cyan", "orange"))(n = length(unique(df_pfldh_trajectories_for_lineplots_melteed$pfldh_trajectories)))

ggplot(df_pfldh_trajectories_for_lineplots_melteed, aes(x = Day, y = Value, color = pfldh_trajectories, group = pfldh_trajectories)) +
  geom_line(alpha =0.5, linewidth = 1) +
  labs(x = "Day", y = "Value", color = "PFldH Trajectories") +
  scale_color_manual(values = color_palette) +
  theme_minimal() +
  theme(legend.position = "bottom")



# # Convert the column contents to lists
# pcr_list <- unique(all_trajectories$pcr_trajectories)
# hrp_list <- unique(all_trajectories$hrp_trajectories)
# pfldh_list <- unique(all_trajectories$pfldh_trajectories)
# 
# # Create the Venn diagram
# venn_diagram <- venn.diagram(
#   x = list(pcr = pcr_list, hrp = hrp_list, pfldh = pfldh_list),
#   category.names = c("pcr", "hrp", "pfldh"),
#   filename = NULL
# )
# 
# grid.draw(venn_diagram) #middle are the categories found from all 3 metrics

counts_traj_pcr  <- table(all_trajectories$pcr_trajectories)
counts_traj_pcr<- as.data.frame(counts_traj_pcr)
colnames(counts_traj_pcr)[2] <- c("traj_counts_pcr")
counts_traj_hrp <- table(all_trajectories$hrp_trajectories)
counts_traj_hrp<- as.data.frame(counts_traj_hrp)
colnames(counts_traj_hrp)[2] <- c("traj_counts_hrp")
counts_traj_pfldh <- table(all_trajectories$pfldh_trajectories)
counts_traj_pfldh<- as.data.frame(counts_traj_pfldh)
colnames(counts_traj_pfldh)[2] <- c("traj_counts_pfldh")

merged_counts <- merge(merge(counts_traj_pcr, counts_traj_hrp, by = "Var1", all= T), counts_traj_pfldh, all = T) 
