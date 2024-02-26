# Parasite Density Trajectory Analysis

This repository contains an analysis aiming to identify clusters of parasite density across four weeks to describe the trajectory of parasites throughout time.

## Dataset
The dataset used in this analysis contains measurements of parasite density across four weeks. Three different measurement techniques were used ("pcr", "hrp", and "pfldh") at four different time points ("day7", "day14", "day21", and "day28").

## Methodology
The analysis involves several steps:

1. **Data Formatting**: The dataset is processed to combine measurements techniques of each sample across different time points.
2. **Correlation Analysis**: The correlation between parasite measurement techniques is examined using scatterplots.
3. **Clustering Analysis**: K-means clustering is performed to identify clusters of parasite density trajectories across time.
4. **Optimal K Selection**: Elbow method and silhouette analysis are used to determine the optimal number of clusters.
5. **Visualization**: The results are visualized using various plots including elbow plots, silhouette plots, and cluster center plots.

## Libraries Used
- `ggplot2`: Data visualization.
- `dplyr`: Data manipulation.
- `ggpubr`: Publication-ready plots.
- `factoextra`: Clustering analysis visualization.
- `tidyr`: Data tidying.
- `cowplot`: Grid arrangement of plots.
