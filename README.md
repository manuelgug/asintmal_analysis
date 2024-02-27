# Parasite Density Trajectory Analysis

This repository contains an analysis aiming to identify clusters that describe the trajectory of *P. falciparum* throughout time.

![img.png](https://github.com/manuelgug/asintmal_analysis/blob/main/imgs/img.png)


## Dataset
The dataset used in this analysis contains measurements of parasite density across four weeks. Three different measurement techniques for parasite density were used ("pcr", "hrp", and "pfldh") at four different time points ("day7", "day14", "day21", and "day28").

## Workflow

1. **Data Import and Preprocessing**: The script reads the data from an Excel file, extracts relevant columns, and preprocesses the data for analysis.

2. **Outlier Removal**: Multivariate outliers are detected and removed using Mahalanobis distance.

3. **Summary Statistics**: Summary statistics such as mean, median, and quartiles are calculated for each assay.

4. **Correlation Analysis**: Pairwise correlation between parasite density measurements from different assays is analyzed and visualized using scatterplots.

5. **Clustering Analysis**: K-means clustering is performed on the data to identify clusters based on parasite density measurements. Elbow plot and silhouette plot are generated to determine the optimal number of clusters. Cluster centers are visualized using line plots.


## Libraries Used
- `ggplot2`: Data visualization.
- `dplyr`: Data manipulation.
- `ggpubr`: Publication-ready plots.
- `factoextra`: Clustering analysis visualization.
- `tidyr`: Data tidying.
- `cowplot`: Grid arrangement of plots.
