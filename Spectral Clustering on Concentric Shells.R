library(plotly)
library(MASS)

generate_shell_clusters <- function(n_shells, k_per_shell, max_radius, noise_sd = 0.1) {
  radii <- seq(max_radius / n_shells, max_radius, length.out = n_shells)
  
  all_points <- list()
  all_labels <- numeric()
  
  for (shell_idx in 1:n_shells) {
    radius <- radii[shell_idx]
    
    theta <- runif(k_per_shell, 0, 2 * pi)
    phi <- acos(runif(k_per_shell, -1, 1))
    
    x <- radius * sin(phi) * cos(theta)
    y <- radius * sin(phi) * sin(theta)
    z <- radius * cos(phi)
    
    noise <- rnorm(k_per_shell, 0, noise_sd)
    r_noisy <- radius + noise
    
    x <- r_noisy * sin(phi) * cos(theta)
    y <- r_noisy * sin(phi) * sin(theta)
    z <- r_noisy * cos(phi)
    
    shell_points <- cbind(x, y, z)
    all_points[[shell_idx]] <- shell_points
    all_labels <- c(all_labels, rep(shell_idx, k_per_shell))
  }
  
  data <- do.call(rbind, all_points)
  
  return(list(data = data, labels = all_labels))
}

spectral_clustering <- function(x, k) {
  n <- nrow(x)
  
  dist_matrix <- as.matrix(dist(x, method = "euclidean"))
  
  d_threshold <- 1
  A <- (dist_matrix < d_threshold) * 1
  diag(A) <- 0
  
  D <- diag(rowSums(A))
  D_inv_sqrt <- diag(1 / sqrt(diag(D) + 1e-10))
  L_sym <- diag(n) - D_inv_sqrt %*% A %*% D_inv_sqrt
  
  eigen_result <- eigen(L_sym, symmetric = TRUE)
  sorted_indices <- order(eigen_result$values)
  k_eigenvectors <- eigen_result$vectors[, sorted_indices[1:k]]
  
  kmeans_result <- kmeans(k_eigenvectors, centers = k, nstart = 20)
  
  return(kmeans_result$cluster)
}

set.seed(42)
sample_data <- generate_shell_clusters(
  n_shells = 4,
  k_per_shell = 100,
  max_radius = 10,
  noise_sd = 0.1
)

fig <- plot_ly(
  x = sample_data$data[, 1],
  y = sample_data$data[, 2],
  z = sample_data$data[, 3],
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 3,
    color = sample_data$labels,
    colorscale = "Viridis",
    showscale = TRUE
  )
) %>%
  layout(
    title = "Concentric Shell Clusters (Ground Truth)",
    scene = list(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "Z")
    )
  )

print(fig)

max_radius_values <- seq(10, 0, by = -0.5)
estimated_clusters <- numeric(length(max_radius_values))

set.seed(123)
for (i in seq_along(max_radius_values)) {
  max_r <- max_radius_values[i]
  
  if (max_r == 0) {
    estimated_clusters[i] <- 1
    next
  }
  
  data_obj <- generate_shell_clusters(
    n_shells = 4,
    k_per_shell = 100,
    max_radius = max_r,
    noise_sd = 0.1
  )
  
  cluster_assignments <- spectral_clustering(data_obj$data, k = 4)
  estimated_clusters[i] <- length(unique(cluster_assignments))
  
  cat(sprintf("max_radius = %.1f, estimated clusters = %d\n",
              max_r, estimated_clusters[i]))
}

plot_data <- data.frame(
  max_radius = max_radius_values,
  estimated_clusters = estimated_clusters
)

fig_results <- plot_ly(
  plot_data,
  x = ~max_radius,
  y = ~estimated_clusters,
  type = "scatter",
  mode = "lines+markers",
  marker = list(size = 8),
  line = list(width = 2)
) %>%
  add_trace(
    x = max_radius_values,
    y = rep(4, length(max_radius_values)),
    mode = "lines",
    name = "True number of clusters",
    line = list(dash = "dash", color = "red", width = 2)
  ) %>%
  layout(
    title = "Spectral Clustering Performance vs Maximum Radius",
    xaxis = list(title = "Maximum Radius"),
    yaxis = list(title = "Estimated Number of Clusters"),
    showlegend = TRUE
  )

print(fig_results)

cat("\nSaving plots to output/figures...\n")

fig_dir <- file.path("output", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("webshot2", quietly = TRUE)) {
  cat("Installing webshot2 for PNG export...\n")
  install.packages("webshot2", repos = "http://cran.us.r-project.org")
}

library(webshot2)

temp_dir <- tempdir()
temp_file1 <- file.path(temp_dir, "temp_plot1.html")
temp_file2 <- file.path(temp_dir, "temp_plot2.html")

htmlwidgets::saveWidget(fig, temp_file1, selfcontained = TRUE)
webshot2::webshot(
  temp_file1,
  file.path(fig_dir, "Concentric Shell Clusters.png"),
  vwidth = 800,
  vheight = 600
)

htmlwidgets::saveWidget(fig_results, temp_file2, selfcontained = TRUE)
webshot2::webshot(
  temp_file2,
  file.path(fig_dir, "Spectral Clustering Performance vs Maximum Radius.png"),
  vwidth = 800,
  vheight = 600
)

cat("Plots saved successfully to:", fig_dir, "\n")
cat("  - Concentric Shell Clusters.png\n")
cat("  - Spectral Clustering Performance vs Maximum Radius.png\n\n")

cat("=== INTERPRETATION ===\n")
cat("As max_radius decreases, the shells become closer together.\n")
cat("When shells are too close (relative to d_threshold = 1), the algorithm\n")
cat("cannot distinguish them because points from different shells become\n")
cat("connected in the similarity graph.\n\n")
cat("The failure point occurs when the spacing between shells becomes\n")
cat("comparable to or smaller than d_threshold.\n\n")
cat("If we used a different d_threshold:\n")
cat("- Smaller d_threshold (e.g., 0.8): Would be more selective, requiring\n")
cat("  larger max_radius to separate shells. More robust to close shells.\n")
cat("- Larger d_threshold (e.g., 1.2): Would connect more distant points,\n")
cat("  failing at larger max_radius values. Less robust to close shells.\n")