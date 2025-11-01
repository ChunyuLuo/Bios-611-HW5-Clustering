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

# Spectral clustering wrapper function
spectral_clustering <- function(x, k) {
  n <- nrow(x)
  
  dist_matrix <- as.matrix(dist(x, method = "euclidean"))
  d_threshold <- 1
  A <- (dist_matrix < d_threshold) * 1
  diag(A) <- 0  
  
  D <- diag(rowSums(A))
  D_inv_sqrt <- diag(1 / sqrt(diag(D) + 1e-10)) 
  L_sym <- diag(n) - D_inv_sqrt %*% A %*% D_inv_sqrt
  
  # Eigen-decomposition
  eigen_result <- eigen(L_sym, symmetric = TRUE)
  sorted_indices <- order(eigen_result$values)
  
  # Select k eigenvectors corresponding to k smallest eigenvalues
  k_eigenvectors <- eigen_result$vectors[, sorted_indices[1:k]]
  
  # Cluster the Eigenvectors
  kmeans_result <- kmeans(k_eigenvectors, centers = k, nstart = 20)
  
  return(kmeans_result$cluster)
}

# Simulation
set.seed(42)
sample_data <- generate_shell_clusters(n_shells = 4, k_per_shell = 100, 
                                       max_radius = 10, noise_sd = 0.1)

# Create interactive 3D scatter plot
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

for (i in seq_along(max_radius_values)) {
  max_r <- max_radius_values[i]
  
  if (max_r == 0) {
    estimated_clusters[i] <- 1  
    next
  }
  
  # Generate data with current max_radius
  data_obj <- generate_shell_clusters(n_shells = 4, k_per_shell = 100, 
                                      max_radius = max_r, noise_sd = 0.1)
  
  # Apply spectral clustering
  cluster_assignments <- spectral_clustering(data_obj$data, k = 4)
  
  # Count unique clusters (estimated number)
  estimated_clusters[i] <- length(unique(cluster_assignments))
  
  cat(sprintf("max_radius = %.1f, estimated clusters = %d\n", 
              max_r, estimated_clusters[i]))
}

# Visualize results
plot_data <- data.frame(
  max_radius = max_radius_values,
  estimated_clusters = estimated_clusters
)

fig_results <- plot_ly(plot_data, x = ~max_radius, y = ~estimated_clusters, 
                       type = "scatter", mode = "lines+markers",
                       marker = list(size = 8),
                       line = list(width = 2)) %>%
  add_trace(x = max_radius_values, y = rep(4, length(max_radius_values)),
            mode = "lines", name = "True number of clusters",
            line = list(dash = "dash", color = "red", width = 2)) %>%
  layout(
    title = "Spectral Clustering Performance vs Maximum Radius",
    xaxis = list(title = "Maximum Radius"),
    yaxis = list(title = "Estimated Number of Clusters"),
    showlegend = TRUE
  )

print(fig_results)
cat("As the max_radius decreases below 1.0, the shells become closer than the distance threshold (d_threshold = 1.0),
    causing adjacent shells to connect in the similarity graph and merge into a single cluster. This marks the algorithm's 
    failure point, where spectral clustering can no longer separate distinct shells. If d_threshold were smaller (0.8), the
    algorithm would tolerate tighter shells before failing; if larger (1.2), it would fail earlier because clusters would merge sooner.")