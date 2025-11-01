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

fig_dir <- file.path("output", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

cat("Creating 3D visualization of concentric shells...\n")
png(
  file.path(fig_dir, "Concentric Shell Clusters.png"),
  width = 800,
  height = 800
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

colors <- c("#440154", "#31688e", "#35b779", "#fde724")

plot(
  sample_data$data[, 1],
  sample_data$data[, 2],
  col = colors[sample_data$labels],
  pch = 16,
  cex = 0.5,
  xlab = "X",
  ylab = "Y",
  main = "XY Projection"
)
grid()

plot(
  sample_data$data[, 1],
  sample_data$data[, 3],
  col = colors[sample_data$labels],
  pch = 16,
  cex = 0.5,
  xlab = "X",
  ylab = "Z",
  main = "XZ Projection"
)
grid()

plot(
  sample_data$data[, 2],
  sample_data$data[, 3],
  col = colors[sample_data$labels],
  pch = 16,
  cex = 0.5,
  xlab = "Y",
  ylab = "Z",
  main = "YZ Projection"
)
grid()

plot.new()
legend(
  "center",
  legend = paste("Shell", 1:4),
  col = colors,
  pch = 16,
  cex = 1.2,
  title = "Concentric Shell Clusters"
)

dev.off()

cat("Running spectral clustering simulation...\n")
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

cat("\nCreating performance plot...\n")
png(
  file.path(fig_dir, "Spectral Clustering Performance vs Maximum Radius.png"),
  width = 800,
  height = 600
)

plot(
  max_radius_values,
  estimated_clusters,
  type = "b",
  pch = 19,
  col = "blue",
  lwd = 2,
  xlab = "Maximum Radius",
  ylab = "Estimated Number of Clusters",
  main = "Spectral Clustering Performance vs Maximum Radius",
  ylim = c(0, 5),
  cex.main = 1.2
)

abline(h = 4, col = "red", lwd = 2, lty = 2)
grid()

legend(
  "bottomright",
  legend = c("Estimated k", "True k = 4"),
  col = c("blue", "red"),
  lty = c(1, 2),
  lwd = 2,
  pch = c(19, NA)
)

dev.off()

cat("\nPlots saved successfully to:", fig_dir, "\n")
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