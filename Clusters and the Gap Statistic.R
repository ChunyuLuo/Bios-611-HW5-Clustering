library(cluster)

generate_hypercube_clusters <- function(n, k, side_length, noise_sd = 1.0) {
  centers <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    centers[i, i] <- side_length
  }
  
  data <- matrix(0, nrow = n * k, ncol = n)
  labels <- rep(1:n, each = k)
  
  for (i in 1:n) {
    noise <- matrix(rnorm(k * n, mean = 0, sd = noise_sd), nrow = k, ncol = n)
    cluster_points <- sweep(noise, 2, centers[i, ], "+")
    start_idx <- (i - 1) * k + 1
    end_idx <- i * k
    data[start_idx:end_idx,] <- cluster_points
  }
  return(list(data = data, labels = labels, centers = centers))
}

# Simulation
set.seed(123)
dimensions <- c(6, 5, 4, 3, 2)
side_lengths <- seq(10, 1, by = -1)
k <- 100
noise_sd <- 1.0

results <- data.frame()

cat("Running Gap Statistic simulation...\n")
cat("This may take several minutes...\n\n")

for (n in dimensions) {
  cat(sprintf("Testing dimension n  = %d\n", n))
  
  for (side_length in side_lengths) {
    sim_data <- generate_hypercube_clusters(n, k, side_length, noise_sd)
    gap_result <- clusGap(sim_data$data,
                          FUNcluster = kmeans,
                          K.max = 10,
                          nstart = 20,
                          iter.max = 50)

    k_est <- maxSE(gap_result$Tab[, "gap"],
                   gap_result$Tab[, "SE.sim"],
                   method = "firstmax")
    
    results <- rbind(results, data.frame(
      dimension = n,
      side_length = side_length,
      estimated_k = k_est,
      true_k = n
    ))
    
    cat(sprintf("L = %.1f: Estimated k = %d (True k = %d)\n",
                side_length, k_est, n))
  }
  cat("\n")
}

# Visualization
cat("Creating visualizations...\n\n")
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

for (n in dimensions) {
  subset_data <- results[results$dimension == n, ]
  
  plot(subset_data$side_length, subset_data$estimated_k,
       type = "b", pch = 19, col = "blue",
       xlab = "Side Length (L)",
       ylab = "Estimated Number of Clusters",
       main = sprintf("Dimension n = %d", n),
       ylim = c(1, max(n + 1, max(subset_data$estimated_k))),
       cex.main = 1.2)
  
  # reference line for true number of clusters
  abline(h = n, col = "red", lwd = 2, lty = 2)
  
  grid()
  if (n == dimensions[1]) {
    legend("topright",
           legend = c("Estimated k", "True k"),
           col = c("blue", "red"),
           lty = c(1,2),
           lwd = c(1,2),
           pch = c(19, NA))
  }
}
par(mfrow = c(1, 1))

# Critical side_length for each dimension
for (n in dimensions) {
  subset_data <- results[results$dimension == n, ]
  underestimated <- subset_data[subset_data$estimated_k < n, ]
  if (nrow(underestimated) > 0) {
    critical_L <- max(underestimated$side_length)
    failing_range <- subset_data[subset_data$estimated_k < n, ]
    
    cat(sprintf("Dimension n = %d:\n", n))
    cat(sprintf("  Gap satistic begins underestimating at L = %.1f/n", critical_L))
    cat(sprintf("  Estimated k drops to %d at this point\n", min(failing_range$estimated_k)))
    
    ratio <- critical_L / noise_sd
    cat(sprintf("  Critical ratio L/sigma = %.2f\n\n", ratio))
  }
  else {
    cat(sprintf("Dimension n = %d: Gap Statistic correctly identified %d clusters for all tested side lengths\n\n", n, n))
  }
}
cat("According to the simulation result, the Gap Statistic consistently begin to reduce its estimate when L drops to approximately 2-3.")
cat("This corresponds to a critical ratio of L/sigma = 2-3, meaning cluster centers must be separated by at least 2-3 times the within-cluster standard deviation for reliable detection.")