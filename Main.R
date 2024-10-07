# Block 1: Load Necessary Libraries
library(data.table)
library(ggplot2)
library(plotly)

data_filename <- "mnist_train.csv"
train_url <- "https://pjreddie.com/media/files/mnist_train.csv"

if (!file.exists(data_filename)) {
  download.file(train_url, destfile = data_filename)
  cat("MNIST data downloaded and saved as", data_filename, "\n")
} else {
  cat("MNIST data already exists locally as", data_filename, "\n")
}

mnist_data <- fread(data_filename)

# Block 3: Sample a Subset for Efficiency
set.seed(263)
sample_indices <- sample(1:nrow(mnist_data), 15000)
mnist_sample <- mnist_data[sample_indices, ]

# Block 4: Separate Labels and Image Data
labels <- mnist_sample$V1
images <- as.matrix(mnist_sample[, -1, with = FALSE])

# Block 4a: Remove Zero-Variance Columns
variances <- apply(images, 2, var)
zero_var_cols <- which(variances == 0)

if(length(zero_var_cols) > 0) {
  images_filtered <- images[, -zero_var_cols]
  cat("Removed", length(zero_var_cols), "zero-variance columns.\n")
} else {
  images_filtered <- images
  cat("No zero-variance columns found.\n")
}

# Block 5: Perform PCA
pca_result <- prcomp(images_filtered, center = TRUE, scale. = TRUE)

# Block 6: Plot Variance Explained by Principal Components
explained_variance <- pca_result$sdev^2
plot(explained_variance, type = "l", main = "Scree Plot", 
     xlab = "Principal Component", ylab = "Variance Explained")

# Block 7: Interactive Visualization of Principal Components
# Create a data frame with the first few principal components and labels.
pca_data <- data.frame(PC1 = pca_result$x[, 1],
                       PC2 = pca_result$x[, 2],
                       PC3 = pca_result$x[, 3],
                       label = as.factor(labels))

# Set plot_type to "2D" or "3D"
  plot_type <- "3D"
  
  if (plot_type == "2D") {  # 2d plot 
    p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = label)) +
      geom_point(alpha = 0.6) +
      labs(title = "2D PCA of MNIST Dataset",
           x = "Principal Component 1",
           y = "Principal Component 2") +
      theme_minimal()
    
    # Convert ggplot to interactive plot
    interactive_plot <- ggplotly(p)
    
  } else if (plot_type == "3D") { # 2d plot 
    interactive_plot <- plot_ly(pca_data, x = ~PC1, y = ~PC2, z = ~PC3,
                                color = ~label, colors = RColorBrewer::brewer.pal(n = 10, name = "Paired"),
                                type = 'scatter3d', mode = 'markers',
                                marker = list(size = 8,
                                              line = list(width = 2, color = 'black')))
    interactive_plot <- layout(interactive_plot,
                          title = "3D PCA of MNIST Dataset",
                          scene = list(xaxis = list(title = 'Principal Component 1'),
                          yaxis = list(title = 'Principal Component 2'),
                          zaxis = list(title = 'Principal Component 3')))
  } else {
    stop("Invalid plot_type. Choose '2D' or '3D'.")
  }
  
  # Display the interactive plot
  interactive_plot
