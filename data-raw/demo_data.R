## code to prepare `demo_data` dataset goes here

# This R file is not uploaded to GitHub and is not built as a package.
# Parametar
n_items <- 10       # Number of items
n_persons <- 200    # Number of respondents
n_categories <- 5   # Number of response categories

# Generate item parametars
set.seed(123)
a <- runif(n_items, min = 0.1, max = 3)  # 識別力
b_list <- lapply(1:n_items, function(i) sort(runif(n_categories - 1, min = -2, max = 2)))  # 単調増加

a_matrix <- matrix(a, ncol = 1)

d_matrix <- matrix(NA, nrow = n_items, ncol = n_categories - 1)
for (i in 1:n_items) {
  d_matrix[i, ] <- -a[i] * b_list[[i]]
}

# Generate answer data (graded response model) as sim_data
sim_data <- simdata(a = a_matrix,
                    d = d_matrix,
                    N = n_persons,
                    itemtype = rep("graded", n_items))
demo_data <- as.data.frame(sim_data)
head(sim_data)

# Demo data is included in the package.
usethis::use_data(demo_data, overwrite = TRUE)
