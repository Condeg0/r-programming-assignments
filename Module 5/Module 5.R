# Create matrices
A <- matrix(1:100,  nrow = 10)
B <- matrix(1:1000, nrow = 10)

# Inspect dimensions
dim(A)  # 10 × 10 (square)
dim(B)  # 10 × 100 (not square)

# Compute inverse and determinant for A
invA <- solve(A)
detA <- det(A)

# Handle errors for B (not square)
invB <- tryCatch(solve(B), error = function(e) e)
detB <- tryCatch(det(B),   error = function(e) e)

# Print results
#invA
detA
invB
detB
