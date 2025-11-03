# Assignment #11: Debugging and Defensive Programming

# --- Helper Function ---
# The assignment assumes a function tukey.outlier() exists.
# We define one here based on the standard Tukey rule (1.5 * IQR).
tukey.outlier <- function(v) {
  # Calculate Q1, Q3, and IQR
  q1 <- quantile(v, 0.25, na.rm = TRUE)
  q3 <- quantile(v, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  # Define outlier bounds
  lower_bound <- q1 - (1.5 * iqr)
  upper_bound <- q3 + (1.5 * iqr)
  
  # Return a logical vector: TRUE if it's an outlier
  return(v < lower_bound | v > upper_bound)
}

# --- 1. Original Buggy Code [cite: 99-108] ---
tukey_multiple <- function(x) {
  outliers <- array(TRUE, dim = dim(x))
  for (j in 1:ncol(x)) {
    # This line uses && (logical AND) instead of & (element-wise AND) [cite: 103]
    outliers[, j] <- outliers[, j] && tukey.outlier(x[, j])
  }
  outlier.vec <- vector("logical", length = nrow(x))
  for (i in 1:nrow(x)) {
    outlier.vec[i] <- all(outliers[i, ])
  }
  return(outlier.vec)
}

# --- 2. Reproduce the Error [cite: 111-115] ---
set.seed(123)
test_mat <- matrix(rnorm(50), nrow = 10) # 10 rows, 5 columns [cite: 114]

# --- 3. Fix the Code [cite: 120-122] ---
# The corrected function uses & for element-wise comparison.
corrected_tukey <- function(x) {
  outliers <- array(TRUE, dim = dim(x))
  for (j in seq_len(ncol(x))) {
    # THE FIX: Replaced && with & 
    outliers[, j] <- outliers[, j] & tukey.outlier(x[, j])
  }
  outlier.vec <- logical(nrow(x))
  for (i in seq_len(nrow(x))) {
    outlier.vec[i] <- all(outliers[i, ])
  }
  return(outlier.vec) # Use return() for clarity
}


# --- 4. Validate Your Fix  ---
validation_output <- corrected_tukey(test_mat)

# Print the result to verify it works
print("Validation Output:")
print(validation_output)


# --- 5. Defensive Enhancements (Optional) [cite: 140-144] ---
# A final, safer version of the function with input checks.
tukey_defensive <- function(x) {
  
  # --- Defensive Checks ---
  # Check 1: Is the input a matrix? [cite: 142]
  if (!is.matrix(x)) {
    stop("Input 'x' must be a matrix.") # 
  }
  # Check 2: Is the matrix numeric? [cite: 142]
  if (!is.numeric(x)) {
    stop("Input 'x' must be a numeric matrix.") # 
  }
  # --- End Checks ---
  
  outliers <- array(TRUE, dim = dim(x))
  for (j in seq_len(ncol(x))) {
    outliers[, j] <- outliers[, j] & tukey.outlier(x[, j])
  }
  
  outlier.vec <- logical(nrow(x))
  for (i in seq_len(nrow(x))) {
    outlier.vec[i] <- all(outliers[i, ])
  }
  return(outlier.vec)
}

# Test the defensive function
print("Defensive Function Output:")
print(tukey_defensive(test_mat))

# Test the error message
print(tukey_defensive("not a matrix"))
