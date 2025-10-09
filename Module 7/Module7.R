# 1. Load Dataset ----------------------------------------------------------

data("mtcars")

# Inspect data
head(mtcars)
str(mtcars)


# 2. Test Generic Functions ------------------------------------------------

# Base generics on data.frame
print(mtcars)
summary(mtcars)

# Simple plot
plot(mtcars$wt, mtcars$mpg,
     xlab = "Weight (1000 lbs)",
     ylab = "MPG",
     main = "MPG vs Weight")

# Create an S3 object from a linear model
fit <- lm(mpg ~ wt + hp, data = mtcars)

# Test dispatch
print(fit)       # calls print.lm
summary(fit)     # calls summary.lm
plot(fit)        # calls plot.lm


# 3. Explore S3 System -----------------------------------------------------

# Create S3 object
s3_obj <- list(name = "Myself", age = 29, GPA = 3.5)
class(s3_obj) <- "student_s3"
class(s3_obj)

# Define S3 print method
print.student_s3 <- function(x, ...) {
  cat("S3 Student\n")
  cat(sprintf("Name: %s | Age: %d | GPA: %.2f\n",
              x$name, x$age, x$GPA))
  invisible(x)
}

# Test dispatch
print(s3_obj)
summary(s3_obj)   # falls back to summary.default


# 4. Explore S4 System -----------------------------------------------------

# Define S4 class
setClass("student_s4",
         slots = c(name = "character",
                   age = "numeric",
                   GPA = "numeric"))

# Create S4 object
s4_obj <- new("student_s4", name = "Myself", age = 29, GPA = 3.5)

# Confirm S4
isS4(s4_obj)
class(s4_obj)

# Define S4 print (show) method
setMethod("show", "student_s4",
          function(object) {
            cat("S4 Student\n")
            cat(sprintf("Name: %s | Age: %d | GPA: %.2f\n",
                        object@name, as.integer(object@age), object@GPA))
          })

# Display
s4_obj


# 5. Define a custom S4 summary generic -----------------------------------

setGeneric("summaryS4", function(object) standardGeneric("summaryS4"))

setMethod("summaryS4", "student_s4",
          function(object) {
            list(
              name = object@name,
              age = object@age,
              GPA = object@GPA,
              status = ifelse(object@GPA >= 3.0,
                              "Good standing", "Probation")
            )
          })

# Call method
summaryS4(s4_obj)

