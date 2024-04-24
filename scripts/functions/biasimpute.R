# Set of utility functions used in imputation of unidentified bias models project
# Author: T.Varnet P?rez

# Wrapper for multiple inspecting function calls
whatis <- function(x, print_str = TRUE){
  # Numeric aspects of the object
  numeric_vector <- c(capture.output(dim(x)), as.character(length(x)), is.atomic(x))
  names(numeric_vector) <- c("Dimension", "Length", "Atomic?")
  # Type, class and mode 
  character_vector <- c(class(x), mode(x), typeof(x))
  names(character_vector) <- c("Class", "Mode", "Type of")
  # Structure, since it has multiple elements
  structure <- capture.output(str(x))
  names(structure) <- "Structure"
  # Print a list with both vectors
  print(list(numeric_vector, character_vector, if(print_str == TRUE) structure))
}

# Get the canonical equation from the conic section general quadratic form
# For the ellipse defined by a 3x3 correlation matrix fixing one of the values
# The third correlation is the observed correlation value between (X, Y)
conic_to_canonical_ellipse <- function(third_correlation){
  # Based on https://math.stackexchange.com/a/820896
  if (getOption("warn") >= 0) {
    stopifnot(
      "`third_correlation` should be a numeric scalar between -1 and 1" =
        is.numeric(third_correlation) & length(third_correlation) == 1 &
        (third_correlation < 1 & third_correlation > -1)
    )
  }
  
  A_ = 1
  B_ = -2 * third_correlation
  C_ = 1
  D_ = 0
  E_ = 0
  F_ = third_correlation^2 - 1
  
  # Determinant
  det_                     <- (4*A_*C_ - B_^2)
  # Coefficient normalizing factor
  q                        <- 64*(F_*det_ - A_ * E_^2 + B_*D_*E_ - C_*D_^2)/(det_^2)
  # Distance between center and focal point
  s                        <- 0.25*sqrt(abs(q) * sqrt(B_^2 + (A_ - C_)^2))
  
  # Axes
  semi_major_axis          <- 0.125*sqrt(2 * abs(q) * sqrt(B_^2 + (A_ - C_)^2) - 2*q*(A_+ C_))
  semi_minor_axis          <- sqrt(semi_major_axis^2 - s^2)
  
  # Center
  center_x                 <- (B_*E_ - 2*C_*D_)/(4*A_*C_ - B_^2)
  center_y                 <- (B_*D_ - 2*A_*E_)/(4*A_*C_ - B_^2)
  
  return(list(a = semi_major_axis, b = semi_minor_axis, center = list(center_x, center_y)))
}


# Draw a basic plot with the admissible ellipse. If fed a list with samples,
# plot the points corresponding to sampled values from the prior.
# Requires `conic_to_canonical_ellipse` 
draw_admissible_ellipse <- function(corXY,
                                    prior_samples = NULL){
  # Get the axes coordinates from the ellipse
  ellipse_equation <- conic_to_canonical_ellipse(third_correlation = corXY)
  # Draw the ellipse according to the equation
  tmp_plot <- ggplot() +
    coord_fixed()  +  
    ggforce::geom_ellipse(aes(x0    = 0, y0 = 0,
                              a     = ellipse_equation$a,
                              b     = ellipse_equation$b,
                              angle = pi / 4), # Cannot add fill with high transparency
                          # as n is 350 by default so it is adding 350 fills
                          linewidth  = 1.2,
                          color = "gray1") 
  # If samples are fed to the function, draw the points 
  if (!is.null(prior_samples)) {
    tmp_plot + geom_point(aes(x = prior_samples$UX,
                              y = prior_samples$UY),
                          alpha = 0.2) +
      labs(x        = "Sampled Cor(U,X)",
           y        = "Sampled Cor(U,Y)",
           title    = stringr::str_wrap("Admisible correlation space explored by prior draws", wrapping_length))
  }
  return(tmp_plot)
}


# Get the bounds for the correlation of a third variable
# With two variables with whose correlations 'x' and 'y' are fixed

# Overloaded version, can clean code as pmax and pmin can work for both scenarios
# Just need to convert z from matrix to vector if one of the dim == 1
common_corr_bounds <- function(x, y,
                               return_matrix = FALSE){ # Two correlation values
  
  if (getOption("warn") >= 0) {
    stopifnot(
      "`x` must be a numeric value between -1 and +1." =
        is.numeric(x) & length(x) == 1 & (x < 1 & x > -1),
      "`y` must be a scalar or vector of numerics between -1 and +1." =
        (is.numeric(y) & length(y) > 1  & all(y < 1 & y > -1)) ||
        (is.numeric(y) & length(y) == 1 & (y < 1 & y > -1))
    )
  }
  
  quad_formula <- function(a, b, c)
  {
    a <- as.complex(a) # Unnecessary if we are checking arguments within cor range?
    answer <- cbind((-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
                    (-b - sqrt(b^2 - 4 * a * c)) / (2 * a))
    if(all(Im(answer) == 0)) answer <- Re(answer)
    if(answer[1,1] == answer[1,2]) return(answer[1,1])
    answer
  }
  
  # Parameters of quadratic formula
  a = -1
  b = 2*x*y
  c = (1-x^2-y^2)
  
  # If y is a scalar
  if (length(y) == 1) { 
    z <- vector(mode = "numeric", length = 2)
    # Lower bound
    z[1] <- max(-1, quad_formula(a,b,c)[1]) 
    # Upper bound
    z[2] <- min(1, quad_formula(a,b,c)[2])
    if (return_matrix == TRUE) {
      z <- t(z) # "t(): When x is a vector, it is treated as a column, i.e., the result is a 1-row matrix."
    }
    return(z)
  }
  # If y is a vector
  else if (length(y) > 1) { 
    n <- length(y)
    z <- matrix(NA_real_, nrow = n, ncol = 2)
    
    # Lower bound
    z[,1] <- pmax(-1, quad_formula(a,b,c)[,1]) 
    # Upper bound
    z[,2] <- pmin( 1, quad_formula(a,b,c)[,2])
    return(z)
  }
}



# Get a vector of `N` samples
# from a bivariate normal prior with respective means and sds
# trimmed from positive-definiteness constraints
rnorm_bi_pdtrim <- function(N, 
                            UXprior_mean,  
                            UXprior_sd  ,  
                            UYprior_mean,  
                            UYprior_sd,
                            corXY){
  
  # Pre-allocation and initialization
  rhos           <- list(UX = numeric(), UY = numeric())  
  current_length <- 0L
  tmp_bind <- matrix(NA_real_, nrow = 0, ncol = 2) # Zero rows so 'while' loop applies
  
  while (current_length != N) {
    while(dim(tmp_bind)[1] == 0){
      tmp_UX <- rnorm(N, mean = UXprior_mean, sd = UXprior_sd)
      tmp_UY <- rnorm(N, mean = UYprior_mean, sd = UYprior_sd)
      
      # Range truncation
      # Only consider values between -1 and +1
      tmp_UX <- tmp_UX[tmp_UX > -1 & tmp_UX < 1] 
      tmp_UY <- tmp_UY[tmp_UY > -1 & tmp_UY < 1]
      # Trim the other vector to the shortest length truncated vector
      minimum_common_length <- min(length(tmp_UX), length(tmp_UY))
      tmp_UX <- tmp_UX[1:minimum_common_length]
      tmp_UY <- tmp_UY[1:minimum_common_length]
      # Positive-definiteness truncation
      # Only consider values larger than lower bound
      # and smaller than upper bound
      tmp_bind <- cbind(tmp_UX, tmp_UY)
      tmp_bind <- tmp_bind[tmp_UX > common_corr_bounds(corXY, tmp_UY, return_matrix = TRUE)[,1] &
                             tmp_UX < common_corr_bounds(corXY, tmp_UY, return_matrix = TRUE)[,2], ,drop = FALSE] # Will coerce into vector if the truncation returns a N x 1 array, hence drop is needed.
    }
    # Define smallest length as new number of samples to add
    reduced_samples <- min(dim(tmp_bind)[1], N - current_length)
    rhos$UX[(current_length+1):(current_length+reduced_samples)] <- tmp_bind[1:reduced_samples, 1]
    rhos$UY[(current_length+1):(current_length+reduced_samples)] <- tmp_bind[1:reduced_samples, 2]
    
    # Update values
    current_length <- length(rhos$UX)
    tmp_bind <- matrix(NA_real_, nrow = 0, ncol = 2) # Back to original values
  }
  attr(rhos, "distribution") <- "normal"
  return(rhos)
}

# Get 'N' number of samples
# from a bivariate (trimmed) uniform prior
runif_bi_pdtrim <- function(N,
                            corXY,
                            min = -1,
                            max = +1){
  
  rhos           <- list(UX = numeric(), UY = numeric())
  current_length <- 0L
  tmp_bind <- matrix(NA_real_, nrow = 0, ncol = 2) # Pre-allocate with zero rows
  
  while (current_length != N) {
    while(dim(tmp_bind)[1] == 0){ # If all values are truncated, re-draw rather than continue
      tmp_UX <- runif(N, min = min, max = max)
      tmp_UY <- runif(N, min = min, max = max)
      
      # Positive-definiteness truncation
      # Only consider values larger than lower bound
      # and smaller than upper bound
      tmp_bind <- cbind(tmp_UX, tmp_UY)
      tmp_bind <- tmp_bind[tmp_UX > common_corr_bounds(corXY, tmp_UY, return_matrix = TRUE)[,1] &
                             tmp_UX < common_corr_bounds(corXY, tmp_UY, return_matrix = TRUE)[,2], ,drop = FALSE] # Will coerce into vector if the truncation returns a N x 1 array, hence drop is needed.
    }
    # Define smallest length as new number of samples to add
    reduced_samples <- min(dim(tmp_bind)[1], N - current_length)
    # N - current_length) 
    rhos$UX[(current_length+1):(current_length+reduced_samples)] <- tmp_bind[1:reduced_samples, 1]
    rhos$UY[(current_length+1):(current_length+reduced_samples)] <- tmp_bind[1:reduced_samples, 2]
    
    # Update values
    current_length <- length(rhos$UX)
    tmp_bind <- matrix(NA_real_, nrow = 0, ncol = 2) # Back to original values
  }
  attr(rhos, "distribution") <- "uniform"
  return(rhos)
}

# PENDING # to extend to categoricals
generate_correlated <- function(targets, noise, rho, threshold=1e-12) {
  # Based on https://stats.stackexchange.com/a/313138/
  if (getOption("warn") >= 0){
    stopifnot(
      "`targets` must be either a matrix or a vector of numerics" =
        is.numeric(targets),
      "`rho` must be a numeric vector or scalar" =
        is.numeric(rho)
    )
  }
  
  if(is.null(dim(targets))){ # Univariate target case
    if (missing(noise)) noise <- rnorm(length(targets)) # Supply a default if `noise` is not given
    e <- residuals(lm(noise ~ targets))
    z <- rho * sd(e) * targets + sqrt(1 - rho^2) * sd(targets) * e # correlation * scaling * vector
    # "By multiplying each variate by the standard deviation of the other, the standard deviation of each term is the same and equal to the standard deviation of Z"
    return(z)
  } 
  
  else{ # Multivariate target case
    # Process the arguments.
    if(!is.matrix(targets)) targets <- matrix(targets, ncol=1) # Make sure targets is a matrix.
    d <- ncol(targets) # Number of variables
    n <- nrow(targets) # Sample size
    targets <- scale(targets, center=FALSE) # Makes computations simpler
    if (missing(noise)) noise <- rnorm(n) 
    # Remove the random variation effect of `targets` on `noise`.
    e <- residuals(lm(noise ~ targets))
    # Calculate the coefficient `sigma` of `e` so that the correlation of
    # `targets` with the linear combination targets.dual %*% rho + sigma*e is the desired
    # vector.
    targets.dual <- with(svd(targets), (n-1)*u %*% diag(ifelse(d > threshold, 1/d, 0)) %*% t(v))
    sigma2 <- c((1 - rho %*% cov(targets.dual) %*% rho) / var(e))
    
    # Return this linear combination.
    if (sigma2 >= 0) {
      sigma <- sqrt(sigma2) 
      z <- targets.dual %*% rho + sigma*e
    } else { # If the error variance has to be negative, it is impossible
      warning("Correlations are impossible.")
      z <- rep(0, n)
    }
    return(z)
  }
}