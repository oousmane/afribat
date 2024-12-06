#' Calculate Margalef's Diversity Index
#'
#' This function calculates Margalef's diversity index, a measure of species richness
#' relative to the total number of individuals, using species names.
#'
#' @param x A character vector of species names. Each element represents an individual of a species.
#' @param details Logical. If `TRUE`, the function returns a list containing the
#' Margalef index, the number of unique species (\code{S}), and the total number of individuals (\code{N}).
#' Defaults to \code{FALSE}.
#'
#' @return If \code{return_details = FALSE} (default), a numeric value representing Margalef's diversity index.
#' If \code{return_details = TRUE}, a list with the following components:
#' \itemize{
#'   \item \code{Margalef_Index}: The calculated Margalef index.
#'   \item \code{S}: The number of unique species.
#'   \item \code{N}: The total number of individuals.
#' }
#' Returns \code{NA} if the index is undefined (e.g., fewer than two species or individuals).
#'
#' @examples
#' # Example 1: Calculate Margalef's index for a simple dataset
#'  x <- c("jonesi","gambiensis","hispida","hispida","thebaica","helvum","helvum","gambianus","gambianus","pusillus","monstrosus","aegyptiacus")
#' margalef(x) # Simple output
#'
#' # Example 2: Return detailed results
#' margalef(x, details = TRUE)
#'
#' # Example 3: Handle edge cases
#' margalef(c("gambiensis")) # Returns NA with a warning
#'
#' @note Margalef's index is undefined if there is only one species or individual.
#' The function will return \code{NA} in such cases with a warning.
#' @source Margalef, R. (1958) Information Theory in Ecology. General Systems, 3, 36-71.
#' @export
margalef <- function(x, details = FALSE) {

  # Check for empty input
  if (length(x) == 0) stop("x should not be empty")

  # Check if input is a character vector
  if (!is.character(x)) stop("x should be a vector of character")

  # Calculate the number of unique species (s) and total individuals (n)
  s <- length(unique(x))
  n <- length(x)

  # Handle edge cases
  if (n <= 1 || s <= 1) {
    warning("Margalef index is undefined for fewer than 2 species or individuals")
    return(NA)
  }

  # Calculate Margalef's index
  d <- (s - 1) / log(n)

  # Return results
  if (details) {
    return(list(margalef_index = d, S = s, N = n))
  } else {
    return(d)
  }
}

#' Calculate Shannon-Weiner Diversity Index
#'
#' This function calculates the Shannon-Weiner diversity index, a measure of species diversity
#' based on species relative abundance.
#'
#' @param x A character vector of species names. Each element represents an individual of a species.
#'
#' @return A numeric value representing the Shannon-Weiner diversity index. Returns \code{NA} if the input
#' vector is empty or contains fewer than two unique species.
#'
#' @examples
#' # Example 1: Calculate Shannon-Weiner index for a dataset
#' x <- c("jonesi","gambiensis","hispida","hispida","thebaica","helvum","helvum","gambianus","gambianus","pusillus","monstrosus","aegyptiacus")
#' shannon_weiner(x)
#'
#' # Example 2: Handle edge cases
#' shannon_weiner(c("jonesi")) # Returns 0
#' shannon_weiner(character(0)) # Returns NA with a warning
#'
#' @note The Shannon-Weiner index is undefined for an empty vector or vectors with only one unique species.
#' For such cases, the function will return \code{NA} or 0 as appropriate.
#'
#' @export
shannon_weiner <- function(x) {
  # Check if input is a character vector
  if (!is.character(x)) stop("x should be a vector of character")

  # Check for empty input
  if (length(x) == 0) {
    warning("Input vector is empty. Returning NA.")
    return(NA_real_)
  }

  # Calculate relative abundance (p)
  p <- table(x) / length(x)

  # Handle edge case: only one species present
  if (length(p) < 2) {
    return(0) # Shannon-Weiner index is 0 for a single species
  }

  # Calculate Shannon-Weiner index
  H <- -sum(p * log(p))
  return(H)
}

#' Calculate Evenness Index
#'
#' This function calculates the species evenness, which is a measure of how evenly individuals
#' are distributed among the different species in a community. It is based on the Shannon-Weiner diversity index.
#'
#' @param x A character vector of species names. Each element represents an individual of a species.
#'
#' @return A numeric value representing the evenness index. Returns \code{NA} if the input vector is empty
#' or contains fewer than two unique species.
#'
#' @examples
#' # Example 1: Calculate evenness for a dataset
#' x <- c("jonesi","gambiensis","hispida","hispida","thebaica","helvum","helvum","gambianus","gambianus","pusillus","monstrosus","aegyptiacus")
#' evenness(x)
#'
#' # Example 2: Handle edge cases
#' evenness(c("thebaica")) # Returns NA with a warning
#' evenness(character(0)) # Returns NA with a warning
#'
#' @note Evenness is undefined for empty vectors or vectors with fewer than two unique species.
#' The function will return \code{NA} in such cases with a warning.
#'
#' @seealso \code{\link{shannon_weiner}} for the Shannon-Weiner diversity index.
#'
#' @export

evenness <- function(x) {
  # Check if input is a character vector
  if (!is.character(x)) stop("x should be a vector of character")

  # Check for empty input
  if (length(x) == 0) {
    warning("Input vector is empty. Returning NA.")
    return(NA)
  }

  # Calculate the number of unique species
  s <- length(unique(x))

  # Handle edge case: only one species present
  if (s < 2) {
    warning("Evenness is undefined for fewer than 2 species. Returning NA.")
    return(NA)
  }

  # Calculate Shannon-Weiner index
  H <- shannon_weiner(x)

  # Calculate evenness
  J <- H / log(s)

  return(J)
}

#' Calculate Simpson's Diversity Index
#'
#' This function calculates Simpson's diversity index, which measures the probability
#' that two individuals randomly selected from a sample will belong to different species.
#'
#' @param x A character vector of species names. Each element represents an individual of a species.
#'
#' @return A numeric value representing Simpson's diversity index. Returns \code{NA} if the input vector
#' is empty or contains fewer than two unique species.
#'
#' @examples
#' # Example 1: Calculate Simpson's diversity index
#' x <- c("jonesi","gambiensis","hispida","hispida","thebaica","helvum","helvum","gambianus","gambianus","pusillus","monstrosus","aegyptiacus")
#' simpson(x)
#'
#' # Example 2: Handle edge cases
#' simpson(c("pusillus")) # Returns NA with a warning
#' simpson(character(0)) # Returns NA with a warning
#'
#' @note Simpson's index is undefined for an empty vector or vectors with only one unique species.
#' The function will return \code{NA} in such cases with a warning.
#'
#' @export
simpson <- function(x) {
  # Check if input is a character vector
  if (!is.character(x)) stop("x should be a vector of character")

  # Check for empty input
  if (length(x) == 0) {
    warning("Input vector is empty. Returning NA.")
    return(NA_real_)
  }

  # Calculate the number of unique species
  species_count <- length(unique(x))

  # Handle edge case: only one species present
  if (species_count < 2) {
    warning("Simpson's index is undefined for fewer than 2 species. Returning NA.")
    return(NA_real_)
  }

  # Calculate relative abundances (p)
  p <- table(x) / length(x)

  # Calculate Simpson's index
  D <- 1 / sum(p^2)

  return(D)
}


