#' Conditionally Install a Private GitHub Package in CI
#'
#' This helper function installs a private R package from GitHub using a personal access token (PAT)
#' during GitHub Actions runs. It checks if the package is installed and, if not, installs it using
#' \code{devtools::install_github()} with authentication.
#'
#' @param user_name Character. The GitHub username or organization that owns the repository.
#' @param package_name Character. The name of the package and repository.
#' @param secret_name Character. The name of the environment variable holding the GitHub PAT
#'   (excluding the \code{"github_pat_"} prefix).
#'
#' @details
#' This function runs only when the environment variable \code{GITHUB_ACTIONS} is \code{"true"},
#' which indicates it is running in a GitHub Actions workflow. It assumes that the provided PAT is stored
#' in an environment variable named \code{secret_name}, and the full token is formed as
#' \code{paste0("github_pat_", Sys.getenv(secret_name))}.
#'
#' If \code{devtools} is not available, it will be installed from CRAN first.
#' If the target package is already loaded, it will be detached before reinstallation.
#'
#' @note This function is intended for use in CI environments and should not be called during package loading.
#' Ensure the secret specified by \code{secret_name} is configured in your GitHub repository settings.
#'
#' @importFrom utils install.packages
#' @importFrom devtools install_github
#'
#' @return NULL. Called for its side effect of installing a package.
#' @export
install_from_private_repo <- function(user_name, package_name, secret_name) {
  if (Sys.getenv("GITHUB_ACTIONS") == "true" | Sys.getenv("CALLR_IS_RUNNING") == "true") {
    if (!requireNamespace(package_name, quietly = TRUE)) {
      # if (paste0("package:", package_name) %in% search()) {
      #   detach(paste0("package:", package_name), unload = TRUE, character.only = TRUE)
      # }
      devtools::install_github(
        paste0(user_name, "/", package_name),
        force = TRUE,
        upgrade = "never",
        auth_token = paste0("github_pat_", Sys.getenv(secret_name))
      )
    }
  }
}