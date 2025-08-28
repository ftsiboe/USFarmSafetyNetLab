#' Push R Markdown Output to Overleaf via Git
#'
#' This is the main function that renders an R Markdown file and pushes the result
#' (and any additional files) to an Overleaf Git project.
#'
#' @param rmd_file Path to the R Markdown (.Rmd) file.
#' @param overleaf_project_id Overleaf Git project ID (from Overleaf Git settings).
#' @param local_dir Local directory where the repo will be cloned (default: `"overleaf_project"`).
#' @param figure_files Optional vector of file paths (e.g., images, .bib) to copy to the Overleaf repo.
#' @param commit_msg Git commit message (default: `"Update from R"`).
#' @param auth_token Overleaf Git token. Defaults to `Sys.getenv("OVERLEAF_TOKEN")`.
#' @param git_user_name Git username to configure locally in the repo (optional).
#' @param git_user_email Git email to configure locally in the repo (optional).
#'
#' @return Invisibly returns `TRUE` if successful, `FALSE` otherwise.
#' @export
push_to_overleaf <- function(
    rmd_file,
    overleaf_project_id,
    local_dir = "overleaf_project",
    figure_files = NULL,
    commit_msg = "Update from R",
    auth_token = Sys.getenv("OVERLEAF_TOKEN"),
    git_user_name = NULL,
    git_user_email = NULL
){
  
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  
  if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("Missing 'rmarkdown' package.")
  if (!nzchar(Sys.which("git"))) stop("Git not found in system PATH.")
  if (!file.exists(rmd_file)) stop("Rmd file not found: ", rmd_file)
  if (!nzchar(auth_token)) stop("Missing auth_token. Set it or use Sys.getenv('OVERLEAF_TOKEN').")
  
  # --- Git URL ---
  overleaf_git_url <- paste0("https://git:", auth_token, "@git.overleaf.com/", overleaf_project_id)
  
  # --- Set up repo & Git identity ---
  setup_git_repo(
    overleaf_git_url = overleaf_git_url,
    local_dir = local_dir,
    user_name = git_user_name,
    user_email = git_user_email
  )
  
  # --- Ensure figures directory exists ---
  figures_dir <- file.path(local_dir, "figures")
  if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)
  
  # --- Copy rmarkdown file ---
  file.copy(rmd_file, local_dir, overwrite = TRUE)
  
  # --- Render and restore environment ---
  render_rmarkdown_to_dir(normalizePath(rmd_file), output_dir = local_dir)
  
  # list.files(local_dir, pattern = "\\.pdf$", full.names = TRUE)
  
  # --- Copy figure files ---
  if (!is.null(figure_files)) {
    valid <- figure_files[file.exists(figure_files)]
    if (length(valid)) file.copy(valid, figures_dir, overwrite = TRUE)
    missing <- setdiff(figure_files, valid)
    if (length(missing)) warning("Missing files skipped: ", paste(missing, collapse = ", "))
  }
  
  # --- Commit and push ---
  setwd(local_dir)
  success <- commit_and_push(commit_msg)
  if (success) {
    message("Successfully pushed to Overleaf.")
  } else {
    warning("Push failed after retry.")
  }
  
  invisible(success)
}

#' Render R Markdown to a Target Directory with Environment Backup
#'
#' Safely renders an R Markdown document to a specified output directory.
#' Temporarily saves and restores the global environment in case the Rmd contains
#' destructive commands like `rm(list = ls())`.
#'
#' @param rmd_file Path to the `.Rmd` file to render.
#' @param output_dir Directory to render output files into.
#'
#' @return Invisibly returns `TRUE` on success. Will `stop()` if rendering fails.
#' @export
render_rmarkdown_to_dir <- function(rmd_file, output_dir) {
  env_backup <- tempfile(fileext = ".RData")
  save(list = ls(envir = globalenv()), file = env_backup, envir = globalenv())
  
  message("Rendering R Markdown...")
  tryCatch({
    rmarkdown::render(input = rmd_file, output_dir = output_dir)
  }, error = function(e) {
    stop("Render failed: ", e$message)
  })
  
  load(env_backup, envir = globalenv())
  message("Environment restored after render.")
}

#' Clone and Configure Overleaf Git Repository
#'
#' Clones the Overleaf Git repository if it doesn't already exist locally, and
#' sets local Git user identity if provided.
#'
#' @param overleaf_git_url Full Git clone URL with embedded token.
#' @param local_dir Directory to clone the repo into (if not already cloned).
#' @param user_name Git username (optional). If provided, set locally for the repo.
#' @param user_email Git email (optional). If provided, set locally for the repo.
#'
#' @return Invisibly returns `TRUE`. Will `stop()` on clone failure.
#' @export
setup_git_repo <- function(overleaf_git_url, local_dir, user_name = NULL, user_email = NULL) {
  if (!dir.exists(local_dir)) {
    message("Cloning Overleaf repo...")
    system2("git", c("clone", overleaf_git_url, local_dir), stdout = TRUE, stderr = TRUE)
    if (!dir.exists(local_dir)) stop("Failed to clone Overleaf repo.")
  }
  
  old_wd <- getwd()
  setwd(local_dir)
  on.exit(setwd(old_wd), add = TRUE)
  
  if (!is.null(user_name))  system2("git", c("config", "user.name", shQuote(user_name)))
  if (!is.null(user_email)) system2("git", c("config", "user.email", shQuote(user_email)))
}

#' Commit and Push Changes to Overleaf Git Repo
#'
#' Stages all modified files, commits them with the provided message,
#' and pushes to the Overleaf remote. Automatically handles common push
#' conflicts by attempting a `git pull` followed by a re-push.
#'
#' @param commit_msg Commit message to use.
#'
#' @return Returns `TRUE` if push succeeded, `FALSE` otherwise.
#' @export
commit_and_push <- function(commit_msg) {
  git_safe <- function(cmd) {
    result <- system2("git", cmd, stdout = TRUE, stderr = TRUE)
    status <- attr(result, "status")
    if (!is.null(status) && status != 0) {
      warning("Git command failed: git ", paste(cmd, collapse = " "), "\n", paste(result, collapse = "\n"))
      return(FALSE)
    }
    return(TRUE)
  }
  
  if (dir.exists(".git/rebase-merge")) {
    warning("Detected stuck rebase. Cleaning...")
    unlink(".git/rebase-merge", recursive = TRUE)
  }
  
  has_changes <- system("git diff --quiet || echo 'changed'", intern = TRUE)
  if (length(has_changes)) {
    add_success <- git_safe(c("add", "."))
    commit_success <- git_safe(c("commit", "-m", shQuote(commit_msg)))
    push_success <- git_safe(c("push"))
    
    if (!push_success) {
      message("Push failed - trying 'git pull' and re-push...")
      pull_success <- git_safe(c("pull"))
      if (pull_success) push_success <- git_safe(c("push"))
    }
    
    return(push_success)
  } else {
    message("No changes to commit.")
    return(TRUE)
  }
}


