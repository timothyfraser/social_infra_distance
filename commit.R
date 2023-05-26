#' @name commit.R
#' This script commits to github.

#install.packages("gert")
library(gert)
library(credentials)
library(usethis)
library(git2r)

create_github_token(scopes = c("admin:org", "repo", "workflow", "user:email"), description = "social_infra_distance")
set_github_pat()


# And clone a repository that you made on github
create_from_github(
  rstudio = FALSE,
  repo_spec = "https://github.com/timothyfraser/social_infra_distance",
  destdir = "/cloud/project/",
  fork = FALSE, protocol = "https", open = FALSE)

# Set current project to current directory
proj_set(path = ".")
# Check current project
proj_get()

path = "social_infra_distance"

# Get list of all files in new directory
myfiles <- dir(path, include.dirs = TRUE, full.names = FALSE)

# Copy repository contents to main project folder
file.copy(
  from = paste("/cloud/project/", path, "/", myfiles, sep = ""),
  to = "/cloud/project/",
  recursive = TRUE, overwrite = TRUE
)

# Delete repository folder
unlink(paste0("/cloud/project/", path), recursive = TRUE)

# (Re)Set working directory to project
setwd("/cloud/project/")

# Set up git locally in that specific directory (/cloud/project/)
use_git_config(
  user.name = "Timothy Fraser",
  user.email = "timothy.fraser.1@gmail.com")

# Initiate Git (in project directory from about)
use_git(message = "OK!")

use_git_remote(name = "origin", url = "")
# Add your sensitive files to .gitignore
usethis::git_vaccinate()

# Link git remote to our repository URL
usethis::use_git_remote(
  name = "origin", 
  url = "https://github.com/timothyfraser/social_infra_distance.git")

# Configure the default branch to be main
usethis::git_default_branch_configure(name = "main")

# Tell R to re-recognize the default branch you just set, if needed
usethis::git_default_branch_rediscover()

# Merge main branch with default branch
usethis::pr_merge_main()

# Do a preliminary commit, to ready R for pushing
gert::git_commit_all(message = "Setup commit")
 
# Check situation
usethis::git_sitrep()
# Should be ready to push.


gert::git_add(dir(all.files = TRUE))
gert::git_commit_all(message = "---")
gert::git_push()

library(googledrive)
# Link up to this account
googledrive::drive_auth(email = "tmf77@cornell.edu")

