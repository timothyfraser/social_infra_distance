#' @name commit.R
#' This script commits to github.

#install.packages("gert")
library(gert)
library(credentials)
library(usethis)


create_github_token(scopes = c("admin:org", "repo", "workflow", "user:email"), description = "social_infra_distance")
set_github_pat(force_new = TRUE)

gert::git_add(dir(all.files = TRUE))
gert::git_commit_all(message = "---")


