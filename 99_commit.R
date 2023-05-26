#' @name 99_commit.R
#' This script commits to github.
#' For use by repository owner only. Readers will not need this.

#install.packages("gert")
library(gert)

gert::git_add(dir(all.files = TRUE))
gert::git_commit_all(message = "---")
gert::git_push()
