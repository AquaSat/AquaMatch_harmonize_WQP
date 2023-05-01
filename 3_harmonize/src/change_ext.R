# Functions from https://github.com/robitalec/targets-parameterized-bookdown

# Change extension from md to Rmd
#  since (at the moment) bookdown ignores md files unless explicitly stated in
#  rmd_files
#  https://github.com/rstudio/bookdown/issues/956
change_ext <- function(file, inext, outext) {
  newfile <- gsub(inext, outext, file)
  file.rename(file, newfile)
  newfile
}


# Bookdown render, but with explicitly defined dependencies
render_with_deps <- function(index, deps) {
  bookdown::render_book(index,
                        # This is how you introduce code folding in gitbook
                        output_format = gitbook(code_folding = "hide"))
}