# script to set up GitHub tokens

if (!requireNamespace("usethis", quietly = TRUE)) {
  install.packages("usethis")
} else {
  print("The package is installed.")
}

if (!requireNamespace("gitcreds", quietly = TRUE)) {
  install.packages("gitcreds")
} else {
  print("The package is installed.")
}
usethis::use_github()

readline(prompt = "Now go and fill in the browser form.
         Once done, press [Enter] to continue...")
print("Script resuming...")

print("Get ready to paste your token into the console!")
gitcreds::gitcreds_set()

usethis::git_sitrep()

print("That's us done!")
