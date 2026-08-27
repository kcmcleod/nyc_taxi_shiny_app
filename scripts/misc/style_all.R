# Install styler if you don't already have it on your Mac
if (!requireNamespace("styler", quietly = TRUE)) install.packages("styler")

# Automatically format every .R file in your project
styler::style_dir()
