# Dynamically locate project root whether running from root, tests/, or data-raw/
codePath <- getwd()
while (!dir.exists(file.path(codePath, "config")) && codePath != dirname(codePath)) {
  codePath <- dirname(codePath)
}


bucket_name <- Sys.getenv("TAXI_S3_BUCKET")
object_key <- Sys.getenv("TAXI_S3_OBJECT")

if (bucket_name == "" || object_key == "") {
  stop("Missing AWS configuration in .Renviron")
}

aws_profile <- "write-user"

system2(
  command = "aws",
  args = c(
    "s3", "sync",
    paste0(codePath, "/data/yellow_aggregation_partitioned"),
    sprintf("s3://%s/%s", bucket_name, object_key),
    "--profile", aws_profile
  )
)
