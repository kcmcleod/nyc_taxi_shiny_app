bucket_name <- Sys.getenv("TAXI_S3_BUCKET")
object_key <- Sys.getenv("TAXI_S3_OBJECT")

if (bucket_name == "" || object_key == "") {
  stop("Missing AWS configuration in .Renviron")
}

Sys.setenv(AWS_PROFILE = "admin-user")

system(paste0(
  "aws s3 sync ./data/yellow_aggregation_partitioned s3://",
  bucket_name, "/", object_key, " --profile admin-user"
))
