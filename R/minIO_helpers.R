library(paws.storage)

# 1. Verbinding maken met Deltares MinIO
connect_dsr <- function(access_key = Sys.getenv("MINIO_ACCESS_KEY"),
                        secret_key = Sys.getenv("MINIO_SECRET_KEY")) {
  s3(
    config = list(
      credentials = list(
        creds = list(
          access_key_id = access_key,
          secret_access_key = secret_key
        )
      ),
      endpoint = "https://s3.deltares.nl",
      region = "eu-west-1",
      s3_force_path_style = TRUE,
      close_connection = TRUE
    )
  )
}


# 2. Bestanden oplijsten uit de bucket (optioneel gefilterd op map/prefix)
list_dsr <- function(prefix = "", s3_client = s3, bucket = "dsr") {
  all_contents <- list()
  token <- NULL

  repeat {
    res <- s3_client$list_objects_v2(
      Bucket = bucket,
      Prefix = prefix,
      ContinuationToken = token
    )
    all_contents <- c(all_contents, res$Contents)
    if (!isTRUE(res$IsTruncated)) break
    token <- res$NextContinuationToken
  }

  if (length(all_contents) == 0) {
    message("Geen bestanden gevonden onder: ", bucket, "/", prefix)
    return(data.frame(Key = character(), Size_MB = numeric(), LastModified = as.POSIXct(character())))
  }

  data.frame(
    Key = vapply(all_contents, function(x) x$Key, character(1)),
    Size_MB = round(vapply(all_contents, function(x) x$Size, numeric(1)) / (1024^2), 3),
    LastModified = do.call(c, lapply(all_contents, function(x) x$LastModified)),
    stringsAsFactors = FALSE
  )
}

# 3. Uploaden: .csv, .rds, .parquet, .qs2, ggplots of lokale bestanden
upload_to_dsr <- function(x, key, s3_client = s3, bucket = "dsr", ...) {
  ext <- tolower(tools::file_ext(key))

  if (is.character(x) && length(x) == 1 && file.exists(x)) {
    file_path <- x
    cleanup <- FALSE
  } else {
    file_path <- tempfile(fileext = paste0(".", ext))
    cleanup <- TRUE

    if (inherits(x, "ggplot")) {
      ggplot2::ggsave(file_path, plot = x, ...)
    } else if (ext == "csv") {
      write.csv(x, file_path, row.names = FALSE, ...)
    } else if (ext == "rds") {
      saveRDS(x, file_path, ...)
    } else if (ext == "parquet") {
      if (!requireNamespace("arrow", quietly = TRUE)) stop("Installeer eerst het 'arrow' package.")
      arrow::write_parquet(x, file_path, ...)
    } else if (ext == "qs2") {
      if (!requireNamespace("qs2", quietly = TRUE)) stop("Installeer eerst het 'qs2' package.")
      qs2::qs_save(x, file_path, ...)
    } else {
      stop("Niet-ondersteunde extensie: .", ext)
    }
  }

  raw_body <- readBin(file_path, what = "raw", n = file.info(file_path)$size)
  res <- s3_client$put_object(Bucket = bucket, Key = key, Body = raw_body)

  if (cleanup) unlink(file_path)
  message("Opgeslagen op MinIO: ", bucket, "/", key)
  invisible(res)
}

# 4. Inlezen: .csv, .rds, .parquet of .qs2 direct weer in R laden
read_from_dsr <- function(key, s3_client = s3, bucket = "dsr", ...) {
  ext <- tolower(tools::file_ext(key))
  obj <- s3_client$get_object(Bucket = bucket, Key = key)

  tf <- tempfile(fileext = paste0(".", ext))
  writeBin(obj$Body, tf)
  on.exit(unlink(tf))

  if (ext == "csv") {
    return(read.csv(tf, ...))
  } else if (ext == "rds") {
    return(readRDS(tf, ...))
  } else if (ext == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) stop("Installeer eerst het 'arrow' package.")
    return(arrow::read_parquet(tf, ...))
  } else if (ext == "qs2") {
    if (!requireNamespace("qs2", quietly = TRUE)) stop("Installeer eerst het 'qs2' package.")
    return(qs2::qs_read(tf, ...))
  } else {
    stop("Kan extensie .", ext, " niet automatisch inlezen.")
  }
}
