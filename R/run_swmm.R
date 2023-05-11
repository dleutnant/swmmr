#' @title Initiate a simulation run
#' @description This function runs a swmm inp file. If rpt and out files are not 
#' specified files are automatically created in the same directory of
#' the inp file.
#' @param inp Name and path to an input file.
#' @param rpt Name and path to a report file.
#' @param out Name and path to an out file.
#' @param exec Name and path to swmm5 executable. If not manually set, 
#' the following paths are looked up when package gets loaded:
#' windows: "C:/Program Files (x86)/EPA SWMM 5.X.XXX/swmm5.exe" 
#' not windows: "/usr/local/bin/swmm5" , "/usr/bin/swmm5"
#' @param debug set to \code{TRUE} if debug messages are to be shown. Default: 
#'   \code{FALSE} 
#' @inheritParams base::system2
#' @details The path to a swmm5 executable is read by calling 'getOption("swmmr.exec")'.
#' @examples
#' \dontrun{
#' result <- run_swmm("model.inp")
#' }
#' @rdname run_swmm
#' @export
run_swmm <- function(
    inp, 
    rpt = NULL,
    out = NULL,
    exec = NULL,
    stdout = "", 
    stderr = "",
    wait = TRUE,
    debug = TRUE
)
{
  # get the path of the executable on the precoded paths...
  option_name <- "swmmr.exec"
  
  if (is.null(exec)) {
    exec <- getOption(option_name)
  }
  
  if (is.null(exec)) {
    stop_formatted(
      paste0(
        "Could not determine the path to the SWMM executable. ",
        "Please set the path to the executable ",
        "(typically 'swmm5.exe' on Windows or 'swmm5' on Linux or macOS) ",
        "either in option '%s': options(%s = <path-to-executable>) ",
        "or call run_swmm() with its 'exec' argument being set accordingly."
      ), 
      option_name,
      option_name
    )
  }
  
  # check if inp and exec exists
  stopifnot(file.exists(inp), file.exists(exec))
  
  # Now that we know that the exectuable exists, get the absolute path (by e.g.
  # replacing "." or "..") so that we can use it without problems on the command
  # line
  exec <- normalizePath(exec)
  
  # get the name of the directory which is used to create rpt and out files if
  # not provided.
  dir_path <- base::dirname(inp)
  
  # helper function to create a default path
  default_unless_given <- function(x, extension) {
    if (given(x)) {
      x
    } else {
      # Replace the file name extension of the inp file
      file.path(dir_path, replace_extension(base::basename(inp), extension))
    }
  }
  
  # if rpt or out file are not provided, set default paths next to inp file
  rpt <- default_unless_given(rpt, extension = ".rpt")
  out <- default_unless_given(out, extension = ".out")
  
  # Change working directory, saving the current working directory
  original_dir <- setwd(dir_path)
  
  # On exit, come back
  on.exit(setwd(original_dir))

  args <- shQuote(normalizePath(c(inp, rpt, out), mustWork = FALSE))
  
  if (debug) {
    writeLines(c("Original working directory:", original_dir))
    writeLines(c("Current working directory:", getwd()))
    writeLines("Calling system2() with...")
    writeLines(c("- exec:", exec))
    writeLines(c("- args:", args))
  }
  
  # execute command
  base::system2(
    command = exec, 
    args = args, 
    stdout = stdout,
    stderr = stdout,
    wait = wait,
    minimized = FALSE,
    invisible = TRUE
  )
  
  # flush console
  utils::flush.console()
  
  # return a list with file paths
  paths <- list(
    inp = inp,
    rpt = rpt, 
    out = out
  )
  
  # Are the files there?
  is_there <- sapply(paths, file.exists)
  
  # Stop if there is no output file and report about errors in report file
  if (!is_there["out"] && is_there["rpt"]) {
    clean_warning(    
      text_not_created(paths$out, "output"), "\n",
      text_errors_in_report(paths$rpt)
    )
  }

  # Stop if there is no report file  
  if (!is_there["rpt"]) {
    clean_stop(text_not_created(paths$rpt, "report"))
  }
  
  paths[is_there] <- lapply(paths[is_there], normalizePath)
  paths[!is_there] <- NA
  
  invisible(paths)
}

# text_not_created -------------------------------------------------------------
text_not_created <- function(file, type)
{
  paste_lines(
    sprintf("No %s file has been created. It was expected to be here:", type),
    file
  )
}

# text_errors_in_report --------------------------------------------------------
text_errors_in_report <- function(path_rpt)
{
  errors <- extract_errors_from_report(path_rpt)
  n_errors <- length(errors)
  n_show <- min(n_errors, 3L)
  
  paste_lines(
    if (n_errors > 0L) {
      c(
        sprintf("There were %d errors reported in the report file.", n_errors),
        sprintf("The first %d errors are:", n_show),
        errors[seq_len(n_show)]
      )
    },
    "Please have a closer look at the report file:", 
    path_rpt
  )
}

# extract_errors_from_report ---------------------------------------------------
extract_errors_from_report <- function(file)
{
  x <- readLines(file, warn = FALSE)
  
  error_indices <- grep("ERROR", x)
  
  if (length(error_indices) == 0L) {
    return(character())
  }
  
  empty_indices <- grep("^\\s*$", x)
  
  sapply(error_indices, function(i) {
    j <- min(empty_indices[empty_indices > i]) - 1L
    paste(trimws(x[i:j]), collapse = "\n")
  })
}  
