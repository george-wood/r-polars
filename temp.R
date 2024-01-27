#' Update the text in the usage section of an Rd file
#'
#' @param file path of an Rd file
#' @param pattern Pattern to look for
#' @param replacement Regex value
#' @return The updated Rd file content invisibly
#'
#' @examples
#' rd_usage_replace("man/DataFrame_clone.Rd", "DataFrame_", "<DataFrame>$")
rd_usage_replace = function(file, pattern, replacement) {
  content_rd = tools::parse_Rd(file)
  tags = tools:::RdTags(content_rd)

  which_usage = which(tags == r"(\usage)")
  if (length(which_usage) > 0) {
    content_rd[[which_usage]] = lapply(content_rd[[which_usage]], function(x) {
      if (length(x) > 1) {
        return(x)
      }
      attr_rd_tag = attr(x, "Rd_tag")
      out = gsub(pattern, replacement, x)
      attr(out, "Rd_tag") = attr_rd_tag
      out
    })
    attr(content_rd[[which_usage]], "Rd_tag") = "\\usage"

    content_rd |>
      as.character(deparse = TRUE) |>
      paste0(collapse = "") |>
      write(file = file)
  } else {
    return(invisible())
  }
}

#' Update all usage sections in Rd files
#' @param files A character vector of Rd files to update. If NULL, all Rd files in the package are updated.
#' @return NULL
#' @examples
#' rd_usage_update()
rd_usage_update = function(files = NULL) {
  files = files %||% list.files("man", pattern = r"(.*\.Rd$)", full.names = TRUE, recursive = TRUE)

  patterns = rbind(
    c("DataFrame_", "<DataFrame>$"),
    c("DynamicGroupBy_", "<DynamicGroupBy>$"),
    c("Expr_", "<Expr>$"),
    c("ExprBin_", "<Expr>$bin$"),
    c("ExprCat_", "<Expr>$cat$"),
    c("ExprDT_", "<Expr>$dt$"),
    c("ExprList_", "<Expr>$list$"),
    c("ExprMeta_", "<Expr>$meta$"),
    c("ExprName_", "<Expr>$name$"),
    c("ExprStr_", "<Expr>$str$"),
    c("ExprStruct_", "<Expr>$struct$"),
    c("GroupBy_", "<GroupBy>$"),
    c("(IO|pl)_read_", "pl$read_"), # file names are "IO_read_", function names are "pl_read_"
    c("(IO|pl)_scan_", "pl$scan_"), # file names are "IO_scan_", function names are "pl_scan_"
    c("(IO|LazyFrame)_sink_", "<LazyFrame>$sink_"), # file names are "IO_sink_", function names are "LazyFrame_sink_"
    c("(IO|DataFrame)_write_", "<DataFrame>$write_"), # file names are "IO_write_", function names are "DataFrame_write_"
    c("LazyFrame_", "<LazyFrame>$"),
    c("LazyGroupBy_", "<LazyGroupBy>$"),
    c("pl_", "pl$"),
    c("RField_", "<RField>$"),
    c("RThreadHandle_", "<RThreadHandle>$"),
    c("Series_", "<Series>$"),
    c("SQLContext_", "<SQLContext>$")
  ) |> as.data.frame()
  names(patterns) = c("pattern", "replacement")

  for (i in 1:nrow(patterns)) {
    pat = patterns[i, "pattern"]
    repl = patterns[i, "replacement"]
    files_with_pattern = grep(paste0("/", pat), x = files, value = TRUE)
    for (f in files_with_pattern) {
      print(f)
      tryCatch(
        rd_usage_replace(f, pat, repl),
        error = function(e) {
          print(e)
          cat("Error updating usage section in ", f, "\n", sep = "")
        }
      )
    }
  }
}

# TODO: remove after R 4.4 released
`%||%` = function(x, y) {
  if (is.null(x)) y else x
}



future::plan(future::multisession)
fs::dir_copy("man", "man_old")
rd_usage_update()
source("altdoc/altdoc_preprocessing.R")
altdoc::render_docs(freeze = TRUE, parallel = TRUE, verbose = TRUE)
source("altdoc/altdoc_postprocessing.R")
fs::dir_delete("man")
fs::file_move("man_old", "man")
