library(yaml)
library(here)
library(pkgload)

pkgload::load_all(".")


##############################
## Part 1: temporarily write the Rd files ##
##############################

# This part takes care of the formatting of the Usage section,
# e.g from "DataFrame_clone()" to "<DataFrame>$clone()"

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

rd_usage_update()






##############################
## Part 2: automatically write the "Reference" section in mkdocs.yml ##
##############################

#' Find whether a file has the "internal" tag
#' @param file A single file path
#' @return NULL

is_internal = function(file) {
  y = capture.output(tools::Rd2latex(file))
  z = grepl("\\\\keyword\\{", y)
  if (!any(z)) {
    return(FALSE)
  }
  reg = regmatches(y, gregexpr("\\{\\K[^{}]+(?=\\})", y, perl = TRUE))
  test = vapply(seq_along(y), function(foo) {
    z[foo] && ("internal" %in% reg[[foo]] || "docs" %in% reg[[foo]])
  }, FUN.VALUE = logical(1L))
  any(test)
}


##############
## Make docs hierarchy ##
##############

other = list.files("man", pattern = "\\.Rd")
other = Filter(\(x) !is_internal(paste0("man/", x)), other)
other = sub("Rd$", "md", other)
out = list()
# order determines order in sidebar
classes = c(
  "pl", "Series", "DataFrame", "LazyFrame", "GroupBy",
  "LazyGroupBy", "RollingGroupBy", "DynamicGroupBy", "ExprList", "ExprBin",
  "ExprCat", "ExprDT", "ExprMeta", "ExprName", "ExprStr", "ExprStruct",
  "ExprArr", "Expr", "IO", "RThreadHandle", "SQLContext", "S3"
)
for (cl in classes) {
  files = grep(paste0("^", cl, "_"), other, value = TRUE)
  tmp = sprintf("%s: man/%s", sub("\\.md", "", sub("[^_]*_", "", files)), files)
  cl_label = ifelse(cl == "pl", "Polars", cl)
  cl_label = ifelse(cl == "IO", "Input/Output", cl_label)
  cl_label = ifelse(cl == "S3", "S3 Methods", cl_label)
  out = append(out, setNames(list(tmp), cl_label))
  other = setdiff(other, files)
}
# expr: nested
nam = c(
  "Expr" = "All others",
  "ExprArr" = "Array",
  "ExprList" = "List",
  "ExprBin" = "Binary",
  "ExprCat" = "Categorical",
  "ExprDT" = "DateTime",
  "ExprMeta" = "Meta",
  "ExprName" = "Name",
  "ExprStr" = "String",
  "ExprStruct" = "Struct"
)

tmp = lapply(names(nam), \(n) setNames(list(out[[n]]), nam[n]))
out = out[!names(out) %in% names(nam)]
out[["Expressions"]] = tmp
# other
tmp = sprintf("%s: man/%s", sub("\\.md$", "", other), other)
hierarchy = append(out, setNames(list(tmp), "Other"))


##############
## Convert to yaml format ##
##############

yml = read_yaml("altdoc/mkdocs_static.yml")

hierarchy = append(list("Reference" = "reference_home.md"), hierarchy)

# Insert the links in the settings
yml$nav[[3]]$Reference = hierarchy

# These two elements should be lists in the yaml format, not single elements,
# otherwise mkdocs breaks
for (i in c("extra_css", "plugins")) {
  if (!is.null(yml[[i]]) && !is.list(length(yml[[i]]))) {
    yml[[i]] = as.list(yml[[i]])
  }
}

# Write the settings to the `altdoc/` directory
out = as.yaml(yml, indent.mapping.sequence = TRUE)
out = gsub("- '", "- ", out)
out = gsub("\\.md'", "\\.md", out)
cat(out, file = "altdoc/mkdocs.yml")
