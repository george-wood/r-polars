### To be run after altdoc::render_docs()

list_man_html = list.files("docs/man",
  pattern = "\\.html$", full.names = TRUE,
  recursive = TRUE
)

for (i in list_man_html) {
  orig = readLines(i, warn = FALSE)
  # fix escaping of left-angle brackets (not needed for right-angle brackets)
  new = gsub("\\\\&lt;", "&lt;", orig)
  new = gsub("\\\\&gt;", "&gt;", orig)
  new = gsub("\\\\\\$", "$", new)
  new = gsub("\\\\_", "_", new)
  writeLines(new, i)
}
