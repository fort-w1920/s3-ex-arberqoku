new_bb <- function(text) {
  checkmate::assert_character(unlist(unclass(text)))
  # replace ((consonants) (1-2 vowels) (consonants)) with
  # ((consonants) (vowels) b (same vowels again) (consonants)):
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  structure(gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text), class = "bb")
}

validate_bb <- function(x) {
  checkmate::assert_class(x, "bb")
  x
}

bb <- function(text, ...) {
  UseMethod("bb", text)
}

bb.default <- function(text, ...) {
  validate_bb(new_bb(text))
}

bb.list <- function(text, ...) {
  text <- lapply(text, bb)
  class(text) <- append(class(text), "bb")
  text
}

bb.factor <- function(text, ...) {
  levels(text) <- bb(levels(text))
  class(text) <- append(class(text), "bb")
  text
}