# modifies tokenizers::tokenize_words to exclude urls and user names ("u/)
# test_string <- "test u/j3434 tHIs-- a TEST a www.test.com website http://thisis.com"
library(stringi)

check_input <- function(x) {
  check_character <- is.character(x) |
    if (is.list(x)) {
      check_list <- all(vapply(x, is.character, logical(1))) &
        all(vapply(x, length, integer(1)) == 1L)
    } else {
      check_list <- FALSE
    }
  if (!(check_character | check_list))
    stop("Input must be a character vector of any length or a list of character\n",
         "  vectors, each of which has a length of 1.")
}

simplify_list <- function(x, simplify) {
  stopifnot(is.logical(simplify))
  if (simplify && length(x) == 1) x[[1]] else x
}

remove_stopwords <- function(x, stopwords) {
  out <- x[!x %in% stopwords]
  if(!length(out)){
    return(NA_character_)
  }
}

tokenize_words_reddit <- function(x,
                                  lowercase = TRUE,
                                  stopwords = NULL,
                                  strip_punct = TRUE,
                                  strip_url = TRUE,
                                  strip_username = TRUE) {

  check_input(x)
  if (lowercase) x <- stri_trans_tolower(x)

  # split on white space
  out <- stri_split_charclass(x, "\\p{WHITE_SPACE}")

  # get document indexes to vectorize tokens
  doc_lengths <- cumsum(lengths(out))
  docindex <- c(0, doc_lengths)
  # convert the list into a vector - avoids all those mapplys
  out <- unlist(out)

  # get the index of http(s) URLs
  index_url <- stri_detect_regex(out, "http|www")

  if (strip_url) {
    out[index_url] <- ""
  }

  # get the index of usernames
  index_user <- stri_detect_regex(out, "^u/ *")

  if (strip_username) {
    out[index_user] <- ""
  }

  # get the index of reddit names
  index_reddit <- stri_detect_regex(out, "r/ *")

  if (strip_username) {
    out[index_reddit] <- ""
  }

  # get the index of numbers
  index_num <- stri_detect_regex(out, "[:digit:]")
  out[index_num] <- ""

  # get the index of misc
  index_misc <- stri_detect_regex(out, "amp|nbsp|gt|\\^ *")
  out[index_misc] <- ""

  # strip all punctuation
  out <- stri_replace_all(out,"", regex = "[:punct:]")
  out

}


