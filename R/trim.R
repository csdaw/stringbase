#' Trim whitespace from a string
#'
#' @description `str_trim()` removes whitespace from start and end of string;
#' `str_squish()` also reduces repeated whitespace inside a string.
#'
#' @param string `character vector`
#' @param side `string`, side on which to remove whitespace (left, right or both).
#' @return Returns a `character vector` the same length as `string`.
#' @export
#' @seealso `str_pad()` to add whitespace
#' @examples
#' str_trim("  String with trailing and leading white space\t")
#' str_trim("\n\nString with trailing and leading white space\n\n")
#'
#' str_squish("  String with trailing,  middle, and leading white space\t")
#' str_squish("\n\nString with excess,  trailing and leading white   space\n\n")
str_trim <- function(string, side = c("both", "left", "right")) {
  side <- match.arg(side)

  switch(side,
         both  = sub("\\s+$", "", sub("^\\s+", "", string)),
         left  = sub("^\\s+", "", string),
         right = sub("\\s+$", "", string),
  )
}

#' @export
#' @rdname str_trim
str_squish <- function(string) {
  str_trim(stringrb::str_replace_all(string, "\\s+", " "), side = "both")
}
