# test that str_flatten() is equivalent to paste with collapse
expect_equal(str_flatten(letters), paste0(letters, collapse = ""))

# test that collapse must be a single string
expect_error(
  str_flatten("A", c("a", "b")),
  "`collapse` must be a single string"
)

# test that `last` is optionally used instead of a final separator
expect_equal(str_flatten(letters[1:3], ", ", ", and "), "a, b, and c")
expect_equal(str_flatten(letters[1:2], ", ", ", and "), "a, and b")
expect_equal(str_flatten(letters[1], ", ", ", and "), "a")
