# test that nothing happens if all is good
expect_silent(stringrb:::check_pattern("orange"))

# test that error occurs if length != 1
expect_error(stringrb:::check_pattern(c("apple", "orange")),
             "`pattern` must be a single string")

# test that error occurs if pattern is not character
expect_error(stringrb:::check_pattern(1),
             "`pattern` must be a single string")

# test that error occurs if pattern is NA
expect_error(stringrb:::check_pattern(NA_character_),
             "`pattern` can't be NA")
