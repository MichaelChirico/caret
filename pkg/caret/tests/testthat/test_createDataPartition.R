test_that("createDataPartition basic functionality with factor y", {
  withr::local_seed(123)
  y_factor <- factor(rep(c("A", "B", "C"), each = 10))
  partitions <- createDataPartition(y_factor, times = 2, p = 0.5)

  expect_type(partitions, "list")
  expect_length(partitions, 2)
  expect_true(all(sapply(partitions, is.integer)))
  expect_true(all(sapply(partitions, function(idx) all(idx >= 1 & idx <= length(y_factor)))))
  # Check if roughly half the data is in each partition
  expect_true(all(sapply(partitions, length) >= floor(0.5 * length(y_factor) / 3) * 3)) # Check per class
  expect_true(all(sapply(partitions, length) <= ceiling(0.5 * length(y_factor) / 3) * 3 + 2)) # Allow some leeway
})

test_that("createDataPartition with list = FALSE", {
  withr::local_seed(123)
  y_numeric <- rnorm(30)
  partitions_matrix <- createDataPartition(y_numeric, times = 3, p = 0.7, list = FALSE)

  expect_true(is.matrix(partitions_matrix))
  expect_equal(ncol(partitions_matrix), 3)
  expect_equal(nrow(partitions_matrix), 22L)
  expect_true(is.integer(partitions_matrix))
})

test_that("createDataPartition with numeric y", {
  withr::local_seed(123)
  y_numeric <- rnorm(50)
  partitions <- createDataPartition(y_numeric, times = 1, p = 0.8)

  expect_type(partitions, "list")
  expect_length(partitions, 1)
  expect_type(partitions[[1]], "integer")
  expect_length(partitions[[1]], 42L)
})

test_that("createDataPartition handles NA values in factor y", {
  withr::local_seed(123)
  y_factor_na <- factor(rep(c("A", "B", NA), c(10L, 10L, 5L)))
  # NA will be treated as a level.
  # The function issues a warning for classes with no records if 'NA's are dropped by factor()
  # but createDataPartition itself converts y to character and then factor, so NA becomes "NA" level.

  partitions_na <- createDataPartition(y_factor_na, times = 1, p = 0.5)
  expect_type(partitions_na, "list")
  expect_length(partitions_na, 1)
  expect_type(partitions_na[[1]], "integer")

  # Check that indices are valid and within the range of non-NA original data if NAs are excluded
  # or within the full length if NAs are included as a category.

  # Let's check the length of the output based on non-NA values if NAs are implicitly dropped,
  # or total length if "NA" is a class.
  # Given the code: `y <- factor(as.character(y))`, NA becomes "NA" level.
  num_A <- ceiling(sum(y_factor_na == "A", na.rm = TRUE) * 0.5)
  num_B <- ceiling(sum(y_factor_na == "B", na.rm = TRUE) * 0.5)
  num_NA_level <- ceiling(sum(is.na(y_factor_na)) * 0.5)
  expected_len <- num_A + num_B + num_NA_level
  expect_length(partitions_na[[1]], expected_len)

  # Ensure no out-of-bounds indices
  expect_true(all(partitions_na[[1]] >= 1 & partitions_na[[1]] <= length(y_factor_na)))
})

test_that("createDataPartition handles character y with NA and string 'NA'", {
  withr::local_seed(456)
  # y contains regular strings, actual NA values, and string literals "NA"
  y_string_mix <- c(rep("Apple", 5), rep("Banana", 5), NA, NA, "NA", "Cherry", "NA", "NA", "NA") # Total 16 items
  # Counts: Apple: 5, Banana: 5, actual NA: 2, string "NA": 3, Cherry: 1
  # createDataPartition converts y to factor via as.character.
  # Both NA and "NA" will become the factor level "NA".
  # So, "NA" level count = 2 (from NA) + 3 (from "NA") = 5.

  partitions <- createDataPartition(y_string_mix, times = 1, p = 0.5)

  expect_type(partitions, "list")
  expect_length(partitions, 1)
  expect_type(partitions[[1]], "integer")

  # Expected length calculation:
  # Level "Apple": count 5, p=0.5 -> ceiling(5 * 0.5) = 3
  # Level "Banana": count 5, p=0.5 -> ceiling(5 * 0.5) = 3
  # Level "NA": count 5 (2 actual NAs + 3 "NA" strings), p=0.5 -> ceiling(5 * 0.5) = 3
  # Level "Cherry": count 1, p=0.5 -> ceiling(1 * 0.5) = 1 (selected as it's a single-item group)
  # Total expected length = 3 + 3 + 3 + 1 = 10
  expected_len <- 10
  expect_length(partitions[[1]], expected_len)

  # Ensure all indices are valid
  expect_true(all(partitions[[1]] >= 1 & partitions[[1]] <= length(y_string_mix)))

  # Check that the single "Cherry" element is part of the sample
  cherry_original_indices <- which(y_string_mix == "Cherry")
  expect_true(any(cherry_original_indices %in% partitions[[1]]))
})

test_that("createDataPartition with y having very few data points", {
  expect_error(createDataPartition(c(1), times = 1, p = 0.5), "y must have at least 2 data points")
  
  y_small <- c(1,2)
  part_small <- createDataPartition(y_small, times = 1, p = 0.5)
  expect_type(part_small, "list")
  expect_length(part_small[[1]], 1) # ceiling(2 * 0.5) = 1

  y_factor_small <- factor(c("A", "B"))
  part_factor_small <- createDataPartition(y_factor_small, times = 1, p = 0.5)
  expect_type(part_factor_small, "list")
  # Each class has 1, so 1 sample from each class, total 2 * 0.5 = 1 from each, total 2.
  # No, subsample will take ceiling(1 * 0.5) = 1 from each group. So total 2.
  expect_length(part_factor_small[[1]], 2)


  y_factor_one_each <- factor(c("A", "B", "C"))
  # With p=0.5, ceiling(1 * 0.5) = 1 sample from each class.
  part_factor_one_each <- createDataPartition(y_factor_one_each, times = 1, p = 0.5)
  expect_length(part_factor_one_each[[1]], 3)
})

test_that("createDataPartition warnings for single/no record classes", {
  # Single record class
  y_single <- factor(c("A", "A", "B"))
  expect_warning(
    createDataPartition(y_single, p = 0.5),
    "Some classes have a single record \\( B \\) and these will be selected for the sample"
  )
  partitions_single <- suppressWarnings(createDataPartition(y_single, p = 0.5))
  # Class A: ceiling(2*0.5)=1. Class B: 1 (selected as per warning). Total = 2.
  expect_length(partitions_single[[1]], 2)
  expect_true(which(y_single == "B") %in% partitions_single[[1]])

  # No record class (after factor conversion, this is less likely unless explicitly creating such factor)
  y_no_rec_factor <- factor(c("A", "A"), levels = c("A", "B", "C"))
  expect_warning(
    createDataPartition(y_no_rec_factor, p = 0.5),
    "Some classes have no records \\( B, C \\) and these will be ignored"
  )
  partitions_no_rec <- suppressWarnings(createDataPartition(y_no_rec_factor, p = 0.5))
  # Class A: ceiling(2*0.5)=1. Total = 1.
  expect_length(partitions_no_rec[[1]], 1)
})
