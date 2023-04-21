context("Testcase for utility functions")

test_that("Quoting and sanitisation", {
    expect_equal("'hello'", quote_value("hello"))
    expect_equal("hello", default_sanitise_character("test_col", "    hello    "))
    expect_equal("hello goodbye", default_sanitise_character("test_col", "    hello goodbye   "))
    expect_equal("hello goodbye", default_sanitise_character("test_col", "    hello
goodbye   "))
    expect_equal("hello goodbye", default_sanitise_character("test_col", "    hello\r
goodbye   "))

    ## Check value censtoring works
    expect_warning(expect_true(0.00==default_sanitise_numeric("test_col", 1.0e-8, float_precision=1e-4)),
                   "Column test_col contains decimal values with greater precision than 1e-04")
    
    expect_true(1.0e-8==default_sanitise_numeric("test_col", 1.0e-8))
})

test_that("Preprocessing", {

    table1_kodes = default_preprocess_dataset(fd_18005_r)
    table1_nokodes = default_preprocess_dataset(fd_18005_r, factors_to_codes=FALSE)

    expect_false(is.factor(table1_kodes$gender))
    expect_false(is.factor(table1_nokodes$gender))
    expect_false(is.factor(table1_kodes$education))
    expect_false(is.factor(table1_nokodes$hobby))

    expect_true(is.numeric(table1_kodes$gender))
    expect_true(is.character(table1_nokodes$gender))
    expect_true(is.numeric(table1_kodes$education))
    expect_true(is.character(table1_nokodes$education))   
})

test_that("File utilities", {

    message(sprintf("Working in %s", file.path(tempdir(), "test_me_dir1")))
    
    expected_dir = file.path(tempdir(), "test_me_dir1","subdir")
    other_dir = file.path(tempdir(), "test_me_dir1","subdir2")
    unexpected_dir = file.path(tempdir(), "test_me_dir1","should_warn")
    expected_file = file.path(tempdir(), "test_me_dir1", "bad_file")
    other_unexpected_file = file.path(tempdir(), "test_me_dir1", "another_bad_file")
    
    ## Ensure we cleanup
    on.exit({unlink(file.path(tempdir(), "test_me_dir1"), recursive=TRUE)})
    
    expect_error(ensure_file(expected_file,
                             create_structure=TRUE,
                             source_file=system.file("extdata", "FD_18005_table1.RDS", package="RigsArkivetRInfoPkg")),
                 NA)

    dir.create(other_dir)
    
    ensure_directory(expected_dir, create_structure=TRUE)

    expect_true(dir.exists(expected_dir))
    ensure_directory(other_dir, create_structure=FALSE)

    expect_error( ensure_directory(unexpected_dir, create_structure=FALSE))

    expect_error( ensure_directory(expected_file, create_structure=TRUE))

    expect_error(ensure_file(expected_file, create_structure=FALSE),
                 NA)
    
    expect_error( ensure_file( other_unexpected_file, create_structure=FALSE))
    
})

test_that("cleanup data", {

    test_data_dir = file.path(tempdir(), "test_data_dir")
    test_table_file = file.path(test_data_dir, "table1", "table1.csv")
    source_file_path = system.file("extdata", "FD_18005_table1.csv", package="RigsArkivetRInfoPkg")
    stray_file = file.path(test_data_dir, "stray_file.txt")
    
    on.exit({unlink(test_data_dir, recursive=TRUE)})
    
    ensure_file(test_table_file, create_structure=TRUE, source_file=source_file_path)
    
    expect_true(file.exists(test_table_file))                    
    cleanup_data_dir(test_data_dir)
    expect_false(file.exists(test_table_file))
    expect_true(dir.exists(test_data_dir))
    expect_false(dir.exists(file.path(test_data_dir,"table1")))

    ## Test for stray files
    ensure_file(test_table_file, create_structure=TRUE, source_file=source_file_path)
    ensure_file(stray_file, create_structure=TRUE, source_file=source_file_path)
    expect_true(file.exists(test_table_file))
    expect_true(file.exists(stray_file))
    expect_warning(cleanup_data_dir(test_data_dir),sprintf("Stray files '%s' in data dir '%s'. Please remove these before validating the package with ASTA","stray_file.txt", test_data_dir))
    expect_true(file.exists(stray_file))
    expect_false(file.exists(test_table_file))
})

test_that("naming functions", {
    expect_equal("simple_dataset", dataset_to_name("simple_dataset"))
    expect_equal("simple_dataset", dataset_to_name("simple_dataset.csv"))
    expect_equal("simple_dataset", dataset_to_name("/path/to/simple_dataset.csv"))
    expect_equal("simple_dataset", dataset_to_name("/path/to/../simple_dataset.csv"))
})
