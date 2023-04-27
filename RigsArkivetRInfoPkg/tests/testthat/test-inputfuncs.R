context("Testcase for input functions")

table1_rds = system.file("extdata", "FD_18005_table1.RDS", package="RigsArkivetRInfoPkg")
table2_rds = system.file("extdata", "example_table2.RDS", package="RigsArkivetRInfoPkg")
            
test_that("Base loading works", {
    table1 = load_and_truncate_dataset(table1_rds)
    expect_equal(20, nrow(table1))
    expect_equal(20, nrow(load_and_truncate_dataset(table2_rds)))
    expect_equal(19, ncol(table1))

    expect_equal(3, nlevels(table1$gender))
})

test_that("Relabelling works - dataframe and from file", {
    table1 = load_and_truncate_dataset(table1_rds)
    table1_r = relabel_dataset(table1, fd_18005_r_labels)
    table1_r2 = relabel_dataset(table1, system.file("extdata", "FD_18005_labels.csv",
                                                    package="RigsArkivetRInfoPkg"))

    expect_equal(table1_r$child_id,
                 table1_r2$child_id)
    
    expect_true("label" %in% names(attributes(table1_r$child_id)))
    expect_type(attributes(table1_r$child_id)$label, "character")

    expect_equal(attributes(table1_r$child_id)$label,
                 attributes(table1_r2$child_id)$label)

    
    expect_true("label" %in% names(attributes(table1_r$gender)))
    expect_type(attributes(table1_r$gender)$label, "character")

    expect_equal(attributes(table1_r$gender)$label,
                 attributes(table1_r2$gender)$label)
})

test_that("Truncation works", {
    table1 = relabel_dataset(load_and_truncate_dataset(table1_rds, row_limit=10),
                             fd_18005_r_labels)
   
    expect_equal(10, nrow(table1))
    expect_equal(19, ncol(table1))
    
    #Check attrs are still there
    expect_true("label" %in% names(attributes(table1$child_id)))
    expect_type(attributes(table1$child_id)$label, "character")
})

test_that("Context doc counting works", {
    expect_equal(7, count_context_documents(
                        system.file("extdata",
                                    "FD_18005_contextDocumentationIndex.xml",
                                    package="RigsArkivetRInfoPkg")))
})

test_that("Missing labels/dataset description", {
    table1 = relabel_dataset(fd_18005_r, fd_18005_r_labels)

    table1_missing = relabel_dataset(fd_18005_r, fd_18005_r_labels[1:17,])

    table1_desc = describe_dataset(table1)
    
    missing1 = check_for_missing_labels( table1_desc)
    missing2 = expect_warning(check_for_missing_labels( describe_dataset(table1_missing)),
                              "2 VARIABLES ARE MISSING DESCRIPTIONS")

    expect_equal(3, ncol(table1_desc))
    expect_equal(ncol(table1), nrow(table1_desc))
    expect_equal(0, nrow(missing1))
    expect_equal(2, nrow(missing2))
    expect_true("enrollment_time" %in% missing2$variable_name)
    expect_true("reason" %in% missing2$variable_name)
    
})


test_that("Param verification - base", {
    expect_true(verify_named_list(
        list(required="yes"),
        "test_list",
        list(required="character"),
        list(optional=c("integer","character"))))

    expect_true(verify_named_list(
        list(required="yes",
             optional=as.integer(3)),
        "test_list",
        list(required="character"),
        list(optional=c("integer","character"))))

    expect_true(verify_named_list(
        list(required="yes",
             optional="foo"),
        "test_list",
        list(required="character"),
        list(optional=c("integer","character"))))

    expect_warning(expect_false(verify_named_list(
        list(required="yes",
             optional=3.142),
        "test_list",
        list(required="character"),
        list(optional=c("integer","character")))),
        "Parameter optional has type numeric but requires type of integer,character")

    expect_warning(expect_false(verify_named_list(
        list(optional="ok"),
        "test_list",
        list(needed="character"),
        list(optional=c("integer","character")))),
        "Required params 'needed'")

     expect_warning(expect_false(verify_named_list(
        list(needed="here",
             optional="ok",
             nonsense="parp"),
        "test_list",
        list(needed="character"),
        list(optional=c("integer","character")))),
        "Unknown params 'nonsense'")    
})


test_that("Param verification - file structure", {
    pkg_dir = file.path( tempdir(), "test_pkg1")
    on.exit({unlink(pkg_dir, recursive=TRUE)})
    
    ok_result=expect_warning(
        verify_pkg_file_structure( pkg_dir,
                                  pkg_id="FD.18005",
                                  archive_index=system.file("extdata", "FD_18005_archiveIndex.xml", package="RigsArkivetRInfoPkg"),
                                  context_doc_index=system.file("extdata", "FD_18005_contextDocumentationIndex.xml", package="RigsArkivetRInfoPkg")
                                  ),
        "You will need to provide this file before"
    )
    
    expect_equal("FD.18005",
                 ok_result$pkg_id)
    expect_equal(file.path(pkg_dir, "FD.18005", "Data"),
                 ok_result$data_dir)
    expect_true(dir.exists(ok_result$data_dir))

    unlink(pkg_dir, recursive=TRUE)

    dir.create(file.path(pkg_dir, "FD.18005"), recursive=TRUE)
    
    expect_warning(
        verify_pkg_file_structure( file.path(pkg_dir, "FD.18005"),
                                  pkg_id="FD.99999",
                                  archive_index=system.file("extdata", "FD_18005_archiveIndex.xml", package="RigsArkivetRInfoPkg"),
                                  context_doc_index=system.file("extdata", "FD_18005_contextDocumentationIndex.xml", package="RigsArkivetRInfoPkg")
                                  ),
        "pkg_id is explicitly set to"
    )
    
    expect_error(
       verify_pkg_file_structure( pkg_dir,
                                  archive_index=system.file("extdata", "FD_18005_archiveIndex.xml", package="RigsArkivetRInfoPkg"),
                                  context_doc_index=system.file("extdata", "FD_18005_contextDocumentationIndex.xml", package="RigsArkivetRInfoPkg")
                                  ),
       "pkg_id is not set and cannot be"
    )

    unlink(pkg_dir, recursive=TRUE)
    
    expect_error(
       verify_pkg_file_structure( file.path(pkg_dir, "FD.18005"),
                                  archive_index="/tmp/does/not/exist.xml",
                                  context_doc_index=system.file("extdata", "FD_18005_contextDocumentationIndex.xml", package="RigsArkivetRInfoPkg")
                                  ),
       "Creating.*failed"
    )
})

test_that("Verify package description", {
    ##Just concentrate on the cross-referencing stuff; all other things are checked elsewhere

    pkg_dir = file.path( tempdir(), "test_pkg1")
    on.exit({unlink(pkg_dir, recursive=TRUE)})

    good_ref=list(
                    list(
                        other_dataset="childrens_pets",
                        other_variable="child_id",
                        our_variable="child_id")
                )
    
    good_parms=list(
        archive_index=system.file("extdata", "FD_18005_archiveIndex.xml", package="RigsArkivetRInfoPkg"),
        context_doc_index=system.file("extdata", "FD_18005_contextDocumentationIndex.xml", package="RigsArkivetRInfoPkg"),
        pkg_id="FD.18005",
        tables=list(
            list(
                name="R_testfil",
                label_file=fd_18005_r_labels,                             
                key_variable=c("child_id"),
                description="Danish Longitudinal Research Study of Grandparents, Parents and Children - this is data 1",
                table_dataset=fd_18005_r,
                reference=good_ref
            ),
            list(
                name="childrens_pets",
                label_file=example_table2_labels,
                key_variable=c("pet_id"),
                description="Childrens' pets, to provide a simple example of a 2nd table",
                table_dataset=example_table2))
    )
    
    bad_parms1=good_parms
    bad_parms1$tables[[1]]$table_dataset=97
    
    expect_error(
        expect_warning(verify_pkg_description(pkg_dir, bad_parms1),
                       "Parameter table_dataset has type numeric but requires type of character,data.frame")
    )    
    unlink(pkg_dir, recursive=TRUE)
    bad_parms1=good_parms
    bad_parms1$tables[[1]]$reference[[1]]$other_dataset="non_existent"
    
    expect_error(
        expect_warning(verify_pkg_description(pkg_dir, bad_parms1),
                       "other_dataset 'non_existent' is not a named dataset")
    )       
})
