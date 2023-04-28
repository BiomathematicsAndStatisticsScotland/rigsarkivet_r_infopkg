context("Test emitting in full information package form")

library(tools)

TABLE1_DATA_NOKODES="8201d6041ddcc0a8feba5e7deda94f6a"
TABLE1_DESC_NOKODES="e25d48df034dff3e5b9d474fb1d8fe37"
TABLE1_DATA_KODES="51e71b804037b25e6b38eea2f04a322b"
TABLE1_DESC_KODES="5d6a444f4f203381ce6edd549febe249"

TABLE1_DESC_KODES_MULTI="1d2e428a9218dd038ee4e6e718b20c6b"

TABLE2_DATA_KODES="203d9bd496a462affa5806749556c0a9"
TABLE2_DESC_KODES="544f0da7a9399bffc754b4736f76cb96"

## Create some fake context documents to suppress warnings.
## The labels file is chosen simply because we have it. 
create_context_docs <- function(ctx_doc_base, ctx_doc_count=7) {
    for (doc_id in seq(1,ctx_doc_count)) {
        ctx_doc_path=file.path(ctx_doc_base, doc_id, "1.tif")
        ensure_file(ctx_doc_path,
                    create_structure=TRUE,
                    source_file=system.file("extdata","FD_18005_labels.csv",package="RigsArkivetRInfoPkg"))
    }
}


test_that("Single data table package, no codes", {
    pkg_dir=file.path(tempdir(), "full-pkg1", "FD.18005")
    old_enc = options()$encoding
    options(encoding="UTF-8")
    on.exit({
        options(encoding=old_enc)
        unlink(pkg_dir, recursive=TRUE)
    })

    create_context_docs(file.path(pkg_dir, "ContextDocumentation/docCollection1"))
    
    pkg_description = list(
        archive_index=system.file("extdata", "FD_18005_archiveIndex.xml", package="RigsArkivetRInfoPkg"),
        context_doc_index=system.file("extdata", "FD_18005_contextDocumentationIndex.xml", package="RigsArkivetRInfoPkg"),
        pkg_id="FD.18005",
        tables=list(
            list(
                name="R_testfil",
                label_file=fd_18005_r_labels,                             
                key_variable=c("child_id"),
                description="Danish Longitudinal Research Study of Grandparents, Parents and Children - this is data 1",
                table_dataset=fd_18005_r)))
    
    process_full_info_pkg(pkg_description,
                          output_dir = pkg_dir,
                          factors_to_codes=FALSE)

    ##Check files exist and MD5 signatures of files present
    expect_true(file.exists(file.path(pkg_dir, "Data", "table1", "table1.csv")))
    expect_true(file.exists(file.path(pkg_dir, "Data", "table1", "table1.txt")))
    expect_true(file.exists(file.path(pkg_dir, "Indices", "archiveIndex.xml")))
    expect_true(file.exists(file.path(pkg_dir, "Indices", "contextDocumentationIndex.xml")))
    expect_false(file.exists(file.path(pkg_dir, "Data", "table1", "table1_missing_labels.csv")))

    ##Really we are testing our own test code here lol
    expect_true(file.exists(file.path(pkg_dir, "ContextDocumentation", "docCollection1", "1","1.tif")))
    expect_true(file.exists(file.path(pkg_dir, "ContextDocumentation", "docCollection1", "7","1.tif")))
    
    expect_equal(TABLE1_DATA_NOKODES,
                 unname(md5sum(file.path(pkg_dir, "Data", "table1", "table1.csv"))))
    expect_equal(TABLE1_DESC_NOKODES,
                 unname(md5sum(file.path(pkg_dir, "Data", "table1", "table1.txt"))))
})

test_that("Single data table package, with codes", {
    pkg_dir=file.path(tempdir(), "full-pkg1", "FD.18005")
    old_enc = options()$encoding
    options(encoding="UTF-8")
    on.exit({
        options(encoding=old_enc)
        unlink(pkg_dir, recursive=TRUE)
    })

    create_context_docs(file.path(pkg_dir, "ContextDocumentation/docCollection1"))

    pkg_description = list(
        archive_index=system.file("extdata", "FD_18005_archiveIndex.xml", package="RigsArkivetRInfoPkg"),
        context_doc_index=system.file("extdata", "FD_18005_contextDocumentationIndex.xml", package="RigsArkivetRInfoPkg"),
        pkg_id="FD.18005",
        tables=list(
            list(
                name="R_testfil",
                label_file=fd_18005_r_labels,                             
                key_variable=c("child_id"),
                description="Danish Longitudinal Research Study of Grandparents, Parents and Children - this is data 1",
                table_dataset=fd_18005_r)))
    
    process_full_info_pkg(pkg_description,
                          output_dir = pkg_dir,
                          factors_to_codes=TRUE)

    ##Check files exist and MD5 signatures of files present
    expect_true(file.exists(file.path(pkg_dir, "Data", "table1", "table1.csv")))
    expect_true(file.exists(file.path(pkg_dir, "Data", "table1", "table1.txt")))
    expect_true(file.exists(file.path(pkg_dir, "Indices", "archiveIndex.xml")))
    expect_true(file.exists(file.path(pkg_dir, "Indices", "contextDocumentationIndex.xml")))
    expect_false(file.exists(file.path(pkg_dir, "Data", "table1", "table1_missing_labels.csv")))

    ##Really we are testing our own test code here lol
    expect_true(file.exists(file.path(pkg_dir, "ContextDocumentation", "docCollection1", "1","1.tif")))
    expect_true(file.exists(file.path(pkg_dir, "ContextDocumentation", "docCollection1", "7","1.tif")))
    
    expect_equal(TABLE1_DATA_KODES,
                 unname(md5sum(file.path(pkg_dir, "Data", "table1", "table1.csv"))))
    expect_equal(TABLE1_DESC_KODES,
                 unname(md5sum(file.path(pkg_dir, "Data", "table1", "table1.txt"))))
})


test_that("Multiple data table package, with codes", {
    pkg_dir=file.path(tempdir(), "full-pkg1", "FD.18005")
    old_enc = options()$encoding
    options(encoding="UTF-8")
    on.exit({
        options(encoding=old_enc)
        unlink(pkg_dir, recursive=TRUE)
    })

    create_context_docs(file.path(pkg_dir, "ContextDocumentation/docCollection1"))
    
    pkg_description = list(
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
                reference=list(
                    list(
                        other_dataset="childrens_pets",
                        other_variable="child_id",
                        our_variable="child_id")
                )
            ),
            list(
                name="childrens_pets",
                label_file=example_table2_labels,
                key_variable=c("pet_id"),
                description="Childrens' pets, to provide a simple example of a 2nd table",
                table_dataset=example_table2)))

    process_full_info_pkg(pkg_description,
                          output_dir = pkg_dir)

    expect_true(file.exists(file.path(pkg_dir, "Data", "table1", "table1.csv")))
    expect_true(file.exists(file.path(pkg_dir, "Data", "table1", "table1.txt")))
    expect_true(file.exists(file.path(pkg_dir, "Data", "table2", "table2.csv")))
    expect_true(file.exists(file.path(pkg_dir, "Data", "table2", "table2.txt")))
    expect_true(file.exists(file.path(pkg_dir, "Indices", "archiveIndex.xml")))
    expect_true(file.exists(file.path(pkg_dir, "Indices", "contextDocumentationIndex.xml")))
    expect_false(file.exists(file.path(pkg_dir, "Data", "table1", "table1_missing_labels.csv")))

    ##Really we are testing our own test code here lol
    expect_true(file.exists(file.path(pkg_dir, "ContextDocumentation", "docCollection1", "1","1.tif")))
    expect_true(file.exists(file.path(pkg_dir, "ContextDocumentation", "docCollection1", "7","1.tif")))
    
    expect_equal(TABLE1_DATA_KODES,
                 unname(md5sum(file.path(pkg_dir, "Data", "table1", "table1.csv"))))
    expect_equal(TABLE1_DESC_KODES_MULTI,
                 unname(md5sum(file.path(pkg_dir, "Data", "table1", "table1.txt"))))

    expect_equal(TABLE2_DATA_KODES,
                 unname(md5sum(file.path(pkg_dir, "Data", "table2", "table2.csv"))))
    expect_equal(TABLE2_DESC_KODES,
                 unname(md5sum(file.path(pkg_dir, "Data", "table2", "table2.txt"))))
})

test_that("Single data table package, missing context docs (check warning)", {
    pkg_dir=file.path(tempdir(), "full-pkg1", "FD.18005")
    old_enc = options()$encoding
    options(encoding="UTF-8")
    on.exit({
        options(encoding=old_enc)
        unlink(pkg_dir, recursive=TRUE)
    })

    create_context_docs(file.path(pkg_dir, "ContextDocumentation/docCollection1"), ctx_doc_count=6)

    pkg_description = list(
        archive_index=system.file("extdata", "FD_18005_archiveIndex.xml", package="RigsArkivetRInfoPkg"),
        context_doc_index=system.file("extdata", "FD_18005_contextDocumentationIndex.xml", package="RigsArkivetRInfoPkg"),
        pkg_id="FD.18005",
        tables=list(
            list(
                name="R_testfil",
                label_file=fd_18005_r_labels,                             
                key_variable=c("child_id"),
                description="Danish Longitudinal Research Study of Grandparents, Parents and Children - this is data 1",
                table_dataset=fd_18005_r)))

    expect_warning(
        process_full_info_pkg(pkg_description,
                              output_dir = pkg_dir,
                              factors_to_codes=TRUE),
        "docCollection1/7/1.tif as no source file is provided"
    )

    ##Check files exist and MD5 signatures of files present
    expect_true(file.exists(file.path(pkg_dir, "Data", "table1", "table1.csv")))
    expect_true(file.exists(file.path(pkg_dir, "Data", "table1", "table1.txt")))
    expect_true(file.exists(file.path(pkg_dir, "Indices", "archiveIndex.xml")))
    expect_true(file.exists(file.path(pkg_dir, "Indices", "contextDocumentationIndex.xml")))
    expect_false(file.exists(file.path(pkg_dir, "Data", "table1", "table1_missing_labels.csv")))

    ##Really we are testing our own test code here lol
    expect_true(file.exists(file.path(pkg_dir, "ContextDocumentation", "docCollection1", "1","1.tif")))
    expect_true(file.exists(file.path(pkg_dir, "ContextDocumentation", "docCollection1", "6","1.tif")))
    expect_false(file.exists(file.path(pkg_dir, "ContextDocumentation", "docCollection1", "7","1.tif")))
    
    expect_equal(TABLE1_DATA_KODES,
                 unname(md5sum(file.path(pkg_dir, "Data", "table1", "table1.csv"))))
    expect_equal(TABLE1_DESC_KODES,
                 unname(md5sum(file.path(pkg_dir, "Data", "table1", "table1.txt"))))
})

## More tests are possible, e.g. testing that sanitisation behaves in
## a full test but this seems excessive for now.
