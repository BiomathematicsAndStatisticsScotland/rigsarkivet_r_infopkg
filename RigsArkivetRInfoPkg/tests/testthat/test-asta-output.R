context("Test emitting in a form suitable for dataset construction in ASTA")

MISSING_LABELS_MD5="b6daea6996dc778157a4c2c094841814"
KODELISTE_MD5="a9b1beed456dea6f155b78e853ff1925"
DATASET_NOKODES_MD5="8201d6041ddcc0a8feba5e7deda94f6a"
DATASET_KODES_MD5="51e71b804037b25e6b38eea2f04a322b"
VLIST_NOKODES_MD5="e0e077379b0b3a159e65c2749b77ce34"
VLIST_KODES_MD5="96247122a504af5a20dfc9039f3dcb50"
VDESC_MD5="cd9f97067cefd53ca76f6d1f82bf1b9c"

test_that("ASTA export without KODELIST works", {
    pkg_dir=file.path(tempdir(), "asta-pkg1")
    dir.create(pkg_dir)    
    on.exit({unlink(pkg_dir, recursive=TRUE)})

    expect_warning(
        process_as_asta_export_script(fd_18005_r, 
                                      "my_dataset",
                                      output_dir=pkg_dir,
                                      factors_to_codes=FALSE),
        "VARIABLES ARE MISSING DESCRIPTIONS"
    )
           
    expect_true(file.exists(file.path(pkg_dir, "my_dataset.csv")))
    expect_true(file.exists(file.path(pkg_dir, "my_dataset_VARIABEL.txt")))
    expect_true(file.exists(file.path(pkg_dir, "my_dataset_missing_labels.csv")))
    expect_false(file.exists(file.path(pkg_dir, "my_dataset_VARIABELBESKRIVELSE.txt")))
    expect_false(file.exists(file.path(pkg_dir, "my_dataset_KODELISTE.txt")))

    expect_equal(MISSING_LABELS_MD5,
                 unname(md5sum(file.path(pkg_dir, "my_dataset_missing_labels.csv"))))
    expect_equal(DATASET_NOKODES_MD5,
                 unname(md5sum(file.path(pkg_dir, "my_dataset.csv"))))
    expect_equal(VLIST_NOKODES_MD5,
                 unname(md5sum(file.path(pkg_dir, "my_dataset_VARIABEL.txt"))))
})

## A further invalid test - 
test_that("ASTA export with KODELIST works", {
    pkg_dir=file.path(tempdir(), "asta-pkg2")
    dir.create(pkg_dir)    
    on.exit({unlink(pkg_dir, recursive=TRUE)})

    expect_warning(
        process_as_asta_export_script(fd_18005_r, 
                                      "my_dataset",
                                      output_dir=pkg_dir,
                                      factors_to_codes=TRUE),
        "VARIABLES ARE MISSING DESCRIPTIONS"
    )
        

    expect_true(file.exists(file.path(pkg_dir, "my_dataset.csv")))
    expect_true(file.exists(file.path(pkg_dir, "my_dataset_VARIABEL.txt")))
    expect_true(file.exists(file.path(pkg_dir, "my_dataset_missing_labels.csv")))
    expect_false(file.exists(file.path(pkg_dir, "my_dataset_VARIABELBESKRIVELSE.txt")))
    expect_true(file.exists(file.path(pkg_dir, "my_dataset_KODELISTE.txt")))

    expect_equal(MISSING_LABELS_MD5,
                 unname(md5sum(file.path(pkg_dir, "my_dataset_missing_labels.csv"))))
    expect_equal(DATASET_KODES_MD5,
                 unname(md5sum(file.path(pkg_dir, "my_dataset.csv"))))
    expect_equal(KODELISTE_MD5,
                 unname(md5sum(file.path(pkg_dir, "my_dataset_KODELISTE.txt"))))
    expect_equal(VLIST_KODES_MD5,
                 unname(md5sum(file.path(pkg_dir, "my_dataset_VARIABEL.txt"))))
    
})

test_that("ASTA export with Descriptions and KODELIST works", {
    
    pkg_dir=file.path(tempdir(), "asta-pkg3")
    dir.create(pkg_dir)
    old_enc = options()$encoding
    options(encoding="UTF-8")
    on.exit({
        options(encoding=old_enc)
        unlink(pkg_dir, recursive=TRUE)
    })

    labelled_fd_18005_r = relabel_dataset( fd_18005_r, fd_18005_r_labels)
    
    process_as_asta_export_script(labelled_fd_18005_r, 
                                  "my_dataset",
                                  output_dir=pkg_dir,
                                  factors_to_codes=TRUE)

    expect_true(file.exists(file.path(pkg_dir, "my_dataset.csv")))
    expect_true(file.exists(file.path(pkg_dir, "my_dataset_VARIABEL.txt")))
    expect_false(file.exists(file.path(pkg_dir, "my_dataset_missing_labels.csv")))
    expect_true(file.exists(file.path(pkg_dir, "my_dataset_VARIABELBESKRIVELSE.txt")))
    expect_true(file.exists(file.path(pkg_dir, "my_dataset_KODELISTE.txt")))

    expect_equal(DATASET_KODES_MD5,
                 unname(md5sum(file.path(pkg_dir, "my_dataset.csv"))))
    expect_equal(KODELISTE_MD5,
                 unname(md5sum(file.path(pkg_dir, "my_dataset_KODELISTE.txt"))))
    expect_equal(VLIST_KODES_MD5,
                 unname(md5sum(file.path(pkg_dir, "my_dataset_VARIABEL.txt"))))

    expect_equal(VDESC_MD5,
                 unname(md5sum(file.path(pkg_dir, "my_dataset_VARIABELBESKRIVELSE.txt"))))
    
})
          
