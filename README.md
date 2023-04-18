# R package for creating statistical information packages to fulfil requirements of Executive Order 128

This package can construct statistical information packages for
submission to the Danish National Archives (Rigsarkivet). You require
a package identifier and metadata files from the Rigsarkivet to do
this. 

## Installation

The simplest way is 

```
  library(devtools)
  install_github("BiomathematicsAndStatisticsScotland/rigsarkivet_r_infopkg/RigsArkivetRInfoPkg")
```

## Relationship with ASTA

When exporting this tool performs various simple checks for data
consistency and tries to warn the user of problems, it is ''not'' a
complete package consistency checker. Any output from this tool
''must'' be checked using ASTA.

See the [Rigsarkivet guidance
pages](https://en.rigsarkivet.dk/transfer-and-submit/creating-research-data/submitting-research-data/submitting-statistics-files/)
for further information on using ASTA

## Simple example

A simple invocation of the package will rebuild a version of the
FD.18005 example data package from example data stored within the
RigsArkivetRInfoPkg source directory:

```
    library(RigsArkivetRInfoPkg)
    pkg_output_dir=tempdir()

    pkg_description = list(
     archive_index=system.file("extdata", "FD_18005_archiveIndex.xml", package="RigsArkivetRInfoPkg"),
     context_doc_index=system.file("extdata", "FD_18005_contextDocumentationIndex.xml", package="RigsArkivetRInfoPkg"),
     pkg_id="FD.18005",
                   tables=list(
                       list(
                          name="R_testfil",
                          label_file=fd_18005_r_labels,
                          user_codes=list( gender='3',
                                           grade='3',
                                           education='6',
                                           hobby=c('8')
                                          ),                          
                          key_variable=c("child_id"),
                          description="Danish Longitudinal Research Study of Grandparents, Parents and Children - this is data 1 ",
                          table_dataset=fd_18005_r)))

    process_full_info_pkg(pkg_description,
                          output_dir = pkg_output_dir)   
```

Note that the emitted package will differ slightly from the original
FD.18005 package. This is due to transforms applied to the base data
in the package to make it more "R-like" - the categorical variables
gender, grade, education, hobby and so on are transformed into R
factors which results in the numeric values for these fields changing
when re-emitted as a dataset.

A simple command-line tool is available which emits a single RDS file
as an information package. See Convert_To_ASTA.R in the Github
repository for this. 

For more complex invocations (e.g. where multiple data tables are
involved) it is recommended to write a script which contains the
description of your information package as above.

The package vignette contains further examples of how to use this
software to emit more complex information packages. 

# Customisation options

## Relabelling the dataset

You can control relabelling operations by using the function
relabel_dataset. This is called implicitly when the parameter
label_file is supplied to process_full_info_pkg

## Modifying processing of the dataset

By replacing the function parameters:

* preprocess_dataset
* sanitise_numeric
* sanitise_character

when calling process_full_info_pkg or process_as_asta_export_script,
you can control the processing of the dataset, including variable
cleanup and so on. 

It is recommended to write a new function which encapsulates a call to
default_preprocess_dataset as there's a lot of useful logic which you
will probably want to reuse. 


