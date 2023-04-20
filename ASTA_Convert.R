library(optparse)

library(devtools)
load_all("RigsArkivetRInfoPkg")

#library(RigsArkivetInfoPkg)

## 

running_sourced = FALSE 
if(sys.nframe()>0) {
    ## Handle case where this script is source()ed
    source_directory <- dirname(sys.frame(1)$ofile)
    running_sourced <- TRUE
} else {
    ## Handle case where we are run from Rscript
    file_args = grep("^--file=", commandArgs(trailingOnly=FALSE), value=TRUE)

    source_directory <- dirname(gsub("^--file=","",file_args[1]))

}

if (is.null(source_directory) ||
    length(source_directory)==0 ||
    !file.exists(source_directory)) {
    stop("Unable to determine source directory for executing script. Aborting")
}

option_list = list(
  make_option(c("-o", "--output-dir"),
              type="character",
              default=getwd(),
              dest="output_dir",
              help="Output directory to write files to. If --fullpkg is set,
                it is expected to be in the archive structure format (and will be
                validated as such)",
              metavar="character"),

  make_option(c("-A", "--archive-index"),
              type="character",
              default=NULL,
              dest="archive_index",
              help="Path to the Archive Index file. Required if using --fullpkg",
              metavar="character"),
  make_option(c("-C", "--ctxdoc-index"),
              type="character",
              default=NULL,
              dest="context_doc_index",
              help="Path to the contextDocumentationIndex.xml file. Required
                if using --fullpkg",
              metavar="character"),
  make_option(c("-P", "--pkg-id"),
              type="character",
              default=NULL,
              dest="pkg_id",
              help="Package identifier (FD.XXXXX). Required if using --fullpkg,
                but may be inferred from --output-dir",
              metavar="character"),
  make_option(c("-D", "--description"),
              type="character",
              default=NULL,
              dest="archive_description",
              help="Plain text description of the archive. Required if using
                --fullpkg",
              metavar="character"),
  make_option(c("-K", "--key-variable"),
              type="character",
              default=NULL,
              dest="key_variable",
              help="Name of the key variable. Required if using --fullpkg",
              metavar="character"),
  
  make_option(c("-f", "--fullpkg"),
              type="logical",
              default=FALSE,
              dest="make_fullpkg",
              help="If set, the output directory is treated as the root of a data
                package (FD.50XXXX) containing context documentation and other files.
                Attempts will be made to validate the structure, and full metadata
                will be emitted for each table. If unset, tables and their metadata
                will be output in a form ready to be consumed by ASTA",
              metavar="logical",
              action="store_true"),
  make_option(c("-i", "--input-dataset"),
              type="character",
              default=NULL,
              dest="input_dataset",
              help="Input .RDS file to convert. At present this script can only handle
                a single data file, no references",
              metavar="character"),
  make_option(c("-r", "--row-limit"),
              type="integer",
              default=NULL,
              dest="row_limit",
              help="If set, limit the number of rows returned. This is useful for
                testing as ASTA can take a long time to run tests on completed data packages",
              metavar="integer"),
  make_option(c("-l", "--labels"),
              type="character",
              default=NULL,
              dest="input_labels",
              help="Labelling file allowing replacement of labels that are missing.
                Must be CSV with 2 columns, 'variable_name' and 'description'. See
                --missing-labels-file as well",
              metavar="/path/to/input-labels.csv"),
  make_option(c("", "--encoding"),
              type="character",
              default=NULL,
              dest="use_encoding",
              help="Specify the file encoding to use when running this script",
              metavar="(UTF-8|native.enc|...)"),              
  make_option(c("", "--factors-to-codes"),
              type="logical",
              default=FALSE,
              dest="factors_to_codes",
              help="If set, factor variables will be added to KODELISTE and emitted
                as their numeric equivalent",
              action="store_true"),
  make_option(c("", "--missing-labels-file"),
              type="character",
              default=NULL,
              dest="missing_labels_file",
              help="When --fullpkg is NOT in effect, the tool will write out any
                variables missing labels to this file which can then be edited and
                fed back in using the -l command line flag. Defaults to

                   paste0(basename(input_dataset),'missing_labels.csv')",
              metavar="/path/to/missing-labels.csv")
)

if (running_sourced) {
    ## Useful for debugging when source()ing this script
    opt=list()
    opt$input_dataset="/tmp/input/test.RDS"
    opt$output_dir="/tmp/output/FD.50099"
    opt$missing_labels_file="/tmp/output/missing-labels.csv"
    #opt$row_limit=as.integer(20)
    opt$make_fullpkg=FALSE
    opt$factors_to_codes=TRUE
    opt$row_limit=NULL
    opt$input_labels="/tmp/input/new_labels.csv"
    options(error=browser)

} else {



    opt_parser = OptionParser(option_list=option_list, description="
This tool is a simple wrapper around the data package export
routines in RigsArkivetRInfoPkg. It is used for testing and
experimentation. It is recommended that for dataset archival
export you write your own R script to invoke the routines in
RigsArkivetRInfoPkg. This gives the option to emit multiple tables,
provide user codes and much more.

The package vignette gives examples.

Example invocation to create a full package:

 Rscript ASTA_Convert.R --fullpkg -P FD.18005\
        -A RigsArkivetRInfoPkg/inst/extdata/FD_18005_archiveIndex.xml\
        -C RigsArkivetRInfoPkg/inst/extdata/FD_18005_contextDocumentationIndex.xml\
        -D \"Danish Longitudinal Study\" \
        -o /tmp/testit-fullpkg\
        -K child_id \
        -i RigsArkivetRInfoPkg/inst/extdata/FD_18005_table1.RDS\
        -l RigsArkivetRInfoPkg/inst/extdata/FD_18005_labels.csv\
        --factors-to-codes

The above emits a data package ready to be tested by ASTA. 

Example invocation to export data in the form used to create data packages in ASTA:

 Rscript ASTA_Convert.R -P FD.18005\
        -o /tmp/testit-asta\
        -i RigsArkivetRInfoPkg/inst/extdata/FD_18005_table1.RDS\
        -l RigsArkivetRInfoPkg/inst/extdata/FD_18005_labels.csv\
        --factors-to-codes

You can use the second form to convert multiple tables into a form
that ASTA will accept and then use ASTA to construct the full data package.
")
    opt = parse_args(opt_parser)
}

if (!is.null(opt$use_encoding)) {
    options(encoding=opt$use_encoding)
}

if (!file.exists(opt$input_dataset)) {
    stop(sprintf("Input dataset '%s' does not exist. Aborting", opt$input_dataset))
}
opt$input_dataset=normalizePath(opt$input_dataset)

if (!dir.exists(opt$output_dir)) {
    message(sprintf("Creating output directory '%s'", opt$output_dir))
    dir.create(opt$output_dir, recursive=TRUE)
    if (!dir.exists(opt$output_dir)) {
        stop(sprintf("Output directory %s was not created. Aborting!", opt$output_dir))
    }
} else {
    message(sprintf("Using existing output directory %s", opt$output_dir))
}

if ( !is.null(opt$row_limit) && !(is.integer(opt$row_limit) && opt$row_limit > 0)) {
    stop("Option -r/--row-limit, must be a positive integer")   
}

if (!is.null(opt$input_labels)) {
    if (!file.exists(opt$input_labels)) {
        stop(sprintf("Input labels file '%s' does not exist. Aborting", opt$input_labels))
    }
    opt$input_labels=normalizePath(opt$input_labels)
}

file_name <- dataset_to_name(opt$input_dataset)

if (is.null(opt$missing_labels_file)) {
    opt$missing_labels_file = file.path(opt$output_dir, sprintf("%s_missing_labels.csv", file_name))
}


if (opt$make_fullpkg) {    
    pkg_desc=list(
        archive_index=opt$archive_index,
        context_doc_index=opt$context_doc_index,
        pkg_id=opt$pkg_id,
        tables=list(
            list(
                name=file_name,
                label_file=opt$input_labels,
                key_variable=opt$key_variable,
                description=opt$archive_description,
                table_dataset=opt$input_dataset                
            )
        )
    )
    
    process_full_info_pkg(pkg_desc,
                          output_dir=opt$output_dir,
                          row_limit=opt$row_limit,
                          factors_to_codes=opt$factors_to_codes)
} else {
    
    ## Old program logic - we're replicating what the ASTA export script SHOULD output
    loaded_dataset <- load_and_truncate_dataset(opt$input_dataset, opt$row_limit)

    ## Set working dir - Also the path for saving files
    old_wd=getwd()
    setwd(opt$output_dir)

    ## Apply any explicit labels/other alterations we need to the dataframe before processing
    loaded_dataset = relabel_dataset(loaded_dataset, opt$input_labels)

    ## Identify variables that still need a label, write them out
    df_desc=describe_dataset(loaded_dataset)
    missing_desc = df_desc[df_desc$description %in% c("NULL",""),]

    if (nrow(missing_desc) > 0) {
        warning(sprintf("SOME VARIABLES ARE MISSING DESCRIPTIONS. these have been written to %s", opt$missing_labels_file))
        write.csv(missing_desc,
                  file=opt$missing_labels_file,
                  row.names = FALSE)
    } else {
        message(sprintf("No missing labels, skipping writing missing labels file %s", opt$missing_labels_file))
    }


    ## Process file
    process_as_asta_export_script(loaded_dataset, file_name, factors_to_codes=opt$factors_to_codes)

    setwd(old_wd)
}
