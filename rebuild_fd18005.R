## Simple integration test to rebuild package FD.18005 from the data within 
## RigsArkivetRInfoPkg


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

#library(RigsArkivetRInfoPkg)

library(devtools)
load_all(file.path(source_directory,"RigsArkivetRInfoPkg"))


#data("fd_18005_r", package="RigsArkivetRInfoPkg")

pkg_output_dir=file.path(tempdir(), "FD.18005")

pkg_info=list(
    archive_index=system.file("extdata", "FD_18005_archiveIndex.xml", package="RigsArkivetRInfoPkg"),
    context_doc_index=system.file("extdata", "FD_18005_contextDocumentationIndex.xml", package="RigsArkivetRInfoPkg"),
    pkg_id="FD.18005",
                  tables=list(
                      list(
                          name="R_testfil",
                          label_file=fd_18005_r_labels,
                          ## NOTE: these user codes are DIFFERENT to
                          ## the original information package due to
                          ## coercion of these variables to R factor
                          ## (which doesn't preserve codes)
                          user_codes=list( gender='3',
                                           grade='3',
                                           education='6',
                                           hobby=c('8')
                                          ),                          
                          key_variable=c("child_id"),
                          description="Danish Longitudinal Research Study of Grandparents, Parents and Children - this is data 1 ",
                          table_dataset=fd_18005_r))
)


process_full_info_pkg(pkg_info,
                      output_dir = pkg_output_dir)                    

message(sprintf("Output test package to %s", pkg_output_dir))
