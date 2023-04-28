#' FD.18005 example dataset (without labels, with levels)
#'
#' This contains the data from the example information package
#' FD.18005. It contains levels for categorical variables (to make the
#' data more R-like) but does not contain labels (to allow for testing
#' of dataset labelling)
#'
#' @docType data
#' @keywords datasets
#' @name fd_18005_r
#' @seealso fd_18005_r_labels
#' @format A data frame
NULL


#' Labels for FD.18005
#'
#' This contains labels that can be applied to fd_18005_r using relabel_dataset
#'
#' @docType data
#' @keywords datasets
#' @name fd_18005_r_labels
#' @seealso fd_18005_r
#' @format A data frame containing variable_name and description columns
NULL

#' Example Table2 dataset (without labels, with levels)
#'
#' This contains some fabricated data that is linked to fd_18005_r
#' using the variable 'child_id'. This allows us to test the multiple
#' table functionality when emitting full information packages
#'
#' @docType data
#' @keywords datasets
#' @name example_table2
#' @seealso example_table2_labels, fd_18005_r
#' @format A data frame
NULL


#' Labels for example_table2
#'
#' This contains labels that can be applied to example_table2 using relabel_dataset
#'
#' @docType data
#' @keywords datasets
#' @name example_table2_labels
#' @seealso example_table2
#' @format A data frame containing variable_name and description columns
NULL
