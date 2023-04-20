## Constants and data
#' @include sql_reserved_words.R
NULL

DEFAULT_OPTION_SCIPEN=1000
PKG_PATTERN="FD.(\\d+)"
## End constants and data

## Sanitisation functions

#' quote_value
#'
#' Put quotes around value x
#' @param x Value to quote
#' @return Quoted value
quote_value <- function(x){
  return(paste0("'", x, "'"))
}


#' default_sanitise_character
#' 
#' The default character value cleanup routine. This will strip
#' newlines and leading/trailing whitespace to help comply with the
#' requirements of Executive Order 128. Override this function to
#' perform other cleaning tasks and pass to emit_dataset
#'
#' @param col_name The data currently being cleaned (e.g. column or
#'     variable name). Useful for outputting meaningful warnings or
#'     errors
#' @param x The value(s) being cleaned
#'
#' @return The cleaned value(s)
#' @export
default_sanitise_character <- function(col_name, x) {
    return (gsub("^\\s*","",
                 gsub("\\s*$","",
                      gsub("\n", " ",
                           gsub("\r","",x)))))
}

#' default_sanitise_numeric
#' 
#' The default numeric value cleanup routine. This currently only
#' performs one function: ASTA seems to have issues with very small
#' decimals (high negative exponents). So we censor these very small
#' numbers to 0.00 and issue a warning. 
#'
#' Executive order 128 doesn't appear to impose any such limitation on
#' decimals so the precision limit is empirically determined to keep
#' ASTA happy. 
#' 
#' Override this function to perform other cleaning tasks and pass the
#' new function to emit_dataset/default_preprocess_dataset
#'
#' @param col_name The data currently being cleaned (e.g. column or
#'     variable name). Useful for outputting meaningful warnings or
#'     errors
#' @param x The value(s) being cleaned
#' @param float_precision_threshold The precision of float
#'     (decimal/double values) which will be censored. Usually this
#'     affects very small values
#'
#' @return The cleaned value(s)
#' @export
default_sanitise_numeric <- function(col_name, x, float_precision_threshold = 1e-10) {

    
    if (!is.integer(x) && is.double(x) ) {      
        censor_x= (abs(x)<float_precision_threshold) & (abs(x) > 0.00)
        
        if (!is.na(any(censor_x)) && any(censor_x)) {
            warning(sprintf("Column %s contains decimal values with greater precision than %s which appears to cause problems for ASTA. Censoring these values to 0.00. Rows affected: %s.", col_name, float_precision_threshold, paste(which(censor_x),collapse=",")))
            
            x[ censor_x ] = 0.00
        }
    }

    ## For all other numeric types we do nothing
    return (x)
}


#' default_preprocess_dataset
#' 
#' Preprocess the dataset to fix common problems. Called by
#' emit_dataset. By default this will try and get any data frame into
#' a form that will comply with Executive Order 128 when it is emitted
#' to CSV. However this is BY NO MEANS A COMPLETE CHECK. You may need
#' to perform additional manipulations to your data if ASTA complains.
#' 
#' Therefore if you wish to customise this function for your own purposes either
#'
#' * create a new closure which calls this function and make any
#'   changes you need AFTER this default function has been called and
#'   pass your new closure to emit_dataset
#'
#' * create a new closure which provides custom implementation of
#'   sanitise_numeric and sanitise_character
#'
#' See the vignette for details
#' 
#' @param df The data frame to preprocess
#' @param factors_to_codes If true, any factor variables will be
#'     converted to their numeric equivalent. Used to ensure the
#'     KODELIST contains the correct factor definitions when the
#'     dataset is emitted. If FALSE factors will be emitted as their
#'     text equivalent.
#' @param sanitise_numeric Function to sanitise numeric columns. By default the function default_sanitise_numeric() is used
#' @param sanitise_character Function to sanitise factors and character columns. By default the function default_sanitise_character() is used
#' @return The preprocessed dataset
#' @export
default_preprocess_dataset <- function (df,
                                        factors_to_codes=TRUE,
                                        sanitise_numeric=default_sanitise_numeric,
                                        sanitise_character=default_sanitise_character) {
    for (col_name in colnames(df)){

        ## N.B. we don't quote here as there is no point as write.table does its own quoting
        is_factor = "factor" %in% class(df[[col_name]])
        is_character = "character" %in% class(df[[col_name]])
        if (is_factor && factors_to_codes) {
            ## We're emitting factors as numeric levels and adding them to KODELISTE latere
            ## Type will be converted in variable description as well
            df[[col_name]] <- as.numeric(df[[col_name]])
        } else if (is_factor || is_character) {

            ## Remove factors and cleanup all values so they create a
            ## "good" CSV file if we're emitting factors as characters            
            df[[col_name]] <- sanitise_character(col_name, as.character(df[[col_name]]))
        } else if (is.numeric(df[[col_name]])) {
            df[[col_name]] <- sanitise_numeric(col_name, df[[col_name]])
        }
    }

    return (df)
}

#' check_for_missing_labels
#'
#' Examines df_desc (obtained using describe_dataset) for missing
#' label attributes If missing_labels_file is provided, the missing
#' labels will be written there.  Once edited to include labels, this
#' file can then be supplied to relabel_dataset to get the dataset
#' ready for import
#'
#' @param df_desc A description of the dataset as returned by describe_dataset
#' @param missing_labels_file
#' @return A data frame containing the missing labels, such as would be written to missing_labels_file
#' @export
check_for_missing_labels <- function( df_desc, missing_labels_file=NULL ) {
    missing_desc = df_desc[df_desc$description %in% c("NULL",""),]
    
    if (!is.null(missing_labels_file)) {
        if (file.exists(missing_labels_file)) {
            file.remove(missing_labels_file)
        }
        
        if (nrow(missing_desc) > 0) {
            warning(sprintf("%d VARIABLES ARE MISSING DESCRIPTIONS. these have been written to %s", nrow(missing_desc), missing_labels_file))
            write.csv(missing_desc,
                      file=missing_labels_file,
                      row.names = FALSE)
        } else {
            message(sprintf("No missing labels, skipping writing missing labels file %s", missing_labels_file))
        }
    } else if (nrow(missing_desc) > 0) {
        warning(sprintf("%d VARIABLES ARE MISSING DESCRIPTIONS.", nrow(missing_desc)))
    }
    
    return (missing_desc)
}

#' detect_originator_format
#' 
#' Determine format of dataframe to detect originating software. This
#' looks for special attributes usually added by the "haven" package.
#'
#' @param df Dataframe to analyse
#' @return "spss", "stata" "sas" or "other" depending on the detected
#'     format. Format "other" will encompass R-originated datasets
#'     without the special attributes.
#' @export
detect_originator_format <- function(df) {
    base_fmt_list <- sapply(df, function(x) names(attributes(x))[grepl("format.",names(attributes(x)))])
    if ("format.spss" %in% base_fmt_list){
        fmt <- "spss"
    } else if ("format.stata" %in% base_fmt_list){
        fmt <- "stata"
    } else if ("format.sas" %in% base_fmt_list){
        fmt <- "sas"
    } else {
        fmt <- "other" ## This includes R-originated datasets.
    }

    return(fmt)
}

#' describe_dataset
#'
#' Return a dataframe describing the dataset including any "label" attributes on columns.
#' Executive Order 128 requires all columns/variables be labelled; this function's output
#' is in a form that can be used to write a file suitable for passing to relabel_dataset
#' 
#' @param dataset Data frame to describe
#' @return Description of the dataset containing columns
#'     "variable_name", "description" and "levels" for each variable
#'     in the dataset
describe_dataset <- function(dataset) {
    variable_labels=sapply(names(dataset), function(x) {
        attributes(dataset[,x])$label
    })
    
    variable_levels=sapply(names(dataset), function(x) {
        if (is.factor(dataset[,x])) {
            paste(levels(dataset[,x]),sep="x ",collapse="; ")
        } else {
            ""
        }
    })

    as.data.frame(cbind(variable_name=names(dataset),
                        description=as.character(variable_labels),
                        levels=as.character(variable_levels)),
                  stringsAsFactors=FALSE)
}

## End sanitisation functions


## File management functions

#' ensure_directory
#'
#' Ensure the directory specified exists, with option to create. Will stop() if directory does not exist
#'
#' @param file_path Directory path to create. Will create recursively if necessary
#' @param create_structure if TRUE the directory will be created if it does not exist
#' @param warn_only If true, only issue warnings, not stop()
ensure_directory <- function(file_path,
                             create_structure=FALSE,
                             warn_only=FALSE) {
    if (!file.exists(file_path)) {
        if (create_structure) {
            dir.create(file_path, recursive=TRUE)
        } else {
            msg = sprintf("Directory %s does not exist and cannot be created", file_path)
            if (warn_only) {
                warning(msg)
            } else {
                stop(msg)
            }
        }
    }

    if (!dir.exists(file_path)) {
        msg=sprintf("File %s is not a directory", file_path)
        if (warn_only) {
            warning(msg)
        } else {
            stop(msg)
        }
    }
    
    return(file_path)
}

#' ensure_file
#'
#' Ensure the file specified exists, with option to create from a
#' specified source_file. Will stop() if file does not exist
#'
#' @param file_path File path to create. Will create recursively if necessary
#' @param create_structure if TRUE the file will be created from source_file if it does not exist
#' @param warn_only If true, only issue warnings, not stop()
#' @param source_file The file to copy to file_path when creating. If file exists already, the contents will be updated. 
ensure_file <- function(file_path,
                        create_structure=FALSE,
                        warn_only=FALSE,
                        source_file=NULL) {
    msg = NULL
    if (!file.exists(file_path)) {
        if (create_structure) {
            if (!is.null(source_file)) {
                file.copy(source_file, file_path)
            } else {
                msg = sprintf("Cannot create file %s as no source file is provided", file_path)
            }                               
        } else {
            msg = sprintf("File %s does not exist and cannot be created", file_path)
        }
    } else {
        if (!is.null(source_file)) {
            file.copy(source_file, file_path)
        }
    }
    
    if (!is.null(msg)) {
        if (warn_only) {
            warning(paste(msg, "You will need to provide this file before the package is complete"))
        } else {
            stop(msg)
        }
    }
    
    return(file_path)
}

#' setup_data_dir
#'
#' Cleans up the data directory in a package folder, removing any old
#' tableX directories and warning of other files that exist
#'
#' @param data_dir The path to the data directory to be cleaned
#'     up. Only files/directories beginning table1, table2..tableNN
#'     will be cleaned up.
#' @param cleanup If TRUE (default) old data will be removed. Otherwise, warnings will be printed
#' @export
cleanup_data_dir <- function(data_dir, cleanup=TRUE) {
    if (!dir.exists(data_dir)) {
        stop(sprintf("data_dir=%s does not exist", data_dir))
    }

    TABLE_PATTERN="table\\d+"
    
    ## cleanup old data
    if (cleanup) {
        for (old_table in list.files(data_dir, TABLE_PATTERN)) {
            path_to_table = file.path(data_dir, old_table)
            message(sprintf("Removing old data table %s", path_to_table))
            unlink(path_to_table, recursive=TRUE)
        }
    }

    stray_files=Filter(function (x) { x!="." && x!=".." }, list.files(data_dir, all.files=TRUE))

    if (length(stray_files) > 0) {
        if (length(stray_files) > 5) {
            msg = sprintf("Stray files '%s' (and %d more) in data dir '%s'. Please remove these before validating the package with ASTA",
                          paste(stray_files[1:5], collapse=","),
                          length(stray_files) - 5,
                          data_dir) 
        } else {
            msg = sprintf("Stray files '%s' in data dir '%s'. Please remove these before validating the package with ASTA",
                          paste(stray_files, collapse=","),
                          data_dir) 
        }
        warning(msg)
    }
}

#' dataset_to_name
#'
#' @param dataset_file Path to a dataset
#' @return basename of the file shorn of any extensions. This can then
#'     be used wherever a short identifying name of the dataset is
#'     required.
#' @export
dataset_to_name <- function(dataset_file) {
    return(sub(pattern = "(.*?)\\..*$", replacement = "\\1", basename(dataset_file)))
}
## End file management functions

## Output functions

#' emit_dataset
#'
#' Emits the dataset to a CSV file which should be compliant with the
#' requirements of section 9G in Executive Order 128. Emit to STDOUT by
#' default
#'
#' @param df Dataframe to emit, after being processed by
#'     preprocess_dataset
#' @param dataset_conn Connection or file where the dataset should be
#'     emitted. STDOUT by default.  If NULL no dataset will be emitted
#'     but the pre-processed dataset will be returned
#' @param factors_to_codes If true, any factor variables will be
#'     converted to their numeric equivalent. Used to ensure the
#'     KODELIST contains the correct factor definitions when the
#'     dataset is emitted. If FALSE factors will be emitted as their
#'     text equivalent.
#' @param preprocess_dataset A function to preprocess the dataset to
#'     remove or correct problem values. An example implementation is
#'     provided in default_preprocess_dataset which should work for
#'     many datasets. If not, customise it!
#' @param option_scipen Localised value to set the "scipen" option of
#'     R when writing the dataset. This is an attempt to suppress
#'     exponential notation when printing numeric decimals as Exec
#'     Order 128 requires NR2 decimals (ISO-6093-1985) which does not
#'     allow exponential notation.
#' @return a dataframe containing the pre-processed dataset
#' @export
emit_dataset <- function(df,
                         dataset_conn=stdout(),
                         factors_to_codes=TRUE,
                         preprocess_dataset=default_preprocess_dataset,
                         option_scipen=DEFAULT_OPTION_SCIPEN) {

    old_options <- options(scipen=option_scipen)
    on.exit(options(old_options))

    ## Warn the user what is happening with decimals. Note that
    ## default_sanitise_numeric will also censor certain very small,
    ## precise values as ASTA doesn't like those either. 
    message(sprintf("NOTE: Decimals wider than %d digits will be printed in exponential form.
      This will result in an ASTA error as Exec Order 128 requires decimals be printed
      without exponentation. If so, modify option_scipen or filter your dataset to remove
      such values. A custom sanitise_numeric function will also help", option_scipen))
    
            
    df_csv <- preprocess_dataset(df, factors_to_codes=factors_to_codes)
    
    ## Output .csv file with ";" as seperator and UTF-encoding
    ##
    ## See section 9.G. in exec order 128 for how to handle
    ## content.
    ##
    ## Write the header unquoted then the data quoted
    ## This keeps ASTA happy
    ##
    ## Datasets are only written if there is a connection
    col_names_only = as.data.frame(matrix(ncol=ncol(df_csv), nrow=0))
    colnames(col_names_only) = colnames(df_csv)
    
    if (!is.null(dataset_conn)) {
        write.table(col_names_only,
                    file      = dataset_conn,
                    sep       = ";",
                    eol       = "\r\n",
                    row.names = FALSE,
                    col.name  = TRUE,
                    quote     = FALSE,
                    na        = "")
        
        write.table(df_csv,
                    file      = dataset_conn,
                    sep       = ";",
                    eol       = "\r\n",
                    row.names = FALSE,
                    col.names = FALSE,
                    quote     = TRUE,
                    append    = TRUE,
                    qmethod   = "double",
                    na        = "")
    }

    ## Return to caller in case anyone wants it
    return(df_csv)
}

#' emit_variable_list
#' 
#' Creates "VARIABEL" the section of the table metadata file,
#' including inferring appropriate types and if factors_to_codes is
#' TRUE, links to the KODELISTE (see emit_kodeliste). Usually called by
#' emit_metadata_file
#'
#' VARIABEL is described in Figure 9.4 and Section 9.I.4 in Executive Order 128
#'
#' @param df Dataframe to describe
#' @param variabel_conn Connection or file to emit the variable list
#'     to. STDOUT by default. 
#' @param factors_to_codes If true, any factor variables will have a
#'     link to the KODELISTE inserted Used to ensure the KODELISTE
#'     contains the correct factor definitions when the dataset is
#'     emitted. If FALSE factors will be emitted as their text
#'     equivalent.
#' @seealso emit_kodeliste
#' @return a dataframe containing the variable list
#' @export
emit_variable_list <- function (df, variabel_conn=stdout(), factors_to_codes=TRUE) {
    ## Initialize data frame for output
    df_variabel <- data.frame(stringsAsFactors = FALSE)
    
    fmt = detect_originator_format(df)

    
    ## Loop over each column and extract necessary information
    for (col in colnames(df)){

        col_classes = class(df[[col]])

        if (fmt=="other" || fmt=="sas") {        
            ## 1) Handle formats for non-statfiles and sas
            if (factors_to_codes &&
                "factor" %in% col_classes) {
                ## Handle R factor variables - these are emitted as integer levels
                variabel_i <- paste(col, "int")
            } else if ("Date" %in% col_classes) {
                variabel_i <- paste(col, "date")
                
            } else if ("hms" %in% col_classes) {
                variabel_i <- paste(col, "time")
                
            } else if ("times" %in% col_classes) {
                variabel_i <- paste(col, "time")
                
            }else if ("POSIXct" %in% col_classes) {
                variabel_i <- paste(col, "datetime")
                
            } else if ("double" %in% col_classes) {
                variabel_i <- paste(col, "decimal")
                
            } else if ("numeric" %in% col_classes) { 
                variabel_i <- paste(col, "decimal")
                
            } else if ("integer" %in% col_classes) {
                variabel_i <- paste(col, "int")
                
            } else {
                variabel_i <- paste(col, "string")
            }
        }
        
        ## 2) Handle formats for spss/stata files
        var_fmt_list <- c("%tdCCYY-NN-DD",
                          "%tcHH:MM:SS",
                          "%tcCCYY-NN-DD!THH:MM:SS",
                          "%tcCCYY-NN-DD!THH:MM:SS.s",
                          "%tcCCYY-NN-DD!THH:MM:SS.ss",
                          "%tcCCYY-NN-DD!THH:MM:SS.sss",
                          "h:m:s")
        if (fmt=="spss" || fmt=="stata"){
            fmt_special <- attributes(df[[col]])[grepl("format.", 
                                                       names(attributes(df[[col]])))]
            
            if (fmt_special %in% var_fmt_list){
                variabel_i <- paste(col, fmt_special)
                
            } else {
                variabel_i <- paste(col, tolower(fmt_special))
            }
        }

        ## Handle "kodeliste" reference for converted variables        
        if ("haven_labelled" %in% col_classes) {
            ## Note: Make assumption that all converted
            ## i.e. haven_labelled variables are decimals (except
            ## stata/spss)
            
            ## Thus, no dates, strings etc. have a description
            if (fmt=="sas"){
                variabel_i <- paste(col, "decimal", paste0(attributes(df[[col]])$format.sas,"."))
                
            } else if (fmt=="spss" || fmt=="stata"){
                fmt_special <- attributes(df[[col]])[grepl("format", 
                                                           names(attributes(df[[col]])))]
                variabel_i <- paste(col, 
                                    tolower(fmt_special), 
                                    paste0(col,'.'))
                
            } else {
                ## Note: If non-stat format, but haven_labelled, we assume decimal
                variabel_i <- paste(col, "decimal", paste0(col,'.'))
            }
        } else if (!is.null(attributes(df[[col]])$labels)){
            ## Note: If "labels" are attached to column, but not haven_labelled class
            ## Not sure how this interacts with the work done in part 1) above...  
            if (fmt=="sas"){
                
                if ("Date" %in% col_classes) {
                    variabel_i <- paste(col, "date", paste0(attributes(df[[col]])$format.sas,"."))
                    
                } else if ("hms" %in% col_classes){
                    variabel_i <- paste(col, "time", paste0(attributes(df[[col]])$format.sas,"."))
                    
                } else if ("times" %in% col_classes){
                    variabel_i <- paste(col, "time", paste0(attributes(df[[col]])$format.sas,"."))
                    
                } else if ("POSIXct" %in% col_classes){
                    variabel_i <- paste(col, "datetime", paste0(attributes(df[[col]])$format.sas,"."))
                    
                } else if ("double" %in% col_classes){
                    variabel_i <- paste(col, "decimal", paste0(attributes(df[[col]])$format.sas,"."))
                    
                } else if ("numeric" %in% col_classes){
                    variabel_i <- paste(col, "decimal", paste0(attributes(df[[col]])$format.sas,"."))
                    
                } else if ("integer" %in% col_classes){
                    variabel_i <- paste(col, "int", paste0(attributes(df[[col]])$format.sas,"."))
                    
                } else {
                    variabel_i <- paste(col, "string", paste0(attributes(df[[col]])$format.sas,"."))
                }                           
            } else {
                
                variabel_i <- paste(col, "decimal", paste0(col,'.'))
            }            
        } else if (factors_to_codes && is.factor(df[[col]])) {
            ## KODELIST reference for R factor variables, which have been coerced to int
            variabel_i <- paste(col, "int", paste0(col,'.'))            
        }
        
        ## Concatenate result to data frame
        df_variabel <- rbind(df_variabel, data.frame(variabel_i, stringsAsFactors = FALSE))
        
    }
    
    ## If variables exist proces and write file
    if (nrow(df_variabel)>0){
                
        ## Save data frame
        write.table(df_variabel,
                    file      = variabel_conn,
                    row.names = FALSE,
                    quote     = FALSE,
                    col.names = FALSE,
                    eol       = "\r\n")
    }

    return (df_variabel)
}

#' emit_variable_descriptions
#'
#' Emit the descriptions of all variables (the "VARIABELBESKRIVELSE"
#' section of the table metadata file. The sanitisation functions are
#' used to clean the descriptions before emitting them.
#'
#' VARIABELBESKRIVELSE is described in Figure 9.4 in Executive Order 128
#' 
#' @param df Dataframe to describe
#' @param description_conn Connection or file to emit the variable list
#'     to. STDOUT by default. 
#' @param sanitise_numeric Function to sanitise numeric data. By default the function default_sanitise_numeric() is used
#' @param sanitise_character Function to sanitise character data. By default the function default_sanitise_character() is used
#' @return a dataframe containing the variable descriptions
#' @export
emit_variable_descriptions <- function(df,
                                       description_conn=stdout(),
                                       sanitise_numeric=default_sanitise_numeric,
                                       sanitise_character=default_sanitise_character) {
    
    ## Extract variable description
    col_lbl_list <- sapply(df, function(x) attributes(x)$label)
    
    ## Remove variables with no label available
    col_lbl_list <- col_lbl_list[!sapply(col_lbl_list, is.null)]

    ## Warn of variable names with 'bad' characters in them
    ## (primarily quotes). The caller must address this issue.    
    ## Exec order 128 states variable/column names must comply with
    ##   ISO/IEC 9075:1999 - Database Language SQL (SQL-99)
    ## and escaping isn't allowed
    bad_names = as.character(Filter(Negate(is.null),
                                    sapply(colnames(df),
                                           function (x) { if (toupper(x) %in% SQL99_RESERVED_WORDS) { return(x) }})))

    if (length(bad_names) > 0) {
        warning(sprintf("Variable names %s conflict with reserved words in SQL99",
                paste(bad_names, collapse=",")))
    }  

    ## Finally sanitise the variable labels themselves
    col_lbl_list <- sanitise_character("Variable Labels", col_lbl_list)
    
    ## Convert to data frame
    ## unlist() is necessary to make as.data.frame build row-wise rather than column-wise
    df_description <- as.data.frame(unlist(col_lbl_list), 
                                    stringsAsFactors = FALSE, 
                                    row.names = names(col_lbl_list))
    
    ## If descriptions exist process and write file
    if ( nrow(df_description) > 0 ){
        
        ## Paste description and column name in single column - if description exist
        if (ncol(df_description)>0){
            df_description[,1] <- paste(rownames(df_description), quote_value(df_description[,1]))
        }
                
        ## Save data frame
        write.table(df_description,
                    file      = description_conn,
                    row.names = FALSE,
                    quote     = FALSE,
                    col.names = FALSE,
                    eol       = "\r\n")
    }

    return(df_description)
}

#' emit_kodelist
#'
#' Produce the KODELISTE section of the table metadata file, linking
#' categorical variable codes to their definitions. Only useful when
#' factors_to_codes=TRUE
#'
#' KODELISTE is defined in Figure 9.4 and section 9.I.5 of Executive Order 128
#' 
#' @param df Dataframe to extract codes for factors/categorical variables from
#' @param kodeliste_conn The connection or file to write the KODELISTE to. STDOUT by default
#' @param factors_to_codes If TRUE, the KODELISTE will be produced. If FALSE nothing will be produced. 
#' @return data frame containing the KODELISTE for the dataset
#' @export
emit_kodeliste <- function(df, kodeliste_conn=stdout(), factors_to_codes = TRUE) {
   
    ## Initialize data frame
    df_kodeliste <- data.frame(stringsAsFactors = FALSE)
    fmt = detect_originator_format(df)
    
    ## Loop over each column and extract kodeliste
    for (col in colnames(df)){

        col_lbls_i  <- attributes(df[[col]])$labels
        
        col_vals_i  <- names(col_lbls_i)
        col_names_i <- unname(col_lbls_i)
        
        if (!is.null(col_lbls_i)){
            if (fmt=="sas"){
                
                ## If variable already exists in kodeliste continue
                if (attributes(df[[col]])$format.sas %in% df_kodeliste[,1]){
                    next
                }
                
                kodeliste_i <- c(attributes(df[[col]])$format.sas,
                                 paste(quote_value(col_names_i[!is.na(col_lbls_i)]), 
                                       quote_value(col_vals_i[!is.na(col_lbls_i)])))
                
            } else {
                kodeliste_i <- c(col,
                                 paste(quote_value(col_names_i[!is.na(col_lbls_i)]), 
                                       quote_value(col_vals_i[!is.na(col_lbls_i)])))
            }
            
        } else if ( factors_to_codes &&
                    is.factor(df[[col]])) {
            col_vals_i = levels(df[[col]])
            col_names_i = rep(1:length(col_vals_i))
            kodeliste_i <- c(col,
                             paste(quote_value(col_names_i[!is.na(col_names_i)]), 
                                   quote_value(col_vals_i[!is.na(col_vals_i)])))                        
        } else {
            kodeliste_i <- NULL
        }
        
        df_kodeliste <- rbind(df_kodeliste, data.frame(kodeliste_i, stringsAsFactors = FALSE))
    }

    ## If any kodeliste input exist proces and write file
    if (nrow(df_kodeliste)>0){
                
        ## Save data frame
        write.table(df_kodeliste,
                    file      = kodeliste_conn,
                    row.names = FALSE,
                    quote     = FALSE,
                    col.names = FALSE,                
                    eol       = "\r\n")
    }

    return (df_kodeliste)
}

#' emit_metadata_file
#'
#'
#' Based on the data frame provided as df, this routine emits the
#' table metadata file as described in Section 9.I of Executive Order
#' 128. Further table metadata is supplied in parameter table_info;
#' this is the same metadata you are required to enter when creating a
#' dataset using ASTA
#' 
#' @param df The data frame whose metadata shall be emitted
#' @param table_info A named list describing the table. See the
#'     example and the expected format, all documented below
#' @param metadata_conn The connection or file to write the metadata
#'     file to. Default STDOUT
#' @param sanitise_numeric Function to sanitise numeric columns. By
#'     default the function default_sanitise_numeric() is used
#' @param sanitise_character Function to sanitise factors and
#'     character columns. By default the function
#'     default_sanitise_character() is used
#' @param factors_to_codes if true, factor variables will be converted
#'     to their numeric representation and KODELISTE will be
#'     emitted. Otherwise, the string equivalent of factor variables
#'     will be used in the main dataset and no KODELISTE will be
#'     produced
#' @return a named list with names matching each of the sections of
#'     the metadata file
#' @examples
#' \dontrun{
#' df = cbind(data.frame(model=rownames(mtcars)), mtcars)
#' 
#' emit_metadata_file(df,
#'    list( name="mtcars",
#'          key_variable="model",
#'          description="The MTCars dataset",
#'          table_dataset=mtcars,
#'          reference=list(
#'            list( other_dataset="some_other_dataset",
#'                  other_key_variable="other_car_model",
#'                  our_key_variable="model")
#'          )
#'  )
#' }
#' 
#' @section Format of table_info
#'
#' table_info is a named list with the following parameters:
#'
#' name
#' :the human-readable short name of the dataset
#' 
#' key_variable
#' :character vector denoting the key (identifying) variables
#' 
#' description
#' :a paragraph of text describing what the dataset contains
#' 
#' table_dataset
#' :a data frame, connection or filename, the latter two options should allow the dataset to be read in as RDS format. Data labels are stored on each column in the "label" attribute
#' 
#' user_codes
#' :a named list of variables in df with a list of special codes for missing data in that variable. Optional. See the "format of user codes" section below
#' 
#' reference
#' :a list containing further named list which contain references to other tables in this information package. The cross-table reference format is discussed below. Optional
#' 
#' label_file
#' :a file, connection or dataframe containing two columns "variable_name" and "description" allowing descriptive labels to be set on all variables in table_dataset using the function relabel_dataset. Optional. 
#' 
#' @section Format of cross-table references
#'
#' "reference" is a list containing zero or more named lists
#' describing a cross-table reference. These lists contain the
#' following parameters:
#' 
#' other_dataset
#' :the "name" used in the table_info definition of the dataset this reference refers to 
#' 
#' other_key_variable
#' :the name of a key variable in the other dataset
#' 
#' our_key_variable
#' :the name of a key variable in this dataset
#' 
#'
#' other_key_variable and our_key_variable should be the same
#' information (e.g. an ID number) and can thus be used to form a link
#'
#' @section Format of user codes
#'
#' A short example is the easiest way to explain. In this case we
#' declare that values ("C" and "F") in the "spray" column in the InsectSprays
#' dataset represent missing data.
#'
#' \dontrun{
#'   table_info=list(name="InsectSprays,
#'                   description="The insect sprays dataset",
#'                   ...
#'                   user_codes = list ( spray=c("C", "F") ),
#'                   ...)
#' }
#'
#' @seealso relabel_dataset, emit_dataset, emit_variable_list, 
#' @export
emit_metadata_file <- function (df,
                                table_info,
                                metadata_conn=stdout(),
                                sanitise_numeric=default_sanitise_numeric,
                                sanitise_character=default_sanitise_character,
                                factors_to_codes=FALSE) {

    file_opened = FALSE
    if (is.character(metadata_conn)) {        
        output_conn = file(metadata_conn, open="w")
        file_opened=TRUE
    } else if ("connection" %in% class(metadata_conn)) {
        output_conn = metadata_conn
    }

    sep='\r\n'

    ## Our references have been validated elsewhere so now we just need to format them
    if (is.list(table_info$reference)) {
        references=c()
        for (ref in table_info$reference) {
            ref_text = sprintf("%s '%s' '%s'",
                               ref$other_dataset,
                               ref$other_key_variable,
                               ref$our_key_variable)
            references=append(references, ref_text)
        }
        reference_text=paste(append(references,""), collapse=sep)
    } else {
        reference_text=""
    }
    

    cat(paste(c("SYSTEMNAVN","R","",
                "DATAFILNAVN",table_info$name,"",
                "DATAFILBESKRIVELSE", table_info$description,"",
                "NØGLEVARIABEL", paste(table_info$key_variable,collapse=sep),"",
                "REFERENCE", reference_text,  
                "VARIABEL",""), collapse=sep),
        file=output_conn)

    var_list = emit_variable_list(df, output_conn, factors_to_codes=factors_to_codes)

    cat(paste(c("","VARIABELBESKRIVELSE",""), collapse=sep), file=output_conn)

    var_desc = emit_variable_descriptions(df,
                                          output_conn,
                                          sanitise_numeric=sanitise_numeric,
                                          sanitise_character=sanitise_character)

    cat(paste(c("","KODELISTE",""), collapse=sep), file=output_conn)

    kodeliste = emit_kodeliste(df, output_conn, factors_to_codes=factors_to_codes)

    cat(paste(c("","BRUGERKODE",""), collapse=sep), file=output_conn)

    if (is.list(table_info$user_codes)) {
        for (coded_var in names(table_info$user_codes)){
            if (coded_var %in% colnames(df)) {
                cat(
                    paste(coded_var, sapply(table_info$user_codes[coded_var], quote_value),
                          sep=" ", collapse=" "),
                    paste(sep),
                    file=output_conn)
            } else {
                warning(sprintf("User code variable '%s' does not exist as a variable in table '%s'. Ignoring", coded_var, table_info$name))
            }
        }
    }        
    
    cat(paste(c("",""), collapse=sep), file=output_conn)
    
    if (file_opened) {
        close(output_conn)
    }

    return (list(
        SYSTEMNAVN="R",
        DATAFILNAVN=as.character(table_info$name),
        DATAFILBESKRIVELSE=as.character(table_info$description),
        NØGLEVARIABEL=as.list(table_info$key_variable),
        REFERENCE=reference_text,
        VARIABEL=var_list,
        VARIABELBESKRIVELSE=var_desc,
        KODELISTE=kodeliste,
        BRUGERKODE=list()))
}

## End output functions

## Data loading functions

#' count_context_documents
#'
#' Count the expected number of context documents required by this
#' information package. N.B this supposes a single document collection
#' rather than multiple.
#' 
#' @param ctx_doc_file Path to a contextDocumentationIndex.xml (or connection to same)
#' @return a count of the expected context documents
#' @export
count_context_documents <- function(ctx_doc_file) {
    ctx_data = readLines(ctx_doc_file, warn=FALSE)
    return (length(grep("<document>", ctx_data)))
}


#' load_and_truncate_dataset
#'
#' Utility function to load an RDS file (or data frame) and optionally
#' truncate it. This is useful for testing as ASTA can take a long
#' time to validate large datasets, therefore truncation allows a
#' quick test to be done and any structural problems fixed before
#' loading the full dataset.
#'
#' This function preserves column attributes as the rest of the tool
#' relies on them.
#'
#' @param source_file The RDS file (or data frame) to load/truncate
#' @param row_limit If a positive integer, the loaded data will be truncated at this number of rows. 
#' @return the loaded/truncated dataset, as a data frame
#' @export
load_and_truncate_dataset <- function(source_file, row_limit=NULL) {
    if (is.data.frame(source_file)) {
        file = source_file
    } else {
        file = readRDS(source_file)
    }
    
    if (is.integer(row_limit)) {
        if (nrow(file) > row_limit) {
            message(sprintf("Truncating file %s to first %d rows of %d", source_file, row_limit, nrow(file)))

            ## Subset and preserve attributes. Should really use the "sticky" package for this        
            new_file = file[1:opt$row_limit, ]
            for (col in colnames(file)) {
                old_attrs = attributes(file[, col])
                attributes(new_file[, col]) = old_attrs
            }
            file = new_file
        }
    }
    return (file)
}

#' relabel_dataset
#'
#' Add label attributes to the column(s) in df listed in label_data.
#'
#' @param df The data frame where label attributes will be added
#' @param label_data  A data frame with two columns, "variable_name" and "description" used to label corresponding variables in df
#' @return a copy of df containing the relabelled data. Other attributes are preserved
#' @export
relabel_dataset <- function( df, label_data ) {
    ## Do nothing if we have no new labels
    if (is.null(label_data)) {
        return(df)
    }
    
    if (is.character(label_data)) {
        label_data =  read.csv(label_data,
                               stringsAsFactors=FALSE)
    } else if (!is.data.frame(label_data)) {
        stop("label_data is not a data frame. Aborting")
    }
    
    for (col in colnames(df)) {
        new_label = label_data[label_data$variable_name == col, "description"]
        if (! (is.null(new_label) || identical(new_label, character(0))) ) {
            attributes(df[, col])$label=new_label
        }
    }  

    return(df)    
}
## End data loading functions

## Verification functions

#' verify_named_list
#' 
#' Check that list x with description list_name has all names in
#' required_names of required class, any names present in
#' optional_names have the right type and there are no names present
#' that aren't in either list
#'
#' @param x the list to check
#' @param list_name A user-defined name associated with x. This is
#'     used to output meaningful error messages
#' @param required_params A named list linking parameter names to
#'     type. For example: list(param_name=c("character",
#'     "numeric")). All parameters in this list are expected to be in
#'     x and have the correct type
#' @param optional_params As required_params but these parameters do
#'     not need to be present in x. If present, the type will be
#'     checked.
#' @param show_warnings If true (default) warnings will be shown
#' @return TRUE if the parameters in x are OK, FALSE otherwise
#' 
verify_named_list <- function(x, list_name, required_params, optional_params, show_warnings=TRUE) {
   

    all_params = append(required_params, optional_params)
    
    params_ok = TRUE
    
    x_names = names(x)
  
    missing_required = required_params[!(names(required_params) %in% x_names)]
    unknown_params = x_names[!(x_names %in% names(all_params))]

    if (length(missing_required) > 0) {
        params_ok=FALSE
        if (show_warnings) {
            warning(sprintf("Required params %s are missing in %s",
                            paste(missing_required, collapse=","), list_name))
        }
    }

    if (length(unknown_params) > 0) {
        params_ok=FALSE
        if (show_warnings) {
            warning(sprintf("Unknown params %s are present in %s",
                            paste(unknown_params, collapse=","), list_name))
        }
    }

    if (params_ok) {
        for (param_name in x_names) {
            actual_class = class(x[[param_name]])
            expected_class = all_params[[param_name]]
            if (! (actual_class %in% expected_class) ) {
                params_ok=FALSE
                if (show_warnings) {
                    warning(sprintf("Parameter %s has type %s but requires type of %s",
                                    param_name,
                                    actual_class,
                                    paste(expected_class, collapse=",")))
                }
            }
        }
    }

    return(params_ok)
}

#' verify_pkg_description
#'
#' Verify that the package description appears to be correct and will
#' create a valid package. Optionally create that package in pkg_dir,
#' ready to accept data
#' 
#' @param pkg_dir The base directory that the package will be created
#'     in. If this path contains a package identifier 
#'     as the terminal path component (FD.XXXXX) then that will be used as the package ID.
#' 
#' @param package_description A named list describing the package you
#'     want to create, allowing the tool to create the necessary
#'     metadata structures required by the Rigsarkivet and fill it
#'     with content. The contents of this list is discussed below
#' @param create_structure If TRUE, the package structure will be
#'     created if it is not already there. If FALSE an existing
#'     package structure will simply be verified
#' @param show_warnings If TRUE, warnings are shown rather than errors
#' @return A named list describing the package structure created (as
#'     returned from verify_pkg_file_structure.
#' 
#' @section Package Description Format
#'
#' Describe your package in a named list with the following keys:
#'
#' archive_index
#' :the path to an archiveIndex.xml file associated with your package. This is supplied by the Rigsarkivet
#'
#' context_doc_index
#' :the path to a contextDocumentationIndex.xml file associated with your package. This is supplied by the Rigsarkivet
#' 
#' pkg_id
#' :the package identifier as supplied by the Rigsarkivet (in the form FD.XXXXX). If not supplied, the tool will attempt to infer it from the pkg_dir parameter
#' 
#' tables
#' :a list of tables in the dataset. These are discussed in the documentation for emit_metadata_file 
#'
#' @examples
#'
#' ```{r, eval=FALSE}
#' library(RigsArkivetRInfoPkg)
#' pkg_output_dir=tempdir()
#' pkg_descroption = list(
#'     archive_index=system.file("extdata", "FD_18005_archiveIndex.xml", package="RigsArkivetRInfoPkg"),
#'     context_doc_index=system.file("extdata", "FD_18005_contextDocumentationIndex.xml", package="RigsArkivetRInfoPkg"),
#'     pkg_id="FD.18005",
#'                  tables=list(
#'                     list(
#'                       name="R_testfil",
#'                          label_file=fd_18005_r_labels,                             
#'                          key_variable=c("child_id"),
#'                          description="Danish Longitudinal Research Study of Grandparents, Parents and Children - this is data 1",
#'                          table_dataset=fd_18005_r,
#'                          reference=list(
#'                              list(
#'                                  other_dataset="childrens_pets",
#'                                  other_key_variable="child_id",
#'                                  our_key_variable="child_id")
#'                              )
#'			),
#'		       list(
#'                          name="childrens_pets",
#'                          label_file=example_table2_labels,
#'                          key_variable=c("pet_id"),
#'                          description="Childrens' pets, to provide a simple example of a 2nd table",
#'                          table_dataset=example_table2)))
#'
#'  process_full_info_pkg(pkg_description,
#'                          output_dir = pkg_output_dir)   
#' ```
#' 
#' 
#' @seealso emit_metadata_file, verify_pkg_file_structure
verify_pkg_description <- function(pkg_dir, package_description, create_structure=TRUE, show_warnings=TRUE) {

    required_toplevel_params = list(archive_index="character",
                                   context_doc_index="character",
                                   tables="list")
    optional_toplevel_params = list(pkg_id="character")


    required_table_params = list(name="character",
                             key_variable="character",
                             description="character",
                             table_dataset=c("character", "data.frame"))
    optional_table_params = list(label_file=c("character", "data.frame"),
                                 user_codes="list",
                                 reference="list")    
    
    required_reference_params = list(other_dataset="character",
                                 other_key_variable="character",
                                 our_key_variable="character")
    optional_reference_params = list()

    ## Validate all names are present/correct in the various bits of the list
    names_ok = verify_named_list(package_description,
                                 "Package Description",
                                 required_toplevel_params,
                                 optional_toplevel_params)

    keys_by_dataset=list()

    keys_by_other_dataset=list()
        
    for (table_def in package_description$tables) {
        this_table_ok = verify_named_list(table_def,
                                          sprintf("Table %s",table_def$name),
                                          required_table_params,
                                          optional_table_params,
                                          show_warnings=show_warnings)

        names_ok = names_ok && this_table_ok 

        if (this_table_ok) {
            keys_by_dataset[table_def$name] = list(table_def$key_variable)
             
            for (reference_def in table_def$reference) {
                this_ref_ok = verify_named_list(reference_def,
                                                sprintf("Table reference %s", table_def$name),
                                                required_reference_params,
                                                optional_reference_params,
                                                show_warnings=show_warnings)

                names_ok = names_ok && this_ref_ok
            
                if (this_ref_ok) {
                     keys_by_other_dataset[reference_def$other_dataset] =
                         list(reference_def$other_key_variable)
                   
                }
            }
            
        }
    }

    ## Check that other_dataset refers to a table that exists in the
    ## package description
    for (ref_dataset in names(keys_by_other_dataset)) {
        actual_keys=keys_by_dataset[[ref_dataset]]       
        if (is.null(actual_keys)) {
            names_ok=FALSE
            if (show_warnings) {
                warning(sprintf("other_dataset '%s' is not a named dataset in the table list", ref_dataset))
            }
        }
    }

    if (!names_ok) {
        stop("Package definition is invalid. Please correct any errors and retry")
    }
    
    return(verify_pkg_file_structure(pkg_dir,
                                     pkg_id = package_description$pkg_id,
                                     create_structure=create_structure,
                                     archive_index=package_description$archive_index,
                                     context_doc_index=package_description$context_doc_index))
}

#' verify_pkg_file_structure
#'
#' This routine verifies that the package file structure on disk is
#' valid and warns of problems with it If create_structure is TRUE,
#' any missing elements will be created. This routine relies on
#' possessing a valid package identifier (FD.XXXXX), archiveIndex.xml
#' and contextDocumentationIndex.xml from the Danish National
#' Archives.
#'
#' In addition to the basic file structure the presence of a
#' sufficient number of context documents (defined by the return value
#' of count_context_documents) in
#' 
#'   pkg_dir/ContextDocumentation/docCollection1
#'
#' is assured by checking for the file(s)
#' 
#'    pkg_dir/ContextDocumentation/docCollection1/{1..N}/1.tif
#'
#' where N is the number of context documents. This is a limited test
#' and as such only issues a warning. Generating correct context
#' documentation is beyond the scope of this tool. See
#'
#' https://en.rigsarkivet.dk/transfer-and-submit/creating-research-data/submitting-research-data/submitting-document-collections/
#'
#' for further details
#'
#' @param pkg_dir The directory in which the package will be written
#'     (or verified)
#' @param pkg_id The package identifier to associate with this package
#'     (FD.XXXXX). If NULL attempts will be made to infer this from
#'     the provided pkg_dir. In the case where both a package exists
#'     on disk AND a pkg_id is provided AND they conflict, the on-disk
#'     identifier will be used.
#' @param archive_index A file or connection which contains the
#'     archiveIndex.xml file for this archive. It will be read and
#'     placed into the pkg_dir/Indices directory. May be NULL if the
#'     file already exists in a package structure on disk
#' @param context_doc_index A file or connection which contains the
#'     contextDocumentationIndex.xml file for this archive. It will be
#'     read and placed into the pkg_dir/Indices directory and used to
#'     check the number of context documents. May be NULL if the
#'     file already exists in a package structure on disk
#' @param create_structure if TRUE attempts will be made to create the
#'     structure of the package. If FALSE, an existing package structure will be checked.
#' 
#' @return a named list describing the package layout. For example:
#'
#'   list(pkg_id= "FD.99999",
#'        pkg_dir= "/tmp/pkg-out/FD.99999"
#'        context_doc_dir= "/tmp/pkg-out/contextDocumentation/docCollection1"
#'        data_dir= "/tmp/pkg-out/FD.99999/Data")
#' 
#' @seealso count_context_documents, verify_pkg_description
#' @export
verify_pkg_file_structure <- function(pkg_dir,
                                      pkg_id = NULL,
                                      archive_index=NULL,
                                      context_doc_index=NULL,
                                      create_structure=TRUE) {
    candidate_pkg_id = basename(pkg_dir)
    if (length(grep(PKG_PATTERN, candidate_pkg_id)) > 0 ) {
        if (!is.null(pkg_id) && pkg_id!=candidate_pkg_id) {
            warning(sprintf("pkg_id is explicitly set to %s but output directory %s is for package %s. Proceeding with latter ID", pkg_id, pkg_dir, candidate_pkg_id))
            }
            pkg_id = candidate_pkg_id
    } else if (!is.null(pkg_id)) {
        message(sprintf("Output directory %s does not match pattern %s for a data package. Assuming you want to use directory %s/%s", pkg_dir, PKG_PATTERN, pkg_dir, pkg_id))
        pkg_dir = file.path(pkg_dir, pkg_id)
    } else {
        stop("pkg_id is not set and cannot be determined from the output directory. Aborting")
    }

    ensure_directory(pkg_dir, create_structure=create_structure)
    ctx_doc_dir =
        ensure_directory(file.path(pkg_dir, "ContextDocumentation", "docCollection1"),
                         create_structure=create_structure)
    data_dir =
        ensure_directory(file.path(pkg_dir, "Data"), create_structure=create_structure)
    ensure_directory(file.path(pkg_dir, "Indices"), create_structure=create_structure)
    ensure_file(file.path(pkg_dir, "Indices", "archiveIndex.xml"),
                create_structure=create_structure,
                source_file=archive_index)
    ctx_idx_file =
        ensure_file(file.path(pkg_dir, "Indices", "contextDocumentationIndex.xml"),
                    create_structure=create_structure,
                    source_file=context_doc_index)

    ## Check we have enough context documents; ensure their
    ## directories and at least the first TIFF files exist. Not
    ## existing is just a warning rather than a stop() as we can add
    ## them later   
    ctx_doc_count = count_context_documents(ctx_idx_file)
    for (ctx_doc_id in rep(1:ctx_doc_count)) {
        ensure_directory( file.path(ctx_doc_dir, ctx_doc_id),
                         create_structure=create_structure,
                         warn_only=TRUE)

        ensure_file( file.path(ctx_doc_dir, ctx_doc_id, "1.tif"),
                    create_structure=create_structure,
                    warn_only=TRUE)
    }


    ## Return a nice object describing our package layout
    return (list(pkg_id= pkg_id,
                 pkg_dir= pkg_dir,
                 context_doc_dir= ctx_doc_dir,
                 data_dir= data_dir))
}

#' check_for_missing_labels
#'
#' Checks a data frame description produced by describe_dataset for
#' any variables/columns with missing labels and either writes them to
#' a file (missing_labels_file) or simply returns them.
#'
#' @param df_desc A data frame description from describe_dataset
#' @param missing_labels_file A file or connection where the missing
#'     label(s) will be written. If this parameter is NULL or there
#'     are no missing labels, no file will be written. Once written
#'     the file can be edited and fed back in to relabel_dataset
#' @return a data frame containing the variables with missing
#'     labels. This can be modified to include labels and passed
#'     straight to relabel_dataset
#'
#' @seealso describe_dataset, relabel_dataset
#' @export
check_for_missing_labels <- function( df_desc, missing_labels_file=NULL ) {
    missing_desc = df_desc[df_desc$description %in% c("NULL",""),]
    
    if (!is.null(missing_labels_file)) {
        if (file.exists(missing_labels_file)) {
            file.remove(missing_labels_file)
        }
        
        if (nrow(missing_desc) > 0) {
            warning(sprintf("%d VARIABLES ARE MISSING DESCRIPTIONS. these have been written to %s", nrow(missing_desc), missing_labels_file))
            write.csv(missing_desc,
                      file=missing_labels_file,
                      row.names = FALSE)
        } else {
            message(sprintf("No missing labels, skipping writing missing labels file %s", missing_labels_file))
        }
    } else if (nrow(missing_desc) > 0) {
        warning(sprintf("%d VARIABLES ARE MISSING DESCRIPTIONS.", nrow(missing_desc)))
    }
    
    return (missing_desc)
}

## End verification functions

## Master processing functions (the main user entrypoints into the package

#' process_as_asta_export_script
#'
#' Export the data frame provided "just like" the original ASTA R
#' export script. Assuming the name provided to parameter f_name is
#' "my_dataset" this will output 3, possibly 4 files:
#'
#' my_dataset.csv
#' :a CSV file produced by emit_dataset
#' 
#' my_dataset_VARIABEL.txt
#' :The variable list (with types and KODELISTE references)
#' 
#' my_dataset_VARIABELBESKRIVELSE.txt
#' :The descriptions of each variable
#' 
#' my_dataset_KODELISTE.txt
#' :the code list for all categorical/factor variables in the dataset. Only produced if there are categorical/factor variables and factors_to_codes = TRUE
#'
#' @param f_name A name to use as the basename for all the output files (see above)
#' @param preprocess_dataset A function to preprocess the dataset to
#'     remove or correct problem values. An example implementation is
#'     provided in default_preprocess_dataset which should work for
#'     many datasets. If not, customise it!
#' @param sanitise_numeric Function to sanitise numeric columns. By
#'     default the function default_sanitise_numeric() is used
#' @param sanitise_character Function to sanitise factors and
#'     character columns. By default the function
#'     default_sanitise_character() is used
#' @param option_scipen See emit_dataset for documentation of this parameter
#' @param row_limit If set, the number of rows in parameter
#'     df will be truncated to this value. This facilitates testing
#' @param output_dir A directory in which to write the output
#'     files. Defaults to current working directory
#' @param factors_to_codes If true, any factor variables will be
#'     converted to their numeric equivalent. Used to ensure the
#'     VARIABEL list and KODELISTE contains the correct factor definitions when the
#'     dataset is emitted. If FALSE factors will be emitted as their
#'     text equivalent.
#'
#' @seealso emit_dataset, default_preprocess_dataset
#' @export
process_as_asta_export_script <- function(df,
                                          f_name,
                                          preprocess_dataset=default_preprocess_dataset,
                                          sanitise_numeric=default_sanitise_numeric,
                                          sanitise_character=default_sanitise_character,
                                          option_scipen=DEFAULT_OPTION_SCIPEN,
                                          row_limit=NULL,
                                          output_dir=getwd(),
                                          factors_to_codes=TRUE){

  if (options()$encoding!="UTF-8") {
      warning(sprintf("ASTA expects all files to be UTF-8 encoded. Running with encoding %s is unsupported", options()$encoding))
  }
    
  df = load_and_truncate_dataset(df, row_limit)
    
  ## Write out the dataset itself
  emit_dataset(df,
               file.path(output_dir,
                         paste0(f_name, ".csv")),
               factors_to_codes=factors_to_codes,               
               preprocess_dataset=preprocess_dataset,
               option_scipen=option_scipen) 

  ## Write the variable list
  ## N.B. this will write empty files which may not be what we want
  ## Could pass in filename and detect type inside the function (connection-or-filename)
  emit_variable_list(df,
                     file.path(output_dir, paste0(f_name, "_", "VARIABEL.txt")),
                     factors_to_codes=factors_to_codes)

  ## Write the variable descriptions file
  emit_variable_descriptions(df,
                             file.path(output_dir,
                                       paste0(f_name, "_", "VARIABELBESKRIVELSE.txt")
                                       ),
                             sanitise_numeric=sanitise_numeric,
                             sanitise_character=sanitise_character)

  ## Write the code list
  emit_kodeliste(df,
                 file.path(output_dir,
                           paste0(f_name, "_", "KODELISTE.txt")),
                 factors_to_codes=factors_to_codes)    
  
  message(sprintf("Data processing succesfully completed - emitted dataset of %d rows with %d variables", nrow(df), ncol(df)))
  
}

#' process_info_pkg
#' 
#' Write the overall metadata file and assemble the whole package as
#' described in section 9I of exec order 128. This accounts for
#' multiple table(s) derived from different datasets, their link
#' variables, all context documentation and so on.
#'
#' @param pkg_description The description of the information package
#'     to create, as discussed in the documentation for
#'     verify_pkg_description
#' @param output_dir Where to write the finished package
#' @param preprocess_dataset A function to preprocess the dataset to
#'     remove or correct problem values. An example implementation is
#'     provided in default_preprocess_dataset which should work for
#'     many datasets. If not, customise it!
#' @param sanitise_numeric Function to sanitise numeric columns. By
#'     default the function default_sanitise_numeric() is used
#' @param sanitise_character Function to sanitise factors and
#'     character columns. By default the function
#'     default_sanitise_character() is used
#' @param option_scipen See emit_dataset for documentation of this parameter
#' @param row_limit If set, the number of rows all dataset(s) in
#'     pkg_description will be truncated to. This facilitates testing
#' @param factors_to_codes if TRUE all factors/categorical variables
#'     will be converted to their numeric representation and KODELISTE
#'
#' @seealso verify_pkg_description, emit_dataset
#' @export
process_full_info_pkg <- function (pkg_description,
                                   output_dir=getwd(),
                                   preprocess_dataset=default_preprocess_dataset,
                                   sanitise_numeric=default_sanitise_numeric,
                                   sanitise_character=default_sanitise_character,
                                   option_scipen=DEFAULT_OPTION_SCIPEN,
                                   row_limit=NULL,
                                   factors_to_codes=TRUE) {


    if (options()$encoding!="UTF-8") {
        warning(sprintf("ASTA expects all files to be UTF-8 encoded. Running with encoding %s is unsupported", options()$encoding))
    }

    ## Validate that we have a good package description
    verified_pkg = verify_pkg_description(output_dir, pkg_description)

    ## Clear out old data and setup data directory with new tables
    cleanup_data_dir(verified_pkg$data_dir)

    ##reference-counting variables
    variables_by_table=list()
    references_ok=TRUE
    
    
    table_count=1
    ## For each table, export the data and related metadata file 
    for (table in pkg_description$tables) {        
        table_name = sprintf("table%d", table_count)
        table_dir = file.path(verified_pkg$data_dir, table_name)

        
        if (!dir.exists(table_dir)) {
            dir.create(table_dir)
        }
        table_file = file.path(table_dir, sprintf("%s.csv", table_name))
        table_metadata_file = file.path(table_dir, sprintf("%s.txt", table_name))

        
        table_data = load_and_truncate_dataset(table$table_dataset,
                                               row_limit)

        ## Store our variable list for later
        variables_by_table[table$name] = list(colnames(table_data))
        
        table_data = relabel_dataset( table_data, table$label_file)

        for (key_var in table$key_variable) {
            if (!key_var %in% colnames(table_data)) {
                stop(sprintf("Key variable %s is not present in %s", key_var, table$name))
            }
        }

        ## Write any missing labels 
        df_description = describe_dataset(table_data)
        check_for_missing_labels( df_description,
                                 file.path(table_dir, sprintf("%s_missing_labels.csv", table$name) )) 

        emit_dataset(table_data,
                     table_file,
                     factors_to_codes=factors_to_codes,
                     preprocess_dataset=preprocess_dataset,
                     option_scipen=option_scipen)
        
        emit_metadata_file(table_data,
                           table_info=table,
                           metadata_conn=table_metadata_file,
                           sanitise_numeric=sanitise_numeric,
                           sanitise_character=sanitise_character,
                           factors_to_codes=factors_to_codes)
        
        table_count=table_count+1
        message(sprintf("Wrote %s to directory %s", table$name, table_dir))
    }

    #browser()
    
    ## Check our references
    for (table in pkg_description$tables) {
        for (reference_def in table$reference) {
            ## Check our key variable exists
            our_vars = variables_by_table[[table$name]]
            if (!reference_def$our_key_variable %in% our_vars) {
                references_ok=FALSE

                warning(sprintf("Reference from '%s' to '%s': our_key_variable '%s' is not present in columns of %s",
                                table$name,
                                reference_def$other_dataset,
                                reference_def$our_key_variable,
                                table$name))
                
            }

            ## Check our target reference exists in the other table
            other_vars = variables_by_table[[reference_def$other_dataset]]
            if (!is.null(other_vars)) {
                if (! (reference_def$other_key_variable %in% other_vars)) {
                    references_ok=FALSE
                    warning(sprintf("Reference from '%s' to '%s' other_key_variable '%s' is not present in variables for dataset '%s' ",
                                    table$name,
                                    reference_def$other_dataset,
                                    reference_def$other_key_variable,
                                    reference_def$other_dataset))
                }
            } else {
                ## This error should already have been caught but we check again to be sure
                warning(sprintf("other_dataset '%s' is not a named dataset in the table list", ref_dataset))
                references_ok=FALSE
            }
        }
    }

    if (!references_ok) {
        stop("Invalid relationships between datasets have been detected. See the warnings for details")
    }
    
}
## End master processing functions


