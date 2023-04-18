
## Label a supplied variable as factor, using the supplied
## labels. Will result in the original numeric codes being lost but
## the structure of the data is preserved. Could supplement with a
## labelling function that labels in the "haven" style, preserving the
## label information (and allowing us to test the code related to
## SAS/SPSS/Stata originated data
label_factor <- function( x, factor_labels ) {
    x_factor = as.factor(x)
    x_levels = levels(x_factor)
    levels(x_factor) = as.character(factor_labels[x_levels])

    return (x_factor)
}

## Label information from the FD.18005 table1.txt file
gender_levels=c('1'='Mand', '2'='Kvinde', '9'='uoplyst')

grade_levels=c('1'='Ja', '2'='Nej', '9'='uoplyst')

hobby_levels=c('1'='Sport',
               '2'='Film',
               '3'='Rejse',
               '4'='Kunst',
               '5'='Bøger',
               '6'='Håndarbejde',
               '7'='Puslespil',
               '8'='Andet',
               '9'='uoplyst',
               '10'='irrelevant')

education_levels=c('1'='Gymnasial uddannelse (STX)',
                   '2'='Gymnasial uddannelse ( HF)',
                   '3'='Gymnasial uddannelse ( HHX)',
                   '4'='Gymnasial uddannelse (HTX)',
                   '5'='Andet',
                   '6'='Ingen',
                   '9'='uoplyst')

## Read in the FD.18005 data CSV file

fd_18005_r=read.csv("RigsArkivetRInfoPkg/inst/extdata/FD_18005_table1.csv",
                    stringsAsFactors=FALSE,
                    sep=";")

## Add in the levels for all the categorical variables. N.B. R doesn't
## really "do" labels in the same way as SAS/STATA/SPSS so though the
## data will be preserved the numeric values in the original FD.18005
## dataset will not.
##
## This is fine for our test purposes
fd_18005_r$gender = label_factor(fd_18005_r$gender, gender_levels)
fd_18005_r$grade = label_factor(fd_18005_r$grade, grade_levels)
fd_18005_r$education = label_factor(fd_18005_r$education, education_levels)
fd_18005_r$hobby = label_factor(fd_18005_r$hobby, hobby_levels)

## Save as RData ready for inclusion in the package 
save(fd_18005_r, file="RigsArkivetRInfoPkg/data/FD_18005.Rdata")
