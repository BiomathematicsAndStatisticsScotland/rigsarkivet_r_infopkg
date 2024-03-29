# Utility makefile for doing things with R packages

#CHANGE THE NAME HERE TO THAT OF YOUR PACKAGE DIRECTORY
PACKAGE=RigsArkivetRInfoPkg


VERSION:=$(shell sed -e 's/Version: \([[:digit:]]*\)/\1/ p' -e 'd' $(PACKAGE)/DESCRIPTION)
SOURCE_TARBALL=$(PACKAGE)_$(VERSION).tar.gz
VIGNETTEDIR=$(PACKAGE)/vignettes/
VIGNETTEPDFS=$(shell for file in $(VIGNETTEDIR)/*.Rnw; do echo "$(PACKAGE)/inst/doc/"`basename $$file .Rnw`.pdf; done)

SWEAVE_OPTIONS=--options=eval=FALSE

TESTINSTALLTREE=testinstall
MOST_RECENT=$(shell find ${PACKAGE} -type f ! -iname 'DESCRIPTION' -a ! -iname 'NAMESPACE' -a ! -iwholename '*\.svn**' -a ! -iname '*.Rd' -printf "%T@\0%p\0" | gawk '{ if ($$0>max) { max=$$0; getline mostrecent } else  getline  } END{print mostrecent}' RS='\0')

EXTRACT_DIR=extractpkg

OUT_TARBALL="ShouldNotExist"

PLATFORM:=$(strip $(shell uname)) #For compatibility with Windows without MinGW use gcc -dumpmachine and change conditionals accordingly. Could also use the RPlatform below
RPLATFORM=$(shell Rscript -e 'cat(R.Version()$$platform)')

#If true, we will attempt to convert our package to other platforms, automatically
#Only works for Pure-R packages, assumes that if $(PACKAGE)/src exists then package contains native code
CONVERT_PKG=$(shell if [ -d $(PACKAGE)/src ]; then echo "native"; else echo "pureR"; fi)

ifeq ($(CONVERT_PKG), pureR)
MAC_PKG_NAME=$(PACKAGE)_$(VERSION).tgz
WIN_PKG_NAME=$(PACKAGE)_$(VERSION).zip
endif

ifeq ($(PLATFORM), Linux )
OUT_TARBALL=$(PACKAGE)_$(VERSION)_R_$(RPLATFORM).tar.gz
OTHER_PLATFORMS=$(MAC_PKG_NAME) $(WIN_PKG_NAME)
PLATFORM_KNOWN:=1
endif 

ifeq ($(PLATFORM), MINGW32_NT-6.2 )
OUT_TARBALL=$(PACKAGE)_$(VERSION).zip
OTHER_PLATFORMS=$(MAC_PKG_NAME)
PLATFORM_KNOWN:=1
endif

ifndef PLATFORM_KNOWN
$(error Platform $(PLATFORM) is unknown)
endif 

eq = $(and $(findstring $(1),$(2)),$(findstring $(2),$(1)))

#Extract a package file in a subdir
#Arguments: pkg_file
define unpackpkg
mkdir -p $(EXTRACT_DIR)

$(if $(call eq,$(suffix $(1)),.zip), 
       unzip $(1) -d "$(EXTRACT_DIR)", 
       tar zxf $(1) -C "$(EXTRACT_DIR)")

endef 

recent:
	echo $(MOST_RECENT)
	echo $(OTHER_PLATFORMS)

.default: build

testme.Rbatch:
	printf "library(devtools)\\ntest(\"${PACKAGE}\")\\n" > testme.Rbatch

roxygenise.Rbatch:
	printf "library(roxygen2)\\nroxygenise(\"${PACKAGE}\")\\n" > roxygenise.Rbatch

doc.stamp: $(MOST_RECENT) roxygenise.Rbatch
	R CMD BATCH roxygenise.Rbatch
	touch doc.stamp

doc: doc.stamp

#Used for quickly checking the vignettes will build (without R being evaluated)
vignette:
	cd $(VIGNETTEDIR); \
	for file in $(VIGNETTEDIR)/*.Rnw; do \
		R CMD Sweave $(SWEAVE_OPTIONS) `basename $$file`; \
		pdflatex `basename $$file .Rnw`.tex; \
	done

getpkgname:
	@echo $(OUT_TARBALL)

getwinpkgname:
	@echo $(WIN_PKG_NAME)

getversion:
	@echo $(VERSION)

getprogram:
	@echo $(PROGRAM)

#Extract the vignette files from the tarball 
fullvignette: build
	tar zxf $(SOURCE_TARBALL) $(VIGNETTEPDFS)

test: testme.Rbatch
	R CMD BATCH testme.Rbatch
	cat testme.Rbatch.Rout

check: doc.stamp $(SOURCE_TARBALL)
	R CMD check $(SOURCE_TARBALL)

cran_check: doc.stamp $(SOURCE_TARBALL)
	R CMD check --as-cran $(SOURCE_TARBALL)

#Clean out the Vignette PDFs otherwise the build process collects them up
$(SOURCE_TARBALL): $(MOST_RECENT) doc.stamp
	rm -f $(VIGNETTEPDFS)
	R CMD build $(PACKAGE)

build: $(SOURCE_TARBALL)

install:
	R CMD INSTALL ${PACKAGE}


#Build natively for this platform
$(OUT_TARBALL): $(SOURCE_TARBALL)
	mkdir -p $(TESTINSTALLTREE)
	R CMD INSTALL -l ${TESTINSTALLTREE} --build $(SOURCE_TARBALL)
	@@echo ""
	@@echo ""
	@@if [ -f $(OUT_TARBALL) ]; then \
	    echo "*** Installable package name is $(OUT_TARBALL)"; \
	    echo "*** From within R run 'install.packages(\"$(OUT_TARBALL)\",repos=NULL)'"; \
            echo "*** Or from the shell, run 'R CMD INSTALL $(OUT_TARBALL)'"; \
        else \
           echo "*** Hmm, the expected package output file $(OUT_TARBALL) does not exist."; \
           echo "*** It's likely you are building on an unusual platform. "; \
           echo "*** Above, look for a line beginning \"packaged installation of ${PACKAGE}\" to identify the output file name."; \
           echo "*** Then install this binary package from within an R session using 'install.packages(\"PKG_FILE_NAME\",repos=NULL)'"; \
        fi 


#Convert the native build to other platforms. N.B this only works for pure-R packages
$(MAC_PKG_NAME): $(OUT_TARBALL)
	$(call unpackpkg,$<)
	@echo "Creating Mac package $(MAC_PKG_NAME)"
	cd $(EXTRACT_DIR) && tar zcf ../$@ $(PACKAGE)/*

ifeq ($(PLATFORM), Linux )
$(WIN_PKG_NAME): $(OUT_TARBALL)
	$(call unpackpkg,$<)
	@echo "Creating windows package $(WIN_PKG_NAME)"
	cd $(EXTRACT_DIR) && zip -qr ../$@ $(PACKAGE)/*
endif

dist: $(OTHER_PLATFORMS) $(OUT_TARBALL) 

testinstall: 
	mkdir -p $(TESTINSTALLTREE)	
	R CMD INSTALL -l ${TESTINSTALLTREE} ${PACKAGE}

updatebuildstamp:
	touch -r $(BUILD_STAMP)

clean:
	rm -f roxygenise.Rbatch testme.Rbatch
	rm -rf ${PACKAGE}/doc
	rm -f ${PACKAGE}/man/*.Rd
	rm -f ${PACKAGE}/inst/doc/*.pdf
	rm -f $(VIGNETTEDIR)/*.aux
	rm -f $(VIGNETTEDIR)/*.tex
	rm -f $(VIGNETTEDIR)/*.log
	#rm -f $(VIGNETTEDIR)/*.pdf
	rm -rf ${PACKAGE}.Rcheck
	rm -f *.Rout
	rm -f *.Rbatch.Rout
	rm -f doc.stamp

distclean: clean
	rm -rf ${TESTINSTALLTREE}
	rm -rf ${EXTRACT_DIR}
	rm -f ${PACKAGE}_*.tar.gz
	rm -f ${PACKAGE}_*.tgz
	rm -f ${PACKAGE}_*.zip
	rm -f .RData
