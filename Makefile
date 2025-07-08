# Determine the location of the root of the repository
export GRASP := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

# If present, load the Make.user file which may contain user-defined overrides
# to environment variables.
MAKE_USER_FILE := $(GRASP)/Make.user
ifeq (exists, $(shell [ -e $(MAKE_USER_FILE) ] && echo exists ))
include $(MAKE_USER_FILE)
endif

# Variables affecting the GRASP build. These can be overridden in Make.user or via
# environment variables.
export FC ?= gfortran
export FC_FLAGS ?= -O3 -fallow-argument-mismatch
export FC_FLAGS_SERIAL ?= -O3 -fno-automatic -fallow-argument-mismatch
export FC_LD ?=
export FC_MPI ?= mpifort
export LAPACK_LIBS ?= -lflexiblas
export FC_MPIFLAGS ?= $(FC_FLAGS)
export FC_MPILD ?= $(FC_LD)

# OpenMP configuration
export OPENMP_FLAGS ?= -fopenmp
ifndef NO_OPENMP
    export FC_FLAGS_OPENMP = $(FC_FLAGS) $(OPENMP_FLAGS)
    export FC_LD_OPENMP = $(FC_LD) $(OPENMP_FLAGS)
else
    export FC_FLAGS_OPENMP = $(FC_FLAGS_SERIAL)
    export FC_LD_OPENMP = $(FC_LD)
endif

LIBRARIES = libmod lib9290 libdvd90 libmcp90 librang90 mpi90
APPLICATIONS = HF jjgen90 rangular90 rbiotransform90 rci90 rcsfgenerate90 \
	rcsfzerofirst90 rmcdhf90 rnucleus90 rtransition90_mpi sms90 jj2lsj90  \
	rangular90_mpi rbiotransform90_mpi rci90_mpi rcsfinteract90 rhfs90    \
	rmcdhf90_mpi  rtransition90  rwfnestimate90  rmcdhf90_mem  rmcdhf90_mem_mpi \
	rdensity ris4 rhfszeeman95 rtransition90_phase

# OpenMP parallel programs in src/ directory
OPENMP_APPLICATIONS = rhfs90 rhfszeeman95 ris4

LIBRARY_TARGETS = $(foreach library,$(LIBRARIES),src/lib/$(library))
APPLICATION_TARGETS = $(foreach application,$(APPLICATIONS),src/appl/$(application))
OPENMP_TARGETS = $(foreach application,$(OPENMP_APPLICATIONS),src/$(application))

.PHONY: all lib appl tool openmp parallel serial $(LIBRARY_TARGETS) $(APPLICATION_TARGETS) $(OPENMP_TARGETS)
all: lib appl tool openmp
appl: $(APPLICATION_TARGETS)
lib: $(LIBRARY_TARGETS)
openmp: $(OPENMP_TARGETS)
parallel: openmp
serial: NO_OPENMP=1
serial: openmp

$(LIBRARY_TARGETS): src/lib/%:
	@echo "Building: $@"
	$(MAKE) -C $@
$(APPLICATION_TARGETS): src/appl/%: lib
	@echo "Building: $@"
	$(MAKE) -C $@
$(OPENMP_TARGETS): src/%: lib
	@echo "Building OpenMP program: $@"
	$(MAKE) -C $@
tool: lib
	@echo "Building: src/tool"
	$(MAKE) -C src/tool

LIBRARY_CLEAN_TARGETS = $(foreach library,$(LIBRARIES),clean/lib/$(library))
APPLICATION_CLEAN_TARGETS = $(foreach application,$(APPLICATIONS),clean/appl/$(application))
OPENMP_CLEAN_TARGETS = $(foreach application,$(OPENMP_APPLICATIONS),clean/openmp/$(application))

.PHONY: clean cleanall clean/lib clean/appl clean/tool clean/openmp $(LIBRARY_CLEAN_TARGETS) $(APPLICATION_CLEAN_TARGETS) $(OPENMP_CLEAN_TARGETS)
clean: clean/lib clean/appl clean/tool clean/openmp
cleanall: clean/lib clean/appl clean/tool clean/exec clean/openmp
clean/lib: $(LIBRARY_CLEAN_TARGETS)
$(LIBRARY_CLEAN_TARGETS): clean/lib/%:
	$(MAKE) -C src/lib/$* clean
clean/appl: $(APPLICATION_CLEAN_TARGETS)
clean/openmp: $(OPENMP_CLEAN_TARGETS)
$(OPENMP_CLEAN_TARGETS): clean/openmp/%:
	$(MAKE) -C src/$* clean
clean/exec:
	rm -vf $(GRASP)/bin/*
	rm -vf $(GRASP)/lib/*.a
$(APPLICATION_CLEAN_TARGETS): clean/appl/%:
	$(MAKE) -C src/appl/$* clean
clean/tool:
	$(MAKE) -C src/tool clean

# Configuration and help targets
.PHONY: config help
config:
	@echo "GRASP OpenMP Build Configuration:"
	@echo "================================="
	@echo "FC                 = $(FC)"
	@echo "FC_FLAGS           = $(FC_FLAGS)"
	@echo "FC_FLAGS_SERIAL    = $(FC_FLAGS_SERIAL)"
	@echo "FC_FLAGS_OPENMP    = $(FC_FLAGS_OPENMP)"
	@echo "OPENMP_FLAGS       = $(OPENMP_FLAGS)"
	@echo "LAPACK_LIBS        = $(LAPACK_LIBS)"
	@echo "NO_OPENMP          = $(NO_OPENMP)"
	@echo ""
	@echo "Available programs:"
	@echo "  rhfs90           - RHFS90 (超精细结构计算)"
	@echo "  rhfszeeman95     - RHFSZEEMAN95 (超精细和塞曼效应)"
	@echo "  ris4             - RIS4 (同位素位移计算)"

help:
	@echo "GRASP OpenMP Makefile Help"
	@echo "=========================="
	@echo "Usage: make [target] [variables]"
	@echo ""
	@echo "Main targets:"
	@echo "  all              - Build all libraries and programs"
	@echo "  lib              - Build all libraries"
	@echo "  openmp/parallel  - Build OpenMP parallel programs"
	@echo "  serial           - Build serial versions (NO_OPENMP=1)"
	@echo "  clean            - Clean all build artifacts"
	@echo "  config           - Show build configuration"
	@echo "  help             - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make parallel                    # Build OpenMP versions"
	@echo "  make serial                      # Build serial versions"
	@echo "  make NO_OPENMP=1                 # Disable OpenMP"
	@echo "  make FC=ifort                    # Use Intel compiler"
	@echo "  make clean && make parallel      # Clean and rebuild"
