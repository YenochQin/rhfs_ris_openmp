# 编译器和库配置
export FC = gfortran
export FC_FLAGS = -O3 -fallow-argument-mismatch
export FC_FLAGS_SERIAL = -O3 -fno-automatic -fallow-argument-mismatch
export FC_LD =
export FC_MPI = mpifort
export OMPI_FC = ${FC}
export LAPACK_LIBS ?= -lflexiblas
export MPI_INC = -I/usr/include/openmpi-x86_64

GRASP=..
EXE=${GRASP}/rhfs

# 库文件路径配置
LIB_DIR=${GRASP}/libs
LIBS=-L $(LIB_DIR) -lrhfs90
FC_MODULES=-I $(LIB_DIR)

# OpenMP configuration
OPENMP_FLAGS = -fopenmp

# Set default to use OpenMP version, but allow disabling with NO_OPENMP=1
ifndef NO_OPENMP
    # OpenMP 模式下不使用 -fno-automatic 以避免与 -frecursive 冲突
    FC_FLAGS_FINAL = $(FC_FLAGS) $(OPENMP_FLAGS) $(MPI_INC)
    FC_LD_FINAL = $(FC_LD) $(OPENMP_FLAGS)
else
    # 串行模式下使用完整的标志
    FC_FLAGS_FINAL = $(FC_FLAGS_SERIAL) $(MPI_INC)
    FC_LD_FINAL = $(FC_LD)
endif

OBJS= \
	engouth.o \
	engouth_I.o \
	gethfd.o \
	gethfd_I.o \
	getmixblock.o \
	getmixblock_I.o \
	opt6_C.o \
	matelt.o \
	matelt_I.o \
	rinthf.o \
	rinthf_I.o \
	setdbg.o \
	setdbg_I.o \
	setsum.o \
	setsum_I.o \
	strsum.o \
	strsum_I.o \
	hfsgg.o \
	hfsgg_I.o \
	hfs92.o

$(EXE): $(OBJS)
	$(FC) -o $@ $? $(FC_LD_FINAL) $(LIBS) $(LAPACK_LIBS)

%.o: %.f90
	$(FC) -c $(FC_FLAGS_FINAL) $(FC_MODULES) -o $@ $<

# 编译库文件
$(LIB_DIR)/librhfs90.a:
	@echo "编译库文件..."
	$(MAKE) -C $(LIB_DIR) $(if $(NO_OPENMP),NO_OPENMP=1,)

# 确保可执行文件依赖库文件
$(EXE): $(OBJS) $(LIB_DIR)/librhfs90.a

# Build serial version explicitly
serial: NO_OPENMP=1
serial: $(EXE)

# Build parallel version (default)
parallel: $(EXE)

# Show build configuration
config:
	@echo "OpenMP flags: $(OPENMP_FLAGS)"
	@echo "Fortran flags: $(FC_FLAGS_FINAL)"
	@echo "Linker flags: $(FC_LD_FINAL)"
	@echo "库文件目录: $(LIB_DIR)"
	@echo "库文件链接: $(LIBS)"

# 编译库文件
libs:
	$(MAKE) -C $(LIB_DIR) $(if $(NO_OPENMP),NO_OPENMP=1,)

# 清理库文件
clean-libs:
	$(MAKE) -C $(LIB_DIR) clean

# 重新编译库文件
rebuild-libs:
	$(MAKE) -C $(LIB_DIR) rebuild $(if $(NO_OPENMP),NO_OPENMP=1,)

# 验证库文件
verify-libs:
	$(MAKE) -C $(LIB_DIR) verify

clean:
	-@rm $(EXE)
	-@rm *.o *.mod

# 深度清理（包括库文件）
distclean: clean clean-libs

# 完整重建（包括库文件）
rebuild: clean rebuild-libs all

.PHONY: clean serial parallel config libs clean-libs rebuild-libs verify-libs distclean rebuild
