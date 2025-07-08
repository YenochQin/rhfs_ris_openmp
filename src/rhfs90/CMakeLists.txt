add_executable(rhfs
    engouth.f90
    engouth_I.f90
    gethfd.f90
    gethfd_I.f90
    getmixblock.f90
    getmixblock_I.f90
    opt6_C.f90
    matelt.f90
    matelt_I.f90
    rinthf.f90
    rinthf_I.f90
    setdbg.f90
    setdbg_I.f90
    setsum.f90
    setsum_I.f90
    strsum.f90
    strsum_I.f90
    hfsgg.f90
    hfsgg_I.f90
    hfs92.f90
)
target_link_libraries_Fortran(rhfs PUBLIC rang90 mcp90 9290 mod)

# Link OpenMP if available
if(OpenMP_Fortran_FOUND)
    target_link_libraries(rhfs PUBLIC OpenMP::OpenMP_Fortran)
endif()

install(TARGETS rhfs DESTINATION bin/)
