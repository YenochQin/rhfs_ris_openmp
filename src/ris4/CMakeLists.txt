# Determine executable name based on OpenMP availability
if(OpenMP_Fortran_FOUND)
    set(RIS4_EXECUTABLE_NAME "ris4_omp")
else()
    set(RIS4_EXECUTABLE_NAME "ris4")
endif()

add_executable(${RIS4_EXECUTABLE_NAME}
    ris_C.f90
    teilst_C.f90
    angdata_I.f90
    densread_I.f90
    densread_seltz_I.f90
    densnew_I.f90
    densnew_seltz_I.f90
    edensityfit_I.f90
    getmixblock_I.f90
    getsmd_I.f90
    polint_I.f90
    ris_cal_I.f90
    rintdens_I.f90
    rintdensvec_I.f90
    rinti_nms_I.f90
    rint_sms2_I.f90
    rint_sms3_I.f90
    setdbg_I.f90
    setsum_I.f90
    strsum_I.f90
    smsread_I.f90
    smsnew_I.f90
    sigma_1_I.f90
    sigma_2_I.f90
    vinti_I.f90
    engout_I.f90
    ris.f90
    angdata.f90
    densread.f90
    densread_seltz.f90
    densnew.f90
    densnew_seltz.f90
    edensityfit.f90
    getmixblock.f90
    getsmd.f90
    polint.f90
    ris_cal.f90
    rintdens.f90
    rintdensvec.f90
    rinti_nms.f90
    rint_sms2.f90
    rint_sms3.f90
    setdbg.f90
    setsum.f90
    strsum.f90
    smsread.f90
    smsnew.f90
    sigma_1.f90
    sigma_2.f90
    vinti.f90
    engout.f90
)
target_link_libraries_Fortran(${RIS4_EXECUTABLE_NAME} PUBLIC mod 9290 rang90 mcp90 mod 9290)

# Link OpenMP if available
if(OpenMP_Fortran_FOUND)
    target_link_libraries(${RIS4_EXECUTABLE_NAME} PUBLIC OpenMP::OpenMP_Fortran)
endif()

install(TARGETS ${RIS4_EXECUTABLE_NAME} DESTINATION bin/)
