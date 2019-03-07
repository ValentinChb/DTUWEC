MODULE BuildInfo
IMPLICIT NONE
!
type tbuildinfo
    character*255   :: git_tag = ""
    character*255   :: builder = ""      
    character*255   :: computer_name = ""
    character*255   :: build_time = ""   
    character*255   :: build_date = ""       
end type 
!
CONTAINS
!
SUBROUTINE BuildInfo_initialise(b)
type (tbuildinfo)   :: b
!
include 'version.inc'
END SUBROUTINE
!
SUBROUTINE BuildInfo_echo(b,fin)
type (tbuildinfo)   :: b
integer, optional   :: fin
integer             :: fid
!
if (present(fin)) then
    fid = fin
else
    fid = 6     ! use stdout if not specified in input
endif
!
WRITE(fid,*) "***********************************************************************"
WRITE(fid,*) "*  Build information for HAWC2MB.exe (GIT)"
!DEC$ IF DEFINED(_WIN32)
!DEC$ IF DEFINED(_M_X64)
WRITE(fid,*) "*  WINDOWS 64-bit" 
!DEC$ ELSE
WRITE(fid,*) "*  WINDOWS 32-bit" 
!DEC$ ENDIF
!DEC$ ENDIF
!DEC$ IF DEFINED(_DEBUG)
WRITE(fid,*) "*  DEBUG version"
!DEC$ ENDIF
!DEC$ IF DEFINED(CLUSTER)
WRITE(fid,*) "*  CLUSTER version"
!DEC$ ENDIF
WRITE(fid,*) "***********************************************************************"
WRITE(fid,*) "*  GIT-TAG        = ", trim(b%git_tag)
WRITE(fid,*) "*  BUILDER        = ", trim(b%builder)
WRITE(fid,*) "*  COMPUTER_NAME  = ", trim(b%computer_name)
WRITE(fid,*) "*  BUILD_TIME     = ", trim(b%build_time)
WRITE(fid,*) "*  BUILD_DATE     = ", trim(b%build_date)
WRITE(fid,*) "***********************************************************************"
!
END SUBROUTINE
!
END MODULE

