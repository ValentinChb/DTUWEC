@echo off
pushd %~dp0

for /f %%i in ('git describe --tags --dirty --always') do set GIT_ID=%%i
for /f %%i in ('git tag') do set GIT_VERSION=%%i 


set psign=%%
echo b%psign%git_tag		= "%GIT_ID%"			 > version.inc
echo b%psign%builder		= "%USERNAME%"		>> version.inc
echo b%psign%computer_name= "%COMPUTERNAME%"	>> version.inc
echo b%psign%build_time	= "%TIME%"			>> version.inc
echo b%psign%build_date	= "%DATE%"			>> version.inc
REM

set S=#define GIT_ID "%GIT_ID%"
echo %S% > git_version.h

set GIT_VERSION=%GIT_VERSION: =%

echo %GIT_ID%
echo %GIT_VERSION%

IF not "%GIT_ID%"=="%GIT_VERSION%" (
    set GIT_VERSION=%GIT_VERSION%,-1
)


set S=#define GIT_VERSION %GIT_VERSION:.=,%
echo %S% >> git_version.h

type version.inc

echo ----------------------

type git_version.h
popd


