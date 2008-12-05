@echo off

set Rscript=%1%
set R_HOME=%2%
set WDrive=%3%
set WDir=%4%
set WorkTmp=%5%
set TmpDir=%6%
set Master=%7%
set MapDrive=%8%
set RemotePath=%9%

if %Master%== %COMPUTERNAME% (
cd /D %WDir%
%R_HOME%\bin\Rterm.exe --no-init-file --no-save --slave < %Rscript%
goto:eof 
)

set R_TMP="%USERPROFILE%\Local Settings\Temp"

if %MapDrive%==FALSE goto last 
set FirstOne=FALSE
if exist %WDrive%:\NUL goto next
set FirstOne=TRUE
net use %WDrive%: %RemotePath%
if not errorlevel 0 set FirstOne=FALSE

:next
if %WorkTmp%==TRUE set R_TMP=%TmpDir%

cd /D %WDir%
%R_HOME%\bin\Rterm.exe --no-init-file --no-save --slave < %Rscript% TMP=%R_TMP%
cd /D %TMP%

if %FirstOne%==FALSE goto end 
rem %R_HOME%\library\Rmpi\sleep.exe 500
net use /delete %WDrive%:
goto:eof

:last
cd /D %R_TMP%
%R_HOME%\bin\Rterm.exe --no-init-file --no-save --slave < %Rscript% TMP=%R_TMP%

:end
