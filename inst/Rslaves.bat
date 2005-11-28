@echo off

set Rscript=%1%
set R_HOME=%4%

%R_HOME%\bin\Rterm.exe --no-init-file --no-save --slave < %Rscript%
