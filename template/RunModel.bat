
::~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:: Run the complete Master_Runner travel model
:: Ben Stabler, ben.stabler@rsginc.com, 081215
:: Revised 05/22/17 ben.stabler@rsginc.com, 09/30/20 bmp, br
::~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:: setup iteration sample rate
SET MAX_ITER=3

:: -------------------------------------------------------------------------------------------------
:: Setup folders, IP addresses, file references, etc.
:: -------------------------------------------------------------------------------------------------

@ECHO OFF

:: get ip address of machine
SET PATH=C:\Windows\System32
FOR /f "delims=[] tokens=2" %%a IN ('ping -4 -n 1 %ComputerName% ^| findstr [') DO SET HOST_IP_ADDRESS=%%a
ECHO HOST_IP_ADDRESS: %HOST_IP_ADDRESS%

:: setup dependencies, which are one folder up so they can be shared across scenarios
SET JAVA_PATH=%~dp0..\dependencies\jdk1.8.0_111\bin\java.exe
ECHO JAVA_PATH: %JAVA_PATH%

SET PYTHON=%~dp0..\dependencies\Python37\python.exe
ECHO PYTHON: %PYTHON%

SET R_SCRIPT=%~dp0..\dependencies\R-3.4.1\bin\Rscript
ECHO R_SCRIPT: %R_SCRIPT%

SET R_LIBRARY=%~dp0..\dependencies\R-3.4.1\library
ECHO R_LIBRARY: %R_SCRIPT%

:: setup folders
SET PROJECT_DRIVE=%~d0
ECHO PROJECT_DRIVE: %PROJECT_DRIVE%

SET PROJECT_DIRECTORY=%~dp0
ECHO PROJECT_DIRECTORY: %PROJECT_DIRECTORY%

SET PROJECT_DIRECTORY_FORWARD=%PROJECT_DIRECTORY:\=/%
ECHO PROJECT_DIRECTORY_FORWARD: %PROJECT_DIRECTORY_FORWARD%

:: -------------------------------------------------------------------------------------------------
:: Initial Skims
:: -------------------------------------------------------------------------------------------------

rem # All skims
%PYTHON% VISUM_Runner.py skims
IF %ERRORLEVEL% NEQ 0 GOTO MODEL_ERROR

:: -------------------------------------------------------------------------------------------------
:: Loop
:: -------------------------------------------------------------------------------------------------

SET /A ITERATION=0
:ITER_START
SET /A ITERATION+=1
ECHO MODEL ITERATION %ITERATION%

rem # run JEMNR main module
%R_SCRIPT% modelRunner.R jemnr %ITERATION%
IF %ERRORLEVEL% NEQ 0 GOTO MODEL_ERROR

:: -------------------------------------------------------------------------------------------------
:: Build, load, and assign trip matrices into VISUM
:: -------------------------------------------------------------------------------------------------

rem # write JEMNR demand to OMX
%R_SCRIPT% modelRunner.R buildmat %ITERATION%
IF %ERRORLEVEL% NEQ 0 GOTO MODEL_ERROR

rem # assign JEMNR demand
%PYTHON% VISUM_Runner.py assignment
IF %ERRORLEVEL% NEQ 0 GOTO MODEL_ERROR

rem # check convergence
IF %ITERATION% LSS %MAX_ITER% (
  %R_SCRIPT% modelRunner.R convergence %ITERATION%
)

IF %ERRORLEVEL% EQU 10 GOTO REPORTING rem # converged
IF %ERRORLEVEL% NEQ 0 GOTO MODEL_ERROR rem # error

:: -------------------------------------------------------------------------------------------------
:: Loop again if needed
:: -------------------------------------------------------------------------------------------------

IF %ITERATION% LSS %MAX_ITER% GOTO ITER_START

:: -------------------------------------------------------------------------------------------------
:: Reporting
:: -------------------------------------------------------------------------------------------------
:REPORTING
rem # run JEMNR demand model reporting
%R_SCRIPT% modelRunner.R report %ITERATION%
IF %ERRORLEVEL% NEQ 0 GOTO MODEL_ERROR
:: -------------------------------------------------------------------------------------------------
:: All done
:: -------------------------------------------------------------------------------------------------

ECHO MODEL RUN COMPLETE

PAUSE
GOTO END

:MODEL_ERROR
ECHO Model Failed
PAUSE

:END
