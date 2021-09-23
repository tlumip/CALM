SET PROJECT_DIRECTORY=%1%
SET SAMPLERATE=%2%
SET ITERATION=%3%

cd %PROJECT_DIRECTORY%

:: First save the JAVA_PATH environment variable so it s value can be restored at the end.
:: set OLDJAVAPATH=%JAVA_PATH%
:: set OLDPATH=%PATH%

:: Removed in VISUM conversion -mmm 08/2021
:: Set the directory of the jre version desired for this model run
:: set JAVA_PATH=%JAVA_64_PATH%
:: set RUNTIME=%JAVA_PATH%
rem Keep java path from RunModel.bat
ECHO JAVA_PATH: %JAVA_PATH%

:: Name the project directory.  This directory will hava data and runtime subdirectories
set CONFIG=%PROJECT_DIRECTORY%/ctlfiles

:: Set the name of the properties file the application uses by giving just the base part of the name (with ".xxx" extension)
set PROPERTIES_NAME=tpau_tbm

set LIB_JAR_PATH=%PROJECT_DIRECTORY%/tpau.jar

:: Define the CLASSPATH environment variable for the classpath needed in this model run.
set OLDCLASSPATH=%CLASSPATH%
set CLASSPATH=%CONFIG%;%RUNTIME%;%LIB_JAR_PATH%;

:: Change the PATH environment variable so that JAVA_HOME is listed first in the PATH.
:: Doing this ensures that the JAVA_HOME path we defined above is the on that gets used in case other java paths are in PATH.
set PATH=%JAVA_PATH%\bin;C:\projects\util;%OLDPATH%


:: run ping to add a pause so that hhMgr and mtxMgr have time to fully start
ping -n 10 %MAIN% > nul

:: **************************************************************************************************************************************************

del .\outputs\* /F /Q > con

:: Removed in VISUM conversion -mmm 08/2021
:: rem Convert EMME output to ZMX and copy to emmemat folder
:: "%EMMEPATH%"\%EMME_Python%\python.exe EMXtoZMX.py ..\emme\%EMME_FILE% %EMME_SCENARIO% %EMME_MATS%
:: copy /y "%PROJECT_DIRECTORY%"\..\emme\Database\emmemat\*.zmx "%PROJECT_DIRECTORY%"\emmemat


%JAVA_PATH% -server -Xms10000m -Xmx10000m -cp %CLASSPATH% -Dlog4j.configuration=log4j.xml -Dproject.folder=%PROJECT_DIRECTORY% com.pb.tpau.universitymodel.ModelRunner %PROPERTIES_NAME% -iteration %ITERATION%  -sampleRate %SAMPLERATE% 2>&1
:: The following code was revmoved from the Java call above as the progam Tee.exe is no longer available for Win10  mm 021420
::| %GNUWIN32_PATH%\tee.exe %PROJECT_DIRECTORY%\rpt\universityModelRunnerScreen_%ITERATION%.log

mkdir .\outputs\iter0
copy /y .\outputs\* .\outputs\iter0
copy /y .\rpt\universityModel.log .\rpt\universityModel_iter0.log

%JAVA_PATH% -server -Xms10000m -Xmx10000m -cp %CLASSPATH% -Dlog4j.configuration=log4j.xml -Dproject.folder=%PROJECT_DIRECTORY% com.pb.tpau.universitymodel.ModelRunner %PROPERTIES_NAME% -iteration %ITERATION%  -sampleRate %SAMPLERATE% 2>&1
:: The following code was revmoved from the Java call above as the progam Tee.exe is no longer available for Win10  mm 021420
:: | %GNUWIN32_PATH%\tee.exe %PROJECT_DIRECTORY%\rpt\universityModelRunnerScreen_%ITERATION%.log

mkdir .\outputs\iter1
copy /y .\outputs\* .\outputs\iter1
copy /y .\rpt\universityModel.log .\rpt\universityModel_iter1.log

:done
:: restore saved environment variable values, and change back to original current directory
:: set JAVA_PATH=%OLDJAVAPATH%
:: set PATH=%OLDPATH%
:: set CLASSPATH=%OLDCLASSPATH%
