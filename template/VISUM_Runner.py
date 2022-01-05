#JEMnR to VISUM Procedures
#Adapted from Southern Oregon ABM
#Ben Stabler, ben.stabler@rsginc.com, 04/06/15
#Michael McCarthy, michael.mccarthy@rsginc.com, 04/20/21
#Requires the OMX Import/Export Add-In to be installed

############################################################

#import libraries
import os, shutil, sys, time, csv
sys.path.append("C:/Program Files/PTV Vision/PTV Visum 2020/Exe/Python37Modules/Lib/site-packages")
import win32com.client as com
import VisumPy.helpers
import VisumPy.csvHelpers
#import VisumPy.excel
#import VisumPy.reports
import traceback
import pandas as pd
sys.path.append("scripts")
#from Properties import Properties
import warnings
import tables

import openmatrix as omx
import numpy
warnings.simplefilter('ignore', tables.NaturalNameWarning)

############################################################

### Read Settings.csv
settings_csv = csv.reader(open('inputs/Settings.csv'))
settings = list(settings_csv)

settings_dict = {}

for row in settings:
    key = row[1]
    value = row[2]
    settings_dict[key] = value

### Global settings
inputVersionFile = settings_dict['visumInputFile']
setConnectorTimes = True if settings_dict['setConnectorTimes'] == "TRUE" else False
peakOffPeakTransit = True if settings_dict['runPeakOffPeakTransitAssignment'] == "TRUE" else False

### Define Functions
def startVisum():
  print("start Visum")
  Visum = VisumPy.helpers.CreateVisum(20)
  pathNo = [8,69,2,37,12]
  for i in range(0,len(pathNo)):
    Visum.SetPath(pathNo[i], os.getcwd())
  return(Visum)

def loadVersion(Visum, fileName):
  print("load version file: " + os.getcwd() + "/" + fileName)
  Visum.LoadVersion(os.getcwd() + "/" + fileName)
  pathNo = [8,69,2,37,12]
  for i in range(0,len(pathNo)):
    Visum.SetPath(pathNo[i], os.getcwd())

def saveVersion(Visum, fileName):
  filePath = os.path.join(os.getcwd(), fileName)
  print("save version file: " + filePath)
  Visum.SaveVersion(filePath)

def closeVisum(Visum):
  print("close Visum")
  Visum = 0

def loadProcedure(Visum,parFileName,execute=True):
  print("run procedure file: " + parFileName)
  Visum.Procedures.Open(parFileName)
  if execute:
    Visum.Procedures.Execute()

def writeMatrices(Visum, fileName):
  # write all Visum matrices to OMX using matrix numbers and specified file name
  print("writing matrices to OMX")

  matrixIds = Visum.Net.Matrices.GetMultiAttValues("No") # [0] = Int starting @ 1; [1] = real actual MatNum
  matrixNames = Visum.Net.Matrices.GetMultiAttValues("NAME")
  tazs = VisumPy.helpers.GetMulti(Visum.Net.Zones, "NO")

  #write matrices
  # write attributes for error checking
  print(matrixNames)
  omxFile = omx.open_file(fileName,'w')
  for i in range(len(matrixIds)):
      mat = VisumPy.helpers.GetMatrix(Visum, matrixIds[i][1])
      omxFile[matrixNames[i][1]] = mat
      omxFile[matrixNames[i][1]].attrs.Description = matrixNames[i][1]
  omxFile.close()

  #write lookup
  omxFile = omx.open_file(fileName,'a')
  omxFile.create_mapping('NO', tazs)
  omxFile.close()

def removeAllMatrices(Visum):
  matrixIds = Visum.Net.Matrices.GetMultiAttValues("No")
  matrixNames = Visum.Net.Matrices.GetMultiAttValues("NAME")
  for Id in matrixIds:
    Visum.Net.RemoveMatrix(Visum.Net.Matrices.ItemByKey(Id[1]))

if __name__== "__main__":

  #get command line arguments
  runmode = sys.argv[1].lower()

  print("start " + runmode + " run: " + time.ctime())

  if runmode == 'skims':
    try:
      Visum = startVisum()
      loadVersion(Visum, inputVersionFile)

      # Clear existing matrices
      removeAllMatrices(Visum)

      # Set connector times
      if setConnectorTimes:
          loadProcedure(Visum, "visum/procedures/connector_times.xml")

      periods = ['daily','peak']
      for p in periods:
          removeAllMatrices(Visum)
          loadProcedure(Visum, "visum/procedures/prt_"+p+"_skim.xml")
          loadProcedure(Visum, "visum/procedures/put_skim.xml")
          writeMatrices(Visum, "outputs/matrices/"+p+"_skim.omx")

      # Walk
      removeAllMatrices(Visum)
      loadProcedure(Visum, "visum/procedures/walk_skim.xml")
      writeMatrices(Visum, "outputs/matrices/walk_skim.omx")

      # Bike
      removeAllMatrices(Visum)
      loadProcedure(Visum, "visum/procedures/bike_skim.xml")
      writeMatrices(Visum, "outputs/matrices/bike_skim.omx")

      closeVisum(Visum)
      sys.exit(0)
    except Exception as e:
      print(runmode + " Failed")
      print(e)
      sys.exit(1)

# combined auto + transit assignment
  if runmode == 'assignment':
    try:
      periodName = ['daily','peak'] # peak matrix always has PM vehicle, but may not have peak/off-peak bus
      autoTimePeriod = ['daily','pm1'] # assign daily  auto + off-peak transit if exists

      #versionName = ['daily','peak','offpeak']
      if peakOffPeakTransit:
          transitTimePeriod = ['opad','pkad']
      else:
          transitTimePeriod = ['daily','dummy'] # 'dummy' is a placehold that does not exist, needed on 2nd iteration

      for i in range(len(autoTimePeriod)):
          Visum = startVisum()
          loadVersion(Visum, inputVersionFile)

          # Clear existing matrices
          removeAllMatrices(Visum)

          # Set connector times
          if setConnectorTimes:
              loadProcedure(Visum, "visum/procedures/connector_times.xml")

          # Setup Matrix
          tazIds = VisumPy.helpers.GetMulti(Visum.Net.Zones, "No")
          auto = numpy.zeros((len(tazIds),len(tazIds)))
          bus = numpy.zeros((len(tazIds),len(tazIds)))

          # Read OMX
          trips = omx.open_file("outputs/matrices/demand_"+periodName[i]+".omx",'r')

          # Only fill demand matrix if there are trips; else keep zeros
          if autoTimePeriod[i]+'vehicle' in trips:
              auto = trips[autoTimePeriod[i]+"vehicle"]

          if transitTimePeriod[i]+'bus' in trips:
              bus = trips[transitTimePeriod[i]+"bus"]

          matNums = VisumPy.helpers.GetMulti(Visum.Net.Matrices, "No")

          putMatNum = 1
          autoMatNum = 2

          if autoMatNum not in matNums:
            autoHandle = Visum.Net.AddMatrix(autoMatNum)

          autoHandle = Visum.Net.Matrices.ItemByKey(autoMatNum)
          autoHandle.SetAttValue("DSEGCODE","a")
          autoHandle.SetAttValue("NAME","Auto Demand")
          VisumPy.helpers.SetMatrix(Visum, autoMatNum, auto)

          # Only run assignment when there are trips
          if autoTimePeriod[i]+'vehicle' in trips:
            loadProcedure(Visum, "visum/procedures/set_"+periodName[i]+"_capprt.xml")
            loadProcedure(Visum, "visum/procedures/prt_assignment.xml")

          matNums = VisumPy.helpers.GetMulti(Visum.Net.Matrices, "No")
          if putMatNum not in matNums:
            putHandle = Visum.Net.AddMatrix(putMatNum)

          putHandle = Visum.Net.Matrices.ItemByKey(putMatNum)
          putHandle.SetAttValue("DSEGCODE","PuT")
          putHandle.SetAttValue("NAME","Transit Demand")
          VisumPy.helpers.SetMatrix(Visum, putMatNum, bus)

          if transitTimePeriod[i]+'bus' in trips:
            loadProcedure(Visum, "visum/procedures/put_assignment.xml")

          # Write skims
          loadProcedure(Visum, "visum/procedures/prt_"+periodName[i]+"_skim.xml")
          loadProcedure(Visum, "visum/procedures/put_skim.xml")
          writeMatrices(Visum, "outputs/matrices/"+periodName[i]+"_skim.omx")

          saveVersion(Visum, "outputs/networks/Assignment_"+periodName[i]+".ver")

          #close files
          trips.close()
          closeVisum(Visum)


      sys.exit(0)
    except Exception as e:
      print(runmode + " Failed")
      print(e)
      sys.exit(1)

  print("end model run: " + time.ctime())
