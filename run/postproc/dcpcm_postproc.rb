#-------------------------------------------------------------
# Copyright (c) 2015-2015 Kawai Yuta. All rights reserved.
#-------------------------------------------------------------

# Configuration **********************************
#require File.expand_path(File.dirname(__FILE__)+"/lib/make_figure.rb")

TOPDIR = "/home/ykawai/workspace/DCPCM_data/run_S1366/run3"

CycleBegin_Global = 1
CycleEnd_Global   = 8

CycleBegin_Transit= 1
CycleEnd_Transit = 8

CycleBegin_Mean= 6
CycleEnd_Mean = 8

ATMDATADIR = TOPDIR + "/com_dir/atm/data_longInteg"
OCNDATADIR = TOPDIR + "/com_dir/ocn/data_longInteg"

DCPCM_UTILDIR = File.expand_path(File.dirname(__FILE__))

RUBY = "ruby"

###############################################

#-----------------------------------------------

#ATMNC_DIAG_FLAG = true
ATMNC_DIAG_FLAG = false 
DIAGATM_PROG =DCPCM_UTILDIR + "/atm_analysis/Diagnose.rb"

#-------------------------------------------------

#ATMNC_MERGE_FLAG = true
ATMNC_MERGE_FLAG = false
MERGE_NCF_PROG = "/home/ykawai/workspace/dcpam_tool/bin/merge_ncf"
MERGE_NML_ORI = "/home/ykawai/workspace/DCPCM_data/merge.nml"

#-----------------------------------------------

#ATM_GlobalMeanQuants_FLAG = true
ATM_GlobalMeanQuants_FLAG = false
ATM_GlobalMeanQuants_PROG = DCPCM_UTILDIR + "/atm_analysis/GlobalMeanQuants.rb"

#ATM_EngyFlxLat_FLAG = true
ATM_EngyFlxLat_FLAG = false
EngyFlxLat_PROG = DCPCM_UTILDIR + "/atm_analysis/EngyFlxLat.rb"

#OCN_GlobalMeanQuants_FLAG = true
OCN_GlobalMeanQuants_FLAG = true
OCN_GlobalMeanQuants_PROG = DCPCM_UTILDIR + "/ocn_analysis/GlobalMeanQuants.rb"

#-----------------------------------------------

#OCNNC_DIAGVAR_FLAG = true
OCNNC_DIAGVAR_FLAG = false
DIAGVAROCN_PROG = "/home/ykawai/workspace/Dennou-OGCM/model/sogcm/tool/diagvar_axisym"
DIAGVAR_NML_ORI = "/home/ykawai/workspace/DCPCM_data/defaultConfigDiagVar.nml"

#-----------------------------------------------

MERGE_COUPLERUNDATA_FLAG = false
MERGE_COUPLERUNDATA_DISTDIRNAME = "transition"
MERGE_COPLEDRUNDATA_PROG = DCPCM_UTILDIR + "/common/merge_couplerun_data.rb"
MERGE_COUPLERUNDATA_LIST_ATM = \
{ "GlobalMeanQuants.nc" => "SurfTemp,OLR,OSR,mOSR" }

#-----------------------------------------------

MEAN_COUPLERUNDATA_FLAG = false
MEAN_COPLEDRUNDATA_PROG = DCPCM_UTILDIR + "/common/mean_couplerun_data.rb"
MEAN_COUPLERUNDATA_LIST_ATM = \
{ "U.nc" => "U", "V.nc"=>"V", "Temp.nc"=>"Temp", "QH2OVap.nc"=>"QH2OVap", "Diagnose.nc"=>"MSF", \
  "SurfTempOcn.nc"=>"SurfTempOcn", "PRCP.nc"=>"PRCP", \
}
#  "EnFlxLat.nc"=>"totStatEnFlxLat,dryStatEnFlxLat,moistStatEnFlx" }

MEAN_COUPLERUNDATA_LIST_OCN = \
{ "U.nc" => "U", "V.nc"=>"V", "PTemp.nc"=>"PTemp", "Salt.nc"=>"Salt", "MassStreamFunc.nc"=>"MassStreamFunc", \
  "SIceSurfTemp.nc"=>"SIceSurfTemp", "IceThick.nc"=>"IceThick", "SnowThick.nc"=>"SnowThick"}

MEAN_COUPLERUNDATA_DISTDIRNAME = "mean_state"


################################################

require 'fileutils'

################################################

def mkdir(newdirPath)
  if !File.exist?(newdirPath) then
    puts "Create directory: #{newdirPath}"
    Dir.mkdir(newdirPath)
  end
end

def merge_ATMNC(dirPath)
  puts "Perform merge_ncf.. (Dir=#{dirPath})"
  
  merge_nml = dirPath+"/merge.nml"
#  if ! File.exist?(merge_nml) then
    FileUtils.cp(MERGE_NML_ORI, merge_nml)
#  end
  Dir.chdir(dirPath){
    `#{MERGE_NCF_PROG}`
  }
end

def diagnose_ATMNC(dirPath)
  puts "Perform diagnose .. (Dir=#{dirPath})"
#  diagvar_nml = dirPath+"/diagVarConfig.nml"
#  FileUtils.cp(DIAGVAR_NML_ORI, diagvar_nml)
  Dir.chdir(dirPath){
    `#{RUBY} #{DIAGATM_PROG}`
  }
end

def diagVar_OCNNC(dirPath)
  puts "Perform diagvar .. (Dir=#{dirPath})"
  diagvar_nml = dirPath+"/diagVarConfig.nml"
  FileUtils.cp(DIAGVAR_NML_ORI, diagvar_nml)
  Dir.chdir(dirPath){
    `#{DIAGVAROCN_PROG} --N=#{diagvar_nml}`
  }
end

def run_AtmGlobalMeanQuants(dirPath)
  puts "Perform GlobalMeanQuants.rb .. (Dir=#{dirPath})"
  Dir.chdir(dirPath){
      `#{RUBY} #{ATM_GlobalMeanQuants_PROG}`
  }
end

def run_OcnGlobalMeanQuants(dirPath)
  puts "Perform GlobalMeanQuants.rb .. (Dir=#{dirPath})"
  Dir.chdir(dirPath){
      `#{RUBY} #{OCN_GlobalMeanQuants_PROG}`
  }
end

def run_EngyFlxLat(dirPath)
  puts "Perform EngyFlxLat.rb .. (Dir=#{dirPath})"
  Dir.chdir(dirPath){
    `#{RUBY} #{EngyFlxLat_PROG}`
  }
end

def merge_CoupledRunData(dirPath, cycleBegin, cycleEnd, nc, varList, distDirPath)
  puts "Perform merge_CoupledRunData .. (Dir=#{dirPath})"
  puts "Cycle=#{cycleBegin}..#{cycleEnd}"
  puts "VarList=#{varList}"
  Dir.chdir(dirPath){
    varList.split(",").each{|var|
      `#{RUBY} #{MERGE_COPLEDRUNDATA_PROG} #{cycleBegin} #{cycleEnd} #{nc} #{var} . #{distDirPath}`
    }
  }  
end

def mean_CoupledRunData(dirPath, cycleBegin, cycleEnd, nc, varList, meanAxisNames, distDirPath)
  puts "Perform mean_CoupledRunData .. (Dir=#{dirPath})"
  puts "Cycle=#{cycleBegin}..#{cycleEnd}"
  puts "VarList=#{varList}"
  puts "Axis=#{meanAxisNames}"
  
  Dir.chdir(dirPath){
    varList.split(",").each{|var|
      `#{RUBY} #{MEAN_COPLEDRUNDATA_PROG} #{cycleBegin} #{cycleEnd} #{nc} #{var} #{meanAxisNames} . #{distDirPath}`
    }
  }  
end

for n in CycleBegin_Global..CycleEnd_Global
  atmdir_couplerun = ATMDATADIR + "/cycle#{n}-couple"
  ocndir_couplerun = OCNDATADIR + "/cycle#{n}-couple"
  ocndir_standlonerun = OCNDATADIR + "/cycle#{n}-standalone"
  
  merge_ATMNC(atmdir_couplerun) if ATMNC_MERGE_FLAG
  diagnose_ATMNC(atmdir_couplerun) if ATMNC_DIAG_FLAG
  run_AtmGlobalMeanQuants(atmdir_couplerun) if ATM_GlobalMeanQuants_FLAG
  run_EngyFlxLat(atmdir_couplerun) if ATM_EngyFlxLat_FLAG

  diagVar_OCNNC(ocndir_couplerun) if OCNNC_DIAGVAR_FLAG
  run_OcnGlobalMeanQuants(ocndir_couplerun) if OCN_GlobalMeanQuants_FLAG    
end

MERGE_COUPLERUNDATA_LIST_ATM.each{|nc,varList|
  distDirPath = ATMDATADIR+"/#{MERGE_COUPLERUNDATA_DISTDIRNAME}"
  mkdir(distDirPath)
  merge_CoupledRunData(ATMDATADIR, CycleBegin_Transit, CycleEnd_Transit, nc, varList, distDirPath)
}

MEAN_COUPLERUNDATA_LIST_ATM.each{|nc,varList|
  distDirPath = ATMDATADIR+"/#{MEAN_COUPLERUNDATA_DISTDIRNAME}"
  mkdir(distDirPath)
  mean_CoupledRunData(ATMDATADIR, CycleBegin_Mean, CycleEnd_Mean, nc, varList, "time,lon", distDirPath)
}

MEAN_COUPLERUNDATA_LIST_OCN.each{|nc,varList|
  distDirPath = OCNDATADIR+"/#{MEAN_COUPLERUNDATA_DISTDIRNAME}"
  mkdir(distDirPath)
  mean_CoupledRunData(OCNDATADIR, CycleBegin_Mean, CycleEnd_Mean, nc, varList, "time", distDirPath)
}



  
