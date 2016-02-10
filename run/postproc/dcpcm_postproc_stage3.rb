#-------------------------------------------------------------
# Copyright (c) 2015-2015 Kawai Yuta. All rights reserved.
#-------------------------------------------------------------
# * Run merge_ncf for DCPAM output data
#   and diagvar for Dennou-OGCM output data. 
#
# Configuration **********************************

require 'optparse'

TOPDIR = `pwd`

OPTS = ARGV.getopts('', 'topdir:', 'atmdir:', 'ocndir:', 'cyclesTrans:', 'cyclesMean:').inject({}) {
  |hash,(k,v)| hash[k.to_sym] = v; hash
}

p OPTS

cycles = OPTS[:cyclesTrans].split(":")
CycleBegin_Transit = cycles[0].to_i
CycleEnd_Transit = cycles[1].to_i

cycles = OPTS[:cyclesMean].split(":")
CycleBegin_Mean = cycles[0].to_i
CycleEnd_Mean = cycles[1].to_i

ATMDATADIR = OPTS[:topdir] + OPTS[:atmdir]
OCNDATADIR = OPTS[:topdir] + OPTS[:ocndir]


DCPCM_POSTPROC_DIR = File.expand_path(File.dirname(__FILE__))
DCPCM_POSTPROC_TOOLSDIR = File.expand_path(File.dirname(__FILE__)) + "/tools"

RUBY = "ruby"

###############################################

#-----------------------------------------------

################################################

require 'fileutils'

MERGE_COUPLERUNDATA_DISTDIRNAME = "transition"
MERGE_COPLEDRUNDATA_PROG = DCPCM_POSTPROC_DIR + "/common/merge_couplerun_data.rb"
MERGE_COUPLERUNDATA_LIST_ATM = \
{ "GlobalMeanQuants.nc" => "SurfTemp,OLR,mOSR,RadTOA" }

#-----------------------------------------------

MEAN_COUPLERUNDATA_DISTDIRNAME = "mean_state"
MEAN_COPLEDRUNDATA_PROG = DCPCM_POSTPROC_DIR + "/common/mean_couplerun_data.rb"
MEAN_XT_COUPLERUNDATA_LIST_ATM = {
  "U.nc" => "U", "V.nc"=>"V", "Temp.nc"=>"Temp", "QH2OVap.nc"=>"QH2OVap", "Diagnose.nc"=>"MSF", \
  "SurfTempOcn.nc"=>"SurfTempOcn", "PRCP.nc"=>"PRCP" }
MEAN_T_COUPLERUNDATA_LIST_ATM = {  "EngyFlx.nc"=>"totStatEnFlxLat,dryStatEnFlxLat,moistStatEnFlxLat" }


MEAN_XT_COUPLERUNDATA_LIST_OCN = 
{ "U.nc" => "U", "V.nc"=>"V", "PTemp.nc"=>"PTemp", "Salt.nc"=>"Salt", \
  "SIceSurfTemp.nc"=>"SIceSurfTemp", "IceThick.nc"=>"IceThick", "SnowThick.nc"=>"SnowThick" }
MEAN_T_COUPLERUNDATA_LIST_OCN = { "MassStreamFunc.nc"=>"MassStreamFunc" }


################################################

def mkdir(newdirPath)
  if !File.exist?(newdirPath) then
    puts "Create directory: #{newdirPath}"
    Dir.mkdir(newdirPath)
  end
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

#
distDirPath = ATMDATADIR+"/#{MERGE_COUPLERUNDATA_DISTDIRNAME}"
MERGE_COUPLERUNDATA_LIST_ATM.each{|nc,varList|
  mkdir(distDirPath)
  merge_CoupledRunData(ATMDATADIR, CycleBegin_Transit, CycleEnd_Transit, nc, varList, distDirPath)
}

#
distDirPath = ATMDATADIR+"/#{MEAN_COUPLERUNDATA_DISTDIRNAME}"
MEAN_XT_COUPLERUNDATA_LIST_ATM.each{|nc,varList|
  mkdir(distDirPath)
  mean_CoupledRunData(ATMDATADIR, CycleBegin_Mean, CycleEnd_Mean, nc, varList, "time,lon", distDirPath)
}
MEAN_T_COUPLERUNDATA_LIST_ATM.each{|nc,varList|
  mkdir(distDirPath)
  mean_CoupledRunData(ATMDATADIR, CycleBegin_Mean, CycleEnd_Mean, nc, varList, "time", distDirPath)
}


#
distDirPath = OCNDATADIR+"/#{MEAN_COUPLERUNDATA_DISTDIRNAME}"
MEAN_XT_COUPLERUNDATA_LIST_OCN.each{|nc,varList|
  mkdir(distDirPath)
  mean_CoupledRunData(OCNDATADIR, CycleBegin_Mean, CycleEnd_Mean, nc, varList, "time,lon", distDirPath)
}
MEAN_T_COUPLERUNDATA_LIST_OCN.each{|nc,varList|
  mkdir(distDirPath)
  mean_CoupledRunData(OCNDATADIR, CycleBegin_Mean, CycleEnd_Mean, nc, varList, "time", distDirPath)
}

