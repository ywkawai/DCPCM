#-------------------------------------------------------------
# Copyright (c) 2015-2015 Kawai Yuta. All rights reserved.
#-------------------------------------------------------------

# Configuration **********************************

OutputNCName = 'EngyFlxLat.nc'
CurrentDir=Dir::pwd
PlanetName = 'Earth'

#**************************************************

require "numru/ggraph"
require File.expand_path(File.dirname(__FILE__) + "/../common/ConstUtil.rb")
require File.expand_path(File.dirname(__FILE__) + "/../common/DCModelIOUtil.rb")
require File.expand_path(File.dirname(__FILE__) + "/DennouOGCMUtil.rb")

eval("include ConstUtil::#{PlanetName}")
include DCModelIOUtil
include NumRu
include ConstUtil::Earth

#-----------------------------------------------------------------

@dsogcmUtil = nil
VarDef = DennouOGCMUtil::VarNameDef
AxisDef = DennouOGCMUtil::AxisNameDef

#------------------------------------------------------------------------------------------

puts "CurrentDir=#{CurrentDir} .."
@dsogcmUtil = DennouOGCMUtil.new(PlanetName, "#{CurrentDir}/#{VarDef::U}.nc")

varBasicList = [ VarDef::PTempBasic, VarDef::TotDepthBasic ]
varList = [ VarDef::U, VarDef::V, VarDef::BolusV, VarDef::PTempEdd, VarDef::Salt ]

gp_PTempBasic, gp_TotDepthBasic \
 = GPhysUtil.get_GPhysObjs(varBasicList)

gp_U, gp_V, gp_BolusV, gp_PTempEdd, gp_Salt \
 = GPhysUtil.get_GPhysObjs(varList)

gp_PTemp = GPhysUtil.redef_GPhysObj( gp_PTempBasic.cut("time"=>0) + gp_PTempEdd, \
                                     "PTemp", "potential temperature", "K")
ofile = NetCDF::create(OutputNCName)
GPhys::IO.each_along_dims_write( \
  [gp_V, gp_BolusV, gp_PTemp, gp_Salt], ofile, AxisDef::Time){ \
  |v, bolusV, ptemp, salt|

  time = ptemp.axis("time")
  puts "time=#{time.pos.val[0]} [#{time.pos.units}] .."

  totdepth = gp_TotDepthBasic[true,true,0]
  heatTransEuler = Ocn::Cp0 * Ocn::Dens0 * ptemp * v
  heatTransBolus = Ocn::Cp0 * Ocn::Dens0 * ptemp * bolusV
  heatTransTot = heatTransEuler + heatTransBolus
  
  [ \
    GPhysUtil.redef_GPhysObj( @dsogcmUtil.globalIntLonSig(heatTransTot, totdepth),              \
                              "HtransTot",                                                      \
                              "total ocean heat transport ", "W.m-2" ),                           \
    GPhysUtil.redef_GPhysObj( @dsogcmUtil.globalIntLonSig(heatTransEuler, totdepth),              \
                              "HTransEuler",                                                      \
                              "eulerian contribution to ocean heat transport ", "W.m-2" ),        \
    GPhysUtil.redef_GPhysObj( @dsogcmUtil.globalIntLonSig(heatTransBolus, totdepth),              \
                              "HTransBolus",                                                      \
                              "bolus contribution to ocean heat transport ", "W.m-2" ),        \

  ]
}
ofile.close
