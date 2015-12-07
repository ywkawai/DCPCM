#-------------------------------------------------------------
# Copyright (c) 2015-2015 Kawai Yuta. All rights reserved.
#-------------------------------------------------------------

# Configuration **********************************

OutputNCName = 'SurfFlx.nc'
CurrentDir=Dir::pwd
PlanetName = 'Earth'

OneDaySec = 86400.0

#**************************************************

#-----------------------------------------------------------------

require "numru/ggraph"
require File.expand_path(File.dirname(__FILE__) + "/../common/ConstUtil.rb")
require File.expand_path(File.dirname(__FILE__) + "/../common/DCModelIOUtil.rb")
require File.expand_path(File.dirname(__FILE__) + "/DennouOGCMUtil.rb")

eval("include ConstUtil::#{PlanetName}")
include DCModelIOUtil
include NumRu

#-----------------------------------------------------------------

@dsogcmUtill = nil
VarDef = DennouOGCMUtil::VarNameDef
AxisDef = DennouOGCMUtil::AxisNameDef

#-----------------------------------------------------------------

puts "CurrentDir=#{CurrentDir} .."
@dsogcmUtil = DennouOGCMUtil.new(PlanetName, "#{CurrentDir}/#{VarDef::U}.nc")

varList1 = [VarDef::U, VarDef::V, VarDef::SigDot, VarDef::PTempEdd, VarDef::Salt]
varList2 = [VarDef::PTempBasic, VarDef::TotDepthBasic]

varSurfFlxList = [ \
  "SWUWRFlxAtm", "SWDWRFlxAtm", "LWUWRFlxAtm", "LWDWRFlxAtm", \
  "RainAtm", "SnowAtm", "SensFlxAtm", "LatentFlxAtm"]

gp_U, gp_V, gp_SigDot, gp_PTempEdd, gp_Salt = GPhysUtil.get_GPhysObjs(varList1)
gp_PTempBasic, gp_TotDepthBasic = GPhysUtil.get_GPhysObjs(varList2)

gp_SURFlx, gp_SLRFlx, gp_LURFlx, gp_LDRFlx, gp_Rain, gp_Snow, gp_SensFlx, gp_LantentFlx = GPhysUtil.get_GPhysObjs(varSurfFlxList)

ofile = NetCDF::create(OutputNCName)

GPhys::IO.each_along_dims_write( \
  [gp_SURFlx, gp_SLRFlx, gp_LURFlx, gp_LDRFlx, gp_SensFlx, gp_LantentFlx, gp_Rain, gp_Snow], \
  ofile, AxisDef::Lat){
  |swurflx, swdrflx, lwurflx, lwdrflx, sensflx, latentflx, rain, snow|


  net_swradflx = GPhysUtil.redef_GPhysObj(swurflx - swdrflx, \
                                          "SWRFlxAtm", "net short wave radiation", "W.m-2")
  net_lwradflx = GPhysUtil.redef_GPhysObj(lwurflx - lwdrflx, \
                                          "LWRFlxAtm", "net long wave radiation", "W.m-2")
  net_surfflx = GPhysUtil.redef_GPhysObj(net_swradflx + net_lwradflx - sensflx - latentflx, \
                                         "NetSurfFlx", "net surface heat flux", "W.m-2")
  
  fwflx = GPhysUtil.redef_GPhysObj( (- rain - snow - latentflx/LatentHeatV)/WaterDens * OneDaySec * 1e3, \
                                  "SurfFwFlx", "minus freshwater flux", "mm/day")
#  p net_swradflx.val.shape
#  p net_lwradflx.val.shape
#  [swurflx, swdrflx]
  [net_swradflx, net_lwradflx, sensflx, latentflx, net_surfflx, fwflx]
}

ofile.close
=begin
GPhys::IO.each_along_dims_write(
  [gp_U, gp_V, gp_PTemp, gp_Salt], ofile, AxisDef::Time){
  |u, v, ptemp, salt|

  
}
=end


