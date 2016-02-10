#-------------------------------------------------------------
# Copyright (c) 2015-2015 Kawai Yuta. All rights reserved.
#-------------------------------------------------------------

# Configuration **********************************

OutputNCName = 'GlobalMeanQuants.nc'
CurrentDir=Dir::pwd
PlanetName = 'Earth'

#**************************************************

require "numru/ggraph"
require File.expand_path(File.dirname(__FILE__) + "/../common/ConstUtil.rb")
require File.expand_path(File.dirname(__FILE__) + "/../common/DCModelIOUtil.rb")
require File.expand_path(File.dirname(__FILE__) + "/DCPAMUtil.rb")

eval("include ConstUtil::#{PlanetName}")
include DCModelIOUtil
include NumRu

#-----------------------------------------------------------------

@dcpamUtil = nil
VarDef = DCPAMUtil::VarNameDef
AxisDef = DCPAMUtil::AxisNameDef

#-----------------------------------------------------------------

puts "CurrentDir=#{CurrentDir} .."
@dcpamUtil = DCPAMUtil.new(PlanetName, "#{CurrentDir}/#{VarDef::U}.nc")

varList = [ "SurfTempOcn", "OLRA", "OSRA" ]

gp_SurfTemp, gp_OLR, gp_OSR \
 = GPhysUtil.get_GPhysObjs(varList)

ofile = NetCDF::create(OutputNCName)
GPhys::IO.each_along_dims_write(  
 [gp_SurfTemp, gp_OLR, gp_OSR], ofile, AxisDef::Time){
  |surfTemp, olr, osr|

  time = surfTemp.axis("time").pos
  puts "time=#{time.val[0]} [#{time.units}] .."

  [ \
    GPhysUtil.redef_GPhysObj( @dcpamUtil.globalMeanSurf(surfTemp),         \
                              "SurfTemp",                                  \
                              "global mean of surface temperature", "K" ), \
    GPhysUtil.redef_GPhysObj( @dcpamUtil.globalMeanSurf(olr),              \
                              "OLR",                                       \
                              "global mean of OLR", "W.m-2" ),             \
    GPhysUtil.redef_GPhysObj( @dcpamUtil.globalMeanSurf(-osr),             \
                             "mOSR",                                       \
                             "global mean of minus OSR", "W.m-2"),         \
    GPhysUtil.redef_GPhysObj( @dcpamUtil.globalMeanSurf(olr + osr),        \
                              "RadTOA",                                    \
                              "global mean of net radiation at TOA", "W.m-2" ), \
  ]
}
ofile.close
