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
require File.expand_path(File.dirname(__FILE__) + "/DennouOGCMUtil.rb")

eval("include ConstUtil::#{PlanetName}")
include DCModelIOUtil
include NumRu

#-----------------------------------------------------------------

@dsogcmUtil = nil
OutputNCName_SurfFlx = OutputNCName.gsub(".nc", "_SurfFlx.nc")
VarDef = DennouOGCMUtil::VarNameDef
AxisDef = DennouOGCMUtil::AxisNameDef

#------------------------------------------------------------------------------------------

puts "CurrentDir=#{CurrentDir} .."
@dsogcmUtil = DennouOGCMUtil.new(PlanetName, "#{CurrentDir}/#{VarDef::U}.nc")

varBasicList = [ VarDef::PTempBasic, VarDef::TotDepthBasic ]
varList = [ VarDef::PTempEdd, VarDef::Salt ]
varSurfFlxList = [ "SurfHFlxO", "SurfFwFlxO" ]

gp_PTempBasic, gp_TotDepthBasic \
 = GPhysUtil.get_GPhysObjs(varBasicList)

gp_PTempEdd, gp_Salt \
 = GPhysUtil.get_GPhysObjs(varList)

gp_SurfHFlxO, gp_SurfFwFlxO \
 = GPhysUtil.get_GPhysObjs(varSurfFlxList)

gp_PTemp = GPhysUtil.redef_GPhysObj( gp_PTempBasic.cut("time"=>0) + gp_PTempEdd, \
                                     "PTemp", "potential temperature", "K")

#-----------------------------------------------------------------------------------------

ofile = NetCDF::create(OutputNCName)
GPhys::IO.each_along_dims_write( \
  [gp_PTemp, gp_Salt], ofile, AxisDef::Time){ \
  |ptemp, salt|

  time = ptemp.axis("time")
  puts "time=#{time.pos.val[0]} [#{time.pos.units}] .."

  totdepth = gp_TotDepthBasic[true,true,0]
  [ \
    GPhysUtil.redef_GPhysObj( @dsogcmUtil.globalMean3D(ptemp, totdepth),               \
                              "PTemp",                                                 \
                              "global mean of surface temperature", "K" ),             \
    GPhysUtil.redef_GPhysObj( @dsogcmUtil.globalMean3D(salt, totdepth),                \
                              "Salt",                                                  \
                              "global mean of salinity", "psu" ),                      \
  ]
}
ofile.close

#-----------------------------------------------------------------------------------------
ofile = NetCDF::create(OutputNCName_SurfFlx)
GPhys::IO.each_along_dims_write( \
  [gp_SurfHFlxO, gp_SurfFwFlxO], ofile, AxisDef::Time){ \
  |surfHFlxO, surfFwFlxO|#, surfFwFlxO|

  time = surfHFlxO.axis("time")
  puts "time=#{time.pos.val[0]} [#{time.pos.units}] .."

  [ \
    GPhysUtil.redef_GPhysObj( @dsogcmUtil.globalMeanSurf(surfHFlxO),                   \
                              "SurfHFlxO",                                             \
                              "global mean of surface heat flux", "W.m-2" ),           \
    GPhysUtil.redef_GPhysObj( @dsogcmUtil.globalMeanSurf(surfFwFlxO),                  \
                              "SurfFwFlxO",                                            \
                              "global mean of freshwater flux", "m.s-1" )              \
  ]
}
ofile.close

#-----------------------------------------------------------------------------------------

gp_PTemp_glmean = GPhys::NetCDF_IO.open(OutputNCName, "PTemp")
gp_SurfHFlxO_glmean = GPhys::NetCDF_IO.open(OutputNCName_SurfFlx, "SurfHFlxO")

ax_time = gp_SurfHFlxO_glmean.axis("time")
tlen = ax_time.length
gp_time = ax_time.to_gphys

totTime           = UNumeric[(gp_time.val[tlen-1] - gp_time.val[0])*86400.0, "sec"]
tot_surfHeating = - @dsogcmUtil.globalIntLonLat(gp_SurfHFlxO.sum("time"))*(totTime/tlen.to_f)
ocnMass = @dsogcmUtil.globalIntLonLat(Ocn::Dens0*gp_TotDepthBasic)
dtemp_surfHeating = tot_surfHeating/(Ocn::Cp0*ocnMass)

p "DTemp (due to surface heat flux) = #{dtemp_surfHeating.val[0]} [K]"
p "DTemp/Dt (due to surface heat flux) = #{dtemp_surfHeating.val[0]/totTime*86400.0} [K/day]"
p "----------------------------------"

ax_time = gp_PTemp_glmean.axis("time")
tlen = ax_time.length
dtemp_glmean = gp_PTemp_glmean.val[tlen-1] - gp_PTemp_glmean.val[0]
p "Actual change of global mean temperature = #{dtemp_glmean} [K]"
