require "numru/gphys"
include NumRu

SRCRunDir            = ARGV[0]
SRCReadTimeVal       = ARGV[1].to_f
DISTRunDir           = ARGV[2]
DISTWriteTimeVal      = ARGV[3].to_f

RestartFileName  = "#{SRCRunDir}/Restart.nc"
InitDataFileName = "InitData.nc" 
timeAveragedFlag = false
timeNewAxis = nil

###############

if SRCReadTimeVal < 0.0 then
  timeAveragedFlag = true
  puts "Average a variable along time axis..."
end

puts "SRC=#{SRCRunDir}, DIST=#{DISTRunDir}"

p "VarList associated with initial data:",  GPhys::NetCDF_IO.var_names_except_coordinates(RestartFileName)

#############

ofile = NetCDF.create(InitDataFileName)
output_varNames = GPhys::NetCDF_IO.var_names_except_coordinates(RestartFileName)

for varName in output_varNames
  
  var_ori = GPhys::NetCDF_IO.open(RestartFileName, varName)
  grid_ori = var_ori.grid.copy
  isTimeAxisInclude = var_ori.axnames.include?("time")

  if isTimeAxisInclude && timeNewAxis == nil then
      timeOriAxis = grid_ori.axis("time")
      timeNewAxis_va = timeOriAxis.pos[0..0].copy
      timeNewAxis_va[0] = DISTWriteTimeVal.to_f
      timeNewAxis = Axis.new.set_pos(timeNewAxis_va)
  end

  grid = nil; var = nil
  if isTimeAxisInclude then
    grid = grid_ori.change_axis(grid_ori.dim_index("time"), timeNewAxis)
#    p var_ori.axis("time").pos.val

    initTimeVal = timeOriAxis.pos.val[0]
    var = GPhys.new(grid, var_ori.cut("time"=>initTimeVal..initTimeVal).data.copy)

    if timeAveragedFlag then
      var[false,0] = var_ori.mean("time")
    else
      var[false,0..0] = var_ori.cut("time"=>SRCReadTimeVal..SRCReadTimeVal)
    end
  else
    grid = grid_ori
    var = var_ori
  end

  GPhys::NetCDF_IO.write(ofile, var)
  
end

ofile.close

###################################################################################
#
###################################################################################

if timeAveragedFlag then

  puts "Check whether sea ice has formed in previous coupled run.."
  DensIce = UNumeric[905e0, "kg.m-3"]
  DensSeaWater = UNumeric[1030e0, "kg.m-3"]
  DensSnow = UNumeric[330e0, "kg.m-3"]
  CIce = UNumeric[2100e0, "J.kg-1.degC-1"]
  CSeaWater = UNumeric[3986e0, "J.kg-1.K-1"]
  FreezeTempSW = UNumeric[-2e0, "degC"]
  Mu = UNumeric[0.054e0, "degC.permil-1"]
  LFreeze = UNumeric[334e3, "J.kg-1"]
  SaltSIce = UNumeric[1e0, "permil"]

  def calc_IceLyr1_E(t, s=SaltSIce)
    return CIce*(t + Mu*s) - LFreeze*(1.0 + Mu*s/t)
  end

  def calc_IceLyr2_E(t, s=SaltSIce)
    return CIce*(t + Mu*s) - LFreeze
  end

  def calc_IceLyr1_Temp(e, s=SaltSIce)
    a = CIce; b = -(CIce*Mu*s - LFreeze - e); c = -LFreeze*Mu*s

    tmp_ = b - (b*b - 4.0*a*c).sqrt
    return tmp_/(2.0*a)
#    return (e + LFreeze)/CIce - Mu*s
  end

  def calc_IceLyr2_Temp(e, s=SaltSIce)
    return (e + LFreeze)/CIce - Mu*s
  end
  
  iceThickOri = GPhys::NetCDF_IO.open(RestartFileName, "IceThick")
  siceTempOri = GPhys::NetCDF_IO.open(RestartFileName, "SIceTemp")
  siceSurfTempOri = GPhys::NetCDF_IO.open(RestartFileName, "SIceSurfTemp")
  tAxisLen = iceThickOri.grid.axis("time").pos.length.to_f
  
  initDataFile = NetCDF.open(InitDataFileName, 'a')
  siceCon  = GPhys::NetCDF_IO.open(initDataFile, "SIceCon")
  siceTemp = GPhys::NetCDF_IO.open(initDataFile, "SIceTemp")
  siceSurfTemp = GPhys::NetCDF_IO.open(initDataFile, "SIceSurfTemp")  
  iceThick = GPhys::NetCDF_IO.open(initDataFile, "IceThick")
  snowThick = GPhys::NetCDF_IO.open(initDataFile, "SnowThick")

  ptempEdd = GPhys::NetCDF_IO.open(initDataFile, "PTempEdd")
  sig = ptempEdd.grid.axis("sig").pos


  GPhys.each_along_dims([iceThickOri, siceTempOri, siceSurfTempOri, iceThick, snowThick, siceTemp, siceSurfTemp, siceCon, ptempEdd], ["lat", "lon"]){
    |iceThickOri_, siceTempOri_, siceSurfTempOri_, iceThick_, snowThick_, siceTemp_, siceSurfTemp_, siceCon_, ptempEdd_|

    if (iceThickOri_ > 0.0).any? && (iceThickOri_ <= 0.0).any?  then
      #       p "Melt"
      #       p iceThickOri_.val[0,0,60..71]#

      tMask = (iceThickOri_ > 0.0).where
      tMaskLen = tMask.length.to_f
      p tMask
      iceThickMean = UNumeric[iceThickOri_.val[0,0,tMask].sum / tAxisLen, 'm']
      puts "ptempEdd_:#{ptempEdd_.val[0,0,0,0]}, iceThick_:#{iceThickMean}, siceTemp_:#{siceTempOri_.val[0,0,0,tMask].mean},#{siceTempOri_.val[0,0,1,tMask].mean}"

#      iceLyr1EngyMean = (calc_IceLyr1_E(siceTempOri_[0,0,0,tMask].val)*(0.5*iceThickOri_[0,0,tMask]).val).sum/tAxisLen
#      iceLyr2EngyMean = (calc_IceLyr2_E(siceTempOri_[0,0,1,tMask].val)*(0.5*iceThickOri_[0,0,tMask]).val).sum/tAxisLen
#      iceLyr1NewTemp  = calc_IceLyr1_Temp(iceLyr1EngyMean/(0.5*iceThickMean))
#      iceLyr2NewTemp  = calc_IceLyr2_Temp(iceLyr2EngyMean/(0.5*iceThickMean))

      dzTop = UNumeric[5.2e3,"m"]*(sig[0] - sig[1])*0.5
#      ptempEdd_[false,0,0] = ptempEdd_[false,0,0] + iceLyrEngy/(DensSeaWater*CSeaWater*dzTop)
#      iceThick_[false,0] = iceThickOri_[0,0,tMask].mean #iceThickMean
#      snowThick_[false,0] = snowThickOri_[0,0,tMask].mean  #0e0
      siceTemp_[0,0,0,0] = siceTempOri_[0,0,0,tMask].mean.to_f  #iceLyr1NewTemp.to_f
      siceTemp_[0,0,1,0] = siceTempOri_.val[0,0,1,tMask].mean.to_f  #iceLyr2NewTemp.to_f
      siceSurfTemp_[false,0] = siceSurfTempOri_[0,0,tMask].mean.to_f
#      siceCon_[false,0] = 0e0

      puts "=> new ptempEdd_:#{ptempEdd_.val[0,0,0,0]}, iceThick_:#{iceThickMean}, sicetemp_:#{siceTemp_.val[0,0,0,0]},#{siceTemp_.val[0,0,1,0]},siceSurfTemp_:#{siceSurfTemp_.val[0,0,0]}"
    end
  }
  initDataFile.close
end

#
puts "The generated InitData.nc is moved to #{DISTRunDir}/"
`mv InitData.nc #{DISTRunDir}/`
