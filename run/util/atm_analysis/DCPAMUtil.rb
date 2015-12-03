require "numru/ggraph"
require File.expand_path(File.dirname(__FILE__) + "/../common/ConstUtil.rb")
include NumRu
include ConstUtil

class DCPAMUtil
  attr_accessor :ax_Lon, :ax_Lat, :ax_Sig
  attr_accessor :gp_lonIntWt, :gp_latIntWt, :gp_sigIntWt
  attr_accessor :planetName
  attr_accessor :lonAxisName, :latAxisName, :sigAxisName
  attr_accessor :const
  attr_accessor :globalVolume, :globalSurfArea
  
  class VarNameDef
    Ps = 'Ps'
    U  = 'U'
    V  = 'V'
    Temp  = 'Temp'
    QH2OVap  = 'QH2OVap'
    Height  = 'Height'
    SurfTemp = 'SurfTemp'
  end

  class AxisNameDef
    Lon = 'lon'
    Lat = 'lat'
    Sig = 'sig'
    Time = 'time'
  end
  
  def initialize(planetName, ncpath)

    @planetName = planetName

    @gp_lon = GPhys::IO.open_gturl(ncpath+"@lon")
    @gp_lat = GPhys::IO.open_gturl(ncpath+"@lat")
    @gp_sig = GPhys::IO.open_gturl(ncpath+"@sig")    
    @gp_lonIntWt = GPhys::IO.open_gturl(ncpath+"@lon_weight")
    @gp_latIntWt = GPhys::IO.open_gturl(ncpath+"@lat_weight")
    @gp_sigIntWt = GPhys::IO.open_gturl(ncpath+"@sig_weight")


    @lonAxisName = "lon"
    @latAxisName = "lat"
    @sigAxisName = "sig"
    @timeAxisName = "time"

    @ax_Lon = @gp_lonIntWt.axis(@lonAxisName)
    @ax_Lat = @gp_latIntWt.axis(@latAxisName)    
    @ax_Sig = @gp_sigIntWt.axis(@sigAxisName)

    @const = eval("ConstUtil::#{@planetName}")

    @globalSurfArea = 4.0*PI*@const::RPlanet**2
    
#    puts "Initialize an object of DCPAMUtil class.."
  end

  def globalIntLonSig(gphys, gp_Ps)
    tmp = (@gp_sigIntWt*gphys).sum(@sigAxisName)*gp_Ps/@const::Grav
    return @const::RPlanet * (@gp_lonIntWt*tmp).sum(@lonAxisName)
  end
  
  def globalIntLonLat(gphys)
    return @const::RPlanet**2 * (@gp_lonIntWt*(@gp_latIntWt*gphys)).sum(@lonAxisName, @latAxisName)
  end
  
  def globalIntLonLatSig(gphys, gp_Ps)
    tmp = (@gp_sigIntWt*gphys).sum(@sigAxisName)*gp_Ps/@const::Grav
    return @const::RPlanet**2 * (@gp_lonIntWt*(@gp_latIntWt*tmp)).sum(@lonAxisName, @latAxisName)
  end

  def globalMeanSurf(gphys)
    return globalIntLonLat(gphys)/@globalSurfArea
  end
  
  def gen_3DGPysObj(name, long_name, units, timeAxis=nil)
    na = nil; grid = nil
    if timeAxis != nil
      na = NArray.sfloat(@ax_Lon.length, @ax_Lat.length, @ax_Sig.length, timeAxis.length)
      grid = Grid.new(@ax_Lon, @ax_Lat, @ax_Sig, timeAxis)
    else
      na = NArray.sfloat(@ax_Lon.length, @ax_Lat.length, @ax_Sig.length)
      grid = Grid.new(@ax_Lon, @ax_Lat, @ax_Sig)
    end

    va = VArray.new(na, {"name"=>name, "long_name"=>long_name, "units"=>units})
    return GPhys.new(grid, va)
  end
  
  def calc_Pressure(gp_Ps)
    gp_Press = gen_3DGPysObj("Press", "pressure", "Pa", gp_Ps.axis(@timeAxisName))

    GPhys::each_along_dims([gp_Press, @gp_sig], @sigAxisName){
      |press, sig|
      press[true,true,0,true] = gp_Ps*sig
    }
    gp_Press.units = 'kg.m-1.s-2'
    return gp_Press.rename("Press")
  end

  def calc_Density(gp_Press, gp_Temp)
    gp_Rho = (gp_Press/(@const::Atm::GasRDry*gp_Temp)).rename("Rho")
    gp_Rho.units = 'kg.m-3'
    return gp_Rho
  end
  
end
