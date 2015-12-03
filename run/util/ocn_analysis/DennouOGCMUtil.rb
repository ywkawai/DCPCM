require "numru/ggraph"
require File.expand_path(File.dirname(__FILE__) + "/../common/ConstUtil.rb")
include NumRu
include ConstUtil

class DennouOGCMUtil
  attr_accessor :ax_Lon, :ax_Lat, :ax_Sig
  attr_accessor :gp_lonIntWt, :gp_latIntWt, :gp_sigIntWt
  attr_accessor :planetName
  attr_accessor :lonAxisName, :latAxisName, :sigAxisName
  attr_accessor :const
  attr_accessor :globalVolume, :globalSurfArea

  class VarNameDef
    U  = 'U'
    V  = 'V'
    SigDot  = 'SigDot'
    PTempEdd  = 'PTempEdd'
    Salt  = 'Salt'
    W  = 'W'
    PTemp  = 'PTemp'

    PTempBasic = 'PTempBasic'
    TotDepthBasic = 'TotDepthBasic'
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
#    @gp_lonIntWt = GPhys::IO.open_gturl(ncpath+"@lon_weight")
#    @gp_latIntWt = GPhys::IO.open_gturl(ncpath+"@lat_weight")
#    @gp_sigIntWt = GPhys::IO.open_gturl(ncpath+"@sig_weight")


    @lonAxisName = "lon"
    @latAxisName = "lat"
    @sigAxisName = "sig"
    @timeAxisName = "time"

    @ax_Lon = @gp_lon.axis(@lonAxisName)
    @ax_Lat = @gp_lat.axis(@latAxisName)
    @ax_sig = @gp_sig.axis(@sigAxisName)

    @const = eval("ConstUtil::#{@planetName}")

    @globalSurfArea = 4.0*PI*@const::RPlanet**2
    
#    puts "Initialize an object of DCPAMUtil class.."
  end
  
end
