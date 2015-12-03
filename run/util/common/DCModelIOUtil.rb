#-------------------------------------------------------------
# Copyright (c) 2015-2015 Kawai Yuta. All rights reserved.
#-------------------------------------------------------------

require "numru/ggraph"
include NumRu

###############################################################

module DCModelIOUtil

  module GPhysUtil
    def self.get_GPhysObjs(varNames)
      gp_Array = []
      varNames.each{|varName|
        gturl = "#{CurrentDir}/#{varName}.nc@#{varName}"
        gp_Array.push(GPhys::IO.open_gturl(gturl))
      }

      return gp_Array
    end

    def self.redef_GPhysObj(gphysOri, newname, long_name, units=nil)
      gp = gphysOri.rename(newname)
      if units != nil
        gp.units = units
      else
        gp.units = gphysOri.units
      end
      gp.long_name = long_name
      return gp
    end
  end
  
end
