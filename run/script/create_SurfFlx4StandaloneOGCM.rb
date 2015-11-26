require "numru/gphys"
include NumRu

SRCRunDir                = ARGV[0]
SRCReadInitTimeVal       = ARGV[1]
SRCReadEndTimeVal        = ARGV[2]
DISTRunDir               = ARGV[3]

SurfFlxVarNames = [ "TauXAtm", "TauYAtm",
                    "SWDWRFlxAtm", "LWDWRFlxAtm", "SWUWRFlxAtm", "LWUWRFlxAtm", 
                    "LatentFlxAtm", "SensFlxAtm",
                    "RainAtm", "SnowAtm", "SurfAirTempAtm",
                    "DSurfHFlxDTsAtm", "DSurfLatentFlxDTsAtm" ]

puts "SRC=#{SRCRunDir}, DIST=#{DISTRunDir}"
puts "Averaged time range: #{SRCReadInitTimeVal} .. #{SRCReadEndTimeVal} [day]"
p "Averaed SurfFluxVars:", SurfFlxVarNames

#############

for flxVarName in SurfFlxVarNames
  sFileName = "#{SRCRunDir}/#{flxVarName}.nc"
  oFileName = "#{DISTRunDir}/#{flxVarName}.nc"

  var = GPhys::NetCDF_IO.open(sFileName, flxVarName)
  var = var.cut("time"=>SRCReadInitTimeVal.to_f..SRCReadEndTimeVal.to_f).mean('time')

  ofile = NetCDF.create("#{oFileName}_tmp")
  GPhys::NetCDF_IO.write(ofile, var)
  ofile.close
  `mv #{oFileName}_tmp #{oFileName}`
end


