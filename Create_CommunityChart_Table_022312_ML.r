library(raster)
library(sp)
library(maptools)

# set the current working directory
setwd("/workspace/UA/malindgren/community_charts/NewRun_022312_ML/extracted_output/") # SET TO SOME OUTPUT MASTER DIRECTORY...

# the file that has all of the point information used to create the needed community charts for AKCanada
pointsSPDF <- readShapeSpatial("/workspace/UA/malindgren/community_charts/NewRun_022312_ML/point_shapefile/AKCanada_communities_FINAL_022312_ML.shp")

# lets set some variables to loop through 
vars <- c("tas","pr")
scenarios <- c("sresa1b","sresa2","sresb1")
decades <- c("2010_2019","2020_2029","2030_2039","2040_2049","2050_2059","2060_2069","2070_2079","2080_2089","2090_2099")
months <- c("02","03","04","05","06","07","08","09","10","11","12") #"01", this has been removed to make up for a deficiency in my program
count=1

for(scenario in scenarios){
	print(paste(" SCENARIO: ", scenario, sep=""))
	for (var in vars){
		print(paste("BEGINNING VARIABLE: ", var, sep=""))

		for (dec in decades){
			print(  paste("working on ", dec, sep=""))
		
			#set up some input and output paths
			input_path_mean = paste("/Data/Base_Data/Derived_Data/AK_CAN_2km/ar4_model_statistics/5modelAvg/",scenario,"/",var,"/","decadal_mean/", sep="")
			input_path_sd = paste("/Data/Base_Data/Derived_Data/AK_CAN_2km/ar4_model_statistics/5modelDecadalSD/",scenario,"/",var,"/", sep="")
			input_path_prism = paste("/Data/Base_Data/Climate/AK_CAN_2km/AK_CAN_2km_PRISM/AK_CAN_geotiffs/",var,"/ak83albers/", sep="")
			output_path = "/workspace/UA/malindgren/community_charts/NewRun_022312_ML/extracted_output/"

			# here we stack up the MONTH="01" for the loop since the package::raster needs to have a stack instantiated in order to loop through the other 
			#  rasters that will be placed into the stack during the loop.  This is a small workaround for this issue...
			#   below will stack the mean and standard deviation for the first month side by side 

			if(var=="pr"){
				s <- stack(raster(paste(input_path_mean,"pr_decadal_mean_monthly_total_mm_5modelAvg_",scenario,"_01_",dec,".tif", sep="")), raster(paste(input_path_sd,"pr_5modelDecadalSD_",scenario,"_01_",dec,".tif", sep="")), raster(paste(input_path_prism,"pr_total_mm_akcan_prism_01_1961_1990_ak83alb.tif", sep="")))

			}else{
				s <- stack(raster(paste(input_path_mean,"tas_decadal_mean_monthly_mean_c_5modelAvg_",scenario,"_01_",dec,".tif", sep="")), raster(paste(input_path_sd,"tas_5modelDecadalSD_",scenario,"_01_",dec,".tif", sep="")), raster(paste(input_path_prism,"tas_mean_c_akcan_prism_01_1961_1990_ak83alb.tif",sep="")))
			}
			
			print("      current month: 01")

			for (mon in months){
				print(paste("      current month: ", mon, sep=""))
				
				if(var=="pr"){
					tiff <- paste(input_path_mean,"pr_decadal_mean_monthly_total_mm_5modelAvg_",scenario,"_",mon,"_",dec,".tif", sep="")
					tiff.sd <- paste(input_path_sd,"pr_5modelDecadalSD_",scenario,"_",mon,"_",dec,".tif", sep="")
					tiff.prism <- paste(input_path_prism,"pr_total_mm_akcan_prism_",mon,"_1961_1990_ak83alb.tif", sep="")
				}else{
					tiff <- paste(input_path_mean,"tas_decadal_mean_monthly_mean_c_5modelAvg_",scenario,"_",mon,"_",dec,".tif", sep="")
					tiff.sd <- paste(input_path_sd,"tas_5modelDecadalSD_",scenario,"_",mon,"_",dec,".tif", sep="")
					tiff.prism <- paste(input_path_prism,"tas_mean_c_akcan_prism_",mon,"_1961_1990_ak83alb.tif",sep="")
				}
				
				
				r <- raster(tiff)
				r.sd <- raster(tiff.sd)
				r.prism <- raster(tiff.prism)
									
				s <- stack(s,r,r.sd,r.prism)
			
			} 
			
			# here is the command that performs the extraction of the data from the points on the raster stack
			outFile <- extract(s, pointsSPDF)
			
			# we need something here to make new column headers...
			outFile.mod <- cbind(var, dec, scenario, outFile)
			outFile.mod.xy <- cbind(as.data.frame(pointsSPDF[,1:6]), outFile.mod)
			# here we need to redefine the matrix column namesso that the rbind() will work properly
			colnames(outFile.mod.xy) <- c("region","community","population","country","lon_albers","lat_albers","lon","lat","type","daterange","scenario","Jan","JanSD","Jan6190","Feb","FebSD","Feb6190","Mar","MarSD","Mar6190","Apr","AprSD","Apr6190","May","MaySD","May6190","Jun","JunSD","Jun6190","Jul","JulSD","Jul6190","Aug","AugSD","Aug6190","Sep","SepSD","Sep6190","Oct","OctSD","Oct6190","Nov","NovSD","Nov6190","Dec","DecSD","Dec6190")
			
			if(count == 1 ){
				year1 <- outFile.mod.xy
				# here we need to redefine the matrix column names so that the rbind() will work properly
				colnames(year1) <- c("region","community","population","country","lon_albers","lat_albers","lon","lat","type","daterange","scenario","Jan","JanSD","Jan6190","Feb","FebSD","Feb6190","Mar","MarSD","Mar6190","Apr","AprSD","Apr6190","May","MaySD","May6190","Jun","JunSD","Jun6190","Jul","JulSD","Jul6190","Aug","AugSD","Aug6190","Sep","SepSD","Sep6190","Oct","OctSD","Oct6190","Nov","NovSD","Nov6190","Dec","DecSD","Dec6190")
			}else{
				year1 <- rbind(year1, outFile.mod.xy)
			}

			count=count+1
		}	

	}
	
}
colnames(year1) <- c("region","community","population","country","lon_albers","lat_albers","lon","lat","type","daterange","scenario","Jan","JanSD","Jan6190","Feb","FebSD","Feb6190","Mar","MarSD","Mar6190","Apr","AprSD","Apr6190","May","MaySD","May6190","Jun","JunSD","Jun6190","Jul","JulSD","Jul6190","Aug","AugSD","Aug6190","Sep","SepSD","Sep6190","Oct","OctSD","Oct6190","Nov","NovSD","Nov6190","Dec","DecSD","Dec6190")
write.csv(year1, file="CommunityCharts_InputData_121211_final_FIXcommunity.csv", row.names=FALSE)

