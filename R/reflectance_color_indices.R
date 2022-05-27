#' @title Calculate reflectance and LAB, LCH parameters using an 18 percent reference image
#' @description This function calculates reflectance using a 18 percent reference image, calculates the corresponding Lab and LCH parameters for each pixel and then it extracts the pixel data using a shapefile with the desired ROI. It was designed specifically for Konstantina's workflow so it is unlikelly to function outside that scope.
#' @param camera_file path to the camera file, cube version
#' @param reference_file path to the 18 percent reference file
#' @param roi_file path shapefile with the ROI file
#' @param my_plots logical value to activate-desactivate plots. Not implemented yet
#' @return The function outputs a reflectance tif file, a LAB tif file, a LCH tif file, an excel LAB file stats + pixel values, an excel LCH file stats + pixel values and it returns a R list with lab/lch statistics, the reflectance image, and the lab+lch images
#' @keywords external
#' @export

reflectance_color_indices<-function(camera_file,reference_file, roi_file,my_plots = TRUE){


  reference <- reference_file

  ################################################################################
  #function to extract the wavelengths from the hdr file, should be moved to the package
  import_wl<-function(filename="./"){

    #Reading the bands wl
    data_raw<-scan(filename,sep='\t',character(0),quiet=TRUE)

    data_start <- grep(pattern = "wavelength = ",x=data_raw)+1

    #print(data_start)
    data_end<-length(data_raw)

    data_raw<-data_raw[data_start:data_end]
    wl<-gsub("}","",data_raw)
    #wl<-gsub("             ","",wl)

    #print(data_raw)
    wl<-as.numeric(unlist(strsplit(wl, ",")))

    return(wl)
  }

  #setting up the hdr file
  #need to remove the extension .cube from the name of the file
  length_name <- nchar(camera_file)

  camera_file2 <- substr(camera_file, 1, length_name-5)
  hdr_file <- paste(camera_file2,".hdr",sep="")

  #importing wavelengths
  wl<-import_wl(hdr_file)
  ################################################################################

  #read ROI file
  roi <- rgdal::readOGR(roi_file)


  #removed the dark correction because the aquisition time was very different from the sample images
  #read dark file
  #dark <- raster::stack(dark_file)
  #divide by 3.3 because the dark was acquired at 80 ms
  #normalize_dark <- function(my_dark){

  #  norm_dark <- my_dark / 3.333333

  # return(norm_dark)
  #}

  #raster::beginCluster() #beginning a cluster for multicore processing
  #executing the function defined above
  #dark <- raster::clusterR(dark,raster::calc,args=list(fun = normalize_dark ))
  #raster::endCluster() #closing the cluster

  #raster::plotRGB(raster::stack(dark),r=57, g=36,b=14,stretch='lin')



  #read camera file
  camera <- raster::stack(camera_file)
  #multiply by 1.14 to be at the same integration time as the reference 24 ms
  #name_file <- raster::filename(camera)
  #print(name_file)

  normalize_camera <- function(my_camera){

    norm_camera <- my_camera * 1.142857

    return(norm_camera)
  }

  raster::beginCluster() #beginning a cluster for multicore processing
  #executing the function defined above
  camera <- raster::clusterR(camera,raster::calc,args=list(fun = normalize_camera ))
  raster::endCluster() #closing the cluster

  raster::plotRGB(raster::stack(camera),r=57, g=36,b=14,stretch='lin')


  #calculate reflectance
  #added 1 unit to the reference to avoid having NA values caused by dividing by zero
  #can it be made quicker by parallel processing?

  #my_reflectance <-function(camera, dark = dark, reference = reference){

  reflectance <- (camera)/((1+reference))

  #print("I have calculated the reflectance")

  #reflectance_out <<- reflectance

  #flipping image to oriented as the camera view
  reflectance <- raster::flip(raster::flip(raster::t(reflectance),2),1)

  #print("I have flipped the image")

  #plot the reflectance image
  raster::plotRGB(raster::stack(reflectance),r=57, g=36,b=14,stretch='lin')
  raster::plot(roi,add = TRUE)

  #save reflectance file
  raster::writeRaster(reflectance,paste(camera_file2,"_reflectance.tif",sep=""),format="GTiff", overwrite=TRUE)

  #calculate  LAB
  lab<-hyperfish::lab_raster(reflectance)

  #plot LAB
  raster::plot(lab)
  raster::writeRaster(lab,paste(camera_file2,"_LAB.tif",sep=""),format="GTiff", overwrite=TRUE)


  #calculate  LCH
  lch<-hyperfish::lch_raster(reflectance)

  #plot LCH
  raster::plot(lch)
  raster::writeRaster(lch,paste(camera_file2,"_LCH.tif",sep=""),format="GTiff", overwrite=TRUE)


  #extract ROI statistics file
  stats_lab <- hyperfish::extract_stats(tif_file = paste(camera_file2,"_LAB.tif",sep=""),
                             roi_file = roi_file, type_file = "LAB")

  stats_lch <- hyperfish::extract_stats(tif_file = paste(camera_file2,"_LCH.tif",sep=""),
                             roi_file = roi_file, type_file = "LCH")


  #saving results in excel file
  writexl::write_xlsx(stats_lab$parameters,
                      path = paste(camera_file2,"_LAB_parameters.xlsx",sep = ""),
                      col_names = TRUE,
                      format_headers = TRUE)

  writexl::write_xlsx(stats_lab$pixels,
                      path = paste(camera_file2,"_LAB_pixels.xlsx",sep = ""),
                      col_names = TRUE,
                      format_headers = TRUE)

  writexl::write_xlsx(stats_lch$parameters,
                      path = paste(camera_file2,"_LCH_parameters.xlsx",sep = ""),
                      col_names = TRUE,
                      format_headers = TRUE)

  writexl::write_xlsx(stats_lch$pixels,
                      path = paste(camera_file2,"_LCH_pixels.xlsx",sep = ""),
                      col_names = TRUE,
                      format_headers = TRUE)



  #saving it also in R format for future processing if needed
  save(stats_lab, file = paste(camera_file2,"_LAB.RDATA",sep = ""))
  save(stats_lch, file = paste(camera_file2,"_LCH.RDATA",sep = ""))


  output<-list(stats_lab,stats_lch,reflectance,lab,lch)

  return(output)




}
