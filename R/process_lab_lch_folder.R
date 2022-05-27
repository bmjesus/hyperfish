#' @title Script to apply the reflectance_color_indices function to a folder
#' @description Script to apply the reflectance_color_indices function to a folder. Only applicable to Konstantina's workflow.
#' @param my_folder path to folder with the cube camera files, recursive search.
#' @param my_reference path to the reference file
#' @return Doesn't return any R object directly, only through the reflectance_color_indices function.
#' @keywords external
#' @export

process_lab_lch_folder <- function(my_folder, my_reference){


  ###########################################
  #creating list of files to be processed

  cube_files <-  list.files(path = my_folder, pattern = ".cube", all.files = FALSE,
                            full.names = TRUE, recursive = TRUE, include.dirs = FALSE)

  #print(cube_files)

  shp_files <- list.files(path = my_folder, pattern = ".shp", all.files = FALSE,
                          full.names = TRUE, recursive = TRUE, include.dirs = FALSE)

  #some files have shp and qtr in the name, the following line removes the qtr file
  shp_files <- shp_files[-grep(".qtr", shp_files)]

  #some files have cube and enp in the name, the following line removes the enp file
  #cube_files <- cube_files[-grep(".enp", cube_files)]

  #print(cube_files)

  ###########################################


  ###########################################
  #importing and storing the reference file
  #read reference file, assume it is 18% grey
  reference <- raster::stack(my_reference)
  #multiply by 5.555556 because it's 18%
  normalize_reference <- function(my_reference){
    norm_reference <- my_reference * 5.555556
    return(norm_reference)
  }
  raster::beginCluster() #beginning a cluster for multicore processing
  #executing the function defined above
  reference <- raster::clusterR(reference,raster::calc,args=list(fun = normalize_reference ))
  raster::endCluster() #closing the cluster
  raster::plotRGB(raster::stack(reference),r=57, g=36,b=14,stretch='lin')
  ###########################################

  num_files <- length(cube_files)

  for (i in 1:num_files){

    print(cube_files[i]) #useful to check if the roi is the right one
    print(shp_files[i]) #useful to check if the roi is the right one

    hyperfish::reflectance_color_indices(camera_file = cube_files[i],
                              reference_file = reference,
                              roi_file  = shp_files[i],
                              my_plots = FALSE)


  }


}

