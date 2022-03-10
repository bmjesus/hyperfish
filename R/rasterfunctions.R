#' @title Construct L*a*b* images from an hyperspectral camera
#' @description This function takes as input an hyperspectral image and calculates the corresponding L*a*b* parameters for each pixel, result in a stack of three images corresponding the each of the  L*a*b* parameters. (TODO- add calculations for HUE and Chroma values)
#' @param raster Raster object with the camera file
#' @return The function returns a raster stack object with three layers: L*,a* and b*
#' @keywords external
#' @export
#'

lab_raster<-function(raster){


#1 - defining the function that raster::calc will use
#this is the function that calculates L*a*b* from a spectra acquired with the camera

lab_function<-function(spectra){

#setting up the bands wl
    wl<- c(376.5555, 381.5800, 386.6085, 391.6411,  396.6777,  401.7183,  406.7629, 411.8116,  416.8643,  421.9210,  426.9817,  432.0464,  437.1152,  442.1880, 447.2648,  452.3457,  457.4305,  462.5194,  467.6124,  472.7093,  477.8103,482.9152,  488.0243,  493.1373,  498.2543,  503.3754,  508.5005,  513.6296,518.7628,  523.9000,  529.0411,  534.1863,  539.3356,  544.4888,  549.6461,554.8074,  559.9727,  565.1421,  570.3155,  575.4929,  580.6743,  585.8597,591.0492,  596.2427,  601.4402,  606.6417,  611.8473,  617.0569,  622.2705,627.4882,  632.7098,  637.9354,  643.1652,  648.3989,  653.6367,  658.8784,664.1242,  669.3740,  674.6278,  679.8857,  685.1476,  690.4135,  695.6834,700.9573,  706.2354,  711.5173,  716.8033,  722.0934,  727.3875,  732.6856,737.9877,  743.2938,  748.6040,  753.9182,  759.2364,  764.5586,  769.8848,775.2151,  780.5494,  785.8878,  791.2301,  796.5765,  801.9269,  807.2812,812.6398,  818.0021,  823.3687,  828.7392,  834.1136,  839.4922,  844.8748,850.2614,  855.6520,  861.0466,  866.4454,  871.8480,  877.2548,  882.6655,888.0802,  893.4991,  898.9218,  904.3487,  909.7795,  915.2144,  920.6533,926.0962,  931.5431,  936.9941,  942.4491,  947.9082,  953.3712,  958.8382,964.3093,  969.7844,  975.2636,  980.7467,  986.2338,  991.7251,  997.2203,1002.7196, 1008.2228, 1013.7300, 1019.2415, 1024.7567, 1030.2761, 1035.7996, 1041.3269, 1046.8584)

#creating a colorSpec object
spec_camera <- colorSpec::colorSpec(spectra, wl, quantity="reflectance", organization='auto', specnames="" )

#calculating the XYZ parameters
spec_camera_xyz<-colorSpec::product(colorSpec::D65.1nm, spec_camera, colorSpec::xyz1931.1nm,wave='auto' )

#calculating the L*a*b* parameters from the previous XYZ
spec_camera_lab<-spacesXYZ::LabfromXYZ( spec_camera_xyz/100, spacesXYZ::standardXYZ( 'D65' ))



#TO_DO section for the HUE and Chroma calculations


return(spec_camera_lab) #returns the L*a*b* parameters


}

  #######

#2 - calculating the parameters for each pixel

raster::beginCluster() #beginning a cluster for multicore processing

#executing the function defined above
output_raster<-raster::clusterR(raster,raster::calc,args=list(fun = lab_function))


raster::endCluster() #closing the cluster

#returning the L*a*b image
names(output_raster)<-c("Lightness","a","b")

return(output_raster)

}


######################################################################


#' @title Construct Lch images from an hyperspectral camera
#' @description This function takes as input an hyperspectral image and calculates the corresponding Lch parameters for each pixel, result in a stack of three images corresponding the each of the  Lch parameters.
#' @param raster Raster object with the camera file
#' @return The function returns a raster stack object with three layers: L, c and h
#' @keywords external
#' @export
#'

lch_raster<-function(raster){


  #1 - defining the function that raster::calc will use
  #this is the function that calculates Lhc from a spectra acquired with the camera

  lch_function<-function(spectra){

    #setting up the bands wl
    wl<- c(376.5555, 381.5800, 386.6085, 391.6411,  396.6777,  401.7183,  406.7629, 411.8116,  416.8643,  421.9210,  426.9817,  432.0464,  437.1152,  442.1880, 447.2648,  452.3457,  457.4305,  462.5194,  467.6124,  472.7093,  477.8103,482.9152,  488.0243,  493.1373,  498.2543,  503.3754,  508.5005,  513.6296,518.7628,  523.9000,  529.0411,  534.1863,  539.3356,  544.4888,  549.6461,554.8074,  559.9727,  565.1421,  570.3155,  575.4929,  580.6743,  585.8597,591.0492,  596.2427,  601.4402,  606.6417,  611.8473,  617.0569,  622.2705,627.4882,  632.7098,  637.9354,  643.1652,  648.3989,  653.6367,  658.8784,664.1242,  669.3740,  674.6278,  679.8857,  685.1476,  690.4135,  695.6834,700.9573,  706.2354,  711.5173,  716.8033,  722.0934,  727.3875,  732.6856,737.9877,  743.2938,  748.6040,  753.9182,  759.2364,  764.5586,  769.8848,775.2151,  780.5494,  785.8878,  791.2301,  796.5765,  801.9269,  807.2812,812.6398,  818.0021,  823.3687,  828.7392,  834.1136,  839.4922,  844.8748,850.2614,  855.6520,  861.0466,  866.4454,  871.8480,  877.2548,  882.6655,888.0802,  893.4991,  898.9218,  904.3487,  909.7795,  915.2144,  920.6533,926.0962,  931.5431,  936.9941,  942.4491,  947.9082,  953.3712,  958.8382,964.3093,  969.7844,  975.2636,  980.7467,  986.2338,  991.7251,  997.2203,1002.7196, 1008.2228, 1013.7300, 1019.2415, 1024.7567, 1030.2761, 1035.7996, 1041.3269, 1046.8584)

    #creating a colorSpec object
    spec_camera <- colorSpec::colorSpec(spectra, wl, quantity="reflectance", organization='auto', specnames="" )

    #calculating the XYZ parameters
    spec_camera_xyz<-colorSpec::product(colorSpec::D65.1nm, spec_camera, colorSpec::xyz1931.1nm,wave='auto' )

    #calculating the L*a*b* parameters from the previous XYZ
    spec_camera_lab<-spacesXYZ::LabfromXYZ( spec_camera_xyz/100, spacesXYZ::standardXYZ( 'D65' ))

    spec_camera_lab <- as.data.frame(spec_camera_lab)

    #chroma
    chroma<-spacesXYZ::LCHabfromLab(c(spec_camera_lab$L,spec_camera_lab$a, spec_camera_lab$b))


    return(chroma) #returns the L*a*b* parameters


  }

  #######

  #2 - calculating the parameters for each pixel

  raster::beginCluster() #beginning a cluster for multicore processing

  #executing the function defined above
  output_raster<-raster::clusterR(raster,raster::calc,args=list(fun = lch_function))


  raster::endCluster() #closing the cluster

  #returning the L*a*b image
  names(output_raster)<-c("Lightness","c","h")

  return(output_raster)

}
