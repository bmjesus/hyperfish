#' @title Construct L*a*b* images from a reflectance spectra
#' @description This function takes a reflectance spectra (R object ot file) as an input and calculates the corresponding L*a*b* parameters. TO DO - add info about the models and illuminants used to estimate XYZ and L*a*b*.  (TODO- add calculations for HUE and Chroma values)
#' @param wl vector with the wavelengths
#' @param spectra vector with the reflectance data
#' @param file string with the path for an ASD reflectance file
#' @return The function returns a list with: 1) a dataframe with the input data (wl and spectra); 2) a vector with the XYZ parameters; and 3) a vector with the L*a*b* parameters
#' @keywords external
#' @export
#'

lab_function<-function(wl = NA, spectra = NA, file = NA){

#testing if there is an ASD file to open and extracting the wl and spectra
if (is.na(file) != TRUE){

#import ASD file
spec_asd <- read.table(file , header = TRUE)
names(spec_asd)<-c("wl","reflect")

#creating the wl and spectra objects
wl <- spec_asd$wl
spectra <- spec_asd$reflect

}

#producing a colorSpec object
spec_camera <- colorSpec::colorSpec(spectra, wl, quantity="reflectance", organization='auto' )

#plotting the spectra with the corresponding perceived color
plot(spec_camera)

#calculating the XYZ parameters using the D65 illuminant and the human vision model XYZ 1931 1nm
spec_camera_xyz<-colorSpec::product(colorSpec::D65.1nm, spec_camera, colorSpec::xyz1931.1nm, wave='auto' )

#calculating the L*a*b* parameters using a D65 illuminant
spec_camera_lab<-spacesXYZ::LabfromXYZ( spec_camera_xyz/100, spacesXYZ::standardXYZ( 'D65' ))

#TO_DO section for the HUE and Chroma calculations



#output section
output<-list(as.data.frame(spec_camera),spec_camera_xyz,spec_camera_lab)

names(output) <- c("data","xyz","lab")

return(output)

}
