save_object <- function(object, file){
  # object is the object you want to save
  # file is just the name of the file, the function will handle finding the right directory
  
  # allocate the file to the right directory
  if (Sys.info()[6] == "Dan"){
    file <- paste(getwd(), file, sep = "/")
  } else {
    file <- paste(getwd(), file, sep = "/")
    
  }
  
  # remove old versions
  if (file.exists(file)){
    file.remove(file)
  }
  
  # save the new output
  saveRDS(object, file)
}
