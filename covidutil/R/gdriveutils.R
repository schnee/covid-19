
upload_images <- function(img_name) {
  cc <- googledrive::drive_find(pattern = "covid_img", n_max = 10)

  if(nrow(cc) < 2) {
    googledrive::drive_put(img_name, path = googledrive::as_id(cc$id), img_name)
  }
}

gauth <- function(email) {
  googledrive::drive_auth(email)
}
