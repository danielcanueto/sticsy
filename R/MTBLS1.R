
#' Spectra dataset and ppm of MTBLS1 dataset. 
#'
#' @return Spectra dataset and ppm of MTBLS1 dataset.
#' @export MTBLS1
#'
#' @examples
#' data=MTBLS1()

MTBLS1 = function() {
    load(file.path(system.file(package = "sticsy"),"extdata","MTBLS1.RData"))
	return(saved_data)
	}