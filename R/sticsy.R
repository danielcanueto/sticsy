#' Statistical total correlation spectroscopy with new dependence measures (distance correlation, distance covariance, MIC, gMIC, TIC)
#'
#' @param dataset Matrix of spectra x bins.
#' @param ppm ppm.
#' @param ppm_edges Left and right edges (in ppm) of the spectrum region to analyze.
#' @param method Dependence measure to use for correlation spectroscopy. Available options are 'pearson', 'spearman', 'dcor' (distance correlation), 'dcov' (distance covariance), 'MIC' (Maximal Information Coefficient), 'GMIC' (Generalized Maximal Information Coefficient) and 'TIC' (Total Information Coefficient).  By default TIC.
#' @param visualization_method How to visualize results in spectra dataset analyzed. 'median' shows the median spectrum, a numeric figure shows a random n number of spectra (it is recommended not to plot more than 10). By default 'median'.
#' @return Vector with results of correlation spectroscopy and Plotly interactive figure for visualization of results.
#' @export sticsy
#' @import plotly
#' @import energy
#' @import minerva
#'
#' @examples
#' data=MTBLS1()
#' results=sticsy(data$dataset,data$ppm,c(7.58,7.54),method='TIC',visualization_method='median')


sticsy = function(dataset, ppm,ppm_edges,method='TIC',visualization_method='median') {
      ROI_buckets = which.min(abs(as.numeric(ppm_edges[1])-ppm)):which.min(abs(as.numeric(ppm_edges[2])-ppm))

	if (method=='pearson') result=as.vector(cor(rowSums(dataset[,ROI_buckets,drop=F]),dataset))
	if (method=='spearman') result=as.vector(cor(rowSums(dataset[,ROI_buckets,drop=F]),dataset,method='spearman'))
	if (method=='dcor') result=apply(dataset,2,function(x)dcor(rowSums(dataset[,ROI_buckets,drop=F]),x))
	if (method=='dcov') {
	scaled_dataset=scale(dataset)
	result=apply(scaled_dataset,2,function(x)dcov(rowSums(scaled_dataset[,ROI_buckets,drop=F]),x))
	}
	if (method=='MIC') result=apply(dataset,2,function(x)unlist(mine(rowSums(dataset[,ROI_buckets,drop=F]),x,est="mic_e"))[1])
		if (method=='GMIC') result=apply(dataset,2,function(x)unlist(mine(rowSums(dataset[,ROI_buckets,drop=F]),x,est="mic_e"))[6])
	if (method=='TIC') result=apply(dataset,2,function(x)unlist(mine(rowSums(dataset[,ROI_buckets,drop=F]),x,est="mic_e"))[7])

	if (method=='ransy') result=ransy(dataset,ppm,ppm_edges)
     
      
      # result=result/max(result)
	  result= matrix(result,1,length(result))
if (visualization_method=='median') visual_dataset = matrix(apply(dataset,2,median),1,ncol(dataset))
if (is.numeric(visualization_method)) visual_dataset = dataset[sample(nrow(dataset),visualization_method),,drop=F]
  p=plot_ly(x=~ppm)
  for (i in seq(nrow(visual_dataset))) p=p%>%add_lines(y = visual_dataset[i,])

  p=p%>%layout(xaxis=list(title='ppm',range=c(max(ppm),min(ppm))),yaxis=list(title = "Intensity (arbitrary unit)"))
  p2 <- plot_ly(x=~ppm,z =result, colorscale = "Greys", type = "heatmap")%>%    layout(xaxis=list(title='ppm',range=c(max(ppm),min(ppm))))
  plot <- subplot(p, p2,nrows=2,heights=c(0.8,0.2),margin=0,shareX = T)
output=list(result=result,plot=plot)
	  return(output)
	  
    }
    
    ransy=function(dataset,ppm,ppm_edges) {
      ROI_buckets = which.min(abs(as.numeric(ppm_edges[1])-ppm)):which.min(abs(as.numeric(ppm_edges[2])-ppm))
      scaled_dataset=scale(dataset)

      one = matrix(rep(1, ncol(scaled_dataset)), nrow = 1)
      driver = rowSums(scaled_dataset[, ROI_buckets,drop=F]) %*% one
      R = colMeans(scaled_dataset/driver,na.rm=T)/apply(scaled_dataset/driver, 2, function(x)sd(x,na.rm=T))
      R[R==Inf]=max(R[is.finite(R)])
      R [is.na(R)]=0
      R=R/max(R,na.rm=T)
    }