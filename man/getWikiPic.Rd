% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getWikiPic.R
\name{getWikiPic}
\alias{getWikiPic}
\title{getWikiPic}
\usage{
getWikiPic(
  x,
  width = 220,
  picSaveDir = tempdir(),
  quiet = T,
  openDir = F,
  clearCache = F
)
}
\arguments{
\item{x}{search term for finding a Wikipedia page}

\item{width}{the width of the downloaded image in pixels; default=220}

\item{picSaveDir}{the location to download images to; default=tempdir(); use paste0(getwd(),"/wiki") to save to a subfolder in your working directory or file.choose() to pic a location on the fly}

\item{quiet}{suppress verbose feedback? default=T}

\item{openDir}{open the picSaveDir location to browse images in finder/windows explorer? default=F}

\item{clearCache}{WARNING delete the picSaveDir? This will delete the whole directory, so never set picSaveDir to getwd(); default=F}
}
\description{
Get the thumbnail image from any Wikipedia page
}
\details{
Downloads the top right image in the infobox of a wikipage determined by your search term. Also caches this image so if you ask for the same image again, it doesn't download it.
}
