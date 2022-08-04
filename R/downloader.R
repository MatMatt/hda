#' Downloads files from CLMS server
#'
#' @description
#' This function deals with the authenification and download the files.
#' (currently a bit hardcoded, will be improved soon)
#'
#' @param x \code{list}. The return of \code{\link{selector}} containint the
#' files to be donwloaded.
#' @param rootDir \code{character}. Default is current directory. Base location for the local archive. Inside
#' rootDir the function then creates a file structure similar to the one on the
#' server: e.g. (currently hard coded):
#' CLMS/Pan-European/HRL/Snow/FSC/2020/08/18/FSC_20200818T114635_S2B_T29UNV_V100_1.zip
#' @param user \code{character}. Your username on \url{https://cryo.land.copernicus.eu/finder/}
#' @param password \code{character}. Your password on \url{https://cryo.land.copernicus.eu/finder/}
#'
#' @return
#' \code{vector} with path/filename to the locally stored zip archives.
#'
#' @author
#' Matteo Mattiuzzi
#'
#' @examples
#' \dontrun{
#' x <- composeUrl(productType = 'FSC', productIdentifier = 'T29UNV',
#' startDate='2020-07-01', completionDate='2020-07-31')
#' x <- selector(x)
#' downloader(x, user='yourUser', password='yourPassword',rootDir=tempdir())
#' }
#'
#' @export downloader
#' @name downloader

downloader <- function(x , rootDir='./', user, password)
{
  if(is.null(x$localZip))
  {
    stop("x does not contain any files")
  }

  rootDir <- normalizePath(rootDir, winslash = '/')
  dir.create(rootDir,showWarnings = FALSE,recursive = TRUE)

  dest   <- paste0(rootDir, '/', x$localZip)
  exists <- checkZips(dest)

  while(sum(exists)!=length(exists)) # do auth and get Token only if download is needed
  {
    token <- token(user = user, password = password)
    cmdcurl <- Sys.which('curl')[[1]]

    for(i in seq_along(dest[!exists]))
    { # i=1
      dir.create(dirname(dest[!exists][i]), showWarnings = FALSE, recursive = TRUE)
      url <- paste0(x$downloadUrl[!exists][i],'?token=', token)
      cat(dest[!exists][i],'\n')

      system(paste(cmdcurl, url, "-o", path.expand(dest[!exists][i])))

      if(file.size(path.expand(dest[!exists][i])) < 0.9*x$fileSize[!exists][i])# If downloaded file is smaller than 90% of the expected size restart download.
      {
        break # break for i loop and one level up into the while loop
      }
    }
    exists <- checkZips(dest) # check which files need to be downloaded
  }
return(dest)
}

token <- function(user, password)
{
  if(missing(user))
  {
    stop('"user" is missing! If not registered yet, you can do that here: https://www.wekeo.eu/')
  }
  if(missing(password))
  {
    stop('"password" is missing! If not registered yet, you can do that here: https://www.wekeo.eu/')
  }
  cred <- caTools::base64encode(paste0(user,':', password))

  cmdcurl <- Sys.which('curl')[[1]]

  # I have my doubts that the handling of the token is properly done here.
  # But so far it seems to work...
  token <- system(paste0(cmdcurl," --request GET --header 'authorization: Basic ",cred,"' https://wekeo-broker.apps.mercator.dpi.wekeo.eu/databroker/gettoken"),intern = TRUE)

  if(length(grep(token, pattern = 'error'))!=0)
  {
    stop('Authentification error, please check your credentials for https://www.wekeo.eu/.')
  }

  token <- strsplit(token,split = '\"')[[2]][4]
  return(token)
}


acceptTermsWEkEO <- function(user, password)
{
  cmdcurl <- Sys.which('curl')[[1]]

  tac <- askYesNo("Before downloading data you need to accept the Terms and Conditions found here (this needs to be done only once): 'https://www.wekeo.eu/docs/what-are-the-data-offer-and-data-policy-of-wekeo'\nDo you accept them?")

  if(isTRUE(tac))
  {
    token <- token(user, password)
    x <- system(paste0(cmdcurl," --request PUT --header 'accept: application/json' --header 'authorization: ",token,"' --data 'accepted=true' https://wekeo-broker.apps.mercator.dpi.wekeo.eu/databroker/termsaccepted/Copernicus_General_License"),intern = TRUE)
    if(length(x)==0)
    {
      cat("You accepted the Terms and Conditions")
    } else
    {
      cat("Something went wrong, please check your username and password")
    }
  } else
  {
    cat("Terms and Conditions not accepted")
  }
}
