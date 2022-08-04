#' List all dataset found on WEkEO, allowing you to select the right 'datasetId'.
#'
#' @description
#' Iterates all datasets made avialable by the WEkEO databroker.
#' \url{https://wekeo-broker-k8s.apps.mercator.dpi.wekeo.eu/databroker/datasets}
#'
#' @return
#' \code{data.frame}
#'
#' @author
#' Matteo Mattiuzzi
#'
#' @examples
#' \dontrun{
#' x <- listProducts()
#'
#' grep(x$datasetId,pattern="SENTINEL",value=TRUE,ignore.case = TRUE)
#'
#' x[grep(x$title,pattern="snow",ignore.case = TRUE),]
#' x[grep(x$title,pattern="vege",ignore.case = TRUE),]
#' x[grep(x$title,pattern="Sea Surface Temperature",ignore.case = TRUE),]
#'
#'}
#'
#' @export listWekeoProducts
#' @name listWekeoProducts

listProducts <- function()
{
  cat("Gathering the full list of datasets from WEkEO, this usually takes up to 1-2 minutes...")

  j <- 0
  g <- 0
  while(TRUE)
  {
    xu <- paste0('https://wekeo-broker-k8s.apps.mercator.dpi.wekeo.eu/databroker/datasets?page=',j,'&size=50')
    q  <- jsonlite::fromJSON(xu)

    if(j==0)
    {
      totalResults <- q$totItems
      maxPage      <- q$pages

      datasetId <- title <- vector(length = totalResults)
    }
    itemsInPage  <- q$itemsInPage

    for(i in 1:itemsInPage)
    {
      datasetId[g+i] <- q$content[[i]]$datasetId
      title[g+i]     <- q$content[[i]]$title
    }

    if(is.null(q$nextPage))
    {
      break
    }

    g <- g + itemsInPage
    j <- j + 1
  }

  if(totalResults==0)
  {
    return(NULL)
  }

  result   <- data.frame(title=title, datasetId=datasetId,stringsAsFactors = FALSE)
  return(result)
}


listWekeoDatasetIds <- function(token)
{
  cmdcurl <- Sys.which('curl')[[1]]
  x <- system(paste0(cmdcurl," -X GET --header 'Accept: application/json' --header 'Authorization: ",token,"' 'https://wekeo-broker-k8s.apps.mercator.dpi.wekeo.eu/databroker/catalogue/datasets'"),intern = TRUE)

  x <- x[-1]
  x <- x[-length(x)]
  x <- gsub(x,pattern = ' ',replacement = '')
  x <- gsub(x,pattern = ',',replacement = '')
  x <- gsub(x,pattern = '\\\"',replacement = '')

  return(x)
}

#' List all dataset found on WEkEO.
#'
#' @description
#' Extract file location information (sources and dests) from the API reply
#'
#' @param x \code{character}. The return of \code{\link{composeUrl}} or the REST
#' query generated from the portal \url{https://cryo.land.copernicus.eu/finder/}.
#' @param as.data.frame \code{logical}. FALSE. If true the return is converted to
#' a data.frame
#'
#' @return
#' \code{list} or data.frame if \code{as.data.frame==TURE}
#'
#' @author
#' Matteo Mattiuzzi
#'
#' @examples
#' \dontrun{
#' x <- listWekeoProducts()
#' x[1:3,]
#'
#'}
#'
#' @export listWekeoProducts
#' @name listWekeoProducts


queryMetadata <- function(datasetId, token)
{
  stopifnot(missing(datasetId))
  #stopifnot(missing(token))

  cmdcurl <- Sys.which('curl')[[1]]

  url    <- "' 'https://wekeo-broker.apps.mercator.dpi.wekeo.eu/databroker/querymetadata/"
  preurl <- " -X GET --header 'Accept: application/json' --header 'Authorization: "
  id     <- gsub(datasetId, pattern = ":", replacement = "%3A")

  urlstring <- paste0(cmdcurl, preurl, token, url, id, "'")
  q         <- system(urlstring, intern=TRUE)
  q         <- jsonlite::fromJSON(q)

  return(q)
}

queryProduct <- function(datasetId,token)
{
  stopifnot(missing(datasetId))
  #stopifnot(missing(token))

  cmdcurl <- Sys.which('curl')[[1]]

  url    <- "' 'https://wekeo-broker-k8s.apps.mercator.dpi.wekeo.eu/databroker/datasets/"
  preurl <- " -X GET --header 'Accept: application/json' --header 'Authorization: "
  id     <- gsub(datasetId, pattern = ":", replacement = "%3A")

  urlstring <- paste0(cmdcurl, preurl, token, url, id, "'")
  q         <- system(urlstring, intern=TRUE)
  q         <- jsonlite::fromJSON(q)

  return(q)
}

dataRquest(datasetId,token)
{
  cmdcurl <- Sys.which('curl')[[1]]
  preurl  <- paste0(" -X POST --header 'Content-Type: application/json' --header 'Accept: application/json' --header 'Authorization: ",token,"' -d '{ ")

  req <- toJSON(list(datasetId=datasetId,dateRangeSelectValues=list(end= "2018-12-28T19:40:00.000Z", name ="dtrange",start= "2018-12-28T00:00:00.000Z"), stringChoiceValues=""),pretty = T)
  url <- " 'https://wekeo-broker-k8s.apps.mercator.dpi.wekeo.eu/databroker/datarequest"

  urlstring <- paste0(cmdcurl, preurl, token, req, url,"'")
  q         <- system(urlstring, intern=TRUE)

  q         <- jsonlite::fromJSON(q)
}


curl -X POST --header 'Content-Type: application/json' --header 'Accept: application/json' --header 'Authorization: 61603017-6b70-3092-8342-f3e6793ed16d' -d
'{ \
  "datasetId": "EO:CRYO:DAT:HRSI:FSC", \
  "dateRangeSelectValues": [ \
                            { \
                              "end": "2018-12-28T19:40:00.000Z", \
                              "name": "dtrange", \
                              "start": "2018-12-28T00:00:00.000Z" \
                            } \
                            ], \
  "stringChoiceValues": [] \
}'

postDatarequest {
  boundingBoxValues (Array[BoundingBoxValue], optional),
  datasetId (string),
  dateRangeSelectValues (Array[DateRangeSelectValue], optional),
  multiStringSelectValues (Array[MultiStringSelectValue], optional),
  stringChoiceValues (Array[StringChoiceValue], optional),
  stringInputValues (Array[StringInputValue], optional)
}BoundingBoxValue {
  bbox (Bbox, optional),
  name (string, optional)
}DateRangeSelectValue {
  end (string),
  name (string, optional),
  start (string)
}MultiStringSelectValue {
  name (string),
  value (Array[string], optional)
}StringChoiceValue {
  name (string),
  value (string)
}StringInputValue {
  name (string),
  value (string)
}Bbox [
  number
  ]

curl --request POST \
--url https://wekeo-broker.apps.mercator.dpi.wekeo.eu/databroker/datarequest
--header 'authorization: <access_token>'
--header 'content-type: application/json'
--data '{
    "datasetId": "EO:ESA:DAT:SENTINEL-1:SAR",
    "boundingBoxValues": [
      { "name": "bbox", "bbox": [1.13, 43.9, 1.53, 43.68] }
    ],
    "dateRangeSelectValues": [
      {
        "name": "dtrange",
        "start": "2020-01-01T00:00:00.000Z",
        "end": "2020-01-12T00:00:00.000Z"
      }
    ],
    "stringChoiceValues": [
      { "name": "producttype", "value": "GRD" }
    ]
  }'


