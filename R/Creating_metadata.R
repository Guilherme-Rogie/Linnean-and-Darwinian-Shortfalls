library(EML)
attributes <-
  tibble::tribble(
    ~attributeName, ~attributeDefinition,                                                 ~formatString,         ~unit,   ~numberType,
    "Species",    "the list of the 85 New World coralsnake species.",                 NA, NA,       NA,
    "Description_date", "the date of description of each New World coralsnake species multiplied by -1.",    "-YYYY", NA,       NA,
    "Latitude",        "the latitude of the centroid of each New World coralsnake species.",        NA,   "decimal degrees",    "real"  ,
    "Longitude",   "the longitude of the centroid of each New World coralsnake species.",           NA,   "decimal degrees",    "real",
    "Range_size",     "the geographic range size of each New World coralsnake species.",           NA,      "square kilometers","real",
    "Synonyms",   "the number of synonyms for each New World coralsnake species.", NA,                             NA,       "real",
    "Body_size",    "the total body length of each New World coralsnake species.",       NA,                      "millimeter", "real",
    "Papers",    "the number of scientific papers for each New World coralsnake species.",     NA,                             NA,  "real",
    "Records","the number of occurrence records retrieved from GBIF, scientific papers, and natural
history museums for each New World coralsnake species.",NA,NA,"real",
    "DR","The estimated DR statistic for each New World coralsnake species.",NA,NA,"real",
    "Splitting","the average response of taxonomists regarding the likelihood of an accepted New
World coralsnake species being split after undergoing a taxonomic revision.",NA,NA,"real")

attributeList <- set_attributes(attributes, col_classes = c("character", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric","numeric"))
physical <- set_physical(here::here("data","raw", "coralsnakes_data_ITU.xlsx"))

dataTable <- list(
  entityName = "coralsnakes_data_ITU.xlsx",
  entityDescription = "creating the Index of Taxonomic Uncertainty",
  physical = physical,
  attributeList = attributeList)
geographicDescription <- "New World (USA - Argentina)"

keywordSet <- list(
    keywordThesaurus = "ITU coralsnakes",
    keyword = list("coralsnakes",
                   "Index of Taxonomic Uncertainty",
                   "New World")
  )
dataset <- list(
  keywordSet = keywordSet,
  coverage = geographicDescription,
  dataTable = dataTable)

eml <- list(
  packageId = uuid::UUIDgenerate(),
  system = "uuid", # type of identifier
  dataset = dataset)

eml_validate(eml)
write_eml(eml, here::here("data","raw", "metadata_ITU.xml"))
eml_validate(here::here("data","raw", "metadata_ITU.xml"))