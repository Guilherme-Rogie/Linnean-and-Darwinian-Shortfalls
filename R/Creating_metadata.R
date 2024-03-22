library(EML)
attributes <-
  tibble::tribble(
    ~attributeName, ~attributeDefinition,   ~numberType,
    "R2_lat_antes",    "Adjusted R for the linear model (formula: DR ~ latitude) before incorporating
the Index of",       "real",
    "lambda_lat_antes",        "Page’s for the linear model (formula: DR ~ latitude) before incorporating
the Index of Taxonomic Uncertainty in the simulations and before splitting the species in the
phylogeny.",    "real"  ,
    "alpha_lat_antes",   "for the linear model (formula: DR ~ latitude) before incorporating the Index
of Taxonomic Uncertainty in the simulations and before splitting the species in the phylogeny." ,"real",
    "beta_lat_antes",     " for the linear model (formula: DR ~ latitude) before incorporating the Index
of Taxonomic Uncertainty in the simulations and before splitting the species in the phylogeny.","real",
    "R2_quad_lat_antes",   "Adjusted R for the quadratic model (formula: DR ~ latitude + latitude2)
before incorporating the Index of Taxonomic Uncertainty in the simulations and before
splitting the species in the phylogeny.",       "real",
    "lambda_quad_lat_antes"," Page’s for the quadratic model (formula: DR ~ latitude + latitude2)
before incorporating the Index of Taxonomic Uncertainty in the simulations and before
splitting the species in the phylogeny.","real",
    "alpha_quad_lat_antes"," for the quadratic model (formula: DR ~ latitude + latitude2) before
incorporating the Index of Taxonomic Uncertainty in the simulations and before splitting the
species in the phylogeny.","real",
    "beta1_quad_lat_antes"," for the linear term of the quadratic model (formula: DR ~ latitude +
latitude2) before incorporating the Index of Taxonomic Uncertainty in the simulations and
before splitting the species in the phylogeny.","real",
    "beta2_quad_lat_antes"," for the quadratic term of the quadratic model (formula: DR ~ latitude
+ latitude2) before incorporating the Index of Taxonomic Uncertainty in the simulations and
before splitting the species in the phylogeny.","real",
    "R2_lat_depois","Adjusted R for the linear model (formula: DR ~ latitude) after incorporating the
Index of Taxonomic Uncertainty in the simulations and after splitting the species in the
phylogeny.","real",
    "lambda_lat_depois"," Page’s for the linear model (formula: DR ~ latitude) after incorporating
the Index of Taxonomic Uncertainty in the simulations and after splitting the species in the
phylogeny.","real",
    "alpha_lat_depois","for the linear model (formula: DR ~ latitude) after incorporating the Index
of Taxonomic Uncertainty in the simulations and after splitting the species in the phylogeny.","real",
    "beta_lat_depois","for the linear model (formula: DR ~ latitude) after incorporating the Index
of Taxonomic Uncertainty in the simulations and after splitting the species in the phylogeny.","real",
    "R2_quad_lat_depois","Adjusted R for the quadratic model (formula: DR ~ latitude + latitude2)
after incorporating the Index of Taxonomic Uncertainty in the simulations and after splitting
the species in the phylogeny.","real",
    "lambda_quad_lat_depois"," Page’s for the quadratic model (formula: DR ~ latitude + latitude2)
after incorporating the Index of Taxonomic Uncertainty in the simulations and after splitting
the species in the phylogeny.","real",
    "alpha_quad_lat_depois"," for the quadratic model (formula: DR ~ latitude + latitude2) after
incorporating the Index of Taxonomic Uncertainty in the simulations and after splitting the
species in the phylogeny.","real",
    "beta1_quad_lat_depois","for the linear term of the quadratic model (formula: DR ~ latitude +
latitude2) after incorporating the Index of Taxonomic Uncertainty in the simulations and after
splitting the species in the phylogeny.","real",
    "beta2_quad_lat_depois"," for the quadratic term of the quadratic model (formula: DR ~
latitude + latitude2) after incorporating the Index of Taxonomic Uncertainty in the simulations
and after splitting the species in the phylogeny.","real",
    "split","the number of splits in each simulation.","real",
    "index_run","the number of runs. For each of the 1000 phylogenetic trees, 100 runs were
performed.","real",
    "index_tree","the number of the phylogenetic tree. There are 1000 phylogenetic trees.","real")

attributeList <- set_attributes(attributes, col_classes = c( "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric",
                                                             "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                                             "numeric","numeric","numeric","numeric","numeric","numeric"))
physical <- set_physical(here::here("data","processe", "Simulation_results.csv"))

dataTable <- list(
  entityName = "Simulation_results.csv",
  entityDescription = "Output off simulation",
  physical = physical,
  attributeList = attributeList)

keywordSet <- list(
    keywordThesaurus = "Output and DR",
    keyword = list("coralsnakes",
                   "DR",
                   "New World",
                   "Distribution")
  )
dataset <- list(
  keywordSet = keywordSet,
  dataTable = dataTable)

eml <- list(
  packageId = uuid::UUIDgenerate(),
  system = "uuid", # type of identifier
  dataset = dataset)

eml_validate(eml)
write_eml(eml, here::here("data","processed", "metadata_output.xml"))
eml_validate(here::here("data","processed", "metadata_output.xml"))
