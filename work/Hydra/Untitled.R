ExamplePleSpecs <- Hydra::loadSpecifications(fileName = system.file(
  fileName = "testdata/specifications/ExamplePleSpecs.json",
  package = "Hydra",
  mustWork = TRUE
))

Hydra::hydrate(
  specifications = ExamplePleSpecs,
  outputFolder = here::here("hydrated"),
  skeletonFileName = file.path(system.file("skeletons", package = "Hydra"), 
                               "ComparativeEffectStudy_v0.0.1.zip"),
  packageName = "testingPackage"
)





