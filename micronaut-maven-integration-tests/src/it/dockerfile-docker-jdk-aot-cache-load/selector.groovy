// A training run that does not start the application needs a Micronaut core version with
// micronaut.application.training.mode (micronaut-projects/micronaut-core#13402). The project runs when
// it.micronaut.core.trainingMode.version names one, see the pom.xml of the integration tests.
return binding.hasVariable('trainingModeCoreVersion') && trainingModeCoreVersion as boolean
