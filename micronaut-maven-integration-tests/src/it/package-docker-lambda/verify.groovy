File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Using base image: public.ecr.aws/lambda/java:${System.getProperty("java.specification.version")}")
assert log.text.contains("Built image to Docker daemon as alvarosanchez/package-docker-lambda:0.1")