File log = new File(basedir, 'build.log')
assert log.exists()
assert log.text.contains("Using BASE_IMAGE_RUN: frolvlad/alpine-glibc:alpine-3.12")
assert log.text.contains("Successfully tagged alvarosanchez/demo:0.1")