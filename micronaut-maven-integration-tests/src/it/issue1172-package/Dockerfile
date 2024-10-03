# use the GCR mirror registry to ensure authentication failure
FROM mirror.gcr.io/eclipse-temurin:17-jre
WORKDIR /home/app
COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
EXPOSE 8080
ENTRYPOINT ["java", "-cp", "/home/app/libs/*:/home/app/classes/", "io.micronaut.build.examples.Application"]
