FROM eclipse-temurin:25-jre
WORKDIR /home/app
COPY classes /home/app/classes
COPY dependency/release/ /home/app/libs/
COPY dependency/snapshot/ /home/app/libs/
EXPOSE 8080
ENTRYPOINT ["java", "-cp", "/home/app/libs/*:/home/app/classes/", "io.micronaut.build.examples.Application"]
