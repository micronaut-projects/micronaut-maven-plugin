FROM eclipse-temurin:25-jre
WORKDIR /home/app
COPY classes /home/app/classes
COPY dependency/release/ /home/app/libs/release/
COPY dependency/snapshot/ /home/app/libs/snapshot/
EXPOSE 8080
ENTRYPOINT ["java", "-cp", "/home/app/libs/release/*:/home/app/libs/snapshot/*:/home/app/classes/", "io.micronaut.build.examples.Application"]
