FROM eclipse-temurin:25-jre
WORKDIR /home/app
COPY dependency/release/ /home/app/libs/release/
COPY dependency/snapshot/ /home/app/libs/snapshot/
COPY jdk-aot-cache/application.jar jdk-aot-cache/classpath jdk-aot-cache/training.sh /home/app/
RUN bash /home/app/training.sh train /home/app/app.aot 8080 180 sigterm '/hello' -- java -XX:+UseG1GC -cp @/home/app/classpath 'io.micronaut.build.examples.Application' && rm /home/app/training.sh
EXPOSE 8080
ENTRYPOINT ["java", "-XX:+UseG1GC", "-XX:AOTCache=/home/app/app.aot", "-cp", "@/home/app/classpath", "io.micronaut.build.examples.Application"]
