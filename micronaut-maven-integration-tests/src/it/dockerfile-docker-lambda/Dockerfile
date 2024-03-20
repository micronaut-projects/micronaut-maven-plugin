FROM public.ecr.aws/lambda/java:21
WORKDIR /home/app
COPY classes /home/app/classes
COPY dependency/* /home/app/libs/
EXPOSE 8080
ENTRYPOINT ["java", "-cp", "/home/app/libs/*:/home/app/classes/", "io.micronaut.function.aws.runtime.MicronautLambdaRuntime"]
