FROM clojure:lein-2.9.1 as build

COPY . /build/

WORKDIR /build
RUN lein uberjar

FROM openjdk:8u201-jre-alpine3.9

COPY --from=build /build/target/blaze-0.4-SNAPSHOT-standalone.jar /app/
COPY fhir /app/fhir/

WORKDIR /app

EXPOSE 80

CMD ["/bin/sh", "-c", "java $JVM_OPTS -jar blaze-0.4-SNAPSHOT-standalone.jar"]
