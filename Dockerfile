FROM openjdk:17-jdk-slim

WORKDIR /app

COPY ./build/libs/*.jar app.jar

ENTRYPOINT ["java", "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005", "-jar", "/app/app.jar"]


#불용어 파일 추가한 엘라스틱서치 이미지
FROM docker.elastic.co/elasticsearch/elasticsearch:8.15.1

COPY elasticsearch/stopwords /usr/share/elasticsearch/config/stopwords



