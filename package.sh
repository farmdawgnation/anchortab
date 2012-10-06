#!/bin/bash
echo "Assembling JAR and preparing WAR resources."
sbt assembly package-war

echo "Adding webapp resources to JAR."
cd target
zip -r anchortab.jar webapp -x webapp/WEB-INF/classes\* webapp/WEB-INF/lib\*
cd ..

echo "Now anchortab.jar is ready for deployment."
