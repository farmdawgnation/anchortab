#!/bin/bash
echo "Assembling JAR and preparing WAR resources."
sbt resources:compile-sass resources:copy-scripts assembly package-war

echo "Adding webapp resources to JAR."
cd target
rm anchortab.jar
cp scala-2.10/anchortab.jar anchortab.jar
cp -r javascripts/* webapp/javascripts/
zip -r anchortab.jar webapp -x webapp/WEB-INF/classes\* webapp/WEB-INF/lib\*
cd ..

echo "Now anchortab.jar is ready for deployment."
