#!/bin/bash
echo "Assembling JAR and preparing WAR resources."
sbt resources:compile-sass resources:copy-scripts assembly package-war

echo "Adding webapp resources to JAR."
cd target
cp -r javascripts/* webapp/javascripts/
zip -r anchortab.jar webapp -x webapp/WEB-INF/classes\* webapp/WEB-INF/lib\*
cd ..

echo "Now anchortab.jar is ready for deployment."
