#!/bin/bash
echo "Building SASS and Coffee resources."
sbt resources:compile-sass resources:copy-scripts

./package.sh

echo "Ready to restart service."
sudo supervisorctl restart anchortabstaging
