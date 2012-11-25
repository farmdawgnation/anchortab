#!/bin/bash
echo "[deploy] Pulling latest code."
git pull

echo "[deploy] Deploying Resources"
sbt resources:deploy-resources

echo "[deploy] Packaging JAR"
./package.sh

echo "[deploy] Ready to restart server."
