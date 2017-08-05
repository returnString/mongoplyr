#!/usr/bin/env sh
curl https://raw.githubusercontent.com/mongodb/docs-assets/primer-dataset/primer-dataset.json | mongoimport --drop --db mongoplyr_tests --collection restaurants
