#!/bin/bash

PROJECT_DIR=$( dirname "${BASH_SOURCE[0]}" )
PKG_INFO=(./package.json)
VERSION=$(jq ".version" ${PKG_INFO})
echo "${VERSION//\"/}" > version.tag
