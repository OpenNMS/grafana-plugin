# Setting Up Visual Studio Code for Helm Development

Install the following extensions:

* Babel ES6/ES7
* ESLint

# Building Packages

1. docker run --rm -v `pwd`:/build -w /build --name helm node:10 npm install
2. docker run --rm -v `pwd`:/build -w /build --name helm node:10 npm run build
3. docker run --rm -v `pwd`:/build -w /build opennmsbamboo/node-centos ./makerpm.js
4. docker run --rm -v `pwd`:/build -w /build opennmsbamboo/node-debian ./makedeb.js

# Building Documentation

1. docker run --rm -v $(pwd):/docs opennms/asciibinder package -l debug docs
