# Setting Up Visual Studio Code for Helm Development

Install the following extensions:

* Babel ES6/ES7
* ESLint

# Building Packages

1. docker run --rm -v `pwd`:/build -w /build --name helm node:10 yarn install --pure-lockfile
2. docker run --rm -v `pwd`:/build -w /build --name helm node:10 yarn build
3. docker run --rm -v `pwd`:/build -w /build opennmsbamboo/node-centos ./makerpm.js
4. docker run --rm -v `pwd`:/build -w /build opennmsbamboo/node-debian ./makedeb.js
