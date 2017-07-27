# OpenNMS Helm - PM/FM Console for Grafana [![CircleCI](https://circleci.com/gh/OpenNMS/grafana-opennms-helm-app.svg?style=svg)](https://circleci.com/gh/OpenNMS/grafana-opennms-helm-app)

## Issue Tracking

We use the Helm project in our [JIRA](https://issues.opennms.org/projects/HELM) instance to track bugs and enhancements related this to project.

## Development and Production Versions

Development is done in the `develop` (default GitHub) branch.
Commits to the `develop` branch are automatically compiled and committed to the `master` branch.
Releases will be tagged from the `master` branch and submitted to the [Grafana plugin repository](https://github.com/grafana/grafana-plugin-repository).

## Installing from source

### Requirements

* [Grafana 4.3.x](http://docs.grafana.org/installation) (tested with v4.3.1)
* [yarn](https://yarnpkg.com/en/docs/install) (tested with v0.24.5)
* [Node.js >= 6.x](https://nodejs.org/en/download) (tested with v6.10.3)

### Steps

Download the source tree into a subfolder called `opennms-helm-app` in Grafana's plugin directory i.e.:

```sh
mkdir -p /var/lib/grafana/plugins
cd /var/lib/grafana/plugins
git clone https://github.com/OpenNMS/grafana-opennms-helm-app.git opennms-helm-app
```

Compile the application:

```sh
cd /var/lib/grafana/plugins/opennms-helm-app
yarn
yarn build
```

Restart Grafana:

```
systemctl restart grafana-server
```

Enable the plugin by navigating to `Plugins`, `Apps` and then selecting `OpenNMS Helm`.

### Build with Docker

The source can be compiled from source with a Docker builder image.

```
docker build -t myopennmshelmimage .
```

will checkout the source from GitHub URL of the project, compile and installs the plugin.
The result is a runnable image which can be executed with

```
docker run -p 3000:3000 -t myopennmshelmimage
```

Login on http://your-ip:3000 with login admin:admin and go to Plugins and enable the Helm app.

IMPORTANT: The plugin directory location is changed from `/var/lib/grafana/plugins` to `/opt/grafana/plugins` because the base grafana image defines the `/var/lib/grafana` as a _VOLUME_ which can't be changed.

### Advanced Docker build

If you want to build an image based on a specific GitHub fork or branch you can use `--build-arg`:

```
docker build -t mycustomforkbranch \
            --build-arg OPENNMS_HELM_GIT_URL=https://github.com/OpenNMS/grafana-opennms-helm-app.git \
            --build-arg OPENNMS_HELM_GIT_BRANCH_REF=myBranch
```

This will create the plugin from the given GitHub URL and branch.

### Use Docker compose to build a service stack

There is an example `docker-compose.yml` which builds a whole service stack with a compiled _Grafana OpenNMS Helm App_ which can be tested.

Modify the build arguments in the `docker-compose.yml` if you want to build from a GitHub fork, branch or version tag.

With first running `docker-compose up -d` the _Grafana OpenNMS Helm App_ will be compiled from source.
Any run will use the compiled image from on the local system.
If you want to rebuild the image run `docker-compose build --no-cache` to rebuild the image with the build configuration in the given `docker-compose.yml`.

The URL for the data source in Grafana for the OpenNMS Horizon server is `http://opennms:8980/opennms`.
