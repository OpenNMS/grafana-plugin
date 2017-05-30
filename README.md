# OpenNMS Helm - PM/FM Console for Grafana [![CircleCI](https://circleci.com/gh/OpenNMS/grafana-opennms-helm-app.svg?style=svg)](https://circleci.com/gh/OpenNMS/grafana-opennms-helm-app)


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
