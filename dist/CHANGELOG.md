### v9.0.3

Grafana Plugin for OpenNMS version 9.0.3 is a small release with one final breaking ID change based on Grafana's registry guidelines.

It contains a number of dependency security updates, as well as a few small bugfixes:

* [HELM-398](https://opennms.atlassian.net/browse/HELM-398): Alarm Table / Entity Datasource: Enable "Use Grafana User"
* [HELM-406](https://opennms.atlassian.net/browse/HELM-406): Adding a perf filter query to a dashboard elicits "TypeError: l is undefined"
* [HELM-411](https://opennms.atlassian.net/browse/HELM-411): Alarm datasource query not keeping proper equality

### v9.0.2

Grafana Plugin for OpenNMS version 9.0.2 is a bugfix release.

### v9.0.1

Grafana Plugin for OpenNMS version 9.0.1 is a re-release of 9.0.0, with a change to the plugin ID used for publication to Grafana's registry and updated logo.

### v9.0.0

Grafana Plugin for OpenNMS version 9 is a large refactor of the codebase from Helm 8.

Highlights include:
* rewrite all panels in React
* clean up a bunch of code under the covers to use newer Grafana datastructures
* lots of documentation updates and improvements
* and much, much more!

A **Dashboard Convert Panel** has been provided to migrate your dashboards from Helm 8.
Instructions on backing up your existing dashboards and upgrading can be found in [the Grafana plugin documentation](https://docs.opennms.com/grafana-plugin/latest/installation/upgrading.html).
