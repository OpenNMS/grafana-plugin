# OpenNMS Helm - PM/FM Console for Grafana [![CircleCI](https://circleci.com/gh/OpenNMS/opennms-helm.svg?style=svg)](https://circleci.com/gh/OpenNMS/opennms-helm)

Helm is a [Grafana](https://grafana.com/) application that allows users to create flexible dashboards using both fault management (FM) and performance management (PM) data from [OpenNMS® Horizon™](https://www.opennms.org) and/or [OpenNMS® Meridian™](https://www.opennms.com/).

It is the successor to the original [OpenNMS Datasource for Grafana](https://github.com/OpenNMS/grafana-opennms-datasource).

Consult the [Helm Documentation](http://docs.opennms.org/helm/branches/master/helm/latest/welcome/index.html) for help on installing, configuring and using the application.

## Features

### Fault Management (FM)

#### Flexible alarm filtering

Isolate the alarms you want displayed using custom filters and conditions that can match against over 150 different fields.

#### Configurable displays

Present the fields that are most relevant to you and your teams for improved tracking and triage.

#### Alarm interactions

Acknowledge, escalate and create tickets for alarms directly from the dashboard.

### Performance Management (PM)

#### Storage engine agnostic

Retrieve metrics stored in any of the available persistence engines i.e. rrdtool, JRobin and [Newts](https://github.com/OpenNMS/newts).

### Template support

Populate template variables with all of the nodes belonging to a set of categories, or leverage the complete [filter grammar](https://www.opennms.org/wiki/Filters).

### Trending and Forecasting

Remove outliers and perform trending or forecasting using the built-in series filters. Develop your own filters using Java or [R](https://www.r-project.org/).

Derive new series using
[JEXL](https://commons.apache.org/proper/commons-jexl/reference/syntax.html) expressions.

## Support Matrix

 * Performance Management Data Source
   * OpenNMS Horizon v16.x or greater
   * OpenNMS Meridian v2016.1.0 or greater
 * Fault Management Data Source
   * OpenNMS Horizon v20.1.x or greater
   * OpenNMS Meridian v2017.1.0 or greater

## Issue Tracking

We use the Helm project in our [JIRA](https://issues.opennms.org/projects/HELM) instance to track bugs and enhancements related this to project.

## Changelog

### v2.0.0

- Added a new datasource for querying flow data from OpenNMS
- Added support for "fallback" attributes to the performance datasource
- Added the ability to configure query timeouts for all of the datasources
- Require Grafana 5.x or greater

### v1.1.0

- Added support for custom actions in the Alarm Table panel
- Added the operator instructions field to the alarm details modal
- Updated the package dependencies to support Grafana 5.x
- Fixed a bug where long alarm descriptions and log messages would fill the alarm details modal
- Use consistent colors in both the Alarm Table and Alarm Histogram panels

### v1.0.0

- Initial release

