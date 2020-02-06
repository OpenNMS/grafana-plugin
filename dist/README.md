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

### v4.0.2

This release fixes a bug with the Flow datasource and Grafana 6.4, as well as some other cosmetic changes to the Flow query interface.

* [HELM-190: TypeError: Cannot read property 'toBits' of undefined](https://issues.opennms.org/browse/HELM-190)
* [HELM-192: Rendering Bug in Flow Query Builder](https://issues.opennms.org/browse/HELM-192)
* [HELM-193: Conversation Flow Histogram show "null" instead of Unknown/Other in legend](https://issues.opennms.org/browse/HELM-193)

### v4.0.1

This is a small revision to 4.0.0 which contains dependency updates and a [fix for running under Grafana 6.4](https://issues.opennms.org/browse/JS-45).

### v4.0.0

#### General

Helm 4 now requires Grafana 6.0 or higher.

#### Entities Data Source

The Fault Management Data Source is now the Entities Data Source.

In addition to querying alarms, it can now query nodes as well.
This is useful for filtering in variables, or can be used for just viewing and filtering nodes from within Grafana.

This is a breaking change that will require you to convert existing dashboards to use the new plugin.
For details on upgrading, see [the Helm documentation](http://docs.opennms.org/helm/releases/latest/helm/latest/installation/upgrading.html).

#### Flow Data Source

- Enhancements have been made to support Horizon 25's addition of "top N" queries for conversations, hosts, and applicattions.

#### Filter Panel

A new panel has been added that allows you to configure a series of variables to put in a dashboard.
This panel coordinates with any panels in the dashboard using the Entities Data Source to automatically apply filters matching those variables.

In the future this will be enhanced to do more complicated set operations than are possible using the built-in datasource variables.

#### Alarm Table

- Alarm table code has been sync'd with enhancements from the upstream Grafana table panel.
- The new default columns and formatting for the alarm table have been updated to be more useful.
- It is now possible to configure the alarm table to not automatically refresh when new data is updated if you have navigated away from the first page of the list. When new data arrives in the background, a refresh icon will appear in the page bar.
- String-based columns (like log message) now get a mouseover with the full text of the column.
- Severity columns have been enhanced, including basic support for themes.
- A new column type (`checkbox`) has been added that will render a boolean value as either a checkmark or empty string.
- Support has been added to tag a specific alarm as the "root cause" in a situation when providing feedback in the Alarm Details screen.
- A number of UI cleanups have been made to the Alarm Details screen, fixing word wrapping, resizing when the browser is resized, and more.
- The alarm table can now export CSV and Excel files.  Go to `<panel title>` -> `More...` -> `Export CSV` or `Export Excel`.

### v3.0.1

- Updated a ton of dependencies, there are now 0 warnings from `yarn audit` ([HELM-138](https://issues.opennms.org/browse/HELM-138))
- Fixes for running under Grafana 6 ([HELM-132](https://issues.opennms.org/browse/HELM-132))
- A fix for parsing nested parentheses when using label formatters ([HELM-131](https://issues.opennms.org/browse/HELM-131))
- Fixed relative date formatter initialization ([HELM-134](https://issues.opennms.org/browse/HELM-134)) (Thanks,  Brynjar!)
- Includes bug fixes from [OpenNMS.js v1.3.1](https://github.com/OpenNMS/opennms-js/releases/tag/v1.3.1)

### v3.0.0

#### General

- Improved error messages for incomplete or invalid queries

#### Performance Data Source

- Added support for overriding time intervals and max datapoints
- Labels are now shown in the order they were queried
- Labels can now be formatted using transformation functions like
  `nodeToLabel(<foreignSource:foreignId>)` and `resourceToName(<resourceId>)`
  (Horizon 24 or greater)
- Measurements API requests are now made in `relaxed` mode (if the server
  is missing a particular requested attribute, all others are still returned)

#### Flow Data Source

- Added additional transforms for flow data (`toBits`, `onlyIngress`, `onlyEgress`)
- The Flow Deep Dive dashboard axis labels have been updated to be more intuitive

#### Fault Management Data Source

- Sorting by numeric columns now works as expected
- HTML alarm (event) log messages are now rendered properly
- Alarm multi-select and deselect now works as expected
- "Severity" in the Alarm Table panel is now a normal column, rather than a "Severity icons" check
  box in the config options -- existing configs should be automatically upgraded
- Added support for reordering columns in the Alarm Table panel
- Added support for Situations (correlated alarms), including sending feedback on
  alarm correlations (Horizon 23 or greater)
- Added limited support for multi-select dashboard variables in the Alarm Table panel
- Added a custom `node` attribute in the Alarm Table panel that supports node criteria
  (either passing a `nodeId` or `foreignSource:foreignId` tuple)
- Many enhancements were made to the Alarm Detail view
  (full raw response viewing, related alarms, etc.)
- It is now possible to display dates as a relative time in the Alarm Table panel

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


## Build Information

- Build Date: 2020-02-06T20:36:07.935Z
- Git Revision: c884113158d8c737b52169a441b612126282a590

