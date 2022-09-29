# OpenNMS Helm Alarm Histogram Panel - React

This plugin is used with the overall OpenNMS Helm Application, and this README is an effort to break down the construction of the plugin, 
how it works, and how to modify it.

# Architecture
Like with all Grafana Panel plugins, the plugin itself is defined in the plugin.json file at the root of this directory, and the bootstrap
file that Grafana looks for to kick off the whole plugin is module.ts. Those two files will be consistent in every Grafana plugin, and are good place to start in order to understand how everything is loaded.


## module.ts
This file sets up our main control file (AlarmHistogramControl) and specifies two custom editors, which will allow us to set some options for our panel (AlarmGroupEditor, AlarmDirectionEditor).

## AlarmHistogramControl
This is a very basic file, as it just acts as a ferry between Grafana provided data, and the Grafana provided plugin to plot a Grafana Data Frame.

## alarmhooks.ts
The majority of our logic to transform the data slightly between the Data Frame and the Jquery Plottr so that we're viewing the data requested by the user (as set in AlarmGroupEditor and AlarmDirectionEditor). This hook extracts data from the data frame, and provides it back to the callee, in our case AlarmHistogramControl. This seperation allows us to focus on the splitting data logic from the plotting logic.

## Configuration Items
To view the configuration items below, add a new Alarm Histogram - React panel or edit an existing one. On the right side of the screen under the panel options, you should see a sectionf or 'Alarm Histogram React' which has the two dropdowns as configured below.

## AlarmGroupEditor
This allows the user to select between Severity and Acknowledged data.

## AlarmDirectionEditor
This allows the user to select between Horitontal and Vertical data direction.
