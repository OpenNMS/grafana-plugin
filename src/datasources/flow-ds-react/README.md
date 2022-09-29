# OpenNMS Helm Flow Datasource - React

This plugin is used with the OpenNMS Helm Application, and this README is an effort to break down the construction of the plugin, 
how it works, and how to modify it.

# Architecture
Like with all Grafana Datasource plugins, the plugin itself is defined in the plugin.json file at the root of this directory, and the bootstrap
file that Grafana looks for to kick off the whole plugin is module.ts. Those two files will be consistent in every Grafana plugin, and are good place to start in order to understand how everything is loaded.

## module.ts
Within module.ts, you will see references to three different files in this directory, mainly FlowDataSource, FlowConfigEditor, and FlowQueryEditor. Lets break those down.

## FlowConfigEditor

This is the most basic of the three files, it only includes the Grafana-UI provided DataSourceHttpSettings which gives us the ability to configure the Datasource to connect to our OpenNMS instance via basic auth params. To see this file in action, within Grafana, hover on the Gear icon -> Datasources. Then either click on an existing instance of `OpenNMS Flow React` or create a new Datasource with `Add data source` and select `OpenNMS Flow React`.

## Configuring OpenNMS Flow React
While on the configuration options for OpenNMS Flow React (see step above), you will see fields for a URL, and the option for Basic Auth. Enable Basic Auth, enter the url of your OpenNMS instance (usually ends with opennms/) and provide a username/password with access to the REST role. An ADMIN role by default has access to the API.

## FlowQueryEditor

The FlowQueryEditor is our top-level file to build out our Flow Query. To visually see the contents of this file, create a new Dashboard + Panel in Grafana, and then select 'OpenNMS Flow React' as your datasource. In the bottom portion of the screen under the visual area/graph, you will see a Select dropdown where you can select applications/conversations/hosts/dscps. Everything in the bottom panel is driven by FlowQueryEditor.

Once you've selected a flow type, you have the option to include a variety of query parameters, and optionally values to go with those parameters if they're supported. Each query parameter has a set of rules associated to it, so by selecting certain parameters will have different effects. Some generate a parameter field, others restrict other parameters, and some can be included twice. Those parameter rules are defined in the constants.ts file.

The structure of this file is relatively basic. We have a few methods to enable moving individual options either left or right in the list, or deleting them entirely. We also have a bit of logic for adding new query restritcions. A lot of field level control is passed down FlowQueryFunction, which we will talk about next.

## FlowQueryFunction
In order to sepearte the logic of managing all the selected restrictions/functions, and the logic of each individual function we broke out the field-level functionality into its own file. This file is responsible for displaying an individual restriction/function. This includes the "Tools" which allow you to move the function/restriction left and right, and also delete it from the list. It will also allow you to change the resriction/function chosen, and input any available parameter for the chosen function/restriction. If an available parameter does not have a list of predefined options, a basic text input field is shown, but if there are predefined options for this function/restriction `withDscp` for instance, those fields will query the server when clicked on, and then display a list of options to the user specific to their OpenNMS instance.

### How does the FlowQueryEditor get data to the query method FlowDataSource?
Within the FlowQueryEditor file, you'll see a React useEffect. This is called anytime a value is changed in any of the dropdown/select fields. When onChange/ and onRunQuery are called in succession, control of the application is temporarily ceded to FlowDataSource.

## FlowDataSource
This file is responsible for actually querying our data source. It uses which type of Flow has been selected in FlowQueryEditor, and the provided restrictions/functions are passed to individual ClientDelegate methods depending on the options selected.

In order to keep the file clean and condensed for new developers, any method not directly related to querying has been moved to helper files. So when opening FlowDataSource, you should only see a few methods.

### Why is this file a class instead of function component like the other files?
Grafana seems to have a requirement to build a new Class for the Datasource file. We attempted to work around this for consistency sake and more junior developers who might not be as familiar with class based components, but this breakdown should help figuring out how it works.

When the Datasource is initialized, the constructor is called, and we set a few local variables, and create both our ClientDelegate and SimpleOpenNMSRequest objects and assign them to the class for use later.

There are only two other methods in this file, one called query, and another called testDatasource. The latter is self explanatory, it's what is called when the user clicks "Save & Test" when saving Datasource options within the Gear/Config menu. When testDataSource is called, we simply check that our ClientDelegate can connect to the server, and responds with a success if that's true.

### Datasource Query
The query method in our FlowDataSource file determines which type of Flow has been selected, and then shuffles control over to Flow specific methods within the helpers.ts file. Those methods request data from specific endpoints via ClientDelegate, and then transforms that data into either series data or summary data depending on which options are selected in the functions/restrictions area of FlowQueryEditor.


### Okay, but what are the rest of the files in this directory you haven't touched on yet.

## constants.ts
Contains constants used in the rest of the application, and sets up the restrictions for which functions can be active at what time.

## helpers.ts
The majority of these functions would normally be part of the FlowDataSource class, but in order to ease unit testing, these functions were removed of any Class context, and broken out into strongly-typed decoupled methods. We've tried to include as much relevant info via JSDoc as possible for each method, to declare what it's doing and how it should be used.

## Tools.ts
The little blue buttons that show up when you hover on a function/restriction within FlowQueryEditor.

## useSegmentOptions
Breaking out the logic to determine which segment options (functions/restrictions) should be presented to the user at any time based on the existing selections.
This is used solely in FlowQueryEditor, but instead of cluttering up that file with multiple concerns, it's been broken out into its own hook to seperate that concern from the rest.

