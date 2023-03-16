# OpenNMS Helm Entity Datasource

This plugin is used with the overall OpenNMS Helm Application, and this README is an effort to break down the construction of the Entity Datasource plugin, 
how it works, and how to modify it.

# Architecture
Like with all Grafana Datasource plugins, the plugin itself is defined in the plugin.json file at the root of this directory, and the bootstrap
file that Grafana looks for to kick off the whole plugin is module.ts. Those two files will be consistent in every Grafana plugin, and are good place to start in order to understand how everything is loaded and run.

## module.ts
Within module.ts, you will see references to three different files in this plugin directory, mainly EntityDataSource.ts, EntityConfigEditor.tsx, and EntityQueryEditor.tsx. Let's break those down.

## EntityConfigEditor
This is the most basic of the three files, it only includes the Grafana-UI provided DataSourceHttpSettings which gives us the ability to configure the Datasource to connect to our OpenNMS instance via basic auth params. To see this file in action, within Grafana, hover on the Gear icon -> Datasources. Then either click on an existing instance of `OpenNMS Entities` or create a new Datasource with `Add data source` and select `OpenNMS Entities`. On the screen to configure the plugin, you'll see an area for a URL and an option to select Basic Auth. That's DataSourceHttpSettings.

## Configuring OpenNMS Entities
While on the configuration options for OpenNMS Entities (see step above), you will see fields for a URL, and the option for Basic Auth. Enable Basic Auth, enter the url of your opennms instance (usually ends with opennms/) and provide a username/password with access to the REST role. An ADMIN role by default has access to the API via the REST role.

## EntityQueryEditor
The EntityQueryEditor is our top-level file to build out our Entity Query. To visually see the contents of this file, create a new Dashboard + Panel in Grafana, and then select 'OpenNMS Entity' as your datasource. In the bottom portion of the screen under the visual area/graph, you will see a Select dropdown where you can select Alarms/Nodes/IP Interfaces... . Everything in this bottom panel is driven by EntityQueryEditor.

The structure of this file is relatively basic with the exception of the EntityClauseEditor, which we'll get into next.

EntityQueryEditor allows the user to select which type of entity they want to search for in the first Segment Section, and it also allows you to select the maximum number of results via Limit, how you want the results ordered (DESC or ASC), and if you want to include Featured Attributes or not.

The EntityClauseEditor is where the real complexity lies, as you can build out a relatively complex filter with sub restrictions and clauses. As you add new clauses and restrictions, the EntityClauseEditor will also update and generate an OpenNMS-JS ready API.Filter, which will be passed to the datasource for querying.

Each attribute provided by the OpenNMS API comes with its own set of comparators, and selectable values. It also tracks what type of value the BE is looking for, and will adjust to either a regular string based input, number based input, date based input, and pre-defined list of options as with Severity under Alarms.

### How does the EntityQueryEditor get data to the query method EntityDataSource?
Within the EntityQueryEditor file, you'll see an internal function called updateQuery, this is called by a debounced React useEffect. This is called anytime a value is changed in any of the dropdown/select fields. When onChange/ and onRunQuery are called in succession, control of the application is temporarily ceded to EntityDataSource.

## EntityDataSource
This file is responsible for actually querying our data source. It uses which type of Entity has been selected in EntityQueryEditor, and the provided filter/limit/order options to determine which entity to query, and how.

In order to keep the file clean and condensed for new developers, any method not directly related to querying has been moved to helper files. So when opening EntityDataSource, you should only see a few methods.

### Why is this file a class instead of function component like the other files?
Grafana seems to have a requirement to build a new Class for the Datasource file. We attempted to work around this for consistency sake and more junior developers who might not be as familiar with class based components, but this breakdown should help figuring out how it works.

When the Datasource is initialized (elsewhere in Grafana code, you don't have to do this), the constructor is called, and we set a few local variables, and create both our ClientDelegate and SimpleOpenNMSRequest objects and assign them to the class for use later.

There are only two other methods in this file, one called query, and another called testDatasource. The latter is self explanatory, it's what is called when the user clicks "Save & Test" when saving Datasource options within the Gear/Config menu. When testDataSource is called, we simply check that our ClientDelegate can connect to the server, and responds with a success if that's true. The query method is described below.

### Datasource Query
The query method in our EntityDataSource file determines which type of Entity has been selected, and then shuffles control over to Entity specific methods under the queries directory. Those query methods request data from specific endpoints via ClientDelegate, specify the columns to be shown for this entity, and build out the row data specific to those columns before returning control to the query method. Since we're not longer doing any DOM related work via the Entity Classes like in the previous version of the plugin, we didn't feel it was necessary to have entire classes dedicated to each entity type, instead just having some basic functions to query an endpoint and return a defined datastructure was best.


### Okay, but what are the rest of the files in this directory.

## constants.ts
Just contains a few constants that are shared between files.

## EntityClause.tsx
This is an individual clause, that will appear once for each row in the where clauses/restrictions. We've broken this file out so that the logic to manage the list of clauses is separated from the logic to manage an individual row. Depending on the type of restriction (top-level vs sub-level, AND vs OR, nested) we'll see different labels to the left of the clause, but after that they're fairly consistent. You select an attribute (which are provided by the BE for each entity type). The attribute you select then determines which comparator you can pick in the next box. The attribute you select will also determine the second input field, which will either be a default input, number input, data input, or select dropdown depending on the type of data the BE is expecting (which it provides).

Any time that you modify a value in an EntityClause, it will pass the data up to the EntityClauseEditor, which is in charge of generating a valid OpenNMS API.Filter with the changes in a React useEffect method, which then creates the API.Filter and passes it up to EntityQueryEditor, which then bundles that along with the selected entity type, order by restrictions and limits, and calls the query method via onRunQuery and onChange provided by props to the EntityQueryEditor.

## EntityClauseEditor
As mentioned above, this contains the logic to manage a list of EntityClauses, which are converted into a valid OpenNMS API.Filter when a clause changes. It will pass the API.Filter up to EntityQueryEditor.

## EntityClauseLabel
In order to separate the concern of how to properly display the EntityLabel from managing the EntityClause itself, that logic was broken out into its own component. The EntityLabel is what appears to the left of a clause, and can either be 'WHERE' or if it's a secondary clause an AND/OR statement, or if it's a nested restriction, either 'WHERE' again if it's the first, or a space followed by AND/OR if it's a secondary nested clause. That logic is complicated enough we felt it was worth breaking out into its own component.

## EntityHelper
Just a few methods that are really to support the EntityDataSource class component, but they are easier to test if broken out of context of that Class component, and properly typed with input parameters.

## useEntityProperties
A bit of logic to gather up and display the attributes for the selected entity. Since this is only used in the EntityQueryEditor, it could be brought directly into that file, but by breaking it out into its own file, now this file only has one concern. Getting the EntityProperties for the selected entity.

## How do I make changes to this plugin?

### Adding a new Entity Type
If you want to add a new Entity Type, you'll want to first want to edit EntityQueryEditor.tsx and add a new entry under options under the SegmentSection for 'Select'. Then, in EntityHelper.tsx file, you'll want to add a new entry in the queryEntity select case statement. And then finally you'll want to write a new query method, which will query an OpenNMS endpoint, determine the columns to be shown, and build up the row data with the response from OpenNMS.

### Fixing a bug with how the UI is rendering.
You'll probably want to look at EntityClause, EntityClauseEditor, or EntityConfigEditor first. And work your way up from there to determine where the issue lies.

### Fixing a bug with how the data is presenting.
You'll probably want to look at the queries directory, and the specific entity that's giving you problems. There's probably something wrong in the query function for the specific entity you're having trouble with.