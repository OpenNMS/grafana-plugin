#### Names and labels
- If no label is specified, the query will be identified using the name of the attribute.
- All of the series must be uniquely identified, otherwise the query will fail.

#### Attributes
- Click on the label with the icon to search the available values in the node, resource and attributes fields.
- When searching for a node you can match on any of these fields: label, sysName, ip address, hostname, foreign id

#### Expressions
- Expressions can be used to perform calculations or simple manipulation on previous queries.
- Within the expression, you can refer to other queries using their name, however these must not contain any spaces.
- Values that were typically stored in strings.attributes can be reference using $label.$property i.e. ifInOctets.ifSpeed

#### Filters
- Filters do not support templated values.
- Filters using the R backend required 'Rscript' to be available on OpenNMS server's system path.

#### Template queries
- Query nodes using nodeFilter($filter), where $filter is a valid <a target="_blank" href="https://www.opennms.org/wiki/Filters">OpenNMS filter</a>
- Query child resources using nodeResources($nodecriteria) where $nodecriteria is the node id, or foreign source and foreign id
- Examples
  - nodeFilter(catincProduction & catincLinux)
  - nodeFilter((isSMTP | isPOP3 ) & (categoryName == 'Production'))
  - nodeResources(1)
  - nodeResources(FS:FID)


#### Documentation links

[OpenNMS Wiki Grafana introduction](https://wiki.opennms.org/wiki/Grafana)

[Official OpenNMS Helm Documentation](https://docs.opennms.org/helm/releases/latest/helm/latest/welcome/index.html)
