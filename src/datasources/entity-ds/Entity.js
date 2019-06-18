export default class Entity {
  constructor(client, datasource) {
    this.client = client;
    this.datasource = datasource;
    if (!this.datasource) {
      throw new Error('Datasource is required!');
    }
  }

  getAttributeMapping() {
    throw new Error('Not yet implemented!');
  }

  getColumns() {
    throw new Error('Not yet implemented!');
  }

  getProperties() {
    switch (this.type) {
      case 'alarm': return this.client.getAlarmProperties();
      case 'node': return this.client.getNodeProperties();
    }
  }

  getPropertyComparators(attribute) {
    switch (this.type) {
        case 'alarm': return this.client.getAlarmPropertyComparators(attribute);
        case 'node': return this.client.getNodePropertyComparators(attribute);
    }
  }

  findProperty(attribute) {
    switch(this.type) {
      case 'alarm': return this.client.findAlarmProperty(attribute);
      case 'node': return this.client.findNodeProperty(attribute);
    }
  }

  query(filter) {  // eslint-disable-line
    throw new Error('Not yet implemented!');
  }

}