const randomString = () => {
  const chars = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXTZabcdefghiklmnopqrstuvwxyz'.split('');
  const length = 20;

  let str = '';
  for (let i = 0; i < length; i++) {
      str += chars[Math.floor(Math.random() * chars.length)];
  }
  return str;
}

export class FilterColumn {
  constructor(text, datasource, resource, inputType, entityType, id, selected) {
    this.id = id || randomString();
    this.text = text;

    this.datasource = datasource;
    this.resource = resource;
    this.inputType = inputType || 'multi';
    this.entityType = entityType;
    this.selected = selected || {};
  }
}