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
  constructor(text, label, datasource, resource, inputType, entityType, id, selected) {
    this.id = id || randomString();
    this._text = text;
    this.label = label;

    this.datasource = datasource;
    this.resource = resource;
    this.inputType = inputType || 'multi';
    this.entityType = entityType;
    this.selected = selected || {};
  }

  get text() {
    return (this.label === undefined || this.label.trim().length === 0) ? this._text : this.label;
  }

  set text(text) {
    this._text = text;
  }
}