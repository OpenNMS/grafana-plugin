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
  id: string;
  multi?: boolean;
  type?: 'textbox' | 'query';
  private _text: string;

  constructor(text: string, public label: string|undefined, public datasource: string, public resource: string|undefined, public inputType: 'text'|'multi' = 'multi', public entityType: any, id?: string, public selected: any = {}) {
    this.id = id || randomString();
    this._text = text;
  }

  get text() {
    return (this.label === undefined || this.label.trim().length === 0) ? this._text : this.label;
  }

  set text(text) {
    this._text = text;
  }

  toJSON() {
    return {
      id: this.id,
      text: this._text,
      label: this.label,
      datasource: this.datasource,
      resource: this.resource,
      inputType: this.inputType,
      entityType: this.entityType,
      selected: this.selected,
    };
  }

  static fromJSON(col) {
    return new FilterColumn(col.text, col.label, col.datasource, col.resource, col.inputType, col.entityType, col.id, col.selected);
  }
}
