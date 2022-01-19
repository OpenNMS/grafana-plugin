import { Column } from '@grafana/data';
import { ClientDelegate } from 'lib/client_delegate';
import { AttributeMapping } from './mapping/AttributeMapping';
import { Filter } from './ui/Filter';

export default abstract class Entity {
  abstract name: string;
  abstract type: string;

  constructor(public client: ClientDelegate, public datasource: any) {
    if (!this.datasource) {
      throw new Error('Datasource is required!');
    }
  }

  abstract getAttributeMapping(): AttributeMapping;
  abstract getColumns(): Column[];
  abstract getProperties(): Promise<any[]>;
  abstract getPropertyComparators(attribute: any): Promise<any[]>;
  abstract findProperty(attribute: any): Promise<any>;
  abstract query(filter: Filter): Promise<any>;
}
