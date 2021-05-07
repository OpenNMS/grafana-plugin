import { API } from 'opennms';

import { UI } from '../UI'
import Entity from '../Entity';

export class OrderByMapping {
    constructor(public uiSegmentSrv: any, public entity: Entity) {
    }

    getUiOrderBy(apiOrderBy) {
      return new UI.OrderBy(this.uiSegmentSrv, apiOrderBy.attribute, false);
    }

    getApiOrderBy(uiOrderBy, order) {
      const uiAttribute = uiOrderBy.getAttribute();
      if (!uiAttribute) {
        return null;
      }

      const attribute = this.entity.getAttributeMapping().getApiAttribute(uiAttribute);
      return new API.OrderBy(attribute, API.Order.forLabel(order));
    }
}
