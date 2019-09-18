import {UI} from '../UI'
import {API} from 'opennms';

export class OrderByMapping {
    constructor(uiSegmentSrv, entity) {
      this.uiSegmentSrv = uiSegmentSrv;
      this.entity = entity;
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
