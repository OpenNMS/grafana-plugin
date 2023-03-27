import { DatasourceMetadata, DsType } from './types'

// Convert any legacy Filter Panels to v9, otherwise just return existing panel object
export const convertFilterPanels = (panels: any[], datasourceMap: Map<string, DsType>, dsMetas: DatasourceMetadata[]) => {
  const convertedPanels: any[] = []

  for (const p of panels) {
    let panel = p
    
    if (isLegacyFilterPanel(p)) {
      panel = convertLegacyFilterPanel(p, datasourceMap, dsMetas)
    }

    // recursively process panel panels
    if (p.panels) {
      panel.panels = convertFilterPanels(p.panels, datasourceMap, dsMetas)
    }

    convertedPanels.push(panel)
  }

  return convertedPanels
}

const isLegacyFilterPanel = (panel: any) => {
  return panel && panel.type && panel.type === 'opennms-helm-filter-panel'
}

// NOTE: Selected Filter Panel values will be lost, as they are now saved in
// localStorage instead of Dashboard json.
// New activeFilter values look something like:
//
// {
//   "attribute": {
//     "id": "label",
//     "label": "Label",
//     "name": "Label",
//     "orderBy": true,
//     "type": {
//       "id": "STRING",
//       "label": "string"
//     }
//   },
//   "entity": {
//     "label": "Nodes",
//     "value": "1"
//   },
//   "selectionType": {
//     "label": "Multi",
//     "value": "multi"
//   }
// }
const convertLegacyFilterPanel = (p: any, datasourceMap: Map<string, DsType>, dsMetas: DatasourceMetadata[]) => {
  const panel = { ...p }

  const panelDsMeta = dsMetas.find(d => d.datasourceType === 'entity' && d.pluginVersion >= 9)
  panel.type = 'opennms-filter-panel'
  delete panel.targets

  panel.options = {
    filterEditor: {
      activeFilters: [],
      datasource: {
        label: 'OpenNMS Entites',
        value: {
          name: panelDsMeta?.name || 'OpenNMS Entities',
          type: panelDsMeta?.type || 'opennms-entity-datasource',
          uid: panelDsMeta?.uid || ''
        }
      }
    }
  }

  // Note, setting attribute.type to 'string', even though the attribute may actually be an 'integer'
  // type. See 'hooks/useEntityProperties' if we need to find the correct type via an API call.
  for (const column of (p?.columns as any[])) {
    const filter = {
      altColumnLabel: column.label || undefined,
      attribute: {
        id: column.resource,
        label: column.text,
        name: column.text,
        type: {
          id: 'STRING',
          label: 'string'
        }
      },
      entity: convertLegacyEntityType(column.entityType.id || 'node'),
      selectionType: convertLegacySelectionType(column.inputType || 'single')
    }

    panel.options.filterEditor.activeFilters.push(filter)
  }

  delete panel.columns

  return panel
}

// convert legacy 'datasources/entity-ds/datasources.ts' entityTypes id
// to React hooks/useEntities 'entities' value
const convertLegacyEntityType = (entityId: string) => {
  switch (entityId) {
    case 'alarm':
      return { label: 'Alarms', value: '0' }
    case 'ipInterface':
      return { label: 'IP Interfaces', value: '2' }
    case 'snmpInterface':
      return { label: 'SNMP Interfaces', value: '3' }
    case 'monitoredService':
      return { label: 'Monitored Services', value: '4'}
    case 'outage':
      return { label: 'Outages', value: '5' }
    case 'node':
    default:
      return { label: 'Nodes', value: '1' }
  }
}

const convertLegacySelectionType = (inputType: string) => {
  switch (inputType) {
    case 'multi':
      return {
        label: 'Multi',
        value:' multi'
      }
    case 'text':
      return {
        label: 'Text',
        value:' text'
      }
    case 'single':
    default:
      return {
        label: 'Single',
        value:' single'
      }
  }
}
