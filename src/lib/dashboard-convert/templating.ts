import { getSourceDatasourceInfo } from './datasources'
import { DatasourceMetadata, DsType } from './types'
import { addVariationsToMap, getDatasourceTypeFromPluginId } from './utils'

// Parse the Dashboard 'templating' section, extracting any Datasource mappings and
// updating those items to use Version 9 versions
// The 'name' is the name of the template variable which may be used elsewhere in queries
// The 'query' should be the datasource ID
// We convert this to use the Version 9 versions, plus save off the variable in the map
// to substitute elsewhere
//
// "templating": {
//   "list": [
//     {
//       "current": {
//         "selected": true,
//         "text": "OpenNMS Performance",
//         "value": "OpenNMS Performance"
//       },
//       "hide": 2,
//       "includeAll": false,
//       "label": null,
//       "multi": false,
//       "name": "datasource",
//       "options": [],
//       "query": "grafana-plugin-performance-datasource",
//       "refresh": 1,
//       "regex": "",
//       "skipUrlSync": false,
//       "type": "datasource"
//     },
//   ...
//   ]
// }
export const parseTemplating = (source: any, datasourceMap: Map<string,DsType>, dsMetas: DatasourceMetadata[]) => {
  const result = {
    list: [] as any[]
  }

  const sourceList: any[] = source?.list || []

  for (const s of sourceList) {
    if (s.type === 'datasource' && s.query && s.query.startsWith('opennms')) {
      const target = { ...s }
      const name = s.name
      const pluginId = s.query

      // find corresponding Version 9 datasource info and substitute
      const { datasourceType } = getDatasourceTypeFromPluginId(pluginId)
      let dsMeta = dsMetas.find(d => d.datasourceType && d.datasourceType === datasourceType && d.pluginVersion === 9)

      if (!dsMeta) {
        console.log(`Dashboard convert: did not find Version 9 datasource for '${datasourceType}', falling back to first available:`)
        dsMeta = dsMetas.find(d => d.datasourceType && d.datasourceType === datasourceType)
      }

      if (dsMeta) {
        addVariationsToMap(name, datasourceType as DsType, datasourceMap)
        target.query = dsMeta.type

        if (s.current) {
          target.current.text = dsMeta.name
          target.current.value = dsMeta.name
        }

        result.list.push(target)
        continue
      }
    } else if (s.type === 'query') {
      const sourceDsInfo = getSourceDatasourceInfo(s, datasourceMap)

      if (sourceDsInfo.isOpenNmsDatasource && !sourceDsInfo.isTemplateVariable && sourceDsInfo.datasourceType) {
        const sourceDsMeta = dsMetas.find(d => d.datasourceType === sourceDsInfo.datasourceType && d.pluginVersion === 9)

        if (sourceDsMeta) {
          s.datasource = {
            type: sourceDsMeta.type,
            uid: sourceDsMeta.uid
          }
        }
      }
    }

    // was not an OpenNms datasource query, or we couldn't find a substitute, so just pass it through
    // or we fell through from 'query' block
    result.list.push(s)
  }

  return result
}
