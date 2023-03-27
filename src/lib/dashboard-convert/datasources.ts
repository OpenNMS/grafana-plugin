import { isString } from 'lodash'
import { DataSourceInstanceSettings, DataSourceJsonData } from '@grafana/data'
import { DatasourceMetadata, DsType } from './types'
import { getDatasourceTypeFromPluginId } from './utils'

// Datasource info found in panel or target/query
interface SourceDatasourceInfo {
  isOpenNmsDatasource: boolean
  isTemplateVariable: boolean
  datasourceType: string
}

// Get datasource info from either a panel or a target
export const getSourceDatasourceInfo = (source: any, datasourceMap: Map<string,DsType>): SourceDatasourceInfo => {
  if (source && source.datasource) {
    if (isString(source.datasource) && datasourceMap.has(source.datasource)) {
      const dsType = datasourceMap.get(source.datasource) || ''

      return {
        isOpenNmsDatasource: true,
        isTemplateVariable: true,
        datasourceType: dsType
      }
    } else if (!isString(source.datasource) && source.datasource?.type) {
      const { datasourceType } = getDatasourceTypeFromPluginId(source.datasource.type)

      if (datasourceType) {
        return {
          isOpenNmsDatasource: true,
          isTemplateVariable: false,
          datasourceType
        }
      }
   }
  }

  return {
    isOpenNmsDatasource: false,
    isTemplateVariable: false,
    datasourceType: ''
  }
}

// extract metadata info we care about from datasourceSrv.getList() call
export const getDatasourceMetadata = (data: Array<DataSourceInstanceSettings<DataSourceJsonData>>) => {
  const metas: DatasourceMetadata[] = []

  for (const ds of data) {
    if (ds.type && ds.type.startsWith('opennms-')) {
      const meta = {
        name: ds.name,
        id: ds.id,
        uid: ds.uid,
        type: ds.type,
        version: ds.meta?.info?.version || '',
        pluginVersion: 8,
        datasourceType: undefined
      } as DatasourceMetadata

      if (meta.version) {
        const arr = meta.version.split('.')
        if (arr && arr.length > 0) {
          meta.pluginVersion = parseInt(arr[0], 10)
        }
      }

      const { datasourceType } = getDatasourceTypeFromPluginId(meta.type || '')

      if (datasourceType) {
        meta.datasourceType = datasourceType as DsType
        metas.push(meta)
      }
    }
  }

  return metas
}
