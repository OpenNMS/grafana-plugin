import { DsType } from './types'

// add 'name', '$name' and '${name}' variations
export const addVariationsToMap = (varName: string, dsType: DsType,  datasourceMap: Map<string,DsType>) => {
  const rawName = varName.replace(/[${}]/gi, '')

  datasourceMap.set(rawName, dsType)
  datasourceMap.set('$' + rawName, dsType)
  datasourceMap.set('${' + rawName + '}', dsType)
}

// returns a string which is one of the DsTypes or else empty string, and
// whether this is a "legacy" datasource or not (legacy is version < 9)
export const getDatasourceTypeFromPluginId = (pluginId: string) => {
  const legacyMatch = pluginId.match(/^grafana-plugin-([^-]+)-datasource$/i)

  if (legacyMatch && legacyMatch.length > 0) {
    return {
      isLegacy: true,
      datasourceType: legacyMatch[1]
    }
  }

  const m = pluginId.match(/^opennms-([^-]+)-datasource$/i)

  if (m && m.length > 0) {
    return {
      isLegacy: false,
      datasourceType: m[1]
    }
  }

  return {
    isLegacy: false,
    datasourceType: ''
  }
}

export const getDashboardTitle = (json: string) => {
  try {
    const dashboard = JSON.parse(json)
    return dashboard?.title || ''
  } catch {
  }

  return ''
}
