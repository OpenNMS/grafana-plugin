import { getSourceDatasourceInfo } from './datasources'
import { updateEntityQuery } from './entityDs'
import { convertFilterPanels } from './filterPanel'
import { updateFlowQuery } from './flowDs'
import { updatePerformanceQuery } from './performanceDs'
import { DatasourceMetadata, DsType } from './types'

// Parse Dashboard panels
// If panel has a legacy datasource, convert the query to use the new schema
export const parsePanels = (panels: any[], datasourceMap: Map<string, DsType>, dsMetas: DatasourceMetadata[],
  unhideAllQueries: boolean) => {
  let convertedPanels: any[] = convertPanelDatasources(panels, datasourceMap, dsMetas, unhideAllQueries)

  convertedPanels = convertFilterPanels(convertedPanels, datasourceMap, dsMetas)

  return convertedPanels
}

const convertPanelDatasources = (panels: any[], datasourceMap: Map<string, DsType>, dsMetas: DatasourceMetadata[],
  unhideAllQueries: boolean) => {
  const convertedPanels: any[] = []

  for (const p of panels) {
    const panel = { ...p }

    // p.datasource could be:
    // - a template variable like '$datasource' which points to an OpenNMS DS
    //    in which case we leave as-is
    // - an object like { type, uid }, which points to an OpenNMS DS, in which case we update it to version 9
    // - either of those which points to a non-OpenNMS DS, in which case leave as-is
    // - empty/null/undefined, in which case DS should be in the individual targets, leave as-is
    const panelDsInfo = getSourceDatasourceInfo(panel, datasourceMap)

    if (panelDsInfo.isOpenNmsDatasource && !panelDsInfo.isTemplateVariable && panelDsInfo.datasourceType) {
      const panelDsMeta = dsMetas.find(d => d.datasourceType === panelDsInfo.datasourceType && d.pluginVersion === 9)

      if (panelDsMeta) {
        panel.datasource = {
          type: panelDsMeta.type,
          uid: panelDsMeta.uid
        }
      }
    }

    if (p.targets) {
      const targets: any[] = []

      for (const t of p.targets) {
        let updated = { ...t }

        const targetDsInfo = getSourceDatasourceInfo(t, datasourceMap)
        const isOpenNmsDatasource = panelDsInfo.isOpenNmsDatasource || targetDsInfo.isOpenNmsDatasource
        const dsType = panelDsInfo.datasourceType || targetDsInfo.datasourceType

        if (isOpenNmsDatasource) {
          switch (dsType) {
            case 'entity':
              updated = updateEntityQuery(t)
              break
            case 'performance':
              updated = updatePerformanceQuery(t)
              break
            case 'flow':
              updated = updateFlowQuery(t)
              break
            default:
              break
          }

          if (unhideAllQueries) {
            updated.hide = false
          }

          if (targetDsInfo.isOpenNmsDatasource && !targetDsInfo.isTemplateVariable && targetDsInfo.datasourceType) {
            const targetDsMeta = dsMetas.find(d => d.datasourceType === targetDsInfo.datasourceType && d.pluginVersion === 9)

            if (targetDsMeta) {
              updated.datasource = {
                type: targetDsMeta.type,
                uid: targetDsMeta.uid
              }
            }
          }
        }

        targets.push(updated)
      }

      // TODO: if p.datasource is a pluginId

      panel.targets = targets
    }

    // recursively process panel panels
    if (p.panels) {
      panel.panels = convertPanelDatasources(p.panels, datasourceMap, dsMetas, unhideAllQueries)
    }

    convertedPanels.push(panel)
  }

  return convertedPanels
}
