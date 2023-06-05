import { getDataSourceSrv } from '@grafana/runtime'
import { getDatasourceMetadata } from './datasources'
import { parseInputs } from './inputs'
import { convertPanels } from './panels'
import { parseTemplating } from './templating'
import { DsType } from './types'

interface ConvertResponse {
  json: string
  isError: boolean
  errorMessage?: string
}

interface ConvertOptions {
  unhideAllQueries: boolean
  convertGraphToTimeSeries: boolean
}

export const dashboardConvert = (sourceJson: string, sourceVersion: string, targetVersion: string,
  dashboardTitle: string, options: ConvertOptions): ConvertResponse => {
  let source: any = {}
  
  const dsSrv = getDataSourceSrv()
  const datasources = dsSrv.getList()
  const dsMetas = getDatasourceMetadata(datasources)

  try {
    source = JSON.parse(sourceJson)
  } catch (e: any) {
    return {
      json: '',
      isError: true,
      errorMessage: `Error parsing source Json: ${e.message || '(unknown)'}`
    }
  }

  const dashboard: any = {
    ...source
  }

  // find all datasource aliases / template variables
  const datasourceMap = new Map<string,DsType>()
  const inputsArray = source['__inputs'] || []

  const parsedInputs = parseInputs(inputsArray, datasourceMap, dsMetas)
  dashboard['__inputs'] = parsedInputs

  const templating = source['templating'] || {}
  const parsedTemplating = parseTemplating(templating, datasourceMap, dsMetas)
  dashboard.templating = parsedTemplating

  const panels = source.panels || []
  const convertedPanels = convertPanels(panels, datasourceMap, dsMetas, options.unhideAllQueries, options.convertGraphToTimeSeries)
  dashboard.panels = convertedPanels

  // remove uid, Grafana will create a new unique one
  delete dashboard.uid

  if (dashboardTitle) {
    dashboard.title = dashboardTitle
  }

  dashboard.version = 1

  const dashboardJson = JSON.stringify(dashboard, null, 2)

  return {
    json: dashboardJson,
    isError: false
  }
}
