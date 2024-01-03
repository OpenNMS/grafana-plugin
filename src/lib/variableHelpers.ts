import { getTemplateSrv } from '@grafana/runtime'
import { trimChar, isString } from 'lib/utils'

export const getTemplateVariables = () => {
  const ts = getTemplateSrv()
  return ts.getVariables()
}

export const isTemplateVariable = (property: { id?: string, label?: string }) => {
  const ts = getTemplateSrv()

  return (property?.label && ts.containsTemplate(property.label)) ||
    (isString(property) && ts.containsTemplate(String(property)))
}

export const getTemplateVariable = (property?: { label?: string } | string) => {
  const ts = getTemplateSrv()
  let result = '' 

  if (property) {
    if (property.hasOwnProperty('id')) {
      result = property['id']
    } else if (property.hasOwnProperty('label')) {
      result = trimChar(ts.replace(property['label']), '{', '}')
    } else if (isString(property) && ts.containsTemplate(String(property))) {
      result = trimChar(ts.replace(String(property)), '{', '}')
    }
  }
  return result
}
