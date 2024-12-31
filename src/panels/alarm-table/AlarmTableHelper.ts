import { DataFrame, Field } from '@grafana/data'

export const getAlarmIdFromFields = (fields: Field[], index: number) => {
  return fields.find((d) => d.name === 'ID')?.values.get(index)
}

// given an array of row elements (divs in the Alarm Table), return the alarmIds 
// associated with the rows, in row order
// rows can be generated via something like: table.current?.querySelectorAll('.table-body div[role="row"]')
export const getAlarmIdsForRows = (rows: Element[], frame: DataFrame) => {
  return rows.map(row => getAlarmIdFromRow(row, frame))
}

/**
 * Find the current 0-based column index of the Alarm ID field.
 * This should be the frame after column including/exclusion and column sorting have been applied. 
 */
export const getColumnIndexOfAlarmId = (frame: DataFrame) => {
  return frame.fields.findIndex(f => f.name === 'ID')
}

/**
 * Get the Alarm ID from the table cell HTMLElement.
 */
export const getAlarmIdFromRow = (row: Element, frame: DataFrame) => {
  // const row = cell.parentElement
  const columnIndex = getColumnIndexOfAlarmId(frame)

  if (row && columnIndex >= 0) {
    const dataIndexCell = row?.childNodes?.[columnIndex] || null
    const text = dataIndexCell?.textContent

    const alarmId = Number(text)
    return Number.isInteger(alarmId) ? alarmId : -1
  }

  return -1
}

/**
 * Get the Alarm ID from the table cell Element.
 */
export const getAlarmIdFromCell = (cell: Element, frame: DataFrame) => {
  const row = cell.parentElement

  return row ? getAlarmIdFromRow(row, frame) : -1
}
