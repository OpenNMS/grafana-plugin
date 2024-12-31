import { MutableRefObject, useState, useEffect } from 'react'
import {
  ArrayVector,
  DataFrame,
  Field
} from '@grafana/data'
import cloneDeep from 'lodash/cloneDeep'
import { AlarmTableColumnSizeItem, AlarmTableOptionsState } from '../AlarmTableTypes'

/**
 * Adds customizations to the series data before handing it to the Table display component.
 * Filters and sorts columns, adds cell background color, etc.
 * @param oldProperties DataFrame / series data
 * @param alarmTable Alarm Table Panel options - the 'alarmTable' from AlarmTableControlProps
 */
export const useAlarmProperties = (filteredData: MutableRefObject<DataFrame>, oldProperties: DataFrame, alarmTable: AlarmTableOptionsState) => {
  const [page, setPage] = useState(1)
  const [totalPages, setTotalPages] = useState(0)

  // create map of column names to override width, if the Alarm Table Column Sizes option is enabled
  const createColumnSizeMap: () => Map<string,number> = () => {
    const columnSizeMap = new Map<string,number>()

    if (alarmTable.alarmTableColumnSizes?.active) {
      (alarmTable.alarmTableColumnSizes?.columnSizes as AlarmTableColumnSizeItem[])?.forEach(col => {
        columnSizeMap.set(col.fieldName, col.width)
      })
    }

    return columnSizeMap
  }

  const isAlarmDataFrame = (frame: DataFrame) => {
    const hasEntityMetadata = !!(frame.meta && (frame.meta as any).entity_metadata)

    return frame && hasEntityMetadata && frame.name && frame.name === 'alarms'
  }

  const addSeverityBackgroundColor = (frame: DataFrame) => {
    frame.fields.forEach((field) => {
      if (field.name === 'Severity') {
        field.config.custom = Object.assign(field.config.custom || {}, { displayMode: 'color-background' })
      }
    })
  }

  // Filter our columns according to our configured approved fields.
  const getConfiguredFields = (fields: Field[]) => {
    return fields.filter((fil) => {
      let shouldIncludeThisField = true

      if (alarmTable?.alarmTableData) {
        shouldIncludeThisField = !!alarmTable?.alarmTableData.columns?.find((col) => col.label === fil.name)
      }

      return shouldIncludeThisField
    })
  }

  // Sort our columns based on the user provided order
  const getSortedColumns = (fields: Field[]) => {
    return fields.sort((f1, f2) => {
      const colIndex1 = alarmTable?.alarmTableData?.columns?.findIndex((col) => col.label === f1.name)
      const colIndex2 = alarmTable?.alarmTableData?.columns?.findIndex((col) => col.label === f2.name)
      return colIndex1 - colIndex2
    })
  }

  const addCustomFieldData = (fields: Field[], columnSizeMap: Map<string, number>) => {
    fields.forEach((field) => {
      if (columnSizeMap.has(field.name)) {
        field.config.custom = Object.assign(field.config.custom || {}, { width: columnSizeMap.get(field.name) })
      }
    })
  }

  useEffect(() => {
    if (!oldProperties || !oldProperties.fields || !oldProperties.fields.length) {
      return
    }

    const filteredProps = cloneDeep(filteredData.current) as DataFrame
    const totalRows = filteredProps.fields[0].values.length
    const rowsPerPage = Number(alarmTable.alarmTablePaging?.rowsPerPage || 10)
    const columnSizeMap = createColumnSizeMap()

    if (isAlarmDataFrame(filteredProps)) {
      // Allow background color for severity column.
      if (alarmTable?.alarmTableAlarms?.styleWithSeverity?.value === 1) {
        addSeverityBackgroundColor(filteredProps)
      } 

      // Filter our columns according to our configured approved fields.
      filteredProps.fields = getConfiguredFields(filteredProps.fields)

      // Sort our columns based on the user provided order
      filteredProps.fields = getSortedColumns(filteredProps.fields)

      // Make any custom column width overrides. Underlying 'react-table' will use this custom column width
      if (columnSizeMap.size) {
        addCustomFieldData(filteredProps.fields, columnSizeMap)
      }

      // start/end index of row data to display
      const start = (page - 1) * rowsPerPage
      const end = start + rowsPerPage

      // set which rows of the data we display, based on pagination
      if (rowsPerPage > 0 && totalRows > rowsPerPage) {
        // field.values is an ArrayVector of rows of data to display for the given field (column)
        filteredProps.fields = filteredProps.fields.map((field: Field) => {
          // field.values is a Vector<any>, safest to call 'toArray()'
          // rather than assume it's an ArrayVector with a 'buffer' field
          const values = field.values.toArray()
          const sliced = values.length > start ? values.slice(start, end) : []
          field.values = new ArrayVector(sliced)

          return field
        })

        filteredProps.length = filteredProps.fields[0]?.values.length || 0
      } else {
        filteredProps.length = totalRows
      }

      filteredData.current = filteredProps
      setTotalPages(Math.ceil(totalRows / rowsPerPage))
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [
    oldProperties,
    page,
    alarmTable?.alarmTableData,
    alarmTable?.alarmTableData?.columns,
    alarmTable.alarmTablePaging?.rowsPerPage,
    alarmTable?.alarmTableAlarms?.styleWithSeverity,
    alarmTable?.alarmTableColumnSizes
  ])

  useEffect(() => {
    const scrollView = document.querySelector('.scroll .scrollbar-view')

    if (alarmTable?.alarmTablePaging?.scroll) {
      scrollView?.classList.remove('no-scroll')
    } else {
      scrollView?.classList.add('no-scroll')
    }
  }, [alarmTable?.alarmTablePaging?.scroll])

  return { page, setPage, totalPages }
}
