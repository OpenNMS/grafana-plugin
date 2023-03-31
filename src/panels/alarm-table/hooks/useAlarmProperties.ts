import { useState, useEffect } from 'react'
import { ArrayVector } from '@grafana/data'
import { cloneDeep } from 'lodash'

export const useAlarmProperties = (oldProperties, alarmTable) => {

    const [filteredPropState, setFilteredProps] = useState(cloneDeep(oldProperties))
    const [page, setPage] = useState(1)
    const [totalPages, setTotalPages] = useState(0)

    useEffect(() => {
        const filteredProps = cloneDeep(oldProperties)
        const totalRows = filteredProps.fields[0].values.length
        const rowsPerPage = Number(alarmTable.alarmTablePaging?.rowsPerPage || 10)

        if (filteredProps && filteredProps.meta?.entity_metadata && filteredProps.name && filteredProps.name === 'alarms') {
            // Allow background color for severity column.
            if (alarmTable?.alarmTableAlarms?.styleWithSeverity?.value === 1) {
                filteredProps.fields = filteredProps.fields.map((field) => {
                    if (field.name === 'Severity') {
                        field.config.custom = { displayMode: 'color-background' }
                    }
                    return field
                })
            } 

            // Filter our columns according to our configured approved fields.
            filteredProps.fields = filteredProps.fields.filter((fil) => {
                let shouldIncludeThisField = true

                if (alarmTable?.alarmTableData) {
                    shouldIncludeThisField = !!alarmTable?.alarmTableData.columns?.find((col) => col.label === fil.name)
                }

                return shouldIncludeThisField
            })

            //Sort our columns based on the user provided order
            filteredProps.fields = filteredProps.fields.sort((f1, f2) => {
                const colIndex1 = alarmTable?.alarmTableData?.columns?.findIndex((col) => col.label === f1.name)
                const colIndex2 = alarmTable?.alarmTableData?.columns?.findIndex((col) => col.label === f2.name)
                return colIndex1 - colIndex2
            })

            if (rowsPerPage > 0 && totalRows > rowsPerPage) {
                const myPage = page

                filteredProps.fields = filteredProps.fields.map((field) => {
                    const oldValues = [...field.values.buffer]
                    const start = (myPage - 1) * rowsPerPage
                    const end = start + rowsPerPage
                    const spliced = oldValues.splice(start, end)
                    field.values = new ArrayVector(spliced) 
                    return field
                })

                filteredProps.length = filteredProps.fields[0]?.values.length || 0
            } else {
                filteredProps.length = totalRows
            }

            setFilteredProps(filteredProps)
            setTotalPages(Math.ceil(totalRows / rowsPerPage))
        }

    }, [alarmTable?.alarmTableData, page, alarmTable.alarmTablePaging?.rowsPerPage, oldProperties,alarmTable?.alarmTableAlarms?.styleWithSeverity])

    useEffect(() => {
        const scrollView = document.querySelector('.scroll .scrollbar-view')
        if (alarmTable?.alarmTablePaging?.scroll) {
            scrollView?.classList.remove('no-scroll')
        } else {
            scrollView?.classList.add('no-scroll')
        }
    }, [alarmTable?.alarmTablePaging?.scroll])

    return { filteredProps: filteredPropState, page, setPage, totalPages }
}
