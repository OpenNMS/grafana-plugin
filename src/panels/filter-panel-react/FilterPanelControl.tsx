import { PanelProps } from '@grafana/data'
import { HorizontalGroup, Input, Select } from '@grafana/ui'
import { FieldDisplay } from 'components/FieldDisplay'
import { saveFilterPanel } from 'lib/localStorageService'
import React, { useEffect } from 'react'
import { FilterControlProps } from './FilterPanelTypes'

export const FilterPanelControl: React.FC<PanelProps<FilterControlProps>> = (props) => {

    const getSelectOptions = (filter) => {
        // TODO: Replace these fake values with the real ones when we can get them from metricfindquery
        return props?.options?.filterEditor?.properties?.[filter.attribute.value?.id].values ||
            [{ value: "1", label: '1' }, { value: "2", label: '2' }]
    }

    const selectChanged = () => {
        //TODO: When these values are real, store this selection.
    }


    useEffect(() => {
        if (props.options.filterEditor) {
            saveFilterPanel(props.options.filterEditor)
        }
    }, [props.options.filterEditor])
    return (
        <HorizontalGroup align='flex-start'>
            {props.options?.filterEditor?.activeFilters.map((filter, index) => {

                return (
                    <HorizontalGroup key={index}>
                        <FieldDisplay>{props.options.filterEditor.altColumnLabels[index] || filter.attribute.label}</FieldDisplay>
                        {props.options.filterEditor.filterSelectionTypes[index]?.label === 'Text' ?
                            <Input />
                            : <Select
                                isMulti={props.options.filterEditor.filterSelectionTypes[index]?.label === 'Multi'}
                                options={getSelectOptions(filter)}
                                onChange={selectChanged}></Select>
                        }
                    </HorizontalGroup>
                )
            })}
        </HorizontalGroup>
    )
}
