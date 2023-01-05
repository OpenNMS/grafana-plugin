import { PanelProps } from '@grafana/data'
import { HorizontalGroup, Input, Select } from '@grafana/ui'
import { FieldDisplay } from 'components/FieldDisplay'
import React, { useEffect } from 'react'
import { FilterControlProps } from './FilterPanelTypes'

export const FilterControl: React.FC<PanelProps<FilterControlProps>> = (props) => {

    const getSelectOptions = (filter) => {
        return props?.options?.filterEditor?.properties?.[filter.attribute.value?.id].values || [{ value: "1", label: '1' }, { value: "2", label: '2' }]
    }

    const selectChanged = () => {

    }
    const getCircularReplacer = () => {
        const seen = new WeakSet();
        return (key, value) => {
            if (typeof value === "object" && value !== null) {
                if (seen.has(value)) {
                    return;
                }
                seen.add(value);
            }
            return value;
        };
    };

    useEffect(() => {
        if (props.options.filterEditor) {
            localStorage.setItem('opennms-helm-filter-panel', JSON.stringify(props.options.filterEditor, getCircularReplacer()))
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
                            : <Select isMulti={props.options.filterEditor.filterSelectionTypes[index]?.label === 'Multi'} options={getSelectOptions(filter)} onChange={selectChanged}></Select>}
                    </HorizontalGroup>
                )
            })}
        </HorizontalGroup>
    )
}
