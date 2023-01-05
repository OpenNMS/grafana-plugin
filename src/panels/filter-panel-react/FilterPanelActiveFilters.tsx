import React, { useState, useEffect } from 'react'
import { Button, Label, SegmentInput, Select } from '@grafana/ui'
import { FieldDisplay } from 'components/FieldDisplay';
import { ActiveFilter } from './FilterPanelTypes';
import { SelectableValue } from '@grafana/data';

interface FilterPanelActiveFiltersProps {
    activeFilters: ActiveFilter[],
    setActiveFilters: Function,
    onChange: Function
}
export const FilterPanelActiveFilters: React.FC<FilterPanelActiveFiltersProps> = ({ activeFilters, setActiveFilters, onChange }) => {
    const [filterSelectionTypes, setFilterSelectionTypes] = useState<Array<SelectableValue<string>>>([]);
    const [altColumnLabels, setAltColumnLabels] = useState<string[]>([]);
    useEffect(() => {
        onChange({ altColumnLabels, filterSelectionTypes })
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [altColumnLabels, filterSelectionTypes])

    const updateFilterSelectionType = (value: SelectableValue<string>, index: number) => {
        setFilterSelectionTypes((oldTypes) => {
            const newTypes = [...oldTypes]
            if (value) {
                newTypes[index] = value;
            }
            return newTypes;
        })
    }

    const updateAltColumnLabels = (value: string | number, index: number) => {
        setAltColumnLabels((oldLabels) => {
            const newLabels = [...oldLabels]
            if (typeof value === 'string') {
                newLabels[index] = value;
            }
            return newLabels;
        })
        
    }

    const removeFieldRow = (index: number) => {
        setActiveFilters((oldFilters) => {
            const newFilters = [...oldFilters]
            newFilters.splice(index, 1)
            return newFilters;
        })
    }

    const moveFieldDown = (index: number) => {
        setActiveFilters((oldFilters) => {
            const newFilters = [...oldFilters]
            var stored = newFilters[index];
            newFilters.splice(index, 1);
            newFilters.splice(index + 1, 0, stored);
            return newFilters;
        })
    }

    const moveFieldUp = (index: number) => {
        setActiveFilters((oldFilters) => {
            const newFilters = [...oldFilters]
            var stored = newFilters[index];
            newFilters.splice(index, 1);
            newFilters.splice(index - 1, 0, stored);
            return newFilters;
        })
    }

    return (
        <>
            <style>
                {
                    `
                        .no-margin {
                            margin:0;
                        }
                    `
                }
            </style>
            {activeFilters.length > 0 &&
                <div style={{ marginBottom: '6px' }}>

                    <Label style={{ marginTop: 12, marginBottom: 6 }}>
                        Active Filters
                    </Label>
                    {activeFilters?.map((filter, index) => {
                        return (
                            <div style={{ marginBottom: 10 }} key={index}>
                                <div style={{ display: 'flex' }}>
                                    <FieldDisplay>{filter?.entity?.label}</FieldDisplay>
                                    <SegmentInput
                                        placeholder={filter?.attribute?.label}
                                        value={altColumnLabels[index]}
                                        onChange={(e) => updateAltColumnLabels(e, index)}
                                    />
                                    <div
                                        style={{
                                            display: 'flex',
                                            marginLeft: 'auto',
                                            columnGap: '12px'
                                        }}>
                                        <Select
                                            options={[
                                                { label: 'Single', value: 'single' },
                                                { label: 'Multi', value: 'multi' },
                                                { label: 'Text', value: 'text' }
                                            ]}
                                            onChange={(e) => updateFilterSelectionType(e, index)}
                                            value={filterSelectionTypes[index] || { label: 'Single', value: 'single' }}
                                        />
                                        <Button
                                            disabled={index === 0}
                                            style={{ backgroundColor: 'rgb(61, 113, 217)' }}
                                            onClick={() => moveFieldUp(index)}
                                        >
                                            <i className='fa fa-arrow-up' />
                                        </Button>
                                        <Button
                                            disabled={index === activeFilters.length - 1}
                                            style={{ backgroundColor: 'rgb(61, 113, 217)' }}
                                            onClick={() => moveFieldDown(index)}
                                        >
                                            <i className='fa fa-arrow-down' />
                                        </Button>
                                        <Button
                                            style={{ backgroundColor: '#AA0000' }}
                                            onClick={() => removeFieldRow(index)}
                                        >
                                            <i className='fa fa-trash' />
                                        </Button>
                                    </div>
                                </div>
                            </div>
                        )
                    })}
                </div>
            }
        </>
    )
}
