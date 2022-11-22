import { SelectableValue } from '@grafana/data';
import { Segment, SegmentAsync, SegmentInput, SegmentSection } from '@grafana/ui';
import { SegmentSectionWithIcon } from 'components/SegmentSectionWithIcon';
import React, { useState } from 'react'

export interface FilterItem {
    key: string,
    type: string,
    displayName: string,
    description: string,
    required: boolean,
    default: string
}
export interface FilterResponse extends FilterItem {
    parameter: FilterItem[]
}
export const PerformanceFilter: React.FC<{ updateQuery: Function, loadFilters: (query?: string | undefined) => Promise<Array<SelectableValue<FilterResponse>>> }> = ({ updateQuery, loadFilters }) => {

    const [filter, setFilter] = useState<SelectableValue<FilterResponse>>({});

    const [filterState, setFilterState] = useState<Record<string, string | number>>({});

    const updateFilterState = (propertyName: string, value: string | number) => {
        setFilterState({ ...filterState, [propertyName]: value })
    }
    return (
        <>
            <div className='spacer' />
            <SegmentSectionWithIcon label='Filter' icon='filter'>
                <SegmentAsync
                    value={filter}
                    placeholder='Select Filter'
                    loadOptions={loadFilters}
                    onChange={(value) => {
                        setFilter(value);
                    }}
                />
            </SegmentSectionWithIcon>
            {filter?.parameter?.map((param, index) => {
                return (
                    <>
                        <div className='spacer' />
                        <SegmentSection label={param.displayName} key={index} >
                            {param.type === 'boolean' &&
                                <Segment
                                    placeholder={param.key}
                                    value={filterState[param.displayName]}
                                    onChange={(value: unknown) => {
                                        updateFilterState(param.displayName, value as number)
                                    }}
                                    options={[{ label: 'True', value: 1 }, { label: 'False', value: 0 }]}
                                />
                            }
                            {(param.type === 'double' || param.type === 'int' || param.type === 'long') &&
                                <SegmentInput
                                    type='number'
                                    value={filterState[param.displayName]}
                                    placeholder={param.key}
                                    onChange={(value) => {
                                        updateFilterState(param.displayName, value)
                                    }}
                                />
                            }

                            {
                                param.type !== 'double' && param.type !== 'boolean' && param.type !== 'int' && param.type !== 'long' &&

                                <SegmentInput
                                    value={filterState[param.displayName]}
                                    placeholder={param.key}
                                    onChange={(value) => {
                                        updateFilterState(param.displayName, value)
                                    }}
                                />}
                            <p>
                                {param.description}
                            </p>
                        </SegmentSection>
                    </>
                )
            })}
        </>
    )

}
