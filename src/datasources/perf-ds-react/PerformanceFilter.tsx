import React, { useState, useEffect } from 'react'
import { SelectableValue } from '@grafana/data';
import { Segment, SegmentAsync, SegmentInput } from '@grafana/ui';
import { SegmentSectionWithIcon } from 'components/SegmentSectionWithIcon';

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

export const PerformanceFilter: React.FC<{
    updateQuery: Function,
    loadFilters: (query?: string | undefined) => Promise<Array<SelectableValue<FilterResponse>>>
}> = ({ updateQuery, loadFilters }) => {

    const [filter, setFilter] = useState<SelectableValue<FilterResponse>>({});
    const [filterState, setFilterState] = useState<Record<string, {value: any, filter: any}>>({});

    const updateFilterState = (propertyName: string, value: {value: any, filter: any}) => {
        setFilterState({ ...filterState, [propertyName]: value })
    }

    useEffect(() => {
        updateQuery(filter,filterState);
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [filterState, filter])

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
            { filter?.parameter?.map((param, index) => {
                return (
                    <>
                        <div className='spacer' />
                        <SegmentSectionWithIcon label={param.displayName} key={index} >
                            {param.type === 'boolean' &&
                                <Segment
                                    placeholder={param.key}
                                    value={filterState[param.displayName]?.value || false}
                                    onChange={(value: unknown) => {
                                        updateFilterState(param.displayName, {value,filter:param})
                                    }}
                                    options={[{ label: 'True', value: 'true'}, { label: 'False', value: 'false' }]}
                                />
                            }
                            {(param.type === 'double' || param.type === 'int' || param.type === 'long') &&
                                <SegmentInput
                                    type='number'
                                    value={filterState[param.displayName]?.value || ''}
                                    placeholder={param.key}
                                    onChange={(value) => {
                                        updateFilterState(param.displayName, {value,filter:param})
                                    }}
                                />
                            }

                            {
                                param.type !== 'double' && param.type !== 'boolean' && param.type !== 'int' && param.type !== 'long' &&

                                <SegmentInput
                                    value={filterState[param.displayName]?.value || ''}
                                    placeholder={param.key}
                                    onChange={(value) => {
                                        updateFilterState(param.displayName, {value,filter:param})
                                    }}
                                />}
                            <p style={{ margin: 0, paddingTop: 6, paddingLeft: 5 }}>
                                {param.description}
                            </p>
                        </SegmentSectionWithIcon>
                    </>
                )
            })}
        </>
    )
}
