import React, { useState, useEffect } from 'react'
import { SelectableValue } from '@grafana/data';
import { Segment, SegmentAsync, SegmentInput } from '@grafana/ui';
import { SegmentSectionWithIcon } from 'components/SegmentSectionWithIcon';
import { PerformanceQuery } from './types'

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

export interface PerformanceFilterProps {
    query: PerformanceQuery;
    updateQuery: Function;
    loadFilters: (query?: string | undefined) => Promise<Array<SelectableValue<FilterResponse>>>;
}

export const PerformanceFilter: React.FC<PerformanceFilterProps> = ({ query, updateQuery, loadFilters }) => {
    // TODO: Initialize these from 'query' prop
    const [filter, setFilter] = useState<SelectableValue<FilterResponse>>(query.filter);
    const [filterState, setFilterState] = useState<Record<string, {value: any, filter: any}>>(query.filterState);

    const updateFilterState = (propertyName: string, value: {value: any, filter: any}) => {
        setFilterState({ ...filterState, [propertyName]: value })
    }

    useEffect(() => {
        updateQuery(filter, filterState);

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
                                    value={filterState[param.key]?.value || false}
                                    onChange={(value: unknown) => {
                                        updateFilterState(param.key, { value, filter: param })
                                    }}
                                    options={[{ label: 'True', value: 'true'}, { label: 'False', value: 'false' }]}
                                />
                            }
                            {(param.type === 'double' || param.type === 'int' || param.type === 'long') &&
                                <SegmentInput
                                    type='number'
                                    value={filterState[param.key]?.value || ''}
                                    placeholder={param.key}
                                    onChange={(value) => {
                                        updateFilterState(param.key, { value, filter: param })
                                    }}
                                />
                            }
                            {(param.type !== 'double' && param.type !== 'boolean' && param.type !== 'int' && param.type !== 'long') &&
                                <SegmentInput
                                    value={filterState[param.key]?.value || ''}
                                    placeholder={param.key}
                                    onChange={(value) => {
                                        updateFilterState(param.key, { value, filter: param })
                                    }}
                                />}
                            <p style={{ margin: 0, paddingTop: 6, paddingLeft: 5 }}>
                                { param.description }
                            </p>
                        </SegmentSectionWithIcon>
                    </>
                )
            })}
        </>
    )
}
