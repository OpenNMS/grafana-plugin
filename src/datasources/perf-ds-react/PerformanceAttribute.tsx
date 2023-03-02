import { SelectableValue } from '@grafana/data';
import {
    Segment,
    SegmentAsync,
    SegmentInput
} from '@grafana/ui'
import { SegmentSectionWithIcon } from 'components/SegmentSectionWithIcon';
import React, { useState, useEffect } from 'react'
import { PerformanceAttributeItemState, PerformanceAttributeState } from './types'

export interface PerformanceAttributesProps {
    performanceAttributeState: PerformanceAttributeState;
    updateQuery: Function;
    loadNodes: (query?: string | undefined) => Promise<Array<SelectableValue<PerformanceAttributeItemState>>>;
    loadResourcesByNode: Function;
    loadAttributesByResourceAndNode: Function;
}

export const defaultPerformanceState: PerformanceAttributeState = {
    node: { id: '' },
    resource: { id: '' },
    attribute: { name: '' },
    subAttribute: '',
    fallbackAttribute: { name: '' },
    aggregation: {},
    label: ''
}

export const PerformanceAttribute: React.FC<PerformanceAttributesProps> = ({
    performanceAttributeState,
    updateQuery,
    loadNodes,
    loadResourcesByNode,
    loadAttributesByResourceAndNode
}) => {

    const [performanceState, setPerformanceState] = useState<PerformanceAttributeState>(performanceAttributeState)

    useEffect(() => {
        if (performanceState &&
            performanceState.attribute &&
            (performanceState.node.id || performanceState.node.label) &&
            (performanceState.resource.id || performanceState.resource.label)) {
            updateQuery(performanceState);
        }
    // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [performanceState])

    const setPerformanceStateProperty = (propertyName: string, propertyValue: unknown) => {
        setPerformanceState({...performanceState, [propertyName]: propertyValue})
    }

    return (
        <>
            <div className='spacer' />
            <SegmentSectionWithIcon label='Node' icon='tree'>
                <SegmentAsync
                    value={performanceState?.node}
                    placeholder='Select Node'
                    loadOptions={loadNodes}
                    onChange={(value) => {
                        setPerformanceStateProperty('node', value);
                    }}
                />
            </SegmentSectionWithIcon>

            <div className='spacer' />
            {
                (performanceState?.node?.id || performanceState?.node?.label) &&

                <SegmentSectionWithIcon label='Resource' icon='leaf'>
                    <SegmentAsync
                        value={performanceState?.resource}
                        placeholder='Select Resource'
                        loadOptions={() => loadResourcesByNode(performanceState?.node?.id || performanceState?.node?.label)}
                        onChange={(value) => {
                            setPerformanceStateProperty('resource', value);
                        }}
                    />
                </SegmentSectionWithIcon>
            }
            <div className='spacer' />
            {
                (performanceState?.node?.id || performanceState?.node?.label) &&
                (performanceState?.resource?.id || performanceState?.resource?.label) &&
                <>
                    <SegmentSectionWithIcon label='Attribute' icon='tag'>
                        <SegmentAsync
                            value={performanceState?.attribute}
                            placeholder='Select Attribute'
                            loadOptions={() => loadAttributesByResourceAndNode(performanceState?.node, performanceState?.resource)}
                            onChange={(value) => {
                                setPerformanceStateProperty('attribute', value)
                            }}
                        />
                    </SegmentSectionWithIcon>
                    <div className='spacer' />
                    <SegmentSectionWithIcon label='Sub-Attribute' icon='flash'>
                        <SegmentInput
                            value={performanceState?.subAttribute || ''}
                            placeholder='Sub-Attribute'
                            onChange={(value) => {
                                setPerformanceStateProperty('subAttribute', value)
                            }}
                        />
                    </SegmentSectionWithIcon>
                    <div className='spacer' />
                    <SegmentSectionWithIcon label='Fallback Attribute' icon='tag'>
                        <SegmentAsync
                            value={performanceState?.fallbackAttribute}
                            placeholder='Fallback Attribute'
                            loadOptions={() => loadAttributesByResourceAndNode(performanceState?.node, performanceState?.resource)}
                            onChange={(value) => {
                                setPerformanceStateProperty('fallbackAttribute', value)
                            }}
                        />
                    </SegmentSectionWithIcon>
                    <div className='spacer' />
                    <SegmentSectionWithIcon label='Aggregation' icon='calendar'>
                        <Segment
                            value={performanceState?.aggregation}
                            placeholder='Aggregation'
                            options={[{ label: 'Average' }, { label: 'Min' }, { label: 'Max' }, { label: 'Last' }]}
                            onChange={(value) => {
                                setPerformanceStateProperty('aggregation', value)
                            }}
                        />
                    </SegmentSectionWithIcon>
                </>
            }
            <div className='spacer' />
            <SegmentSectionWithIcon label='Label' icon='font'>
                <SegmentInput
                    value={performanceState?.label}
                    placeholder='Series Label'
                    onChange={(value) => setPerformanceStateProperty('label', value)}
                />
            </SegmentSectionWithIcon>
        </>
    )
}
