import { SelectableValue } from '@grafana/data';
import { Segment, SegmentAsync, SegmentInput } from '@grafana/ui'
import { SegmentSectionWithIcon } from 'components/SegmentSectionWithIcon';
import React, { useState, useEffect } from 'react'

export interface PerformanceAttributesProps {
    updateQuery: Function;
    loadNodes: (query?: string | undefined) => Promise<Array<SelectableValue<{id: string}>>>;
    loadResourcesByNodeId: Function;
    loadAttributesByResourceAndNode: Function;
}

export interface PerformanceAttributeState {
    node: {id: string};
    resource: {id: string};
    attribute: {name: string};
    subAttribute: string | number;
    fallbackAttribute: {name: string};
    aggregation: unknown;
    label: string;
}

export const defaultPerformanceState = {
    node: {id:''},
    resource: {id: ''},
    attribute: {name: ''},
    subAttribute: '',
    fallbackAttribute: { name: ''},
    aggregation: {},
    label: ''
}

export const PerformanceAttribute: React.FC<PerformanceAttributesProps> = ({
    updateQuery,
    loadNodes,
    loadResourcesByNodeId,
    loadAttributesByResourceAndNode
}) => {

    const [performanceState, setPerformanceState] = useState<PerformanceAttributeState>(defaultPerformanceState)

 
    useEffect(() => {
        if (performanceState.resource.id && performanceState.node.id && performanceState.attribute){
            updateQuery(performanceState);
        }
    // eslint-disable-next-line react-hooks/exhaustive-deps
    },[performanceState])

    const setPerformanceStateProperty = (propertyName: string, propertyValue: unknown) => {
        setPerformanceState({...performanceState,[propertyName]:propertyValue})
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
                        setPerformanceStateProperty('node',value);
                    }}
                />
            </SegmentSectionWithIcon>
            <div className='spacer' />

            {
                performanceState?.node?.id &&

                <SegmentSectionWithIcon label='Resource' icon='leaf'>
                    <SegmentAsync
                        value={performanceState?.resource}
                        placeholder='Select Resource'
                        loadOptions={() => loadResourcesByNodeId(performanceState?.node?.id)}
                        onChange={(value) => {
                            setPerformanceStateProperty('resource',value);
                        }}
                    />
                </SegmentSectionWithIcon>
            }
            <div className='spacer' />
            {
                performanceState?.node?.id && performanceState?.resource?.id &&
                <>
                    <SegmentSectionWithIcon label='Attribute' icon='tag'>
                        <SegmentAsync
                            value={performanceState?.attribute}
                            placeholder='Select Attribute'
                            loadOptions={() => loadAttributesByResourceAndNode(performanceState?.node, performanceState?.resource)}
                            onChange={(value) => {
                                setPerformanceStateProperty('attribute',value)
                            }}
                        />
                    </SegmentSectionWithIcon>
                    <div className='spacer' />
                    <SegmentSectionWithIcon label='Sub-Attribute' icon='flash'>
                        <SegmentInput
                            value={performanceState?.subAttribute}
                            placeholder='Sub-Attribute'
                            onChange={(value) => {
                                setPerformanceStateProperty('subAttribute',value)
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
                                setPerformanceStateProperty('fallbackAttribute',value)
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
                                setPerformanceStateProperty('aggregation',value)
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
                    onChange={(value) => setPerformanceStateProperty('label',value)}
                />
            </SegmentSectionWithIcon>
        </>
    )
}
