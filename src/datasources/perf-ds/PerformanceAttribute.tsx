import { SelectableValue } from '@grafana/data';
import {
    Segment,
    SegmentAsync,
    SegmentInput
} from '@grafana/ui'
import { SegmentSectionWithIcon } from 'components/SegmentSectionWithIcon';
import React, { useState, useEffect } from 'react'
import { PerformanceAttributeItemState, PerformanceAttributeState } from './types'
import { OnmsResourceDto } from '../../lib/api_types'

export interface PerformanceAttributesProps {
    performanceAttributeState: PerformanceAttributeState
    updateQuery: Function
    loadNodes: (query?: string | undefined) => Promise<Array<SelectableValue<PerformanceAttributeItemState>>>
    loadResourcesByNode: (value: any) => Promise<OnmsResourceDto[]>
    loadAttributesByResourceAndNode: Function
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
        // Note: this could result in invalid queries if not all parameters have been selected.
        // However, not updating the query results in old parameters still being used. This way,
        // query will always be in sync with UI
        if (performanceState) {
            updateQuery(performanceState)
        }

    // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [performanceState])

    const setPerformanceStateProperty = (propertyName: string, propertyValue: unknown) => {
        setPerformanceState({...performanceState, [propertyName]: propertyValue})
    }

    const setPerformanceStateNode = async (propertyValue: unknown) => {
      // If node changed, need to repopulate the resource part of the query with resources
      // for the newly selected node.
      // Attempt to find the same resource label as existing; otherwise the new node
      // doesn't have the same resources, so clear them
      const node = propertyValue as PerformanceAttributeItemState

      const resourceOptions: OnmsResourceDto[] = await loadResourcesByNode(node.id || node.label)
      const existingLabel = performanceState.resource.label
      const resource = (existingLabel && resourceOptions.filter(r => r.label === existingLabel)?.[0]) || {}

      const state = {
        ...performanceState,
        node,
        resource
      } as PerformanceAttributeState

      setPerformanceState(state)
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
                      (async () => {
                        await setPerformanceStateNode(value)
                      })()
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
                        onChange={(value) => { setPerformanceStateProperty('resource', value) }}
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
