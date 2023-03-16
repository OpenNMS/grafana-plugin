import React, { useState, useEffect } from 'react'
import { SegmentInput } from '@grafana/ui';
import { SegmentSectionWithIcon } from 'components/SegmentSectionWithIcon';
import { PerformanceQuery } from './types'

export interface PerformanceExpressionProps {
    query: PerformanceQuery;
    updateQuery: Function;
}

export const PerformanceExpression: React.FC<PerformanceExpressionProps> = ({ query, updateQuery }) => {
    const [expression, setExpression] = useState<string | number>(query.expression || '')
    const [label, setLabel] = useState<string | number>(query.label || '')

    useEffect(() => {
        updateQuery(expression, label)
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [expression, label])

    return (
        <>
            <div className='spacer' />
            <SegmentSectionWithIcon label='Expression' icon='calendar'>
                <SegmentInput
                    value={expression}
                    placeholder='series expression'
                    onChange={(value) => {
                        if (!label) {
                            setLabel('expression' + query.refId)
                        }
                        setExpression(value);
                    }}
                />
            </SegmentSectionWithIcon>
            <div className='spacer' />
            <SegmentSectionWithIcon label='Label' icon='font'>
                <SegmentInput
                    value={label}
                    placeholder='series label'
                    onChange={(value) => {
                        setLabel(value);
                    }}
                />
            </SegmentSectionWithIcon>
        </>
    )
}
