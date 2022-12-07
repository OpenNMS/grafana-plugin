import { SegmentInput } from '@grafana/ui';
import { SegmentSectionWithIcon } from 'components/SegmentSectionWithIcon';
import React, { useState, useEffect } from 'react'

export const PerformanceExpression: React.FC<{ updateQuery: Function }> = ({ updateQuery }) => {
    const [expression, setExpression] = useState<string | number>('')
    const [label, setLabel] = useState<string | number>('')
    useEffect(() => {
        updateQuery(expression,label)
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [expression,label])
    return (
        <>
            <div className='spacer' />
            <SegmentSectionWithIcon label='Expression' icon='calendar'>
                <SegmentInput
                    value={expression}
                    placeholder='series expression'
                    onChange={(value) => {
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
