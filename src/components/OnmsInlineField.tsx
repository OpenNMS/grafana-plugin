import React from 'react'
import { InlineField } from '@grafana/ui'

interface InlineFieldProps {
    label: string;
    children: React.ReactElement<any, string | React.JSXElementConstructor<any>> & React.ReactNode
}

export const OnmsInlineField: React.FC<InlineFieldProps> = ({ children, ...props }) => {
    return (
        <>
            <style>
                {
                    `
                        .onms-inline-field > label{
                            width:160px;
                        }
                    `
                }
            </style>
            <InlineField {...props} className='onms-inline-field'>
                {children}
            </InlineField>
        </>
    )
}
