import { InlineField } from '@grafana/ui'
import React from 'react'
interface InlineFieldProps {
    label: string;
    children: React.ReactElement<any, string | React.JSXElementConstructor<any>> & React.ReactNode
}
export const HelmInlineField: React.FC<InlineFieldProps> = ({ children, ...props }) => {
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
