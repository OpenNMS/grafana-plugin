import React from 'react'

interface FieldDisplayProps {
    icon?: boolean
    color?: string
    children?: any
}
export const FieldDisplay: React.FC<FieldDisplayProps> = ({children, icon, color}) => {
    return (
        <div style={{
            backgroundColor: color ? color : 'rgb(34, 37, 43)',
            display: 'flex',
            alignItems: 'center',
            justifyContent:icon ? 'center' : 'flex-start',
            padding:icon ? '3px 8px' : '5px 8px',
            borderRadius: '3px',
            marginRight: icon ? 0 : '12px',
            minWidth: icon ? '0' : '160px',
            textAlign: 'left'
        }}>{children}</div>
    )
}
